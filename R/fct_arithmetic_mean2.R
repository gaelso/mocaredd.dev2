#' Calculate emissions and IPCC Tier 1 uncertainty from arithmetic means
#'
#' @description
#' Analytical counterpart of \code{fct_arithmetic_mean()}. Instead of Monte Carlo
#' simulations, activity data and emission factors are aggregated deterministically
#' and their uncertainty is propagated with the IPCC Tier 1 (first-order error
#' propagation) rules: relative uncertainties combine in quadrature for products
#' and absolute standard errors combine in quadrature for sums / differences. The
#' propagation is done numerically over the independent input variables (carbon
#' pool values, carbon fraction, degradation ratios and activity data), so
#' correlations introduced by shared inputs (e.g. a degraded land use that reuses
#' the intact carbon stock, or a global carbon fraction) are handled consistently.
#'
#' The result reports, for each reference / monitoring emission level and for each
#' emission reduction, the mean, its standard error, and the half-width uncertainty
#' \eqn{U\% = z \cdot se / |mean| \cdot 100}, where \eqn{z} is the two-sided normal
#' quantile for \code{setup$conf_level}.
#'
#' @param .checked_data The list returned by \code{fct_checkinput()}. Its
#'             \code{template_version} element drives version-specific behaviour and its
#'             \code{data} element supplies the \code{setup}, \code{time}, \code{area}
#'             and \code{carbon} tables.
#'
#' @return A list with elements \code{ER} (reference / monitoring levels and emission
#'         reductions with mean, se, U\% and confidence bounds), \code{emissions}
#'         (per time period, with uncertainty), \code{gg_emissions} (figure with
#'         confidence intervals) and \code{emissions_table} (a tidy tibble ready to
#'         be turned into a gt table by the app).
#'
#' @importFrom rlang .data
#'
#' @examples
#' library(mocaredd)
#'
#' path <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2")
#'
#' checked <- fct_checkinput(.path = path)
#'
#' res <- fct_arithmetic_mean2(.checked_data = checked)
#' res$ER
#'
#' @export
fct_arithmetic_mean2 <- function(.checked_data){

  ## !!! FOR TESTING ONLY
  # .checked_data <- fct_checkinput(.path = path)
  ## !!!

  setup  <- .checked_data$data$setup
  time   <- .checked_data$data$time
  area   <- .checked_data$data$area
  carbon <- .checked_data$data$carbon
  is_v2  <- isTRUE(.checked_data$template_version == 2)

  if (!"nb_years" %in% names(time)) {
    time <- time |> dplyr::mutate(nb_years = .data$year_end - .data$year_start + 1)
  }
  ad_annual <- isTRUE(setup$ad_annual)

  ## z value for the requested confidence level (two-sided)
  z <- stats::qnorm(1 - (1 - setup$conf_level) / 2)

  ##
  ## 1. Carbon formula and degradation handling (mirror fct_combine_mcs_E) ######
  ##
  carbon_pools <- if (is_v2) dplyr::filter(carbon, .data$c_element != "CF") else carbon

  to_intact <- function(lu_id) {
    out <- stringr::str_remove(lu_id, setup$dg_ext)
    if (is_v2) out <- stringr::str_replace(out, "[^A-Za-z]$", "")
    out
  }

  if (is_v2) {
    c_formula <- carbon_pools |>
      dplyr::summarise(c_el = list(c(.data$c_element)), c_un = list(c(.data$c_unit)),
                       .by = c("c_period", "c_lu_id")) |>
      dplyr::rowwise() |>
      dplyr::mutate(c_form = fct_make_formula(.c_el = .data$c_el, .c_unit = .data$c_un, .version = 2)) |>
      dplyr::ungroup()
  } else {
    c_formula <- carbon_pools |>
      dplyr::summarise(c_el = list(c(.data$c_element)), .by = c("c_period", "c_lu_id")) |>
      dplyr::rowwise() |>
      dplyr::mutate(c_form = fct_make_formula(.c_el = .data$c_el, .c_unit = setup$c_unit, .version = 1)) |>
      dplyr::ungroup()
  }
  form_of <- stats::setNames(c_formula$c_form, c_formula$c_lu_id)

  degrat_lu <- carbon_pools |> dplyr::filter(.data$c_element == "DG_ratio") |> dplyr::pull("c_lu_id")

  ## Pools affected by degradation. "ALL" degrades the whole stock; otherwise the
  ## excluded pools stay at their intact level (as in fct_combine_mcs_E).
  dg_pool_vec <- stringr::str_trim(stringr::str_split(stringr::str_trim(setup$dg_pool), ",")[[1]])
  dg_is_all   <- length(dg_pool_vec) == 1 && dg_pool_vec == "ALL"

  ## Carbon fraction (global). v2: dedicated 'carbon' row; v1: from setup.
  if (is_v2) {
    cf_row <- carbon |> dplyr::filter(.data$c_element == "CF")
    has_cf <- nrow(cf_row) > 0
    cf_mean <- if (has_cf) cf_row$c_value[1] else NA_real_
    cf_se   <- if (has_cf) cf_row$c_se[1]    else NA_real_
  } else {
    has_cf <- is.numeric(setup[["c_fraction"]]) && isTRUE(setup[["c_unit"]] == "DM")
    cf_mean <- if (has_cf) setup$c_fraction    else NA_real_
    cf_se   <- if (has_cf) setup$c_fraction_se else NA_real_
  }

  ##
  ## 2. Independent input variables (means and standard errors) #################
  ##
  ## Carbon pools, namespaced by land use so each is an independent input.
  pool_key  <- paste0(carbon_pools$c_lu_id, "..", carbon_pools$c_element)
  prim_mean <- stats::setNames(carbon_pools$c_value, pool_key)
  prim_se   <- stats::setNames(carbon_pools$c_se,    pool_key)
  ## Activity data, namespaced by transition.
  ad_key    <- paste0("AD..", area$trans_id)
  prim_mean <- c(prim_mean, stats::setNames(area$trans_area, ad_key))
  prim_se   <- c(prim_se,   stats::setNames(area$trans_se,   ad_key))
  ## Global carbon fraction.
  if (has_cf) { prim_mean["CF"] <- cf_mean; prim_se["CF"] <- cf_se }

  prim_keys <- names(prim_mean)

  ## Carbon stock of a land use, from a (possibly perturbed) input vector.
  C_of <- function(lu, pm) {
    if (lu %in% degrat_lu) {
      dg     <- pm[[paste0(lu, "..DG_ratio")]]
      intact <- to_intact(lu)
      c_int  <- C_of(intact, pm)
      if (dg_is_all) return(dg * c_int)
      ## Pools not affected by degradation keep their intact value.
      int_els <- carbon_pools$c_element[carbon_pools$c_lu_id == intact]
      excl    <- setdiff(int_els, dg_pool_vec)
      c_noDG  <- if (length(excl) > 0) sum(pm[paste0(intact, "..", excl)]) else 0
      return(dg * c_int + (1 - dg) * c_noDG)
    }
    els <- carbon_pools$c_element[carbon_pools$c_lu_id == lu]
    env <- as.list(stats::setNames(pm[paste0(lu, "..", els)], els))
    if (has_cf) env$CF <- pm[["CF"]]
    eval(parse(text = form_of[[lu]]), env)
  }

  ## Lookups used by the aggregation
  nb_years   <- stats::setNames(time$nb_years, time$period_no)
  ptype      <- stats::setNames(time$period_type, time$period_no)
  ref_types  <- unique(stats::na.omit(time$period_type[stringr::str_detect(time$period_type, "REF")]))
  mon_types  <- unique(stats::na.omit(time$period_type[stringr::str_detect(time$period_type, "MON")]))
  a_period   <- area$trans_period
  a_activity <- area$redd_activity
  a_li       <- area$lu_initial_id
  a_lf       <- area$lu_final_id
  a_id       <- area$trans_id

  ##
  ## 3. All scalar outputs as a function of the inputs #########################
  ##
  ## PN.<period>        annual emissions of a time period (all activities)
  ## PA.<period>.<act>  annual emissions of a time period for one REDD+ activity
  ## PT.<ptype>         annual emissions of a reference / monitoring period type
  ## ER.<montype>       emission reductions = REF - MON
  metrics <- function(pm) {
    Et <- vapply(seq_along(a_id), function(i) {
      pm[[paste0("AD..", a_id[i])]] * (C_of(a_li[i], pm) - C_of(a_lf[i], pm)) * 44 / 12
    }, numeric(1))
    Ey <- if (ad_annual) Et else Et / nb_years[a_period]

    out <- c()
    for (p in unique(a_period)) {
      sel <- a_period == p
      out[paste0("PN.", p)] <- sum(Ey[sel])
      for (act in unique(a_activity[sel])) {
        out[paste0("PA.", p, ".", act)] <- sum(Ey[sel & a_activity == act])
      }
    }
    for (pt in c(ref_types, mon_types)) {
      per <- names(ptype)[ptype %in% pt & !is.na(ptype)]
      Ty  <- sum(nb_years[per])
      sel <- a_period %in% per
      out[paste0("PT.", pt)] <- if (ad_annual) sum(Et[sel] * nb_years[a_period[sel]]) / Ty else sum(Et[sel]) / Ty
    }
    if (length(ref_types) >= 1) {
      refv <- out[[paste0("PT.", ref_types[1])]]
      for (m in mon_types) out[paste0("ER.", m)] <- refv - out[[paste0("PT.", m)]]
    }
    out
  }

  ##
  ## 4. IPCC Tier 1 propagation: numeric jacobian over the inputs ##############
  ##
  m0  <- metrics(prim_mean)
  keys_out <- names(m0)
  var <- stats::setNames(rep(0, length(m0)), keys_out)

  for (k in prim_keys) {
    sk <- prim_se[[k]]
    if (is.na(sk) || sk == 0) next
    h  <- max(abs(prim_mean[[k]]) * 1e-4, 1e-6)
    pp <- prim_mean; pp[k] <- prim_mean[[k]] + h
    pm <- prim_mean; pm[k] <- prim_mean[[k]] - h
    d  <- (metrics(pp)[keys_out] - metrics(pm)[keys_out]) / (2 * h)
    var <- var + (d * sk)^2
  }
  se_out <- sqrt(var)

  ## helper to fetch (mean, se, U%) for one output key
  stat_of <- function(key) {
    m <- unname(m0[key]); s <- unname(se_out[key])
    u <- if (is.na(m) || m == 0) NA_real_ else z * s / abs(m) * 100
    list(mean = m, se = s, U = u, lower = m - z * s, upper = m + z * s)
  }

  ##
  ## 5. Reference / monitoring levels and emission reductions ##################
  ##
  combi_rows <- c(
    stats::setNames(paste0("PT.", ref_types), ref_types),
    stats::setNames(paste0("PT.", mon_types), paste0("E-", mon_types)),
    stats::setNames(paste0("ER.", mon_types), paste0("ER-", mon_types))
  )
  ari_combi <- purrr::imap_dfr(combi_rows, function(key, label){
    s <- stat_of(key)
    dplyr::tibble(period_type = label, E = s$mean, E_se = s$se, E_U = s$U,
                  E_lower = s$lower, E_upper = s$upper)
  })

  ##
  ## 6. Per time period emissions (with uncertainty) ##########################
  ##
  period_meta <- time |>
    dplyr::select("period_no", "year_start", "year_end", "period_type") |>
    dplyr::arrange(.data$year_start)

  out_combi <- period_meta |>
    dplyr::rowwise() |>
    dplyr::mutate(
      E    = stat_of(paste0("PN.", .data$period_no))$mean,
      E_se = stat_of(paste0("PN.", .data$period_no))$se,
      E_U  = stat_of(paste0("PN.", .data$period_no))$U,
      years = ifelse(.data$year_start == .data$year_end, as.character(.data$year_start),
                     paste0(.data$year_start, "-", .data$year_end))
    ) |>
    dplyr::ungroup()

  ##
  ## 7. Figure: annual emissions per period with confidence intervals #########
  ##
  ref_mean <- stat_of(paste0("PT.", ref_types[1]))$mean
  out_yearly <- purrr::map(seq_len(nrow(period_meta)), function(i){
    r <- period_meta[i, ]
    s <- stat_of(paste0("PN.", r$period_no))
    dplyr::tibble(
      year        = r$year_start:r$year_end,
      E           = round(s$mean  / 1e6, 2),
      E_lower     = round(s$lower / 1e6, 2),
      E_upper     = round(s$upper / 1e6, 2),
      period_type = r$period_type,
      FREL        = round(ref_mean / 1e6, 2)
    )
  }) |> purrr::list_rbind()

  out_yearly_mon <- out_yearly |> dplyr::filter(stringr::str_detect(.data$period_type, "MON"))

  out_gg <- out_yearly |>
    ggplot2::ggplot(ggplot2::aes(x = .data$year)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$FREL), col = "pink", linewidth = 1) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$E_lower, ymax = .data$E_upper, colour = .data$period_type),
      width = 0.2, linewidth = 0.8
    ) +
    ggplot2::geom_point(ggplot2::aes(y = .data$E, colour = .data$period_type), size = 4) +
    ggplot2::geom_segment(
      data = out_yearly_mon,
      ggplot2::aes(xend = .data$year, y = .data$FREL, yend = .data$E),
      col = "limegreen", linewidth = 1,
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), ends = "both")
    ) +
    ggplot2::scale_colour_discrete(na.value = "gray50") +
    ggplot2::scale_x_continuous(breaks = min(out_yearly$year):max(out_yearly$year), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "bottom") +
    ggplot2::labs(x = "Years", y = "Emissions (MtCO2e/y)", color = "")

  ##
  ## 8. Result table (tidy data; gt is built in the server) ####################
  ##
  act_present <- unique(a_activity)
  tbl_period <- period_meta |>
    dplyr::rowwise() |>
    dplyr::transmute(
      grp   = "Emissions by time period",
      item  = dplyr::coalesce(.data$period_type, "No period"),
      years = ifelse(.data$year_start == .data$year_end, as.character(.data$year_start),
                     paste0(.data$year_start, "-", .data$year_end)),
      DF    = { k <- paste0("PA.", .data$period_no, ".DF"); if (k %in% keys_out) unname(m0[k]) else 0 },
      DG    = { k <- paste0("PA.", .data$period_no, ".DG"); if (k %in% keys_out) unname(m0[k]) else 0 },
      total = stat_of(paste0("PN.", .data$period_no))$mean,
      U_pct = stat_of(paste0("PN.", .data$period_no))$U
    ) |>
    dplyr::ungroup()

  tbl_summary <- ari_combi |>
    dplyr::transmute(
      grp   = "Reference, monitoring & emission reductions",
      item  = dplyr::case_when(
        stringr::str_detect(.data$period_type, "^ER-") ~ paste0("Reduction ",  stringr::str_remove(.data$period_type, "^ER-")),
        stringr::str_detect(.data$period_type, "^E-")  ~ paste0("Emissions ",  stringr::str_remove(.data$period_type, "^E-")),
        TRUE                                           ~ paste0("Reference level (", .data$period_type, ")")
      ),
      years = NA_character_, DF = NA_real_, DG = NA_real_,
      total = .data$E, U_pct = .data$E_U
    )

  emissions_table <- dplyr::bind_rows(tbl_period, tbl_summary)

  list(
    ER = ari_combi,
    emissions = out_combi,
    gg_emissions = out_gg,
    emissions_table = emissions_table
  )
}
