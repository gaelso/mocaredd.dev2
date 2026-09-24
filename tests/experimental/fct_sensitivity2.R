#' Sensitivity analysis of REDD+ emissions and emission reductions
#'
#' @description
#' Global sensitivity analysis of the emission / emission reduction estimates with
#' respect to the model inputs, using the same inputs as the simulation. The
#' variance of each output is decomposed into first-order contributions of the
#' independent inputs (activity data, carbon stocks, carbon fraction, degradation
#' ratios) with the delta method: for independent inputs and a first-order
#' expansion, \eqn{Var(Y) \approx \sum_i (\partial Y / \partial X_i)^2 \sigma_i^2},
#' and the normalised contribution \eqn{(\partial Y / \partial X_i \cdot \sigma_i)^2 / Var(Y)}
#' is the first-order Sobol index of input \eqn{i}. Contributions are summed within
#' REDD+ input groups. Emissions are additionally split between deforestation (DF)
#' and degradation (DG) when both are reported, each with its own uncertainty.
#'
#' See the "Sensitivity analysis" vignette for the rationale and the relationship
#' with variance-based (Sobol) sensitivity analysis.
#'
#' @param .checked_data The list returned by \code{fct_checkinput()}.
#'
#' @return A list with \code{variance} (first-order variance contributions of each
#'         input group to each output, in \%), \code{split} (emissions split by
#'         REDD+ activity with uncertainty), \code{gg_variance} (contribution bar
#'         chart) and \code{gg_split} (DF vs DG emissions bar chart).
#'
#' @importFrom rlang .data
#'
#' @examples
#' library(mocaredd)
#'
#' path <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2")
#' checked <- fct_checkinput(.path = path)
#'
#' sa <- fct_sensitivity2(.checked_data = checked)
#' sa$variance
#'
#' @export
fct_sensitivity2 <- function(.checked_data){
  
  ## .checked_data <- fct_checkinput(.path = path)
  
  setup  <- .checked_data$data$setup
  time   <- .checked_data$data$time
  area   <- .checked_data$data$area
  carbon <- .checked_data$data$carbon
  is_v2  <- isTRUE(.checked_data$template_version == 2)

  if (!"nb_years" %in% names(time)) {
    time <- time |> dplyr::mutate(nb_years = .data$year_end - .data$year_start + 1)
  }
  ad_annual <- isTRUE(setup$ad_annual)
  z <- stats::qnorm(1 - (1 - setup$conf_level) / 2)

  ##
  ## 1. Model wiring (identical to fct_arithmetic_mean2) #######################
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

  degrat_lu   <- carbon_pools |> dplyr::filter(.data$c_element == "DG_ratio") |> dplyr::pull("c_lu_id")
  dg_pool_vec <- stringr::str_trim(stringr::str_split(stringr::str_trim(setup$dg_pool), ",")[[1]])
  dg_is_all   <- length(dg_pool_vec) == 1 && dg_pool_vec == "ALL"

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

  pool_key  <- paste0(carbon_pools$c_lu_id, "..", carbon_pools$c_element)
  prim_mean <- stats::setNames(carbon_pools$c_value, pool_key)
  prim_se   <- stats::setNames(carbon_pools$c_se,    pool_key)
  ad_key    <- paste0("AD..", area$trans_id)
  prim_mean <- c(prim_mean, stats::setNames(area$trans_area, ad_key))
  prim_se   <- c(prim_se,   stats::setNames(area$trans_se,   ad_key))
  if (has_cf) { prim_mean["CF"] <- cf_mean; prim_se["CF"] <- cf_se }
  prim_keys <- names(prim_mean)

  C_of <- function(lu, pm) {
    if (lu %in% degrat_lu) {
      dg     <- pm[[paste0(lu, "..DG_ratio")]]
      intact <- to_intact(lu)
      c_int  <- C_of(intact, pm)
      if (dg_is_all) return(dg * c_int)
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

  nb_years   <- stats::setNames(time$nb_years, time$period_no)
  ptype      <- stats::setNames(time$period_type, time$period_no)
  ref_types  <- unique(stats::na.omit(time$period_type[stringr::str_detect(time$period_type, "REF")]))
  mon_types  <- unique(stats::na.omit(time$period_type[stringr::str_detect(time$period_type, "MON")]))
  a_period   <- area$trans_period
  a_activity <- area$redd_activity
  a_li       <- area$lu_initial_id
  a_lf       <- area$lu_final_id
  a_id       <- area$trans_id

  ## Outputs: reference / monitoring levels, emission reductions, and the same
  ## split by REDD+ activity (PTA.<ptype>.<activity>).
  metrics <- function(pm) {
    Et <- purrr::map_dbl(seq_along(a_id), function(i) {
      pm[[paste0("AD..", a_id[i])]] * (C_of(a_li[i], pm) - C_of(a_lf[i], pm)) * 44 / 12
    })
    out <- c()
    for (pt in c(ref_types, mon_types)) {
      per <- names(ptype)[ptype %in% pt & !is.na(ptype)]
      Ty  <- sum(nb_years[per])
      sel <- a_period %in% per
      w   <- if (ad_annual) nb_years[a_period[sel]] / Ty else 1 / Ty
      out[paste0("PT.", pt)] <- sum(Et[sel] * w)
      for (act in unique(a_activity[sel])) {
        s2 <- sel & a_activity == act
        w2 <- if (ad_annual) nb_years[a_period[s2]] / Ty else 1 / Ty
        out[paste0("PTA.", pt, ".", act)] <- sum(Et[s2] * w2)
      }
    }
    if (length(ref_types) >= 1) {
      refv <- out[[paste0("PT.", ref_types[1])]]
      for (m in mon_types) out[paste0("ER.", m)] <- refv - out[[paste0("PT.", m)]]
    }
    out
  }

  ##
  ## 2. Jacobian and per-input variance contributions ##########################
  ##
  m0 <- metrics(prim_mean)
  keys_out <- names(m0)
  ## contribution matrix: rows = outputs, cols = inputs, value = (dY/dX * se)^2
  contrib <- matrix(0, nrow = length(keys_out), ncol = length(prim_keys),
                    dimnames = list(keys_out, prim_keys))
  for (k in prim_keys) {
    sk <- prim_se[[k]]
    if (is.na(sk) || sk == 0) next
    h  <- max(abs(prim_mean[[k]]) * 1e-4, 1e-6)
    pp <- prim_mean; pp[k] <- prim_mean[[k]] + h
    pm <- prim_mean; pm[k] <- prim_mean[[k]] - h
    d  <- (metrics(pp)[keys_out] - metrics(pm)[keys_out]) / (2 * h)
    contrib[, k] <- (d * sk)^2
  }
  var_out <- rowSums(contrib)
  se_out  <- sqrt(var_out)

  ##
  ## 3. Variance decomposition by input group (first-order Sobol) ##############
  ##
  input_group <- function(k) {
    if (startsWith(k, "AD.."))        "Activity data"
    else if (k == "CF")               "Carbon fraction"
    else if (endsWith(k, "..DG_ratio")) "Degradation ratio"
    else                              "Carbon stock"
  }
  grp_vec <- vapply(prim_keys, input_group, character(1))

  target_keys <- c(paste0("PT.", ref_types), paste0("PT.", mon_types), paste0("ER.", mon_types))
  target_lab  <- c(ref_types, paste0("E-", mon_types), paste0("ER-", mon_types))

  variance <- purrr::imap_dfr(stats::setNames(target_keys, target_lab), function(key, lab){
    cvec <- contrib[key, ]
    tot  <- sum(cvec)
    dplyr::tibble(output = lab, group = grp_vec, contribution = cvec) |>
      dplyr::summarise(contribution = sum(.data$contribution), .by = "group") |>
      dplyr::mutate(output = lab,
                    contribution_pct = if (tot > 0) round(.data$contribution / tot * 100, 1) else NA_real_) |>
      dplyr::select("output", "group", "contribution_pct")
  })
  ## keep only groups that actually carry uncertainty somewhere
  keep_groups <- variance |> dplyr::summarise(s = sum(.data$contribution_pct, na.rm = TRUE), .by = "group") |>
    dplyr::filter(.data$s > 0) |> dplyr::pull("group")
  variance <- variance |> dplyr::filter(.data$group %in% keep_groups)

  ##
  ## 4. Emissions split by REDD+ activity (DF vs DG) with uncertainty ##########
  ##
  split_keys <- grep("^PTA\\.", keys_out, value = TRUE)
  split <- purrr::map_dfr(split_keys, function(key){
    parts <- strsplit(key, "\\.")[[1]]
    pt  <- parts[2]; act <- parts[3]
    m <- unname(m0[key]); s <- unname(se_out[key])
    dplyr::tibble(
      period_type = pt, activity = act, E = m, E_se = s,
      E_U = if (is.na(m) || m == 0) NA_real_ else z * s / abs(m) * 100,
      E_lower = m - z * s, E_upper = m + z * s
    )
  })

  ##
  ## 5. Figures ################################################################
  ##
  group_cols <- c("Activity data"     = "#4991B0",
                  "Carbon stock"      = "#77AB16",
                  "Carbon fraction"   = "#E1A100",
                  "Degradation ratio" = "#B0564C")

  gg_variance <- variance |>
    ggplot2::ggplot(ggplot2::aes(x = .data$output, y = .data$contribution_pct, fill = .data$group)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = group_cols, name = "Input group") +
    ggplot2::labs(x = NULL, y = "Contribution to variance (%)",
                  title = "First-order variance decomposition") +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "bottom")

  act_cols <- c("DF" = "#B0564C", "DG" = "#E1A100")
  gg_split <- split |>
    ggplot2::ggplot(ggplot2::aes(x = .data$period_type, y = .data$E / 1e6, fill = .data$activity)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.6) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$E_lower / 1e6, ymax = .data$E_upper / 1e6),
      position = ggplot2::position_dodge(width = 0.7), width = 0.2
    ) +
    ggplot2::scale_fill_manual(values = act_cols, name = "REDD+ activity",
                               labels = c(DF = "Deforestation", DG = "Degradation")) +
    ggplot2::labs(x = NULL, y = "Emissions (MtCO2e/yr)",
                  title = "Emissions split by REDD+ activity") +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(legend.position = "bottom")

  list(
    variance    = variance,
    split       = split,
    gg_variance = gg_variance,
    gg_split    = gg_split
  )
}
