#' Sensitivity analysis of REDD+ emissions and emission reductions (delta method)
#'
#' @description
#' Sensitivity of the reference level, monitoring emissions and emission
#' reductions to the independent model inputs (activity data, carbon elements,
#' carbon fraction, degradation ratios), with the same model as the Monte Carlo
#' chain (\code{fct_combine_mcs_E()}, \code{fct_combine_mcs_P()},
#' \code{fct_combine_mcs_ER()}): carbon elements are shared by all transitions of
#' a land use, and EF = C_i - C_f.
#'
#' Delta method: for independent inputs and a first-order expansion,
#' \eqn{Var(Y) \approx \sum_i (\partial Y / \partial X_i)^2 \sigma_i^2}. The share
#' \eqn{(\partial Y / \partial X_i \cdot \sigma_i)^2 / Var(Y)} is the first-order
#' Sobol index of input \eqn{i}. Shares are summed by input group, and by input
#' type: activity data (AD) vs emission factors (EF: carbon stocks, carbon
#' fraction, degradation ratios). The AD x EF interaction is second order and
#' not included.
#'
#' The AD tornado swings each AD source across \eqn{mean \pm z \cdot se}, all
#' other inputs at their mean. Outputs are linear in each AD, so the swing is
#' exact and the AD shares add up to the AD group share.
#'
#' @param .checked_data The list returned by \code{fct_checkinput()}.
#'
#' @return A list with:
#'   \code{outputs} (mean, delta-method se and U\% of each output),
#'   \code{variance} (share of variance per output, input type and group, in \%),
#'   \code{ad_tornado} (one row per output and AD source: low / high output value,
#'   swing and share of variance, sorted by swing),
#'   \code{split} (emissions per period type and REDD+ activity, with uncertainty)
#'   and \code{gg_variance} (stacked bar chart of the variance shares).
#'   Outputs are named as in the app: reference type (e.g. "REF"), "E-<MON>" and
#'   "ER-<MON>".
#'
#' @importFrom rlang .data
#'
#' @examples
#' path <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2")
#' checked <- fct_checkinput(.path = path)
#'
#' sa <- fct_sensitivity2(.checked_data = checked)
#' sa$variance
#' fct_tornado(sa$ad_tornado, .output = "ER-MON1")
#'
#' @export
fct_sensitivity2 <- function(.checked_data){

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
  z <- stats::qnorm(1 - (1 - setup$conf_level) / 2)


  ##
  ## 1. Carbon stock model (as in fct_combine_mcs_E) ###########################
  ##

  carbon_pools <- if (is_v2) dplyr::filter(carbon, .data$c_element != "CF") else carbon

  to_intact <- function(lu_id) {
    out <- stringr::str_remove(lu_id, setup$dg_ext)
    if (is_v2) out <- stringr::str_replace(out, "[^A-Za-z]$", "")
    out
  }

  c_formula <- carbon_pools |>
    dplyr::summarise(c_el = list(.data$c_element), c_un = list(if (is_v2) .data$c_unit else NA), .by = "c_lu_id") |>
    dplyr::rowwise() |>
    dplyr::mutate(c_form = fct_make_formula2(
      .c_el    = .data$c_el,
      .c_unit  = if (is_v2) .data$c_un else setup$c_unit,
      .version = if (is_v2) 2 else 1
    )) |>
    dplyr::ungroup()

  form_expr <- stats::setNames(purrr::map(c_formula$c_form, ~ parse(text = .x)[[1]]), c_formula$c_lu_id)
  el_of     <- stats::setNames(purrr::map(c_formula$c_el, ~ setdiff(.x, "DG_ratio")), c_formula$c_lu_id)

  degrat_lu   <- carbon_pools |> dplyr::filter(.data$c_element == "DG_ratio") |> dplyr::pull("c_lu_id")
  lu_plain    <- setdiff(c_formula$c_lu_id, degrat_lu)
  dg_pool_vec <- stringr::str_trim(stringr::str_split(stringr::str_trim(setup$dg_pool), ",")[[1]])
  dg_is_all   <- identical(dg_pool_vec, "ALL")

  if (is_v2) {
    cf_row  <- carbon |> dplyr::filter(.data$c_element == "CF")
    has_cf  <- nrow(cf_row) > 0
    cf_mean <- if (has_cf) cf_row$c_value[1] else NA_real_
    cf_se   <- if (has_cf) cf_row$c_se[1]    else NA_real_
  } else {
    has_cf  <- is.numeric(setup[["c_fraction"]]) && isTRUE(setup[["c_unit"]] == "DM")
    cf_mean <- if (has_cf) setup$c_fraction    else NA_real_
    cf_se   <- if (has_cf) setup$c_fraction_se else NA_real_
  }

  ## Input vector: carbon elements '<lu>..<element>', AD 'AD..<trans_id>', CF
  pool_key  <- paste0(carbon_pools$c_lu_id, "..", carbon_pools$c_element)
  ad_key    <- paste0("AD..", area$trans_id)
  prim_mean <- c(stats::setNames(carbon_pools$c_value, pool_key), stats::setNames(area$trans_area, ad_key))
  prim_se   <- c(stats::setNames(carbon_pools$c_se,    pool_key), stats::setNames(area$trans_se,   ad_key))
  if (has_cf) { prim_mean["CF"] <- cf_mean; prim_se["CF"] <- cf_se }
  prim_keys <- names(prim_mean)

  ## Carbon stock of every land use. Degraded: DG_ratio * C_intact, plus
  ## (1 - DG_ratio) * pools excluded from degradation, as in fct_combine_mcs_E().
  c_stocks <- function(pm) {
    cs <- purrr::map_dbl(lu_plain, function(lu) {
      env <- as.list(stats::setNames(pm[paste0(lu, "..", el_of[[lu]])], el_of[[lu]]))
      if (has_cf) env$CF <- pm[["CF"]]
      eval(form_expr[[lu]], env)
    })
    cs <- stats::setNames(cs, lu_plain)
    for (lu in degrat_lu) {
      intact <- to_intact(lu)
      dg     <- pm[[paste0(lu, "..DG_ratio")]]
      excl   <- if (dg_is_all) character(0) else setdiff(el_of[[intact]], dg_pool_vec)
      c_noDG <- if (length(excl) > 0) sum(pm[paste0(intact, "..", excl)]) else 0
      cs[lu] <- dg * cs[[intact]] + (1 - dg) * c_noDG
    }
    cs
  }

  ## EF per transition (tCO2e/ha)
  ef_of <- function(pm) {
    cs <- c_stocks(pm)
    unname((cs[area$lu_initial_id] - cs[area$lu_final_id]) * 44 / 12)
  }


  ##
  ## 2. Outputs as weighted sums of transition emissions #######################
  ##

  ## Y = W %*% (AD * EF). Weights as in fct_combine_mcs_P(): length-weighted mean
  ## of annual emissions. ER = first REF type - MON type, as in fct_combine_mcs_ER().
  nb_years  <- stats::setNames(time$nb_years, time$period_no)
  ref_types <- unique(stats::na.omit(time$period_type[stringr::str_detect(time$period_type, "REF")]))
  mon_types <- unique(stats::na.omit(time$period_type[stringr::str_detect(time$period_type, "MON")]))

  level_w <- function(pt, act = NULL) {
    per <- time$period_no[time$period_type %in% pt]
    sel <- area$trans_period %in% per
    if (!is.null(act)) sel <- sel & area$redd_activity == act
    w   <- if (ad_annual) nb_years[area$trans_period] else rep(1, nrow(area))
    unname(ifelse(sel, w / sum(nb_years[per]), 0))
  }

  W_rows <- list()
  for (pt in c(ref_types, mon_types)) {
    W_rows[[paste0("PT.", pt)]] <- level_w(pt)
    acts <- unique(area$redd_activity[area$trans_period %in% time$period_no[time$period_type %in% pt]])
    for (act in acts) W_rows[[paste0("PTA.", pt, ".", act)]] <- level_w(pt, act)
  }
  if (length(ref_types) >= 1) {
    for (m in mon_types) W_rows[[paste0("ER.", m)]] <- W_rows[[paste0("PT.", ref_types[1])]] - W_rows[[paste0("PT.", m)]]
  }
  W <- do.call(rbind, W_rows)

  metrics <- function(pm) as.numeric(W %*% (pm[ad_key] * ef_of(pm))) |> stats::setNames(rownames(W))


  ##
  ## 3. Jacobian and variance contributions ####################################
  ##

  ## Central differences; exact for AD (linear), and for carbon elements up to
  ## products of two inputs perturbed together (never the case here).
  m0  <- metrics(prim_mean)
  jac <- matrix(0, nrow = length(m0), ncol = length(prim_keys), dimnames = list(names(m0), prim_keys))
  for (k in prim_keys) {
    if (is.na(prim_se[[k]]) || prim_se[[k]] == 0) next
    h  <- max(abs(prim_mean[[k]]) * 1e-4, 1e-6)
    pp <- prim_mean; pp[k] <- prim_mean[[k]] + h
    pm <- prim_mean; pm[k] <- prim_mean[[k]] - h
    jac[, k] <- (metrics(pp) - metrics(pm)) / (2 * h)
  }
  se_mat  <- matrix(dplyr::coalesce(prim_se, 0), nrow = nrow(jac), ncol = ncol(jac), byrow = TRUE)
  contrib <- (jac * se_mat)^2
  var_out <- rowSums(contrib)

  target_keys <- c(paste0("PT.", ref_types), paste0("PT.", mon_types), paste0("ER.", mon_types))
  target_lab  <- c(ref_types, paste0("E-", mon_types), paste0("ER-", mon_types))
  targets     <- stats::setNames(target_keys, target_lab)

  outputs <- dplyr::tibble(
    output = target_lab,
    E      = unname(m0[target_keys]),
    E_se   = sqrt(unname(var_out[target_keys]))
  ) |>
    dplyr::mutate(E_U = dplyr::if_else(.data$E == 0, NA_real_, z * .data$E_se / abs(.data$E) * 100))


  ##
  ## 4. Variance shares by input type and group ################################
  ##

  input_group <- dplyr::case_when(
    startsWith(prim_keys, "AD..")       ~ "Activity data",
    prim_keys == "CF"                   ~ "Carbon fraction",
    endsWith(prim_keys, "..DG_ratio")   ~ "Degradation ratio",
    TRUE                                ~ "Carbon stock"
  )
  input_type <- dplyr::if_else(input_group == "Activity data", "AD", "EF")

  variance <- purrr::imap(targets, function(key, lab) {
    tot <- var_out[[key]]
    dplyr::tibble(input_type = input_type, group = input_group, contribution = contrib[key, ]) |>
      dplyr::summarise(contribution = sum(.data$contribution), .by = c("input_type", "group")) |>
      dplyr::mutate(
        output           = lab,
        contribution_pct = if (tot > 0) .data$contribution / tot * 100 else NA_real_
      )
  }) |>
    purrr::list_rbind() |>
    dplyr::select("output", "input_type", "group", "contribution_pct")

  ## Keep only groups with uncertainty somewhere
  keep_groups <- variance |>
    dplyr::summarise(s = sum(.data$contribution_pct, na.rm = TRUE), .by = "group") |>
    dplyr::filter(.data$s > 0) |>
    dplyr::pull("group")
  variance <- variance |> dplyr::filter(.data$group %in% keep_groups)


  ##
  ## 5. AD tornado #############################################################
  ##

  ef0     <- ef_of(prim_mean)
  ad_se   <- dplyr::coalesce(unname(prim_se[ad_key]), 0)
  ptype_t <- unname(stats::setNames(time$period_type, time$period_no)[area$trans_period])

  ad_tornado <- purrr::imap(targets, function(key, lab) {
    d  <- unname(jac[key, ad_key])
    y0 <- unname(m0[[key]])
    dplyr::tibble(
      output           = lab,
      trans_id         = area$trans_id,
      trans_period     = area$trans_period,
      period_type      = ptype_t,
      redd_activity    = area$redd_activity,
      lu_initial_id    = area$lu_initial_id,
      lu_final_id      = area$lu_final_id,
      AD               = area$trans_area,
      AD_se            = ad_se,
      EF               = ef0,
      value            = y0,
      value_low        = y0 - d * z * ad_se,
      value_high       = y0 + d * z * ad_se,
      swing            = abs(2 * d * z * ad_se),
      contribution_pct = if (var_out[[key]] > 0) (d * ad_se)^2 / var_out[[key]] * 100 else NA_real_
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::filter(.data$swing > 0) |>
    dplyr::arrange(factor(.data$output, levels = target_lab), dplyr::desc(.data$swing))


  ##
  ## 6. Emissions split by REDD+ activity ######################################
  ##

  split_keys <- grep("^PTA\\.", names(m0), value = TRUE)
  split <- purrr::map(split_keys, function(key) {
    parts <- strsplit(key, "\\.")[[1]]
    m <- unname(m0[[key]]); s <- sqrt(unname(var_out[[key]]))
    dplyr::tibble(
      period_type = parts[2], redd_activity = parts[3], E = m, E_se = s,
      E_U     = if (m == 0) NA_real_ else z * s / abs(m) * 100,
      E_lower = m - z * s,
      E_upper = m + z * s
    )
  }) |>
    purrr::list_rbind()


  ##
  ## 7. Figure #################################################################
  ##

  ## Fixed colour per group (AD first, then EF components)
  group_cols <- c("Activity data"     = "#2a78d6",
                  "Degradation ratio" = "#eb6834",
                  "Carbon stock"      = "#1baf7a",
                  "Carbon fraction"   = "#eda100")

  ## AD vs EF totals in the axis labels
  type_lab <- variance |>
    dplyr::summarise(pct = sum(.data$contribution_pct, na.rm = TRUE), .by = c("output", "input_type")) |>
    tidyr::pivot_wider(names_from = "input_type", values_from = "pct", values_fill = 0)
  if (!"AD" %in% names(type_lab)) type_lab$AD <- 0
  if (!"EF" %in% names(type_lab)) type_lab$EF <- 0
  type_lab <- type_lab |>
    dplyr::mutate(label = paste0(.data$output, "\nAD ", round(.data$AD), "% | EF ", round(.data$EF), "%"))

  gg_data <- variance |>
    dplyr::left_join(dplyr::select(type_lab, "output", "label"), by = "output") |>
    dplyr::mutate(
      label = factor(.data$label, levels = rev(type_lab$label[match(target_lab, type_lab$output)])),
      group = factor(.data$group, levels = intersect(names(group_cols), keep_groups)),
      txt   = dplyr::if_else(.data$contribution_pct >= 5, paste0(round(.data$contribution_pct), "%"), "")
    )

  gg_variance <- gg_data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$contribution_pct, y = .data$label, fill = .data$group)) +
    ggplot2::geom_col(width = 0.6, colour = "white", linewidth = 0.5,
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(ggplot2::aes(label = .data$txt), colour = "#0b0b0b", size = 4,
                       position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = group_cols, name = NULL) +
    ggplot2::scale_x_continuous(limits = c(0, 100.01), breaks = seq(0, 100, 25),
                                labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    ggplot2::labs(x = "Share of variance (first-order)", y = NULL) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      legend.position    = "bottom",
      plot.margin        = ggplot2::margin(5, 15, 5, 5)
    )

  list(
    outputs     = outputs,
    variance    = variance,
    ad_tornado  = ad_tornado,
    split       = split,
    gg_variance = gg_variance
  )
}
