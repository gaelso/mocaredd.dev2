#' Calculate emissions and IPCC Approach 1 uncertainty from arithmetic means
#'
#' @description
#' Deterministic counterpart of the Monte Carlo chain (\code{fct_combine_mcs_E()},
#' \code{fct_combine_mcs_P()}, \code{fct_combine_mcs_ER()}), following the same
#' steps with means instead of simulations, and propagating uncertainty with the
#' IPCC Approach 1 rules (IPCC 2006, Vol. 1, Ch. 3, Eq. 3.1 and 3.2):
#' \itemize{
#'   \item product: relative standard errors combine in quadrature,
#'         \eqn{U_{a b} = \sqrt{U_a^2 + U_b^2}};
#'   \item sum / difference: absolute standard errors combine in quadrature,
#'         \eqn{se_{a + b} = \sqrt{se_a^2 + se_b^2}}.
#' }
#'
#' Calculation chain:
#' \enumerate{
#'   \item Carbon stock of each land use from \code{fct_make_formula2()}. Composite
#'         factors are resolved first: \code{(1 + RS)} and \code{(AGB + BGB)}. Then
#'         product rule within each term, sum rule across terms.
#'   \item Degraded land uses: \code{DG_ratio * C_intact} (product rule), plus
#'         pools excluded from degradation when \code{dg_pool} is not "ALL".
#'   \item Emission factors: \code{EF = (C_i - C_f) * 44/12} (sum rule). For
#'         intact to degraded transitions \code{EF = (1 - DG_ratio) * C_intact * 44/12}
#'         (product rule), so the intact stock is not counted twice.
#'   \item Emissions: \code{E = AD * EF} (product rule), annualised as in
#'         \code{fct_combine_mcs_E()}.
#'   \item Emissions per time period and REDD+ activity (sum rule).
#'   \item Reference and monitoring levels: length-weighted mean of the annual
#'         emissions of their time periods (sum rule).
#'   \item Emission reductions: \code{ER = FREL - E_MON} (sum rule).
#' }
#'
#' Approach 1 assumes all combined terms are independent. Correlations from shared
#' inputs (the carbon fraction, a carbon stock used by several transitions, the
#' same emission factors in the reference and monitoring periods) are ignored; the
#' Monte Carlo simulations (Approach 2) account for them.
#'
#' @param .checked_data The list returned by \code{fct_checkinput()}.
#'
#' @return A list with:
#'   \code{ER} (reference / monitoring levels and emission reductions with mean, se,
#'   U\% and confidence bounds), \code{emissions} (per time period),
#'   \code{gg_emissions} (figure), \code{emissions_table} (tidy table for the app),
#'   \code{c_stock} (carbon stock per land use) and \code{trans} (EF and E per
#'   transition). U\% is \eqn{z \cdot se / |mean| \cdot 100}, with \eqn{z} from
#'   \code{setup$conf_level}.
#'
#' @importFrom rlang .data
#'
#' @examples
#' path <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2")
#'
#' checked <- fct_checkinput(.path = path)
#'
#' res <- fct_arithmetic_mean3(.checked_data = checked)
#' res$c_stock
#' res$ER
#'
#' @export
fct_arithmetic_mean3 <- function(.checked_data){

  ## !!! FOR TESTING ONLY
  # .checked_data <- fct_checkinput(.path = path)
  ## !!!

  setup     <- .checked_data$data$setup
  time      <- .checked_data$data$time
  area      <- .checked_data$data$area
  carbon    <- .checked_data$data$carbon
  is_v2     <- isTRUE(.checked_data$template_version == 2)
  is_annual <- isTRUE(setup$ad_annual)

  ## z value for the requested confidence level (two-sided)
  z_score <- .checked_data$data$setup$z_score

  ## TOC ####
  ## 1. Preparation: carbon elements, carbon fraction, degradation settings, formulas
  ## 2. Carbon stock per land use (+ uncertainty)
  ## 3. Emission factor per transition (+ uncertainty)
  ## 4. Emissions per transition, annualised (+ uncertainty)
  ## 5. Emissions per time period and REDD+ activity (+ uncertainty)
  ## 6. Reference and monitoring emission levels (+ uncertainty)
  ## 7. Emission reductions (+ uncertainty)
  ## 8. Outputs: tables and figure


  ##
  ## 1. Preparation ############################################################
  ##

  ## + 1.1. Carbon elements with a unit (v1: one unit for all) ####
  carbon_pools <- carbon |>
    dplyr::filter(.data$c_element != "CF") |>
    dplyr::mutate(
      c_unit = if (is_v2) .data$c_unit else setup$c_unit,
      c_se   = dplyr::coalesce(.data$c_se, 0)
    )

  ## + 1.2. Carbon fraction (global). v2: 'carbon' row; v1: setup ####
  if (is_v2) {
    cf_row <- carbon |> dplyr::filter(.data$c_element == "CF")
    cf     <- c(value = cf_row$c_value[1], se = cf_row$c_se[1])
  } else {
    cf <- c(value = if (is.null(setup$c_fraction))    NA_real_ else setup$c_fraction,
            se    = if (is.null(setup$c_fraction_se)) NA_real_ else setup$c_fraction_se)
  }

  ## + 1.3. Degradation: degraded land uses, their intact land use, pools affected ####
  dg_pool <- stringr::str_split(setup$dg_pool, pattern = ",")[[1]] |> stringr::str_trim()

  c_degrat <- carbon_pools |>
    dplyr::filter(.data$c_element == "DG_ratio") |>
    dplyr::mutate(
      lu_intact = stringr::str_remove(.data$c_lu_id, setup$dg_ext),
      lu_intact = if (is_v2) stringr::str_replace(.data$lu_intact, "[^A-Za-z]$", "") else .data$lu_intact
    ) |>
    dplyr::select("c_lu_id", "lu_intact", DG = "c_value", DG_se = "c_se")

  dg_partial <- nrow(c_degrat) > 0 && !identical(dg_pool, "ALL")

  ## + 1.4. Carbon elements of each land use used in formulas ####
  ## Degraded land uses are handled in step 2.3. If only some pools are degraded,
  ## the intact land uses are also split into two pseudo land uses:
  ## '<lu>..DG' (degraded pools, RS follows AGB) and '<lu>..noDG' (other pools).
  c_elements <- carbon_pools |>
    dplyr::filter(!.data$c_lu_id %in% c_degrat$c_lu_id)

  if (dg_partial) {
    c_elements <- c_elements |>
      dplyr::bind_rows(
        c_elements |>
          dplyr::filter(.data$c_lu_id %in% c_degrat$lu_intact) |>
          dplyr::mutate(
            dg_part = dplyr::if_else(
              .data$c_element %in% dg_pool | (.data$c_element == "RS" & "AGB" %in% dg_pool),
              "DG", "noDG"
            ),
            c_lu_id = paste0(.data$c_lu_id, "..", .data$dg_part)
          ) |>
          dplyr::select(-"dg_part")
      )
  }

  ## + 1.5. Carbon stock formula and its uncertainty formula, per land use ####
  c_formula <- c_elements |>
    dplyr::summarise(c_el = list(.data$c_element), c_un = list(.data$c_unit), .by = "c_lu_id") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      c_form   = fct_make_formula2( .c_el = .data$c_el, .c_unit = if (is_v2) .data$c_un else setup$c_unit, .version = if (is_v2) 2 else 1),
      c_form_U = fct_make_formula_U(.c_el = .data$c_el, .c_unit = if (is_v2) .data$c_un else setup$c_unit, .version = if (is_v2) 2 else 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::select("c_lu_id", "c_form", "c_form_U")


  ##
  ## 2. Carbon stock per land use ##############################################
  ##

  ## + 2.1. Carbon elements and their se as columns, one row per land use ####
  c_values <- c_elements |>
    dplyr::select("c_lu_id", "c_element", "c_value", "c_se") |>
    tidyr::pivot_wider(names_from = "c_element", values_from = c("c_value", "c_se"), names_glue = "{c_element}{ifelse(.value == 'c_se', '_se', '')}") |>
    dplyr::mutate(CF = cf[["value"]], CF_se = cf[["se"]])

  ## + 2.2. Carbon stock and se from the formulas (IPCC Approach 1) ####
  c_stock <- c_values |>
    dplyr::left_join(c_formula, by = "c_lu_id") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      C    = eval(parse(text = .data$c_form),   dplyr::pick(dplyr::everything())),
      C_se = eval(parse(text = .data$c_form_U), dplyr::pick(dplyr::everything()))
    ) |>
    dplyr::ungroup() |>
    dplyr::select("c_lu_id", "c_form", "c_form_U", "C", "C_se")

  ## + 2.3. Degraded land uses: DG_ratio * C_intact (+ pools not degraded) ####
  ## C_intact_DG: stock of the degraded pools of the intact land use (all pools
  ## if dg_pool is "ALL"); C_intact_noDG: other pools (0 if dg_pool is "ALL").
  c_intact <- c_stock |>
    dplyr::filter(.data$c_lu_id %in% c_degrat$lu_intact) |>
    dplyr::select(lu_intact = "c_lu_id", C_intact_DG = "C", C_intact_DG_se = "C_se") |>
    dplyr::mutate(C_intact_noDG = 0, C_intact_noDG_se = 0)

  if (dg_partial) {
    c_intact <- c_stock |>
      dplyr::filter(stringr::str_detect(.data$c_lu_id, "\\.\\.(DG|noDG)$")) |>
      tidyr::separate_wider_delim("c_lu_id", delim = "..", names = c("lu_intact", "dg_part")) |>
      dplyr::select("lu_intact", "dg_part", "C", "C_se") |>
      tidyr::pivot_wider(names_from = "dg_part", values_from = c("C", "C_se"), values_fill = 0) |>
      dplyr::select("lu_intact", C_intact_DG = "C_DG", C_intact_DG_se = "C_se_DG",
                    C_intact_noDG = "C_noDG", C_intact_noDG_se = "C_se_noDG")
  }

  c_stock_dg <- c_degrat |>
    dplyr::left_join(c_intact, by = "lu_intact") |>
    dplyr::mutate(
      c_form   = if (dg_partial) "DG_ratio * C_intact_DG + C_intact_noDG" else "DG_ratio * C_intact",
      c_form_U = if (dg_partial) "sqrt((DG_ratio * C_intact_DG * sqrt((DG_ratio_se / DG_ratio)^2 + (C_intact_DG_se / C_intact_DG)^2))^2 + C_intact_noDG_se^2)" else "DG_ratio * C_intact * sqrt((DG_ratio_se / DG_ratio)^2 + (C_intact_se / C_intact)^2)",
      ## product rule
      C_DG    = .data$DG * .data$C_intact_DG,
      C_DG_se = sqrt((.data$C_intact_DG * .data$DG_se)^2 + (.data$DG * .data$C_intact_DG_se)^2),
      ## sum rule
      C       = .data$C_DG + .data$C_intact_noDG,
      C_se    = sqrt(.data$C_DG_se^2 + .data$C_intact_noDG_se^2)
    )

  c_stock <- c_stock |>
    dplyr::filter(!stringr::str_detect(.data$c_lu_id, "\\.\\.(DG|noDG)$")) |>
    dplyr::bind_rows(dplyr::select(c_stock_dg, "c_lu_id", "c_form", "c_form_U", "C", "C_se")) |>
    dplyr::mutate(C_U = dplyr::if_else(.data$C == 0, NA_real_, z_score * .data$C_se / abs(.data$C) * 100))


  ##
  ## 3. Emission factor per transition #########################################
  ##

  trans <- area |>
    dplyr::select("trans_id", "trans_period", "redd_activity", "lu_initial_id", "lu_final_id",
                  AD = "trans_area", AD_se = "trans_se") |>
    dplyr::mutate(AD_se = dplyr::coalesce(.data$AD_se, 0)) |>
    dplyr::left_join(dplyr::select(c_stock, lu_initial_id = "c_lu_id", C_i = "C", C_i_se = "C_se"), by = "lu_initial_id") |>
    dplyr::left_join(dplyr::select(c_stock, lu_final_id   = "c_lu_id", C_f = "C", C_f_se = "C_se"), by = "lu_final_id") |>
    dplyr::left_join(
      dplyr::select(c_stock_dg, lu_final_id = "c_lu_id", "lu_intact", "DG", "DG_se", "C_intact_DG", "C_intact_DG_se"),
      by = "lu_final_id"
    ) |>
    dplyr::mutate(
      is_dg_trans = !is.na(.data$lu_intact) & .data$lu_initial_id == .data$lu_intact,
      ## intact -> degraded: (1 - DG_ratio) * C_intact_DG, product rule
      ## other transitions:  C_i - C_f, sum rule
      dC    = dplyr::if_else(.data$is_dg_trans, (1 - .data$DG) * .data$C_intact_DG, .data$C_i - .data$C_f),
      dC_se = dplyr::if_else(
        .data$is_dg_trans,
        sqrt((.data$C_intact_DG * .data$DG_se)^2 + ((1 - .data$DG) * .data$C_intact_DG_se)^2),
        sqrt(.data$C_i_se^2 + .data$C_f_se^2)
      ),
      EF    = .data$dC    * 44 / 12,
      EF_se = .data$dC_se * 44 / 12
    )


  ##
  ## 4. Emissions per transition, annualised ###################################
  ##

  ## E = AD * EF, product rule. As in fct_combine_mcs_E(): if AD is annual, E is
  ## annual and E_year = E; otherwise E is the period total and E_year = E / length.
  trans <- trans |>
    dplyr::left_join(
      dplyr::select(time, trans_period = "period_no", "period_type", period_length = "nb_years"),
      by = "trans_period"
    ) |>
    dplyr::mutate(
      E         = .data$AD * .data$EF,
      E_se      = sqrt((.data$EF * .data$AD_se)^2 + (.data$AD * .data$EF_se)^2),
      E_year    = if (setup$ad_annual) .data$E    else .data$E    / .data$period_length,
      E_year_se = if (setup$ad_annual) .data$E_se else .data$E_se / .data$period_length,
      E_year_U  = z_score * .data$E_year_se / abs(.data$E_year) * 100
    )


  ##
  ## 5. Emissions per time period and REDD+ activity ###########################
  ##

  ## Sum rule over transitions
  e_period_act <- trans |>
    dplyr::summarise(
      E    = sum(.data$E_year),
      E_se = sqrt(sum(.data$E_year_se^2)),
      .by = c("trans_period", "period_type", "period_length", "redd_activity")
    )

  e_period <- trans |>
    dplyr::summarise(
      E    = sum(.data$E_year),
      E_se = sqrt(sum(.data$E_year_se^2)),
      .by = c("trans_period", "period_type", "period_length")
    )


  ##
  ## 6. Reference and monitoring emission levels ###############################
  ##

  ## Length-weighted mean of the annual emissions of the periods (as in
  ## fct_combine_mcs_P()), sum rule with constant weights. Untyped periods
  ## (e.g. gap years) are excluded.
  e_level <- e_period |>
    dplyr::filter(stringr::str_detect(.data$period_type, "REF|MON")) |>
    dplyr::summarise(
      E    = sum(.data$E * .data$period_length) / sum(.data$period_length),
      E_se = sqrt(sum((.data$E_se * .data$period_length)^2)) / sum(.data$period_length),
      .by = "period_type"
    )


  ##
  ## 7. Emission reductions ####################################################
  ##

  ## ER = FREL - E_MON, sum rule. First REF type is the FREL, as in fct_combine_mcs_ER().
  frel <- e_level |> dplyr::filter(stringr::str_detect(.data$period_type, "REF")) |> dplyr::slice(1)

  e_er <- e_level |>
    dplyr::filter(stringr::str_detect(.data$period_type, "MON")) |>
    dplyr::mutate(
      period_type = paste0("ER-", .data$period_type),
      E    = frel$E - .data$E,
      E_se = sqrt(frel$E_se^2 + .data$E_se^2)
    )


  ##
  ## 8. Outputs ################################################################
  ##

  ## + 8.1. Levels and emission reductions, same format as fct_arithmetic_mean2() ####
  ari_ER <- e_level |>
    dplyr::mutate(period_type = dplyr::if_else(
      stringr::str_detect(.data$period_type, "MON"), paste0("E-", .data$period_type), .data$period_type
    )) |>
    dplyr::bind_rows(e_er) |>
    dplyr::mutate(
      E_U     = z_score * .data$E_se / abs(.data$E) * 100,
      E_lower = .data$E - z_score * .data$E_se,
      E_upper = .data$E + z_score * .data$E_se
    )

  ## + 8.2. Emissions per time period ####
  out_period <- time |>
    dplyr::select("period_no", "year_start", "year_end", "period_type") |>
    dplyr::left_join(dplyr::select(e_period, period_no = "trans_period", "E", "E_se"), by = "period_no") |>
    dplyr::arrange(.data$year_start) |>
    dplyr::mutate(
      E_U   = z_score * .data$E_se / abs(.data$E) * 100,
      years = dplyr::if_else(.data$year_start == .data$year_end, as.character(.data$year_start),
                             paste0(.data$year_start, "-", .data$year_end))
    )

  ## + 8.3. Figure: annual emissions per period with confidence intervals ####
  gg_data <- out_period |>
    dplyr::mutate(year = purrr::map2(.data$year_start, .data$year_end, seq)) |>
    tidyr::unnest_longer("year") |>
    dplyr::mutate(
      E_lower = round((.data$E - z_score * .data$E_se) / 1e6, 2),
      E_upper = round((.data$E + z_score * .data$E_se) / 1e6, 2),
      E       = round(.data$E / 1e6, 2),
      FREL    = round(frel$E / 1e6, 2)
    )

  gg_data_mon <- gg_data |> dplyr::filter(stringr::str_detect(.data$period_type, "MON"))

  gg_emissions <- gg_data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$year)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$FREL), col = "pink", linewidth = 1) +
    ggplot2::geom_segment(
      data = gg_data_mon,
      ggplot2::aes(xend = .data$year, y = .data$FREL, yend = .data$E),
      col = "limegreen", linewidth = 2,
      arrow = grid::arrow(length = grid::unit(0.2, "cm"), ends = "both")
    ) +
    ggplot2::geom_point(ggplot2::aes(y = .data$E, colour = .data$period_type), size = 4) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$E_lower, ymax = .data$E_upper, colour = .data$period_type),
      width = 0.2, linewidth = 0.8
    ) +
    ggplot2::scale_colour_discrete(na.value = "gray50") +
    ggplot2::scale_x_continuous(breaks = min(gg_data$year):max(gg_data$year), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "bottom") +
    ggplot2::labs(x = "Years", y = "Emissions (MtCO2e/y)", color = "")

  ## + 8.4. Result table (tidy; gt is built in the server) ####
  tbl_period <- out_period |>
    dplyr::left_join(
      e_period_act |>
        dplyr::select(period_no = "trans_period", "redd_activity", "E") |>
        tidyr::pivot_wider(names_from = "redd_activity", values_from = "E"),
      by = "period_no"
    ) |>
    dplyr::transmute(
      grp   = "Emissions by time period",
      item  = dplyr::coalesce(.data$period_type, "No period"),
      .data$years,
      DF    = if ("DF" %in% names(dplyr::pick(dplyr::everything()))) dplyr::coalesce(.data$DF, 0) else 0,
      DG    = if ("DG" %in% names(dplyr::pick(dplyr::everything()))) dplyr::coalesce(.data$DG, 0) else 0,
      total = .data$E,
      U_pct = .data$E_U
    )

  tbl_summary <- ari_ER |>
    dplyr::transmute(
      grp   = "Reference, monitoring & emission reductions",
      item  = dplyr::case_when(
        stringr::str_detect(.data$period_type, "^ER-") ~ paste0("Reduction ", stringr::str_remove(.data$period_type, "^ER-")),
        stringr::str_detect(.data$period_type, "^E-")  ~ paste0("Emissions ", stringr::str_remove(.data$period_type, "^E-")),
        TRUE                                           ~ paste0("Reference level (", .data$period_type, ")")
      ),
      years = NA_character_, DF = NA_real_, DG = NA_real_,
      total = .data$E, U_pct = .data$E_U
    )

  list(
    ER              = ari_ER,
    emissions       = out_period,
    gg_emissions    = gg_emissions,
    emissions_table = dplyr::bind_rows(tbl_period, tbl_summary),
    c_stock         = c_stock,
    trans           = trans
  )
}
