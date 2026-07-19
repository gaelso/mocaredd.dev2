#' Calculate emissions based on arithmetic means
#'
#' @description TBD
#'
#'
#' @param .checked_data The list returned by \code{fct_checkinput()}. Its
#'             \code{template_version} element drives version-specific behaviour and its
#'             \code{data} element supplies the \code{setup}, \code{time}, \code{area}
#'             and \code{carbon} tables.
#'
#' @return A data frame with arithmetic mean of CO2 emissions for each land use
#'         transition, REDD+ activity or emission reductions level.
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
#' res <- fct_arithmetic_mean(.checked_data = checked)
#' res$gg_emissions
#'
#' @export
fct_arithmetic_mean <- function(.checked_data){

  ## !!! FOR TESTING ONLY
  # .checked_data <- fct_checkinput(.path = path)
  ## !!!

  ## Deterministic inputs: single iteration, no standard error, normal PDFs.
  ## Keep everything inside the checked-data object so template_version rides along.
  cd <- .checked_data
  cd$data$setup  <- cd$data$setup  |> dplyr::mutate(n_iter = 1)
  cd$data$area   <- cd$data$area   |> dplyr::mutate(trans_se = 0, trans_pdf = "normal")
  cd$data$carbon <- cd$data$carbon |> dplyr::mutate(c_se = 0, c_pdf = "normal")
  cd$data$time   <- cd$data$time   |> dplyr::mutate(nb_years = .data$year_end - .data$year_start + 1)

  setup_ari <- cd$data$setup
  .time     <- cd$data$time

  ari_trans <- fct_combine_mcs_E(.checked_data = cd)

  ari_REF <- fct_combine_mcs_P(
    .data = ari_trans,
    .time = .time,
    .period_type = "REF",
    .ad_annual = setup_ari$ad_annual
  )

  ari_MON  <- fct_combine_mcs_P(
    .data = ari_trans,
    .time = .time,
    .period_type = "MON",
    .ad_annual = setup_ari$ad_annual
  )

  ari_ER <- fct_combine_mcs_ER(.sim_ref = ari_REF, .sim_mon = ari_MON, .ad_annual = setup_ari$ad_annual) |>
    dplyr::mutate(period_type = paste0("ER-", .data$period_type)) |>
    dplyr::select("period_type", E = "ER_sim")

  ari_REF2 <- ari_REF |> dplyr::select("period_type", "E")
  ari_MON2 <- ari_MON |>
    dplyr::mutate(period_type = paste0("E-", .data$period_type)) |>
    dplyr::select("period_type", "E")

  ari_combi <- ari_REF2 |>
    dplyr::bind_rows(ari_MON2) |>
    dplyr::bind_rows(ari_ER)

  ## Periods with no type (NA period_type, e.g. gap years) are computed at
  ## transition level but excluded from the reference/monitoring emissions summary.
  out_combi <- .time |>
    dplyr::filter(!is.na(.data$period_type)) |>
    dplyr::group_by(.data$period_type) |>
    dplyr::summarize(
      year_start = min(.data$year_start),
      year_end   = max(.data$year_end)
    ) |>
    dplyr::mutate(nb_years = .data$year_end - .data$year_start + 1) |>
    dplyr::arrange(.data$year_start) |>
    dplyr::left_join(dplyr::bind_rows(ari_REF, ari_MON), by = "period_type")


  # mon <- out_combi |> dplyr::filter(stringr::str_detect(.data$period_type, pattern = "MON"))
  #
  # years_mon <- min(mon$year_start):max(mon$year_end)
  #
  # ggdat <- purrr::map(years_mon, function(x){
  #
  #   REF <- out_combi |>
  #     dplyr::filter(.data$period_type == "REF") |>
  #     dplyr::pull("E")
  #
  #   REF  <- round(REF / 10^6, 2)
  #
  #   E <- mon |>
  #     dplyr::filter(.data$year_start <= x, .data$year_end >= x) |>
  #     dplyr::pull("E")
  #
  #   E <- round(E / 10^6, 2)
  #
  #   data.frame(year = x, E = E, REF = REF)
  #
  # }) |> purrr::list_rbind()
  #
  # out_gg <- ggdat |>
  #   ggplot2::ggplot(ggplot2::aes(x = .data$year)) +
  #   #ggplot2::geom_col(ggplot2::aes(y = .data$REF), col = "darkgreen", fill = "lightgreen") +
  #   ggplot2::geom_line(ggplot2::aes(y = .data$REF), col = "darkgreen") +
  #   ggplot2::geom_col(ggplot2::aes(y = .data$E), col = "darkred", fill = "pink", width = 0.1) +
  #   ggplot2::scale_x_continuous(breaks = min(ggdat$year):max(ggdat$year), minor_breaks = NULL) +
  #   ggplot2::theme_bw(base_size = 20) +
  #   ggplot2::labs(
  #     x = "Years",
  #     y = "Emissions (MtCO2e/y)"
  #   )

  ## Add yearly table
  out_yearly <- purrr::map(out_combi$period_type, function(x){

    tt <- out_combi |> dplyr::filter(.data$period_type == x)
    year_start <- tt$year_start
    year_end   <- tt$year_end
    E          <- round(tt$E / 10^6, 2)

    dplyr::tibble(
      year = year_start:year_end,
      E = rep(E, length(year_start:year_end)),
      period_type = x,
      FREL = round(ari_REF$E / 10^6, 2)
    )

  }) |> purrr::list_rbind()

  out_yearly_mon <- out_yearly |> dplyr::filter(stringr::str_detect(.data$period_type, "MON"))
  
  ## Actual annual emissions per time period (every period_no, incl. untyped gap
  ## years). E_year is already annualised in the transition-level arithmetic result.
  ari_period <- ari_trans |>
    dplyr::summarise(E = sum(.data$E_year), .by = "time_period")

  out_yearly <- purrr::map(seq_len(nrow(.time)), function(i){
    tt <- .time[i, ]
    Ep <- ari_period$E[ari_period$time_period == tt$period_no]
    Ep <- if (length(Ep) == 0) NA_real_ else Ep
    dplyr::tibble(
      year        = tt$year_start:tt$year_end,
      E           = round(Ep / 10^6, 2),
      period_type = tt$period_type,
      FREL        = round(ari_REF$E / 10^6, 2)
    )
  }) |> purrr::list_rbind()

  ## Same as out_gg3 (baseline and MON segments unchanged) but each point reflects
  ## the actual emissions of its own time period; untyped (NA) periods are shown in gray.
  out_gg <- out_yearly |>
    ggplot2::ggplot(ggplot2::aes(x = .data$year)) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$FREL),
      col = "pink", linewidth = 1
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
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom") +
    ggplot2::labs(
      x = "Years",
      y = "Emissions (MtCO2e/y)",
      color = ""
    )

  ## Result table: per-period emissions split by REDD+ activity (DF, DG), then a
  ## summary block with the reference / monitoring emission levels and reductions.
  ari_activity <- ari_trans |>
    dplyr::summarise(E = sum(.data$E_year), .by = c("time_period", "redd_activity")) |>
    tidyr::pivot_wider(names_from = "redd_activity", values_from = "E", values_fill = 0)

  act_cols <- setdiff(names(ari_activity), "time_period")
  if (!"DF" %in% names(ari_activity)) ari_activity$DF <- 0
  if (!"DG" %in% names(ari_activity)) ari_activity$DG <- 0

  tbl_period <- ari_activity |>
    dplyr::mutate(total = rowSums(dplyr::across(dplyr::all_of(act_cols)))) |>
    dplyr::left_join(
      dplyr::select(.time, "period_no", "year_start", "year_end", "period_type"),
      by = c("time_period" = "period_no")
    ) |>
    dplyr::arrange(.data$year_start) |>
    dplyr::transmute(
      grp   = "Emissions by time period",
      item  = dplyr::coalesce(.data$period_type, "No period"),
      years = ifelse(.data$year_start == .data$year_end,
                     as.character(.data$year_start),
                     paste0(.data$year_start, "-", .data$year_end)),
      DF    = .data$DF,
      DG    = .data$DG,
      total = .data$total
    )

  tbl_summary <- ari_combi |>
    dplyr::transmute(
      grp   = "Reference, monitoring & emission reductions",
      item  = dplyr::case_when(
        .data$period_type == "REF"                     ~ "Reference level (REF)",
        stringr::str_detect(.data$period_type, "^E-")  ~ paste0("Emissions ",  stringr::str_remove(.data$period_type, "^E-")),
        stringr::str_detect(.data$period_type, "^ER-") ~ paste0("Reduction ",  stringr::str_remove(.data$period_type, "^ER-")),
        TRUE ~ .data$period_type
      ),
      years = NA_character_,
      DF    = NA_real_,
      DG    = NA_real_,
      total = .data$E
    )

  out_gt <- dplyr::bind_rows(tbl_period, tbl_summary) |>
    gt::gt(rowname_col = "item", groupname_col = "grp") |>
    gt::tab_spanner(label = "Emissions (tCO2e/yr)", columns = c("DF", "DG", "total")) |>
    gt::cols_label(years = "Years", DF = "Deforestation", DG = "Degradation", total = "Total") |>
    gt::fmt_number(columns = c("DF", "DG", "total"), decimals = 0, use_seps = TRUE) |>
    gt::sub_missing(columns = gt::everything(), missing_text = "") |>
    gt::cols_align(align = "right", columns = c("DF", "DG", "total"))

  list(
    ER = ari_combi,
    emissions = out_combi,
    gg_emissions = out_gg,
    gt_emissions = out_gt
  )

}




