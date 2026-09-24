#' Combine MCS of emissions to a defined time period
#'
#' @description Calculate the Emission Level for a reference or monitoring period
#'              for each simulation, as the length-weighted average of the annual
#'              emissions (E_year) of the periods it contains.
#'
#' @param .data a data frame containing the simulations, output of fct_combine_mcs_E().
#'              Must contain time_period, period_length, period_type, sim_no and E_year.
#' @param .period_type "REF" or "MON", matched against the period_type column.
#'
#' @return A tibble with simulations at the final estimate per type of period.
#'
#' @importFrom rlang .data
#'
#' @examples
#' path <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2")
#'
#' checked <- fct_checkinput(.path = path)
#'
#' sim_trans <- fct_combine_mcs_E(.checked_data = checked)
#'
#' sim_FREL <- fct_combine_mcs_P(.data = sim_trans, .period_type = "REF")
#'
#' hist(sim_FREL$E)
#' round(median(sim_FREL$E))
#'
#' @export
fct_combine_mcs_P <- function(.data, .period_type){

  ## !!! FOR TESTING ONLY
  # .data = sim_trans
  # .period_type = "REF"
  # # !!!

  ## aggregate redd+ periods for REF or MON. Periods with no type (NA period_type,
  ## e.g. gap years) are computed at transition level but never aggregated here.
  data_sub <- .data |>
    dplyr::filter(!is.na(.data$period_type),
                  stringr::str_detect(.data$period_type, pattern = .period_type))

  ## Total length of each period type, counting each time period once
  length_sub <- data_sub |>
    dplyr::distinct(dplyr::pick("time_period", "period_type", "period_length")) |>
    dplyr::summarise(total_length = sum(.data$period_length), .by = "period_type")

  ## Weighted average of the annual emissions of each period:
  ## volume per period (E_year * period_length) divided by total length
  data_sub |>
    dplyr::summarise(E_vol = sum(.data$E_year * .data$period_length), .by = c("sim_no", "period_type")) |>
    dplyr::left_join(length_sub, by = "period_type") |>
    dplyr::mutate(E = round(.data$E_vol / .data$total_length, 0)) |>
    dplyr::select("sim_no", "period_type", "E")

}
