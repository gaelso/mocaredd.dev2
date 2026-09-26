

fct_cstock_formula <- function(.carbon){

  c_wide <- .carbon |>
    dplyr::filter(c_element != "CF") |>
    dplyr::mutate(c_elements = tidyr::replace_na(.data$c_element, "ratio")) |>
    tidyr::pivot_wider(id_cols = c_lu, names_from = c_element, values_from = c_unit)
  c_wide

  ## FOR TESTING
  c_wide <- c_wide |>
    dplyr::mutate(
      DW = c(10, 15, NA),
      SOC = c(2, 1, NA)
    )

  ## Carbon of trees ######
  if ("ALL" %in% names(c_wide)) {
    c_wide <- c_wide |>
      dplyr::mutate(
        c_tot = dplyr::case_when(
          .data$ALL == "DM" ~ "ALL * CF",
          .data$ALL == "C"  ~ "ALL",
          TRUE ~ NA_character_
        ),
        c_tot_U = dplyr::case_when(
          .data$ALL == "DM" ~ "sqrt(ALL_U^2 + CF_U^2)",
          .data$ALL == "C"  ~ "ALL_U",
          TRUE ~ NA_character_
        )
      )
  }

  if ("AGB" %in% names(c_wide)) {

    if ("RS" %in% names(c_wide)) {
      c_wide <- c_wide |>
        dplyr::mutate(
          c_tree   = "AGB * (1 + RS)",
          c_tree_U = "AGB_U^2 + (RS/(1+RS)*RS_U)^2"
        ) |>
        dplyr::mutate(
          c_tree = dplyr::case_when(
            .data$AGB == "DM" ~ "AGB * (1 + RS) * CF",
            .data$AGB == "C"  ~ "AGB * (1 + RS)",
            TRUE ~ NA_character_
            ),
          c_tree_U = dplyr::case_when(
            .data$AGB == "DM" ~ "sqrt(AGB_U^2 + (RS/(1+RS)*RS_U)^2 + CF_U^2)",
            .data$AGB == "C"  ~ "sqrt(AGB_U^2 + (RS/(1+RS)*RS_U)^2)"
          )
        )
    } else if ("BGB" %in% names(c_wide)) {
      c_wide <- c_wide |>
        dplyr::mutate(
          c_tree = dplyr::case_when(
            .data$AGB == "DM" & .data$BGB == "DM" ~  "(AGB + BGB) * CF",
            .data$AGB == "C"  & .data$BGB == "DM" ~  "AGB * CF + BGB",
            .data$AGB == "DM" & .data$BGB == "C"  ~  "AGB + BGB * CF",
            .data$AGB == "C"  & .data$BGB == "C"  ~  "AGB + BGB",
            TRUE ~ NA_character_
          ),
          c_tree_U = dplyr::case_when(
            .data$AGB == "DM" & .data$BGB == "DM" ~ "sqrt(((AGB * AGB_U)^2 + (BGB * BGB_U)^2)/abs(AGB + BGB) + CF_U^2)",
            .data$AGB == "C"  & .data$BGB == "DM" ~ "sqrt((AGB_U^2 + CF_U^2) * (AGB * CF)^2  + (BGB * BGB_U)^2)/abs(AGB * CF + BGB)",
            .data$BGB == "DM" & .data$AGB == "C"  ~ "sqrt((BGB_U^2 + CF_U^2) * (BGB * CF)^2  + (AGB * AGB_U)^2)/abs(BGB * CF + AGB)",
            .data$AGB == "C"  & .data$BGB == "C"  ~ "sqrt((AGB * AGB_U)^2 + (BGB * BGB_U)^2)/abs(AGB + BGB)",
            TRUE ~ NA_character_
          )
        )
    }

  } ## END IF AGB in names

  other_pools <- c("DW", "SOC", "LI")

  if (any(c("DW", "SOC", "LI") %in% names(c_wide))) {

    c_wide <- c_wide |>
      dplyr::mutate(
        DW = tidyr::replace_na(.data$DW, 0))

  }




}
