

fct_cstock_formula <- function(.carbon){

  ## !!! FOR TESTING ONLY
  # .carbon <- carbon |> dplyr::filter(c_element != "CF")
  ## !!!

  c_wide <- .carbon |>
    dplyr::mutate(c_elements = tidyr::replace_na(.data$c_element, "ratio")) |>
    tidyr::pivot_wider(id_cols = c_lu, names_from = c_element, values_from = c_unit)
  c_wide

  ## Btot
  if ("AGB" %in% names(c_wide)) {

    if ("RS" %in% names(c_wide)) {

      c_wide <- c_wide |>
        dplyr::mutate(
          tree   = "AGB * (1 + RS)",
          tree_U = "AGB_U^2 + (RS/(1+RS)*RS_U)^2"
        ) |>
        dplyr::mutate(
          tree = dplyr::case_when(
            .data$AGB == "DM" ~ paste0("AGB * (1 + RS) * CF"),
            .data$AGB == "C"  ~ paste0("AGB * (1 + RS)"),
            TRUE ~ NA_character_
            ),
          tree_U = dplyr::case_when(
            .data$AGB == "DM" ~ paste0("sqrt(AGB_U^2 + (RS/(1+RS)*RS_U)^2 + CF_U^2)"),
            .data$AGB == "C"  ~ paste0("sqrt(AGB_U^2 + (RS/(1+RS)*RS_U)^2)")
          )
        )


    } else if ("BGB" %in% names(c_wide)) {

      btot   <- "(AGB + BGB)"
      btot_U <- "sqrt((AGB * AGB_U)^2 + (BGB * BGB_U)^2)/abs(AGB + BGB)"

      c_wide <- c_wide |>
        dplyr::mutate(
          tree   = "(AGB + BGB)",
          tree_U = "(AGB * AGB_U)^2 + (BGB * BGB_U)^2"
        ) |>
        dplyr::mutate(
          tree = dplyr::case_when(
            .data$AGB == "DM" & .data$BGB == "DM" ~  paste0("(AGB + BGB) * CF"),
            .data$AGB == "C"  & .data$BGB == "DM" ~  paste0("AGB * CF + BGB"),
            .data$AGB == "DM" & .data$BGB == "C"  ~  paste0("AGB + BGB * CF"),
            .data$AGB == "C"  & .data$BGB == "C"  ~  paste0("AGB + BGB"),
            TRUE ~ NA_character_
          ),
          tree_U = dplyr::case_when(
            .data$AGB == "DM" & .data$BGB == "DM" ~  paste0("(AGB + BGB) * CF"),
            .data$AGB == "C"  & .data$BGB == "DM" ~  paste0("AGB * CF + BGB"),
            .data$AGB == "DM" & .data$BGB == "C"  ~  paste0("AGB + BGB * CF"),
            .data$AGB == "C"  & .data$BGB == "C"  ~  paste0("AGB + BGB"),
            TRUE ~ NA_character_
          ),
        )

    }

  }




}
