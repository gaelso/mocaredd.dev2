

fct_cstock_formula <- function(.carbon){

  c_wide <- .carbon |>
    dplyr::filter(c_element != "CF") |>
    dplyr::mutate(c_elements = tidyr::replace_na(.data$c_element, "ratio")) |>
    tidyr::pivot_wider(id_cols = c(c_period, c_lu), names_from = c_element, values_from = c_unit) |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::select("row_id", dplyr::everything())
  c_wide

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
          c_tree = dplyr::case_when(
            .data$AGB == "DM" ~ "AGB * (1 + RS) * CF",
            .data$AGB == "C"  ~ "AGB * (1 + RS)",
            TRUE ~ NA_character_
            ),
          c_tree_U = dplyr::case_when(
            .data$AGB == "DM" ~ "sqrt(AGB_U^2 + (RS/(1+RS)*RS_U)^2 + CF_U^2)",
            .data$AGB == "C"  ~ "sqrt(AGB_U^2 + (RS/(1+RS)*RS_U)^2)",
            TRUE ~ NA_character_
          ),
          ## E2 = (X*X_U)^2
          c_tree_E2 = dplyr::case_when(
            .data$AGB == "DM" ~ "(AGB * (1 + RS) * CF)^2 * (AGB_U^2 + (RS / (1 + RS) * RS_U)^2 + CF_U^2)",
            .data$AGB == "C"  ~ "(AGB * (1 + RS))^2 * (AGB_U^2 + (RS / (1 + RS) * RS_U)^2)",
            TRUE ~ NA_character_
          )
        )
    } else if ("BGB" %in% names(c_wide)) {
      c_wide <- c_wide |>
        dplyr::mutate(
          c_tree = dplyr::case_when(
            .data$AGB == "DM" & .data$BGB == "DM" ~  "(AGB + BGB) * CF",
            .data$AGB == "C"  & .data$BGB == "C"  ~  "AGB + BGB",
            TRUE ~ NA_character_
          ),
          c_tree_U = dplyr::case_when(
            .data$AGB == "DM" & .data$BGB == "DM" ~ "sqrt( ( (AGB * AGB_U)^2 + (BGB * BGB_U)^2 ) / (abs(AGB + BGB))^2 + CF_U^2 )",
            .data$AGB == "C"  & .data$BGB == "C"  ~ "sqrt((AGB * AGB_U)^2 + (BGB * BGB_U)^2) / abs(AGB + BGB)",
            TRUE ~ NA_character_
          ),
          ## E2 = (X*X_U)^2
          ## For DM:
          ## E2 = [ (AGB + BGB) * CF ]^2 * [ (AGB * AGB_U)^2 + (BGB * BGB_U)^2 ) / (AGB + BGB)^2 + CF_U^2 ]
          ##    =  (AGB + BGB)^2 * CF^2 * [ (AGB * AGB_U)^2 + (BGB * BGB_U)^2 ] / (AGB + BGB)^2 + (AGB + BGB)^2 * CF^2 * CF_U^2
          ##    = CF^2 * [ (AGB * AGB_U)^2 + (BGB * BGB_U)^2 ] + [ (AGB + BGB)^2 * CF^2 * CF_U^2 ]
          ##    = CF^2 * ( (AGB * AGB_U)^2 + (BGB * BGB_U)^2 + (AGB + BGB)^2 * CF_U^2 )
          c_tree_E2 = dplyr::case_when(
            .data$AGB == "DM" & .data$BGB == "DM" ~ "CF^2 * ( (AGB * AGB_U)^2 + (BGB * BGB_U)^2 + (AGB + BGB)^2 * CF_U^2 )",
            .data$AGB == "C"  & .data$BGB == "C"  ~ "(AGB * AGB_U)^2 + (BGB * BGB_U)^2",
            TRUE ~ NA_character_
          )
        )
    } else {
      c_wide <- c_wide |>
        dplyr::mutate(
          c_tree = dplyr::case_when(
            .data$AGB == "DM" ~ "AGB * CF",
            .data$AGB == "C"  ~ "AGB",
            TRUE ~ NA_character_
          ),
          c_tree_U = dplyr::case_when(
            .data$AGB == "DM" ~ "sqrt(AGB_U^2 + CF_U^2)",
            .data$AGB == "C"  ~ "AGB_U",
            TRUE ~ NA_character_
          ),
          c_tree_E2 = dplyr::case_when(
            .data$AGB == "DM" ~ "(AGB_U^2 + CF_U^2) * AGB^2 * CF^2",
            .data$AGB == "C"  ~ "(AGB * AGB_U)^2",
            TRUE ~ NA_character_
          )
        )
    }

  } ## END IF AGB in names

  pools_other <- intersect(c("DW", "SOC", "LI"), names(c_wide))

  if (length(pools_other) > 0) {

    oth_formula <- c_wide |>
      dplyr::select(row_id, dplyr::any_of(pools_other)) |>
      tidyr::pivot_longer(-row_id, names_to = "var", values_to = "type",
                          values_drop_na = TRUE) |>
      dplyr::group_by(row_id) |>
      dplyr::summarise(
        c_oth = stringr::str_c(var, collapse = " + "),
        c_oth_U = stringr::str_c(
          "sqrt(", stringr::str_c("(", var, " * ", var, "_U)^2", collapse = " + "), ")",
          " / abs(", stringr::str_c(var, collapse = "+"), ")"
        ),
        c_oth_E2 = stringr::str_c("(", var, "*", var, "_U)^2", collapse = " + "),
        .groups = "drop"
      )

    c_wide <- c_wide |>
      dplyr::mutate(c_oth = NA, c_oth_U = NA, c_oth_E2 = NA) |>
      dplyr::left_join(oth_formula, by = "row_id", suffix = c("_rm", "")) |>
      dplyr::select(-dplyr::ends_with("_rm"))
  } else {
    c_wide <- c_wide |>
      dplyr::mutate(c_oth = NA, c_oth_U = NA, c_oth_E2 = NA)
  }

  c_wide2 <- c_wide |>
    dplyr::mutate(
      c_tot2_U = paste0("sqrt(", .data$c_tree_E2, " + ", .data$c_oth_E2, ") / (", .data$c_tot, ")"),
      c_tot2 = paste(.data$c_tree, .data$c_oth, sep = " + ")
    )

  c_wide <- c_wide |>
    dplyr::mutate(
      c_tot = dplyr::case_when(
        !is.na(.data$c_tot)                        ~ .data$c_tot,
        !is.na(.data$c_tree) &  is.na(.data$c_oth) ~ .data$c_tree,
         is.na(.data$c_tree) & !is.na(.data$c_oth) ~ .data$c_oth,
        !is.na(.data$c_tree) & !is.na(.data$c_oth) ~ paste(.data$c_tree, .data$c_oth, sep = " + "),
        TRUE ~ NA_character_
      )
    ) |>
    # c_tot_U must be in a separate mutate so it sees the finalized c_tot above,
    # not the pre-merge value from the ALL block (which is NA for tree-only rows)
    dplyr::mutate(
      c_tot_U = dplyr::case_when(
        !is.na(.data$c_tot_U)                      ~ .data$c_tot_U,
        !is.na(.data$c_tree) &  is.na(.data$c_oth) ~ .data$c_tree_U,
        is.na(.data$c_tree)  & !is.na(.data$c_oth) ~ .data$c_oth_U,
        !is.na(.data$c_tree) & !is.na(.data$c_oth) ~ paste0("sqrt(", .data$c_tree_E2, " + ", .data$c_oth_E2, ") / (", .data$c_tot, ")"),
        TRUE ~ NA_character_
      )
    )

    c_wide

}
