

fct_make_EF <- function(.dat) {

  ## !!! FOR TESTING ONLY
  rv <- list(checks = list(), inputs = list(), sims = list(), res = list())
  .path = system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2")
  check_result = fct_checkinput(.path = .path, .minislow = 0.2)
  rv$checks$all_ok <- isTRUE(check_result$all_ok)
  .dat <- check_result$data
  ## !!!

  ##
  ## Helpers ######
  ##

  ## Output in case of error
  out_fail <- list(all_ok = FALSE, data = NULL)

  ## Make vector of expected carbon elements (cf. checklist in fct_checkinput())
  vec_cel_init <- c("ALL", "AGB", "BGB", "DW", "LI", "SOC", "CF", "RS", "DG_ratio")

  ## Identify missing C elements for full carbon pools representation
  missing_cols <- setdiff(vec_cel_init, unique(.dat$carbon$c_element))

  ## carbon_formula() -- construct the carbon formula from the elements in carbon table for each lu x time period
  # carbon_formula <- function(.dat$carbon_txt) {
  #   vec <- stringr::str_split_1(.dat$carbon_txt, pattern = ",")
  #
  #   ## Initiate formula
  #   c_eq_txt <- "(AGB + BGB)*CF + DW + LI + SOC"
  #   c_eq <- c("(",  "AGB", " + ", "BGB", ")", " * ", "CF", " + ", "DW", " + ", "LI", " + ", "SOC")
  #   names(c_eq) <- c("cf_(", "AGB", "plus_bgb", "BGB", "cf_)", "times_cf", "CF", "plus_dw", "DW", "plus_li", "LI", "plus_soc", "SOC")
  #
  #   ## ALL pools combined
  #   if (vec[1] == "ALL" && "CF" %in% vec) return("ALL*CF")
  #   if (vec[1] == "ALL" && !"CF" %in% vec) return("ALL")
  #
  #   ## Remove missing elements in formula
  #   if (!"CF" %in% vec){
  #     c_eq <- c_eq[!(names(c_eq) %in% c("cf_(", "cf_)", "times_cf", "CF"))]
  #   }
  #   if (!"BGB" %in% vec & !"RS" %in% vec){
  #     c_eq <- c_eq[!(names(c_eq) %in% c("plus_bgb", "BGB", "cf_(", "cf_)"))]
  #   } else if (!"BGB" %in% vec & "RS" %in% vec) {
  #     c_eq["BGB"] <- "AGB * RS"
  #   }
  #   if (!'DW' %in% vec)  c_eq <- c_eq[!(names(c_eq) %in% c("plus_dw", "DW"))]
  #   if (!'LI' %in% vec)  c_eq <- c_eq[!(names(c_eq) %in% c("plus_li", "LI"))]
  #   if (!'SOC' %in% vec) c_eq <- c_eq[!(names(c_eq) %in% c("plus_soc", "SOC"))]
  #
  #   ## Output
  #   paste(c_eq, collapse = "")
  # }

  # carbon_formula("AGB,RS,CF")
  # carbon_formula("AGB,RS,SOC,CF")

  ##
  ## Separate ratios ######
  ##

  if ("CF" %in% .dat$carbon$c_element) {
    has_CF <- TRUE
    CF     <- .dat$carbon |> dplyr::filter(c_element == "CF") |> dplyr::pull("c_value")
    CF_se  <- .dat$carbon |> dplyr::filter(c_element == "CF") |> dplyr::pull("c_se")
    CF_pdf <- .dat$carbon |> dplyr::filter(c_element == "CF") |> dplyr::pull("c_pdf")
    carbon <- .dat$carbon |> dplyr::filter(c_element != "CF")
  } else {
    has_CF <- FALSE
    CF     <- NA
    CF_se  <- NA
    CF_pdf <- NA
    carbon <- .dat$carbon
  }

  if ("DG_ratio" %in% unique(carbon$c_element)) {
    has_DGratio <- TRUE
    carbon_pools <- carbon |> dplyr::filter(c_element != "DG_ratio")
    carbon_deg   <- carbon |> dplyr::filter(c_element == "DG_ratio")
  } else {
    has_DGratio <- FALSE
    carbon_pools <- carbon
    carbon_deg <- NULL
  }

  ## Check if BGB expressed as ratio
  # if ("RS" %in% .dat$carbon$c_element && !"BGB" %in% .dat$carbon$c_element) {
  #   has_RS <- TRUE
  #   vec_cel <- vec_cel_init[vec_cel_init != "BGB"]
  # } else if (!"RS" %in% .dat$carbon$c_element && "BGB" %in% .dat$carbon$c_element) {
  #   has_RS <- FALSE
  #   vec_cel <- vec_cel_init[vec_cel_init != "RS"]
  # } else {
  #   message("-- Issue with belowground biomass, both RS and BGB in the data or missing.")
  #   return(out_fail)
  # }

  ##
  ## Test - rely on formula implemented rowwise ####
  ##

  ## Make chr of carbon element per c_lu_id x c_period
  # carbon_combined <- carbon |>
  #   dplyr::summarise(
  #     carbon_grouped = paste(c_element, collapse = ","),
  #     .by = c(c_lu_id, c_lu, c_period)
  #     )
  #
  # if (has_CF) {
  #   carbon_combined <- carbon_combined |>
  #     dplyr::mutate(carbon_grouped = paste(carbon_grouped, "CF", sep = ","))
  # }
  #
  # carbon_combined <- carbon_combined |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(c_form = carbon_formula(carbon_grouped)) |>
  #   dplyr::ungroup()

  ##
  ## Make wide tables for LU x time periods ######
  ##


  ## + C pools ======
  carbon_wide_value <- carbon_pools |>
    tidyr::pivot_wider(id_cols = c(c_period, c_plu_id, c_lu_id, c_lu, c_unit), names_from = "c_element", values_from = c_value) |>
    dplyr::mutate(
      ## Add CF column
      CF = ifelse(c_unit == "DM", CF, NA),
      ## Add other missing columns to get all pools
      !!!purrr::set_names(rep(NA, length(missing_cols)), missing_cols),
      ## Calculate carbon C
      BGB = dplyr::case_when(
        !is.na(RS) & !is.na(AGB) ~ AGB * RS,
        !is.na(BGB)              ~ BGB,
        TRUE ~ NA_real_
        ),
      C = dplyr::case_when(
        !is.na(ALL) & c_unit == "C" ~ ALL,
        !is.na(ALL) & c_unit == "DM" ~ ALL * CF,
        TRUE ~ rowSums(dplyr::across(c(AGB, BGB, DW, LI, SOC)), na.rm = TRUE)
      )
    )
  carbon_wide_value

  ## + DG with ratio ======
  if (has_DGratio) {
    c_plu_intact <- carbon_deg |> dplyr::pull(c_plu_id)

    dg_lu_intact <- carbon_deg |>
      dplyr::filter(.data$c_element == "DG_ratio") |>
      dplyr::mutate(
        c_plu_id_intact = c_plu_id |>
          stringr::str_remove(pattern = .dat$setup$dg_ext) |>
          stringr::str_replace("[^A-Za-z]$", ""), ## Remove trailing special character if any
        c_element = .dat$setup$dg_pool
      ) |>
      dplyr::select(c_plu_id_intact, c_plu_id, c_element, c_value)

  }




    #dplyr::mutate(dplyr::across(.cols = dplyr::where(is.numeric), .fns = ~tidyr::replace_na(.x, 0))) |>
    dplyr::mutate(CF = ifelse(c_unit == "DM", CF, NA)) |>
    dplyr::mutate()





}
