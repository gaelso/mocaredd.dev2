#' Check and load the mocaredd input XLSX file
#'
#' @description
#' Loads an XLSX input file (e.g. from a Shiny \code{fileInput} widget), verifies
#' that all required sheets are present, reads the four data tables, and runs six
#' conformity checks:
#' \enumerate{
#'   \item Column names
#'   \item Table sizes
#'   \item Column data types
#'   \item Category variable values
#'   \item Unique IDs
#'   \item Cross-table matching and logical consistency
#' }
#'
#' Each check emits a \code{message()} prefixed with \code{"\u2713"} (pass) or
#' \code{"\u2717"} (fail), so results can be captured outside the function with
#' \code{withCallingHandlers(message = ...)} and displayed in a console-style UI
#' element. A \code{shinyWidgets} progress bar is advanced after each of the
#' \code{n_steps} steps (1 load + 6 checks).
#'
#' @section Server-side usage:
#' \preformatted{
#' log_lines <- character()
#' result <- withCallingHandlers(
#'   tryCatch(
#'     fct_checkinput(.path = path, .pb_session = session, .pb_id = "prog_allchecks"),
#'     error = function(e) {
#'       log_lines <<- c(log_lines, paste0("\u2717 ERROR: ", conditionMessage(e)))
#'       NULL
#'     }
#'   ),
#'   message = function(m) {
#'     log_lines <<- c(log_lines, trimws(conditionMessage(m)))
#'     invokeRestart("muffleMessage")
#'   }
#' )
#' rv$check_log <- log_lines
#' if (!is.null(result)) {
#'   rv$inputs$usr    <- result$data$usr
#'   rv$inputs$time   <- result$data$time
#'   rv$inputs$ad     <- result$data$ad
#'   rv$inputs$cs     <- result$data$cs
#'   rv$checks$all_ok <- result$all_ok
#' }
#' }
#'
#' @param .path       Character. Path to the XLSX file (e.g. \code{input$load_xlsx$datapath}).
#' @param .pb_session Shiny session object used to update the progress bar. Pass \code{NULL}
#'                    (default) to skip progress bar updates (e.g. when calling outside Shiny).
#' @param .pb_id      Character. The \code{id} of the \code{shinyWidgets::progressBar()} to
#'                    update. Ignored when \code{.pb_session} is \code{NULL}.
#' @param .pb_max     Value between 0 and 100, default to 100. What should progress bar max be
#'                    after all checks are passed.
#' @param .minislow   TRUE or FALSE. If TRUE, add a small 0.1 sec sleep after each check.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{\code{all_ok}}{Logical. \code{TRUE} only if every check passed.}
#'   \item{\code{data}}{Named list with elements \code{usr}, \code{time}, \code{ad}, \code{cs}
#'         (the four loaded tables as tibbles).}
#' }
#' The function throws an error (via \code{stop()}) only for unrecoverable failures
#' (unreadable file, missing sheets). All check outcomes are reported via \code{message()}.
#'
#' @importFrom rlang .data
#'
#' @examples
#' path <- system.file("extdata/example1-4pools.xlsx", package = "mocaredd.dev2")
#'
#' log_lines <- character()
#' result <- withCallingHandlers(
#'   fct_checkinput(.path = path),
#'   message = function(m) {
#'     log_lines <<- c(log_lines, trimws(conditionMessage(m)))
#'     invokeRestart("muffleMessage")
#'   }
#' )
#' cat(log_lines, sep = "\n")
#'
#' @export
fct_checkinput <- function(.path, .pb_session = NULL, .pb_id = NULL, .pb_max = 100, .minislow = T) {


  ## !!! FOR TESTING ONLY
  # .path = system.file("extdata/example1-4pools.xlsx", package = "mocaredd.dev2")
  # .path = system.file("extdata/mocaredd-templatev2-4pools.xlsx", package = "mocaredd.dev2")
  # .path = system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2")
  # .pb_id = NULL ; .pb_session = NULL ; .pb_max = 80 ; .minislow = F
  ## !!!

  ##
  ## Constants #################################################################
  ##

  checklist <- list(
    tabs   = c("user_inputs", "time_periods", "AD_lu_transitions", "c_stocks"),
    tabsv2 = c("setup", "time", "area", "carbon"),
    cats   = list(
      c_unit = c("DM", "C"),
      c_pool = c("AGB", "BGB", "DW", "LI", "SOC", "ALL"),
      r_acti = c("DF", "DG", "EN", "EN_AF", "EN_RE"),
      p_type = c("REF", "REF[0-9]", "MON", "MON[0-9]"),
      pdf    = c("normal", "beta"),
      c_var  = c("CF", "RS", "DG_ratio", "C_all")
    ),
    cols = list(
      setup  = c("n_iter", "c_unit", "dg_ext", "dg_pool", "ad_annual",
                 "conf_level", "digits", "ran_seed", "trunc_pdf"),
      time   = c("period_no", "year_start", "year_end", "period_type"),
      area   = c("trans_no", "trans_id", "trans_period", "lu_initial_id", "lu_final_id",
                 "trans_area", "trans_se", "trans_pdf", "trans_pdf_a",
                 "trans_pdf_b", "trans_pdf_c", "redd_activity"),
      carbon = c("c_no", "c_id", "c_period", "c_element", "c_lu_id", "c_value",
                 "c_se", "c_pdf", "c_pdf_a", "c_pdf_b", "c_pdf_c"),
      areav2 = c("trans_no", "trans_period", "lu_initial", "lu_final",
                 "trans_area", "trans_se", "trans_pdf", "trans_pdf_a",
                 "trans_pdf_b", "trans_pdf_c", "redd_activity"),
      carbonv2 = c("c_no", "c_period", "c_element", "c_lu", "c_value",
                   "c_se", "c_pdf", "c_pdf_a", "c_pdf_b", "c_pdf_c")
    )
  )


  ##
  ## Helpers ###################################################################
  ##

  ## Advance the progress bar by one step
  show_progress <- function(.title, .value) {
    if (!is.null(.pb_session) && !is.null(.pb_id)) {
      shinyWidgets::updateProgressBar(
        session = .pb_session,
        id      = .pb_id,
        value   = round(.value),
        title   = .title,
        status  = "primary"
      )
    }
  }

  ## Emit a pass or fail message; mark all_ok FALSE on failure
   show_msg <- function(check, pass_label, fail_label, problems = NULL, stop_on_fail = TRUE) {
    if (check) {
      message("\u2713 ", pass_label)
    } else {
      detail <- if (length(problems) > 0L) {
        paste0(" [", paste(problems, collapse = ", "), "]")
      } else {
        ""
      }
      message("\u2717 ", fail_label, detail)
      if (stop_on_fail) {
        if (!is.null(.pb_session) && !is.null(.pb_id)) {
          shinyWidgets::updateProgressBar(
            session = .pb_session,
            id      = .pb_id,
            value   = 100,
            title   = "Checks halted \u2014 fix the issue above and re-run.",
            status  = "danger"
            )
          }
        message("-- Subsequent checks skipped: fix the issue above first.")
        return(list(all_ok = FALSE, data = NULL))
        #stop(fail_label, detail, call. = FALSE)
        }
    }
  }

  ## Return names of tables whose check flag is FALSE (replaces the repeated
  ## if/else pattern from fct_check_data2)
  failed_tables <- function(check_flags, table_names) {
    table_names[!check_flags]
  }

  ## Make ID from labels
  label2id <- function(label) {
    label |>
      stringr::str_to_lower() |>
      stringr::str_replace_all(" ", "_") |>
      stringr::str_replace("non.forest", "NF") |>
      stringr::str_remove("_forest") |>
      stringr::str_remove("forest")
  }

  ##
  ## State #####################################################################
  ##

  ## 1 load step + 6 checks — update this if steps are added or removed
  n_steps <- 7L
  if (!is.null(.pb_session) && !is.null(.pb_id)) pb_factor <- .pb_max / n_steps

  ## Initiate
  step   <- 0L

  ## If check fails return out_fail
  out_fail <- list(all_ok = FALSE, data = NULL)

  ##
  ## Step 1: Load data #########################################################
  ##

  show_progress("Loading data...", step * pb_factor)

  sheets_found <- tryCatch(
    readxl::excel_sheets(.path),
    error = function(e) NULL
  )
  if (is.null(sheets_found)) {
    message("-- Cannot read the file. Is it a valid .xlsx file?")
    return(out_fail)
  }

  missing_tabsv1 <- setdiff(checklist$tabs, sheets_found)
  missing_tabsv2 <- setdiff(checklist$tabsv2, sheets_found)

  if (length(missing_tabsv1) == 0L) {
    check_version <- 1

    setup  <- readxl::read_xlsx(.path, sheet = "user_inputs",       na = "NA")
    time   <- readxl::read_xlsx(.path, sheet = "time_periods",      na = "NA")
    area   <- readxl::read_xlsx(.path, sheet = "AD_lu_transitions", na = "NA")
    carbon <- readxl::read_xlsx(.path, sheet = "c_stocks",          na = "NA")

    show_msg(check = T, pass_label = paste0("Tables loaded successfully from template v", check_version))

  } else if (length(missing_tabsv2) == 0L) {
    check_version <- 2

    setup  <- readxl::read_xlsx(.path, sheet = "setup" , na = "NA")
    time   <- readxl::read_xlsx(.path, sheet = "time"  , na = "NA")
    area   <- readxl::read_xlsx(.path, sheet = "area"  , na = "NA")
    carbon <- readxl::read_xlsx(.path, sheet = "carbon", na = "NA")

    show_msg(check = T, pass_label = paste0("Tables loaded successfully from template v", check_version))

  } else {
    show_msg(check = F, fail_label = paste0("Missing required sheet(s): ", paste(missing_tabsv2, collapse = ", ")))
    return(out_fail)
  }

  if (.minislow) Sys.sleep(0.1)
  step <- step + 1L

  ##
  ## Check 1: Column names #####################################################
  ##

  show_progress("Checking column names...", step * pb_factor)

  check_cols_setup  <- all(checklist$cols$setup  %in% names(setup))
  check_cols_time   <- all(checklist$cols$time   %in% names(time))
  check_cols_area   <- if (check_version == 1) {
    all(checklist$cols$area %in% names(area))
  } else {
    all(checklist$cols$areav2 %in% names(area))
    }
  check_cols_carbon <- if (check_version == 1) {
    all(checklist$cols$carbon %in% names(carbon))
  } else {
    all(checklist$cols$carbonv2 %in% names(carbon))
  }

  check_cols_all <- all(check_cols_setup, check_cols_time, check_cols_area, check_cols_carbon)

  show_msg(
    check         = check_cols_all,
    pass_label = "Column names: all required columns present",
    fail_label = "Column names: missing columns in",
    problems   = failed_tables(
      c(check_cols_setup, check_cols_time, check_cols_area, check_cols_carbon),
      c("setup/user_inputs", "time/time_periods", "area/AD_lu_transitions", "carbon/c_stocks")
    )
  )

  if (!check_cols_all) return(out_fail)

  if (.minislow) Sys.sleep(0.1)
  step <- step + 1L

  ##
  ## Check 2: Table sizes ######################################################
  ##

  show_progress("Checking table dimensions...", step * pb_factor)

  check_size_setup  <- nrow(setup)  == 1L  # exactly one settings row
  check_size_time   <- nrow(time)   >= 2L  # at least one REF + one MON period
  check_size_area   <- nrow(area)   >= 2L  # at least two land use transitions
  check_size_carbon <- nrow(carbon) >= 2L  # at least one initial + one final C stock

  check_size_all <- all(check_size_setup, check_size_time, check_size_area, check_size_carbon)

  show_msg(
    check         = check_size_all,
    pass_label = "Table sizes: all tables have sufficient rows",
    fail_label = "Table sizes: unexpected row count in",
    problems   = failed_tables(
      c(check_size_setup, check_size_time, check_size_area, check_size_carbon),
      c("setup/user_inputs", "time/time_periods", "area/AD_lu_transitions", "carbon/c_stocks")
    )
  )

  if (!check_size_all) return(out_fail)

  if (.minislow) Sys.sleep(0.1)
  step <- step + 1L

  ##
  ## Check 3: Data types #######################################################
  ##

  show_progress("Checking column data types...", step * pb_factor)

  check_dt_setup <- all(
    is.logical(setup$trunc_pdf),
    is.numeric(setup$n_iter),
    is.numeric(setup$ran_seed)  | is.logical(setup$ran_seed),
    is.character(setup$c_unit),
    is.character(setup$dg_pool) | is.logical(setup$dg_pool),
    is.logical(setup$ad_annual),
    is.numeric(setup$conf_level),
    is.numeric(setup$digits)
  )

  check_dt_time <- all(
    is.character(time$period_no),
    is.numeric(time$year_start),
    is.numeric(time$year_end),
    is.character(time$period_type)
  )

  check_dt_area <- all(
    is.numeric(area$trans_no),
    if (check_version == 1) is.character(area$trans_id) else TRUE,
    is.character(area$trans_period),
    if (check_version == 1) is.character(area$lu_initial_id) else is.character(area$lu_initial),
    if (check_version == 1) is.character(area$lu_final_id) else is.character(area$lu_final),
    # is.numeric(area$trans_area)  | is.logical(area$trans_area),
    # is.numeric(area$trans_se)    | is.logical(area$trans_se),
    is.numeric(area$trans_area),
    is.numeric(area$trans_se),
    is.character(area$trans_pdf),
    is.numeric(area$trans_pdf_a) | is.logical(area$trans_pdf_a),
    is.numeric(area$trans_pdf_b) | is.logical(area$trans_pdf_b),
    is.numeric(area$trans_pdf_c) | is.logical(area$trans_pdf_c),
    is.character(area$redd_activity)
  )

  check_dt_carbon <- all(
    is.numeric(carbon$c_no),
    if (check_version == 1) is.character(carbon$c_id) else TRUE,
    is.character(carbon$c_period),
    is.character(carbon$c_element),
    if (check_version == 1) is.character(carbon$c_lu_id) else is.character(carbon$c_lu),
    # is.numeric(carbon$c_value)  | is.logical(carbon$c_value),
    # is.numeric(carbon$c_se)     | is.logical(carbon$c_se),
    is.numeric(carbon$c_value),
    is.numeric(carbon$c_se),
    is.character(carbon$c_pdf),
    is.numeric(carbon$c_pdf_a)  | is.logical(carbon$c_pdf_a),
    is.numeric(carbon$c_pdf_b)  | is.logical(carbon$c_pdf_b),
    is.numeric(carbon$c_pdf_c)  | is.logical(carbon$c_pdf_c)
  )

  check_dt_all <- all(check_dt_setup, check_dt_time, check_dt_area, check_dt_carbon)

  show_msg(
    check      = check_dt_all,
    pass_label = "Data types: all columns have correct data types",
    fail_label = "Data types: unexpected column types in",
    problems   = failed_tables(
      c(check_dt_setup, check_dt_time, check_dt_area, check_dt_carbon),
      c("setup/user_inputs", "time/time_periods", "area/AD_lu_transitions", "carbon/c_stocks")
    )
  )

  if (!check_dt_all) return(out_fail)

  if (.minislow) Sys.sleep(0.1)
  step <- step + 1L


  ##
  ## Check 4: Category variables ###############################################
  ##

  show_progress("Checking category values...", step * pb_factor)

  ## Parse dg_pool, which can be a comma-separated string e.g. "AGB, BGB"
  dg_pool_vec <- stringr::str_split(setup$dg_pool, pattern = ",") |>
    purrr::map(stringr::str_trim) |>
    unlist()

  check_cats_setup <- all(
    setup$c_unit %in% checklist$cats$c_unit,
    dg_pool_vec  %in% checklist$cats$c_pool
  )

  ptype_pattern  <- paste0(checklist$cats$p_type, collapse = "|")
  check_cats_time <- all(stringr::str_detect(unique(time$period_type), pattern = ptype_pattern))

  check_cats_area <- all(
    unique(area$redd_activity)                    %in% checklist$cats$r_acti,
    unique(stringr::str_to_lower(area$trans_pdf)) %in% checklist$cats$pdf
  )

  check_cats_carbon <- all(
    unique(carbon$c_element)                    %in% c(checklist$cats$c_pool, checklist$cats$c_var),
    unique(stringr::str_to_lower(carbon$c_pdf)) %in% checklist$cats$pdf
  )

  check_cats_all <- all(check_cats_setup, check_cats_time, check_cats_area, check_cats_carbon)

  show_msg(
    check      = check_cats_all,
    pass_label = "Category values: all categories are valid",
    fail_label = "Category values: invalid categories in",
    problems   = failed_tables(
      c(check_cat_setup, check_cat_time, check_cat_area, check_cat_carbon),
      c("setup/user_inputs", "time/time_periods", "area/AD_lu_transitions", "carbon/c_stocks")
    )
  )

  if (!check_cats_all) return(out_fail)

  if (.minislow) Sys.sleep(0.1)
  step <- step + 1L


  ##
  ## Check 5: Unique IDs #######################################################
  ##

  show_progress("Checking unique IDs...", step * pb_factor)

  ## Create unique IDs for 'area' and 'carbon'
  if (check_version == 2) {
    area <- area |>
      dplyr::mutate(
        lu_initial_id = label2id(lu_initial),
        lu_final_id = label2id(lu_final),
        trans_id = paste(trans_period, lu_initial_id, "2", lu_final_id, sep = "*")
      )

    unique_c_period <- length(unique(carbon$c_period)) == 1 & unique(carbon$c_period) == "ALL"

    carbon <- carbon |>
      dplyr::mutate(
        c_lu_id = label2id(c_lu),
        c_id = if (unique_c_period) paste(c_element, c_lu_id, sep = "*") else paste(c_period, c_element, c_lu_id, sep = "*")
      )
  }


  ## Get dg_ext to IDs
  setup$dg_ext <- label2id(setup$dg_ext) |> stringr::str_remove_all("\"")

  ## Check unique IDs
  check_ids_time   <- nrow(time)   == length(unique(time$period_no))
  check_ids_area   <- nrow(area)   == length(unique(area$trans_id))
  check_ids_carbon <- nrow(carbon) == length(unique(carbon$c_id))

  check_ids_all <- all(check_ids_time, check_ids_area, check_ids_carbon)

  show_msg(
    check      = check_ids_all,
    pass_label = "Unique IDs: no duplicates or missing IDs found",
    fail_label = "Unique IDs: duplicate or missing IDs found in",
    problems   = failed_tables(
      c(check_ids_time, check_ids_area, check_ids_carbon),
      c("setup/user_inputs", "time/time_periods", "area/AD_lu_transitions", "carbon/c_stocks")
    )
  )

  if (!check_ids_all) return(out_fail)

  if (.minislow) Sys.sleep(0.1)
  step <- step + 1L


  ##
  ## Check 6: Cross-table matching and logical consistency #####################
  ##

  show_progress("Checking cross-table consistency...", step * pb_factor)

  ## Periods match between AD and time tables
  check_match_period_area <- all(
    sort(unique(area$trans_period)) == sort(unique(time$period_no))
  )

  ## Periods match between CS and time tables (skip when all rows use "ALL")
  carbon_with_period <- carbon |> dplyr::filter(.data$c_period != "ALL")
  check_match_period_carbon <- if (nrow(carbon_with_period) > 0L) {
    all(sort(unique(carbon$c_period)) == sort(unique(time$period_no)))
  } else {
    TRUE
  }

  ## All land uses in 'area' must be present in 'carbon'
  # lu_in_area <- sort(unique(c(area$lu_initial, area$lu_final)))
  # lu_in_carbon <- sort(unique(carbon$c_lu))
  # check_match_lu <- all(lu_in_area %in% lu_in_carbon)

  lu_in_area <- sort(unique(c(area$lu_initial_id, area$lu_final_id)))
  lu_in_carbon <- sort(unique(carbon$c_lu_id))
  check_match_lu_id <- all(lu_in_area %in% lu_in_carbon)

  ## At least one reference and one monitoring period
  check_match_ref <- any(stringr::str_detect(time$period_type, "REF"))
  check_match_mon <- any(stringr::str_detect(time$period_type, "MON"))

  ## Carbon fraction must be provided when unit is dry matter
  #check_match_dm <- (setup$c_unit == "DM" && is.numeric(setup$c_fraction)) || setup$c_unit == "C"
  check_match_dm <- if (setup$c_unit == "DM") {
    if ("CF" %in% unique(carbon$c_element)) {
      all(carbon$c_value[carbon$c_element == "CF"] > 0, carbon$c_se[carbon$c_element == "CF"] > 0)
    } else {
      FALSE
    }
  } else if (setup$c_unit == "C") {
    TRUE
  } else {
    FALSE
  }

  ## If DG_ratio is used, dg_ext must strip degraded LU names back to intact LUs
  if (!is.na(setup$dg_ext) && "DG_ratio" %in% unique(carbon$c_element)) {
    dg_lu_intact <- carbon |>
      dplyr::filter(.data$c_element == "DG_ratio") |>
      dplyr::pull("c_lu_id") |>
      stringr::str_remove(pattern = setup$dg_ext) |>
      stringr::str_replace("[^A-Za-z]$", "") ## Remove trailing special character if any

    check_match_dg <- all(dg_lu_intact %in% unique(carbon$c_lu_id))
  } else if (is.na(setup$dg_ext) && "DG_ratio" %in% unique(carbon$c_element)) {
    check_match_dg <- FALSE   # DG_ratio used but no extension provided
  } else {
    check_match_dg <- TRUE
  }

  match_flags  <- c(check_match_period_area, check_match_period_carbon, check_match_lu_id,
                    check_match_ref, check_match_mon, check_match_dm, check_match_dg)
  match_labels <- c(
    "trans_period does not match period_no",
    "c_period does not match period_no",
    "land use IDs present in AD/area but missing from c_stocks/carbon",
    "no REF period found in time_periods",
    "no MON period found in time_periods",
    "c_unit is 'DM' but CF is missing in c_stocks/carbon",
    "DG_ratio used but dg_ext does not resolve to existing LU IDs"
  )

  check_match_all <- all(match_flags)

  show_msg(
    check      = check_match_all ,
    pass_label = "Cross-table consistency: all references match",
    fail_label = "Cross-table consistency: mismatches found",
    problems   = match_labels[!match_flags]
  )

  if (!check_match_all) return(out_fail)


  ##
  ## Final progress bar update #################################################
  ##

  all_ok <- all(
    check_cols_all, check_size_all, check_dt_all, check_cats_all, check_ids_all,
    check_match_all
    )

  if (!is.null(.pb_session) && !is.null(.pb_id)) {
    shinyWidgets::updateProgressBar(
      session = .pb_session,
      id      = .pb_id,
      value   = 100,
      title   = if (all_ok) "All checks passed!" else "Some checks failed — see details.",
      status  = if (all_ok) "success" else "danger"
    )
  }

  if (all_ok) {
    message("-- All checks passed.")
  } else {
    message("-- One or more checks failed. Fix the issues above before running simulations.")
  }


  ##
  ## Return ####################################################################
  ##

  list(
    all_ok = all_ok,
    data   = list(setup = setup, time = time, area = area, carbon = carbon)
  )

}
