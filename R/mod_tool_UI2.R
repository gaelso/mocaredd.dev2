#' Tool module UI function (v2)
#'
#' Changes from v1:
#' - div_check_progress now includes a scrollable console div and a
#'   "Show check results" button, mirroring the arenalytics data-upload pattern.
#' - div_btn_show_check removed (button absorbed into div_check_progress).
#' - card_check_msg removed from the check panel: check outcomes are now
#'   displayed in the console div rather than a separate gt table.
#'
#' @noRd
mod_tool_UI2 <- function(id, i18n){

  ns <- NS(id)


  ##
  ## UI Elements ###############################################################
  ##

  ## + Sidebar =================================================================

  ## ++ Accordion 1: load data -------------------------------------------------
  ac_load <- accordion_panel(
    title = i18n$t("Upload your data"),
    icon  = bsicons::bs_icon("1-circle"),
    value = ns("ac_load"),

    div(
      p("{mocaredd} only accepts XLSX files that follow a specific template.
        Download the template here if you haven't converted your data yet."),
      downloadButton(
        outputId = ns("dl_template_v2_simple"),
        label    = "v2 simple",
        class    = "btn-outline-secondary btn-small form-group"
      ),
      downloadButton(
        outputId = ns("dl_template_v2_inter"),
        label    = "v2 intermediate",
        class    = "btn-outline-secondary btn-small form-group"
      ),
      div(
        downloadButton(
          outputId = ns("dl_template_v1_simple"),
          label    = "(legacy) v1 simple",
          class    = "btn-outline-secondary btn-small form-group"
        ),
        style = "color: grey;"
      ),
      style = "margin-bottom: 0.5rem;"
    ),

    div(
      p("Once your data has been converted to the app's template, upload it here:"),
      fileInput(
        inputId = ns("load_xlsx"),
        accept  = ".xlsx",
        label   = NULL
      )
    ),

    ## Status messages
    div(
      id    = ns("msg_no_data"),
      "No data uploaded.",
      class = "text-warning",
      style = "font-style: italic;"
    ),
    shinyjs::hidden(div(
      id    = ns("msg_data_tabs_ok"),
      "Data uploaded with correct tabs.",
      class = "text-success",
      style = "font-style: italic;"
    )),
    shinyjs::hidden(div(
      id    = ns("msg_data_tabs_wrong"),
      "Data uploaded with incorrect tabs.",
      class = "text-danger",
      style = "font-style: italic;"
    )),

    div(
      shinyjs::disabled(
        actionButton(inputId = ns("btn_run_checks"), label = "Run checks")
      ),
      style = "margin-top: 1rem;"
    )
  )


  ## ++ Accordion 2: Run MCS ---------------------------------------------------
  ac_mcs <- accordion_panel(
    title = i18n$t("Create Monte Carlo Simulations"),
    icon  = bsicons::bs_icon("2-circle"),
    value = ns("ac_mcs"),

    div(
      id    = ns("msg_no_check"),
      "Run checks first.",
      class = "text-warning",
      style = "font-style: italic;"
    ),
    shinyjs::hidden(div(
      id    = ns("msg_checks_ok"),
      "All checks passed.",
      class = "text-success",
      style = "font-style: italic;"
    )),
    shinyjs::hidden(div(
      id    = ns("msg_checks_wrong"),
      "Checks not passed.",
      class = "text-danger",
      style = "font-style: italic;"
    )),

    div(
      shinyjs::disabled(
        actionButton(inputId = ns("btn_run_mcs"), label = "Run simulations")
      ),
      style = "margin-top: 1rem;"
    )
  )


  ## ++ Accordion 3: Sensitivity -----------------------------------------------
  ac_sens <- accordion_panel(
    title = i18n$t("Perform sensitivity analysis"),
    icon  = bsicons::bs_icon("3-circle"),
    value = ns("ac_sens"),
    h4("coming soon")
  )


  ## + Check panel =============================================================

  ## ++ Initial message --------------------------------------------------------
  div_check_init <- div(
    id    = ns("check_init_msg"),
    bsicons::bs_icon("arrow-left"), " Start with uploading your data in the sidebar.",
    class = "text-warning",
    style = "font-style: italic;"
  )

  ## ++ Progress bar + console -------------------------------------------------
  ## Hidden on load; revealed when btn_run_checks is clicked.
  ## The console div receives one <br>-terminated line per message() emitted
  ## by fct_checkinput() via shinyjs::html(..., add = TRUE).
  ## The "Show check results" button is enabled by the server once all checks
  ## have completed; clicking it hides this div and reveals the results below.
  div_check_progress <- shinyjs::hidden(div(
    id = ns("check_progress"),

    h4("Checking data"),

    shinyWidgets::progressBar(
      id          = ns("prog_allchecks"),
      value       = 0,
      title       = "Starting checks...",
      display_pct = TRUE
    ),

    br(),

    ## Scrollable console output
    div(
      id    = ns("check_console"),
      style = paste(
        "height: 250px;",
        "overflow-y: auto;",
        "background-color: #f8f9fa;",
        "font-family: monospace;",
        "font-size: small;",
        "padding: 0.5rem;",
        "border-radius: 4px;",
        "border: 1px solid #dee2e6;"
      )
    ),

    br(),

    shinyjs::disabled(
      actionButton(inputId = ns("btn_show_checks"), label = "Show check results")
    )
  ))


  ## ++ Check panel ------------------------------------------------------------

  ## Value boxes
  vb_time <- value_box(
    title    = "Time periods",
    value    = htmlOutput(ns("vb_nb_time")),
    showcase = bsicons::bs_icon("calendar3", size = "40px"),
    theme    = "primary",
    textOutput(ns("vb_nb_ref")),
    textOutput(ns("vb_nb_mon"))
  )

  vb_ad <- value_box(
    title    = "Land use transitions",
    value    = htmlOutput(ns("vb_nb_trans")),
    showcase = bsicons::bs_icon("pin-map", size = "40px"),
    theme    = "secondary",
    textOutput(ns("vb_nb_lu")),
    textOutput(ns("vb_nb_redd"))
  )

  vb_cs <- value_box(
    title    = "Carbon stock",
    value    = htmlOutput(ns("vb_nb_pools")),
    showcase = bsicons::bs_icon("arrow-repeat", size = "48px"),
    theme    = "warning",
    textOutput(ns("vb_c_pools")),
    textOutput(ns("vb_dg_method"))
  )

  ## Check cards
  card_arithmetic_gg <- card(
    h5(i18n$t("Arithmetic mean emission reductions per period (tCO2e/y)")),
    plotOutput(ns("check_arithmetic_gg"))
  )

  card_lumatrix <- card(
    h5(i18n$t("Land use change matrix")),
    layout_column_wrap(
      width = "200px", fixed_width = TRUE,
      uiOutput(outputId = ns("check_select_period_UI")),
      div(
        verbatimTextOutput(outputId = ns("check_show_period_type")),
        style = "margin-top: 29px;"
      )
    ),
    gt::gt_output(ns("check_lumatrix"))
  )

  ## Check panel layout
  div_check_panel <- shinyjs::hidden(div(
    id = ns("check_panel"),
    layout_column_wrap(
      fill = FALSE,
      vb_time, vb_ad, vb_cs
    ),
    br(),
    layout_column_wrap(
      fill = FALSE,
      card_arithmetic_gg,
      card_lumatrix
    )
  ))


  ## + Results panel ===========================================================

  div_res_init <- div(
    id    = ns("res_init"),
    bsicons::bs_icon("gear"), i18n$t(" Run simulations in the sidebar."),
    class = "text-warning",
    style = "font-style: italic;"
  )

  div_res_progress <- shinyjs::hidden(div(
    id = ns("res_progress"),
    shinyWidgets::progressBar(
      id          = ns("prog_res"),
      value       = 0,
      title       = "Simulations progress",
      display_pct = TRUE
    )
  ))

  div_res_show <- shinyjs::hidden(div(
    id = ns("res_show"),
    actionButton(inputId = ns("btn_show_res"), label = "Show simulation results")
  ))

  ## ++ Res cards --------------------------------------------------------------
  card_res_dl <- card(
    card_body(
      fillable = FALSE,
      h5(i18n$t("Download the simulations and aggregated results")),
      downloadButton(
        outputId = ns("dl_ari"),
        label    = "Download the arithmetic mean ERs",
        class    = "btn-outline-secondary btn-small form-group"
      ),
      downloadButton(
        outputId = ns("dl_res"),
        label    = "Download the simulated ERs",
        class    = "btn-outline-secondary btn-small form-group"
      ),
      downloadButton(
        outputId = ns("dl_sim_ER"),
        label    = "Download all the ER simulations",
        class    = "btn-outline-warning btn-small form-group"
      ),
      downloadButton(
        outputId = ns("dl_sim_trans"),
        label    = "Download all the land use transition simulations",
        class    = "btn-outline-warning btn-small form-group"
      )
    )
  )

  card_res_fp <- card(
    full_screen = TRUE,
    h5(i18n$t("Emission reductions details")),
    gt::gt_output(ns("res_ER_fp"))
  )

  card_res_gg <- card(
    full_screen = TRUE,
    h5(i18n$t("Emission reductions histogram")),
    uiOutput(outputId = ns("res_select_ER_hist_UI")),
    plotOutput(ns("res_ER_hist"))
  )

  card_redd_fp <- card(
    full_screen = TRUE,
    h5(i18n$t("REDD+ Activity details")),
    gt::gt_output(ns("res_redd_fp"))
  )

  card_redd_hist <- card(
    full_screen = TRUE,
    h5(i18n$t("REDD+ activity histograms")),
    uiOutput(outputId = ns("res_select_redd_hist_UI")),
    uiOutput(outputId = ns("res_select_period_hist_UI")),
    plotOutput(ns("res_redd_hist"))
  )

  div_res_cards <- shinyjs::hidden(div(
    id = ns("res_cards"),
    card_res_dl,
    layout_columns(col_widths = c(8, 4), card_res_fp, card_res_gg),
    layout_columns(col_widths = c(8, 4), card_redd_fp, card_redd_hist)
  ))


  ## + Sensitivity panel =======================================================
  div_trans_forestplot <- div(
    id = ns("trans_forestplot"),
    gt::gt_output(ns("sens_trans_fp"))
  )


  ##
  ## Layout ####################################################################
  ##

  tagList(

    h2(i18n$t("Run the uncertainty analysis")),

    br(),

    navset_card_tab(
      id = ns("tool_tabs"),

      sidebar = sidebar(
        width = "300px",
        accordion(
          open     = TRUE,
          multiple = TRUE,
          ac_load, ac_mcs, ac_sens
        )
      ),

      nav_spacer(),

      ## + Check panel =========================================================
      nav_panel(
        title = i18n$t("Check your data"),
        value = "check_tab",
        icon  = icon("circle-check"),
        div_check_init,
        div_check_progress, ## progress bar + console + show-results button
        div_check_panel
      ),

      ## + Results panel =======================================================
      nav_panel(
        title = i18n$t("Results"),
        value = "res_tab",
        icon  = icon("chart-simple"),
        div_res_init,
        div_res_progress,
        div_res_show,
        div_res_cards
      ),

      ## + Sensitivity panel ===================================================
      nav_panel(
        title = i18n$t("Sensitivity"),
        value = "sensi_tab",
        icon  = icon("magnifying-glass"),
        div_trans_forestplot
      )

    ) ## END navset_card_tab()

  ) ## END tagList

} ## END module UI function
