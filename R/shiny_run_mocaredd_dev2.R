#' Run Monte Carlo for REDD+ uncertainty's shiny app
#'
#' @description Starts a R Shiny application that guide users to fill in context and
#'              land use changes for forest related greenhouse gas emissions and removals.
#'              Inputs include distribution parameters for various distribution then the app
#'              returns error propagation and sensitivity analysis of the overall uncertainty
#'              for GHG estimates.
#'
#' @param ... arguments to pass to shinyApp
#'
#' @import shiny
#' @import bslib
#' @importFrom rlang .data
#'
#' @examples
#' if (interactive()) {
#'
#' shiny_run_mocaredd_dev2()
#'
#' }
#'
#' @export
shiny_run_mocaredd_dev2 <- function(...) {

  ##
  ## GLOBAL ####################################################################
  ##

  ## + Initiate translation ====================================================

  ## !!! In a package the translation folder needs to be directed to the package location
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file("assets/translations.json", package = "mocaredd.dev2")
    )
  i18n$set_translation_language('en')


  ## + UI Elements =============================================================

  ## App title with logo
  app_title <- div(
    tags$a(
      href = "./",
      alt = "arena-helpers",
      tags$img(src="assets/arena-helpers3.png", height = '60px'),
      .noWS = "before-end"
    ),
    i18n$t("Monte Carlo Simulations for REDD+"),
    style = "display:inline;font-color: black !important"
  )

  app_window_title <- "Mocaredd"

  ## App colors
  app_primary_color   <- "#4991B0"
  app_secondary_color <- "#77AB16"

  ## App theme
  app_theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "yeti",
    base_font = bslib::font_collection(
      "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica Neue",
      "Arial", "Noto Sans", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji",
      "Segoe UI Symbol","Noto Color Emoji"
    ),
    code_font = bslib::font_google("Fira Code"),
    heading_font = bslib::font_google("Inter"),
    primary = app_primary_color,
    secondary = app_secondary_color,
  )

  ## Dropdown list for language selection
  language_selector <- shinyWidgets::pickerInput(
    inputId = "language",
    label = NULL,
    choices = c("en"),
    choicesOpt =  list(content = c('<i class="fi fi-gb"></i> EN')),
    # choices = c("en", "fr", "sp"),
    # choicesOpt =  list(content = c('<i class="fi fi-gb"></i> EN', '<i class="fi fi-fr"></i> FR', '<i class="fi fi-es"></i> ES')),
    selected = "en",
    width = "auto",
    option = shinyWidgets::pickerOptions(style = "z-index:10000;")
  )

  ## Footer div
  app_footer <- div(
    class = "container footer text-center",
    tags$small(
      "(c) 2026 MyApp - Developed by ",
      tags$strong("Your Name"),
      " - With the support of XYZ Institute"
    )
  )


  ##
  ## UI ########################################################################
  ##

  ui <- shiny::tagList(

    ## + Setup =================================================================

    shiny::withMathJax(),
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    shiny.i18n::usei18n(i18n),
    tags$head(tags$script(src = "assets/js_activate_tab.js")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "assets/style.css")),
    htmltools::htmlDependency(
      name = "flag-icons",
      version = "6.6.6",
      src = c(href="https://cdn.jsdelivr.net/gh/lipis/flag-icons@6.6.6/"),
      stylesheet = "css/flag-icons.min.css"
    ),

    ## + Layout UI elements ====================================================

    bslib::page_navbar(
      id = "navbar",
      title = app_title,
      window_title = app_window_title,
      theme = app_theme,
      navbar_options = navbar_options(
        bg = "#f8f9fa",
        position = "fixed-top",
        class = "navbar-expand-sm"
      ),
      fillable = FALSE, ## Not needed for now, make a tab fill the whole browser, cool for leaflets
      # inverse = FALSE, ## Not working well with yeti, overridden in assets/styles.css
      footer = app_footer,

      ## + Panels ####
      nav_spacer(), ## align menu to the right

      nav_panel(
        title = i18n$t("Home"),
        value = "home",
        #icon = icon("campground"),
        mod_home_UI2("tab_home", i18n = i18n)
      ),

      nav_panel(
        title = i18n$t("Tool"),
        value = "tool",
        #icon = icon("mug-hot"),
        mod_tool_UI2("tab_tool", i18n = i18n)
      ),

      # nav_panel(
      #   title = i18n$t("About"),
      #   value = "about",
      #  #icon = icon("info"),
      #   mod_about_UI("tab_about", i18n = i18n)
      # ),

      nav_item(language_selector)

    ) |> ## End page_navbar
      ## Make navbar larger before switch to menu button
      shiny::tagAppendAttributes(.cssSelector = "nav", class = "navbar-expand-md")
  ) ## End tagList


  ##
  ## Server ####################################################################
  ##

  server <- function(input, output, session) {

    ## + Initiate reactive values list to be passed between modules ####
    ## See https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
    rv <- reactiveValues(
      #checklist = app_checklist,
      inputs    = reactiveValues(),
      checks    = reactiveValues(),
      sims      = reactiveValues(),
      res       = reactiveValues(),
      gt        = reactiveValues(),
      histo     = reactiveValues(),
      actions   = reactiveValues()
    )

    ## Save language value to show/hide divs with shinyjs
    r_lang <- reactive({ input$language })

    ## + Module server functions ####
    mod_home_server2("tab_home", rv = rv)

    mod_tool_server2("tab_tool", rv = rv)

    mod_about_server("tab_about", rv = rv)

    ## + Trans modules events ####
    observeEvent(input$language, {
      shiny.i18n::update_lang(language = input$language)
    })

    observeEvent(rv$actions$to_tool, {
      nav_select(id = "navbar", selected = "tool")
    })

    observeEvent(rv$actions$to_about, {
      nav_select(id = "navbar", selected = "about")
    })


  } ## END server

  ## App call ###############################################################
  shinyApp(ui, server, ...)

} ## END function
