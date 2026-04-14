#' Home module UI function (v2)
#'
#' @noRd
mod_home_UI2 <- function(id, i18n){

  ns <- NS(id)

  tagList(

    h2(i18n$t("Welcome to {mocaredd}!")),

    br(), br(),

    div(
      "{mocaredd} is a R package and a Shiny application designed to help you with
      running Monte Carlo Simulations for REDD+ uncertainty analysis.",
      style = "font-size: x-large; text-align: center; font-style: italic; font-family: serif;"
    ),

    br(), br(),

    p(
      "This app is developed in the context of the REDD+ mechanism (Reducing emission from Deforestation
      and forest Degradation 'plus') and carbon accounting calculations to estimate emission reductions
      and removal increases of greenhouse gas (GHG) in the forestry sector."
    ),

    p(
      "{mocaredd} provides a template for organizing data, and a tool that (1) takes the data,
      (2) runs Monte Carlo Simulations and (3) produces improved estimates of and confidence
      intervals around greenhouse gas emissions and emission reductions for REDD+."
    ),

    br(),

    h4(
      icon("arrow-right"), "To start exploring, continue to the:", HTML("&nbsp;"),
      actionButton(inputId = ns("to_tool"), label = "Tool")
    ),

    br(),

    div(
      "{mocaredd} current version: v1.0.", br(),
      "Development supported by FAO and the Aim4Forest Programme.",
      style = "font-style: italic;"
    )

  ) ## END tagList

} ## END module UI function
