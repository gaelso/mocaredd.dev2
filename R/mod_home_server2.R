#' Home module server function (v2)
#'
#' @noRd
mod_home_server2 <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(input$to_tool, {
      rv$actions$to_tool <- input$to_tool
    })

  }) ## END module server function

}
