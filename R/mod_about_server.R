#' About module server function
#'
#' @noRd
mod_about_server <- function(id, rv) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$dl_example <- downloadHandler(
      filename = function(){"mocaredd-templatev2-simple.xlsx"},
      content  = function(file){file.copy(system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2"), file)}
    )

  }) ## END module server function

}
