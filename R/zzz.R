## Make path to 'assets' from inst/assets available for the shiny app
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "assets",
    directoryPath = system.file("assets", package = "mocaredd.dev2")
  )
}

## Remove path to images for the shiny app when package is not loaded
.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("assets")
}
