#' Run fCatVis app
#' @export
#' @return A shiny application
#' @import data.table
#' @rawNamespace import(ggplot2, except = last_plot)
#' @import gridExtra
#' @import plotly
#' @rawNamespace import(shinyalert, except = runExample)
#' @rawNamespace import(shinyBS, except = closeAlert)
#' @import shinyFiles
#' @import shinythemes
#' @import stringr
#' @rawNamespace import(shinyjs, except = colourInput)

runFcatVis <- function(){
    appDir <- system.file("fCatVis", package = "fCatVis")
    if (appDir == "") {
        stop(
            "Could not find apps directory. Try re-installing `fCatVis`.",
            call = FALSE
        )
    }

    shiny::runApp(
        appDir,
        launch.browser = TRUE,
        display.mode = "normal"
    )
}
