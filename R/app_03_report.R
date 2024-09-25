#' @import shiny
NULL

#' @export
ghqc_report_app <- function() {
  app <- shinyApp(
    ui = ghqc_report_ui(
      id = "ghqc_report_app"
    ),
    server = function(input, output, session) {
      ghqc_report_server(
        id = "ghqc_report_app"
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5256))
  runApp(app, port = port)
}
