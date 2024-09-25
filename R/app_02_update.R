#' @import shiny
NULL

#' @export
ghqc_update_app <- function() {
  app <- shinyApp(
    ui = ghqc_update_ui(
      id = "ghqc_update_app"
    ),
    server = function(input, output, session) {
      ghqc_update_server(
        id = "ghqc_update_app"
      )
    }
  )
  port <- as.numeric(Sys.getenv("GHQC_SHINY_PORT", 5454))
  runApp(app, port = port)
}
