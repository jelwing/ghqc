#' @import shiny
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniButtonBlock
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter waiter_show_on_load spin_1
NULL

ghqc_report_ui <- function(id) {
  ns <- NS(id)
  ui <- miniPage(
    use_waiter(),
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "ghqc/css/styles.css"),
    ),
    waiter_show_on_load(
      html = tagList(
        spin_1(),
        h4("Loading in ...", style = "color: white;")
      ),
      color = "darkgrey"
    ),
    div(
      id = ns("main_container"),
      gadgetTitleBar(
        title = span("QC Report Shiny Tool"),
        left = actionButton(ns("close"), "Close", class = "btn-sm"),
        right = actionButton(ns("reset"), "Reset", class = "btn-sm")
      ),
      miniContentPanel(
        div(
          id = ns("center_content"),
          checkboxInput(ns("closed_only"), "Only closed milestones", TRUE),
          selectizeInput(ns("select_milestone"), "Select QC Item List(s)", choices = "", multiple = TRUE),
          textAreaInput(ns("pdf_name"), "PDF name", placeholder = "(Optional)"),
          textAreaInput(ns("pdf_location"), "PDF location", value = get_simple_path()),
          checkboxInput(ns("just_tables"), "Just tables", FALSE)
        )
      ),
      div(
        class = "button_block",
        miniButtonBlock(
          actionButton(ns("generate_report"), "Generate QC Report")
        )
      )
    )
  )
  return(ui)
}


