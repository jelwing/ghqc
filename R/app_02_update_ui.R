#' @import shiny
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniButtonBlock
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter waiter_show_on_load spin_1
NULL

ghqc_update_ui <- function(id) {
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
        title = span("Update QC Shiny Tool"),
        left = actionButton(ns("close"), "Close", class = "btn-sm"),
        right = actionButton(ns("reset"), "Reset", class = "btn-sm")
      ),
      miniContentPanel(
        div(
          id = ns("center_content"),
          selectInput(ns("select_milestone"), "Select QC Item List (Github milestone)", choices = "", multiple = FALSE),
          selectInput(ns("select_issue"), "Select QC Item (Github issue)", choices = "", multiple = FALSE),
          textAreaInput(ns("message"), "Message", ""),
          checkboxInput(ns("show_diff"), "Show file difference?", TRUE),
          radioButtons(ns("compare"), "Compare to:",
            inline = TRUE,
            choices = c(
              "Compare original with most recent" = "init",
              "Select commit comparators" = "comparators"
            )
          ),
          conditionalPanel(
            condition = "input.compare === 'comparators'", ns = ns,
            div(
              class = "inline-selectize",
              selectizeInput(ns("ref_commits"), "Reference",
                choices = "",
                multiple = FALSE,
                options = list(
                  placeholder = "No commits since QC initialization."
                )
              ),
              selectizeInput(ns("comp_commits"), "Comparator",
                choices = "",
                multiple = FALSE,
                options = list(
                  placeholder = "No commits since reference commit."
                )
              )
            )
          )
        )
      ),
      div(
        class = "button_block",
        miniButtonBlock(
          actionButton(ns("preview"), "Preview Comment"),
          actionButton(ns("post"), "Post Comment")
        )
      )
    )
  )
  return(ui)
}
