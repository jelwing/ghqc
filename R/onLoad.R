

#' Initialize the ghqc package
#'
#' This function sets up necessary configurations for the ghqc package,
#' including adding a resource path for assets. Used pattern from:
#' https://github.com/dreamRs/esquisse/blob/master/R/onLoad.R
#'
#' @importFrom shiny addResourcePath
#' @noRd
.onLoad <- function(...) {
  shiny::addResourcePath("ghqc", system.file(".", package = "ghqc"))
  logger <- init_logger()
  client_info_path <- load_client_info()
}

.onUnload <- function(...) {
  shiny::removeResourcePath("ghqc")
}
