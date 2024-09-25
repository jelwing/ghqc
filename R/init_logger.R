
.le <- new.env() # parent = emptyenv()

#' @import log4r
NULL

init_logger <- function() {
  LEVEL_NAMES <- c("DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  verbosity <- Sys.getenv("GHQC_VERBOSE", unset = "INFO")
  if (!(verbosity %in% LEVEL_NAMES)){
    cat("Invalid verbosity level. Available options are:", paste(LEVEL_NAMES, collapse = ", "), "\n")
  }

  # logger <- logger(verbosity, appenders = console_appender(logfmt_log_layout()))
  logger <- logger(verbosity, appenders = console_appender(my_layout))
  assign("logger", logger, envir = .le)

  # log for logger
  #other <- ifelse(verbosity == "INFO", "DEBUG", "INFO")
  #info(.le$logger, glue::glue("logger level set to {verbosity}. Use ghqc_toggle_logger() to change to {other}"))
  if (verbosity == "INFO") {
    info(.le$logger, glue::glue("logger level set to INFO. Use Sys.setenv(\"GHQC_VERBOSE\" = \"DEBUG\") to change to DEBUG mode"))
  }
  if (verbosity == "DEBUG") {
    info(.le$logger, glue::glue("logger level set to DEBUG. Use Sys.setenv(\"GHQC_VERBOSE\" = \"INFO\") to change to INFO mode"))
  }

}

my_layout <- function(level, ...) {
  paste0(format(Sys.time()), " [", level, "] ", ..., "\n", collapse = "")
}

#' @export
ghqc_toggle_logger <- function() {
  verbosity <- Sys.getenv("GHQC_VERBOSE")

  ifelse(verbosity != "DEBUG",
         Sys.setenv("GHQC_VERBOSE" = "DEBUG"),
         Sys.setenv("GHQC_VERBOSE" = "INFO"))

  init_logger()

}


