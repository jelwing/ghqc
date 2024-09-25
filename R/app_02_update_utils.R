#' Convert Issue Data Frame Format
#'
#' This function converts a list of issue data frames into a structured format for further processing.
#' It extracts issue number, title, and state, then organizes them into a named list split by their state
#' for shiny input.
#'
#' @param issue_df A list of issue data frames, where each data frame contains the columns: number, title, and state.
#'
#' @return A named list where the names are the issue states ("Open Items" or "Closed Items")
#' and the values are named vectors with issue details formatted as "Item <number>: <title>".
#' @import dplyr
#' @importFrom purrr map_df
#' @examples
#' issues <- list(
#'   list(number = 1, title = "Issue 1", state = "open"),
#'   list(number = 2, title = "Issue 2", state = "closed")
#' )
#' convert_issue_df_format(issues)
#' @noRd
convert_issue_df_format <- function(issue_df) {
  debug(.le$logger, "Converting issue data frame format to named list")

  issues_df <- map_df(issue_df, ~ {
    tibble(
      number = .x$number,
      title = .x$title,
      state = .x$state
    )
  })

  debug(.le$logger, glue::glue("Issues data frame created: {nrow(issues_df)} row(s)"))

  issues_choices <- issues_df %>%
    mutate(state = case_when(
      state == "open" ~ "Open Items",
      state == "closed" ~ "Closed Items"
    )) %>%
    split(.$state) %>%
    rev() %>%
    lapply(function(x) {
      setNames(nm = paste0("Item ", x$number, ": ", x$title))
    })

  debug(.le$logger, "Successfully created issues choices list")

  return(issues_choices)
}


#' Convert Commits Data Frame Format
#'
#' This function converts a data frame of commits into a named list for further processing.
#' It splits the commits by their date, then organizes them into a named list for shiny input.
#'
#' @param commit_df A data frame of commits, where each row contains the columns: date, commit, and display.
#'
#' @return A named list where the names are the commit display names and the values are the corresponding commit hashes.
#' @import dplyr
#' @examples
#' commits <- tibble(
#'   date = as.Date(c("2023-01-01", "2023-01-02")),
#'   commit = c("abc123", "def456"),
#'   display = c("Commit 1", "Commit 2")
#' )
#' convert_commits_df_format(commits)
#' @noRd
convert_commits_df_format <- function(commit_df) {
  debug(.le$logger, "Converting commits data frame format to named list")

  commits <- commit_df %>%
    split(.$date) %>%
    rev() %>%
    lapply(function(x) {
      setNames(
        object = x$commit,
        nm = x$display
      )
    })

  debug(.le$logger, "Successfully created commits list")

  return(commits)
}


#' Split Issue Parts
#'
#' This function splits an issue string into its components: issue number and issue title for functions that take one or the other.
#'
#' @param issue A character string representing an issue in the format "Item <number>: <title>".
#'
#' @return A list containing the issue number and issue title.
#' @examples
#' split_issue_parts("Item 1: Issue Title")
#' @noRd
split_issue_parts <- function(issue) {
  tryCatch(
    {
      debug(.le$logger, glue::glue("Splitting issue parts for: {issue}"))

      issue_parts <- strsplit(sub("Item ", "", issue), ": ")[[1]]
      issue_number <- as.numeric(issue_parts[1])
      issue_title <- as.character(issue_parts[2])

      debug(.le$logger, glue::glue("Issue parts split: number = {issue_number}, title = {issue_title}"))

      list(issue_number = issue_number, issue_title = issue_title)
    },
    warning = function(w) {
      debug(.le$logger, glue::glue("Error in split_issue_parts: {w$message}"))
      rlang::abort(w$message)
    }
  )
}
