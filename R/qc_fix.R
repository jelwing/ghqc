get_init_qc_commit <- function(owner, repo, issue_number) {
  issue <- get_issue(owner, repo, issue_number)
  get_metadata(issue$body)$`git sha`
}

create_assignees_list <- function(assignees) {
  sapply(assignees, function(assignee) glue::glue("@{assignee$login}"))
}

create_assignees_body <- function(assignees_list) {
  if (length(assignees_list) == 0) ""
  else {
    list <- glue::glue_collapse(assignees_list, sep = "\n")
    glue::glue("{list}\n\n\n")
  }
}

create_message_body <- function(message) {
  if (is.null(message)) ""
  else glue::glue("{message}\n\n\n")
}

get_script_hash <- function(script) {
  collapsed_script <- glue::glue_collapse(script, "\n")
  digest::digest(collapsed_script)
}

create_metadata_body <- function(reference_commit,
                                 reference_script,
                                 comparator_commit,
                                 comparator_script) {

  # get script hashes
  reference_script_hash <- get_script_hash(reference_script)
  comparator_script_hash <- get_script_hash(comparator_script)

  glue::glue("## Metadata\n",
             "* reference commit: {reference_commit}\n",
             "* reference script hash: {reference_script_hash}\n",
             "* comparator commit: {comparator_commit}\n",
             "* comparator script hash: {comparator_script_hash}\n")
}

create_diff_body <- function(diff, reference_commit, reference_script, comparator_commit, comparator_script) {
  if (!diff) ""

  else {
    # get context for diff
    context <- glue::glue(
      "reference commit (older version): {reference_commit}\n
        comparator commit (newer version): {comparator_commit}\n"
    )

    diff_formatted <- format_diff(reference_script = reference_script, comparator_script = comparator_script)
    glue::glue("## File Difference\n",
               "{context}\n",
               "{diff_formatted}\n\n",)
  }
}

#' @export
create_comment_body <- function(owner,
                                repo,
                                issue_number,
                                message = NULL,
                                diff = FALSE,
                                reference_commit = "original",
                                comparator_commit = "current") {

  issue <- get_issue(owner, repo, issue_number)

  # log
  debug(.le$logger, glue::glue("Creating comment body for issue #{issue_number} in {owner}/{repo}"))

  assignees_list <- create_assignees_list(issue$assignees)
  assignees_body <- create_assignees_body(assignees_list)

  message_body <- create_message_body(message)

  # get reference and comparator scripts if default
  if (reference_commit == "original" && comparator_commit == "current") {
    # reference = oldest
    reference_commit <- get_init_qc_commit(owner, repo, issue_number)
    # comparator = newest
    comparator_commit <- gert::git_log(max = 1)$commit
  }

  script_contents <- get_script_contents(issue$title, reference = reference_commit, comparator = comparator_commit)
  reference_script <- script_contents$reference_script
  comparator_script <- script_contents$comparator_script

  diff_body <- create_diff_body(diff = diff,
                           reference_commit = reference_commit,
                           reference_script = reference_script,
                           comparator_commit = comparator_commit,
                           comparator_script = comparator_script)

  metadata_body <- create_metadata_body(reference_commit = reference_commit,
                                        reference_script = reference_script,
                                        comparator_commit = comparator_commit,
                                        comparator_script = comparator_script)

  comment_body <- glue::glue("{assignees_body}",
                             "{message_body}",
                             "{diff_body}",
                             "{metadata_body}",
                             .trim = FALSE)

  # log
  log_assignees <- if (length(assignees_list) == 0) "None" else paste(assignees_list, collapse = ', ')

  info(.le$logger, glue::glue("Created comment body for issue #{issue_number} in {owner}/{repo} with
                              Assignee(s):       {log_assignees}
                              Reference commit:  {reference_commit}
                              Comparator commit: {comparator_commit}"))

  as.character(comment_body)
}

#' @export
post_comment <- function(owner, repo, issue_number, body) {
  debug(.le$logger, glue::glue("Posting comment to issue #{issue_number} in {owner}/{repo}..."))

  comment <- gh::gh("POST /repos/:owner/:repo/issues/:issue_number/comments",
                    .api_url = Sys.getenv("GHQC_API_URL"),
                    owner = owner,
                    repo = repo,
                    issue_number = issue_number,
                    body = body
  )

  info(.le$logger, glue::glue("Posted comment to issue #{issue_number} in {owner}/{repo}"))
}

add_fix_comment <- function(owner,
                            repo,
                            issue_number,
                            message = NULL,
                            diff = FALSE,
                            reference_commit = "original",
                            comparator_commit = "current") {

  body <- create_comment_body(owner,
                              repo,
                              issue_number,
                              message,
                              diff,
                              reference_commit = reference_commit,
                              comparator_commit = comparator_commit)

  post_comment(owner, repo, issue_number, body)
}
