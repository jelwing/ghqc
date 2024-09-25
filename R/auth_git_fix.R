#' @import log4r
check_stored_token_matches_renviron_token <- function(renviron_token, gitcreds_token) {
  if (renviron_token != gitcreds_token) {
    error(.le$logger, glue::glue("gitcreds_get token doesn't match .Renviron token"))
    # error(.le$logger, glue::glue("gitcreds_get token: {gitcreds_token}"))
    # error(.le$logger, glue::glue("Renviron token: {renviron_token}"))
  }
  else {
    info(.le$logger, "Renviron token matches gitcreds_get token")
  }
}


#' @import log4r
run_gitcreds_get <- function(url, renviron_token) {
  tryCatch({
    retrieved_creds <- gitcreds::gitcreds_get(url, use_cache = FALSE)
    gitcreds_token <- retrieved_creds$password

    check_stored_token_matches_renviron_token(renviron_token = renviron_token,
                                              gitcreds_token = gitcreds_token)

    return(retrieved_creds)
  },
  error = function(e) {
    error(.le$logger, e$message)
    error(.le$logger, "Could not retrieve credentials")

    return(NULL)
  })
}


#' @import log4r
try_api_call <- function(url) {
  user <- gh::gh("GET /user",
                 .api_url = url)
}


#' @import log4r
run_gitcreds_approve <- function(creds) {
  info(.le$logger, glue::glue("Running gitcreds_approve..."))
  gitcreds::gitcreds_approve(creds)
}


#' @import log4r
run_gitcreds_reject <- function(creds) {
  # needs this weird format
  reject_creds <- list(url = creds$url)
  info(.le$logger, glue::glue("Running gitcreds_reject..."))
  gitcreds::gitcreds_reject(reject_creds)
}


#' @import log4r
run_giteds_reject_then_approve <- function(desired_creds, renviron_token) {
  tryCatch({
    run_gitcreds_reject(desired_creds)

  }, error = function(e) {
    error(.le$logger, "Failed to run gitcreds_reject")
    error(.le$logger, e$message)
  })

  retrieved_creds <- run_gitcreds_get(url = desired_creds$url,
                                      renviron_token = renviron_token)

  tryCatch({
    run_gitcreds_approve(desired_creds)

  }, error = function(e) {
    error(.le$logger, "Failed to run gitcreds_approve")
    error(.le$logger, e$message)
  })
}


#' @import log4r
get_renvion_token <- function() {
  info(.le$logger, "Retrieving GHQC_GITHUB_PAT environment variable from Renviron...")
  renviron_token <- Sys.getenv('GHQC_GITHUB_PAT')
  info(.le$logger, glue::glue("Retrieved Renviron token: {renviron_token}"))

  token_length <- nchar(renviron_token)

  if (token_length != 40) {
    error(.le$logger, "Renviron token not 40 characters")
    error(.le$logger, glue::glue("Renviron token length: {token_length}"))
  }
  else {
    info(.le$logger, "Renviron token has correct length of 40 characters")
  }
  return(renviron_token)
}


#' @import log4r
get_url <- function() {
  info(.le$logger, "Retrieving GHQC_GITHUB_URL environment variable from .Renviron...")
  url <- Sys.getenv("GHQC_GITHUB_URL")
  info(.le$logger, glue::glue("Retrieved url: {url}"))

  return(url)
}


#' @import log4r
#' @export
ghqc_authenticate <- function() {
  url <- get_url()

  username <- "PersonalAccessToken"

  renviron_token <- get_renvion_token()

  # try api call
  tryCatch({
    api_call <- try_api_call(url)
    info(.le$logger, "Successful API call")
    info(.le$logger, "Git authentication complete")
    return()
  }, error = function(e) {
    error(.le$logger, "API call unsuccessful")
    error(.le$logger, e$message)
  })

  desired_creds <- list(
    url = url,
    username = username,
    password = renviron_token
  )

  # FIRST: try to approve
  run_gitcreds_approve(desired_creds)

  # try api call
  tryCatch({
    api_call <- try_api_call(url)
    info(.le$logger, "Successful API call")
    info(.le$logger, "Git authentication complete")
    return()
  }, error = function(e) {
    error(.le$logger, "API call unsuccessful")
    error(.le$logger, e$message)
  })

  # check what creds are stored
  retrieved_creds <- run_gitcreds_get(url = url,
                   renviron_token = renviron_token)

  # SECOND: try to remove stored creds and approve desired creds
  run_giteds_reject_then_approve(desired_creds = desired_creds, renviron_token = renviron_token)

  retrieved_creds2 <- run_gitcreds_get(url = url,
                                      renviron_token = renviron_token)

  # try api call
  tryCatch({
    api_call <- try_api_call(url)
    info(.le$logger, "Successful API call")
    info(.le$logger, "Git authentication complete")
    return()
  }, error = function(e) {
    error(.le$logger, "API call unsuccessful")
    error(.le$logger, e$message)
    error(.le$logger,
          glue::glue("Automatic git authentication unsuccessful. Manually set git credentials with gitcreds::gitcreds_set(\"{url}\"), then follow interactive prompts."))
    return()
  })

} # ghqc_authenticate





