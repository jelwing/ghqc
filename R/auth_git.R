#' @import log4r
check_git_inited <- function() {
  tryCatch(
    {
      repo <- gert::git_find()
    },
    error = function(e) {
      error(.le$logger, "There was no local Git repository found.")
      rlang::abort("There was no local Git repository found.")
    }
  )
}

#' @import log4r
check_remote_set <- function() {
  remotes <- gert::git_remote_list()

  if (nrow(remotes) == 0) {
    error(.le$logger, "There was no remote URL set.")
    rlang::abort("There was no remote URL set.")
  }
}

#' @import log4r
check_upstream_set <- function(remote_name) {
  repo <- get_simple_path()

  current_branch <- gert::git_branch()

  if (is.null(current_branch)){
    error(.le$logger, glue::glue("There were no branches found for the existing repository: {repo} \n",
                                 "To create a branch, use one of the below for you default branch name: \n",
                                 "  git branch -M main \n",
                                 "  git branch -M master \n",
                                 "Push the branch to the remote repository using: \n",
                                 "  git push -u {remote_name} main \n",
                                 "  git push -u {remote_name} master"))
    rlang::abort(glue::glue("There were no branches found for the existing repo: {repo}"))
  }

  tracking_branch <- gert::git_branch_list() %>%
    dplyr::filter(name == current_branch & upstream != "") %>%
    dplyr::pull(upstream)


  if (length(tracking_branch) == 0) {
    error(.le$logger, glue::glue(
      "The current branch '{current_branch}' has no tracking information.  \n",
      "If you are planning on basing your work on an upstream branch that already exists at the remote, retrieve them with: \n",
      "  git fetch {remote_name} \n",
      "If you wish to set tracking information for this branch you can do so with: \n",
      "  git branch --set-upstream-to={remote_name}/{current_branch} {current_branch}"
    ))
    rlang::abort(glue::glue(
      "The current branch '{current_branch}' has no tracking information.
      Please set upstream and restart the app."
    ))
  }
}

#' @import log4r
get_env_url <- function() {
  env_url <- Sys.getenv("GHQC_GITHUB_URL")
  env_url <- gsub("/$", "", env_url)

  # if GHQC_GITHUB_URL not set, use github.com
  if (!nzchar(env_url)) {
    warn(.le$logger, "No GHQC_GITHUB_URL environment variable found. Using Github URL \"https://github.com\". To specify otherwise, set GHQC_GITHUB_URL environment variable, likely in your ~/.Renviron file.")
    env_url <- "https://github.com"
  }
  # else if was set
  else {
    info(.le$logger, glue::glue("Retrieved GHQC_GITHUB_URL environment variable: {env_url}"))
  }

  # error if not https
  url_starts_with_https <- stringr::str_starts(env_url, "https://")
  if (!url_starts_with_https) {
    error(.le$logger, glue::glue("Retrieved GHQC_GITHUB_URL: {env_url} does not start with https"))
    rlang::abort(message = glue::glue("Retrieved GHQC_GITHUB_URL: {env_url} does not start with https"))
  }

  # remove /api/v3 if at the end
  env_url <- stringr::str_remove(env_url, "/api/v3$")
  return(env_url)
}


#' @import log4r
#' @export
get_gh_url <- function(remote_url) {
  env_url <- get_env_url()

  check_remote_matches_env_url(remote_url, env_url)

  return(env_url)
}

#' @import log4r
check_remote_matches_env_url <- function(remote_url, env_url) {
  if (remote_url != env_url) {
    error(.le$logger, glue::glue("GHQC_GITHUB_URL environment variable: \"{env_url}\" does not match remote URL: \"{remote_url}\""))
    rlang::abort(message = glue::glue("GHQC_GITHUB_URL environment variable: \"{env_url}\" does not match remote URL: \"{remote_url}\""))
  }
}

#' @import log4r
#' @export
get_gh_api_url <- function(remote_url) {
  gh_url <- tryCatch(
    {
      get_gh_url(remote_url)
    },
    error = function(e) {
      rlang::abort(message = e$message)
    }
  )

  res <- glue::glue("{gh_url}/api/v3")
  info(.le$logger, glue::glue("Configured api url: {res}"))
  res
}

#' @import log4r
#' @export
get_gh_token <- function() {
  res <- Sys.getenv("GHQC_GITHUB_PAT")
  if (!nzchar(res)) {
    error(.le$logger, "No Github token found. Please set GHQC_GITHUB_PAT environment variable, likely in your ~/.Renviron file.")
    rlang::abort(message = "No Github token found. Please set GHQC_GITHUB_PAT environment variable, likely in your ~/.Renviron file.")
  }
  info(.le$logger, glue::glue("Retrieved GHQC_GITHUB_PAT environment variable: {substr(res, 1, 4)}************************************"))
  debug(.le$logger, glue::glue("Retrieved GHQC_GITHUB_PAT environment variable: {res}"))
  res
}

#' @import log4r
#' @export
check_github_credentials <- function(remote) {
  if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")

  # Check for errors
  check_git_inited()

  check_remote_set()

  remote <- get_remote()
  remote_name <- get_remote()$name
  remote_url <- get_remote_url(remote)

  check_upstream_set(remote_name)

  tryCatch(
    {
      api_url <- get_gh_api_url(remote_url)
      token <- get_gh_token()
    },
    error = function(e) {
      error(.le$logger, glue::glue("There was an error setting credentials."))
      rlang::abort(message = glue::glue("There was an error setting credentials."))
    }
  )

  if (token == "") {
    error(.le$logger, glue::glue(
      "To configure GitHub Enterprise connectitivity run:
    {usethis::ui_code(paste0('usethis::create_github_token(host = \"', get_gh_url(), '\")'))}
    and generate token
    Then use {usethis::ui_code('usethis::edit_r_environ()')}
    and fill in {usethis::ui_code('GHQC_GITHUB_PAT = [your token]')}"
    ))
    stop("stopping", call. = TRUE)
  }

  ## workaround to avoid ssl error, remove the following lines if possible
  dconf <- gert::git_config_global()
  if (!identical(dconf$value[dconf$name %in% "http.sslverify"], "false")) {
    gert::git_config_global_set(name = "http.sslverify", value = "false")
  }

  if (nchar(token) == 40) {
    creds <- list(
      url = api_url,
      username = "PersonalAccessToken",
      password = token
    )

    tryCatch({
      # Case 1: gitcreds_approve works if git isn't authenticated for the url
      # OR if it is already authenticated
      debug(.le$logger, glue::glue("Approving credentials..."))
      gitcreds::gitcreds_approve(creds)
      debug(.le$logger, glue::glue("Approved credentials"))

      debug(.le$logger, glue::glue("Attempting test api call..."))
      try_api_call(api_url)
      info(.le$logger, glue::glue("Successful test api call"))
    }, error = function(e) {
      # Case 2: git is incorrectly authenticated for the url
      tryCatch({
          # get uncached creds
          debug(.le$logger, e$message)
          debug(.le$logger, glue::glue("Retrieving uncached credentials..."))
          run_gitcreds_get(url = api_url, renviron_token = token)

          # approve again (this has worked every time so far)
          debug(.le$logger, glue::glue("Approving credentials..."))
          gitcreds::gitcreds_approve(creds)
          debug(.le$logger, glue::glue("Approved credentials"))

          # try api call again
          debug(.le$logger, glue::glue("Attempting test api call..."))
          try_api_call(api_url)
          info(.le$logger, glue::glue("Successful test api call"))
        },
        # Case 3: if authentication fails, have user run gitcreds manually
        error = function(e) {
          error(.le$logger, glue::glue("Could not set github credentials for {api_url}. Double check that the GHQC_GITHUB_PAT and GHQC_GITHUB_URL environment variables are correct, then run gitcreds::gitcreds_set()")) #, then run ghqc_authenticate() to set Github credentials.
          rlang::abort(message = e$message)
        } # error
      ) # tryCatch
    } # error
    ) # tryCatch

    info(.le$logger, glue::glue("GitHub credentials set"))
  } else {
    error(.le$logger, glue::glue("Token not equal to 40 characters. Please reset GHQC_GITHUB_PAT environment variable, likely in your ~/.Renviron file."))
    rlang::abort(message = "Token not equal to 40 characters. Please reset GHQC_GITHUB_PAT environment variable, likely in your ~/.Renviron file.")
  }

  return(remote)
}

