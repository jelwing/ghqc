#' @import log4r
get_names_and_usernames <- function(username) {
  user <- gh::gh("GET /users/{username}", .api_url = Sys.getenv("GHQC_API_URL"), username = username)
  return(list(
    username = user$login,
    name = user$name
  ))
}

#' @import log4r
get_repos <- function(org) {
  debug(.le$logger, glue::glue("Retrieving repos in org {org}..."))
  repos <- tryCatch(
    {
      gh::gh("GET /orgs/:org/repos", .api_url = Sys.getenv("GHQC_API_URL"), org = org, .limit = Inf)
    },
    error = function(e) {
      error(.le$logger, glue::glue("Failed to get repos in org {org}. {e$message}"))
    }
  )

  purrr::map_chr(repos, "name")
}


filter_for_non_empty_milestones <- function(milestones) {
  non_empty_milestones <- lapply(milestones, function(milestone) {
    if (check_that_milestone_is_non_empty(milestone)) {
      milestone
    }
    else NULL
  })
  # delete NULLs from list
  non_empty_milestones <- Filter(Negate(is.null), non_empty_milestones)
  return(non_empty_milestones)
}

#' @import log4r
get_open_milestone_objects <- function(owner, repo) {
  debug(.le$logger, glue::glue("Retrieving open milestones in organization {owner}, repo {repo}..."))

  milestones <- gh::gh("GET /repos/:owner/:repo/milestones", .api_url = Sys.getenv("GHQC_API_URL"), owner = owner, repo = repo, state = "open", .limit = Inf)
  info(.le$logger, glue::glue("Retrieved {length(milestones)} open milestone(s) in repo {repo}"))
  non_empty_milestones <- filter_for_non_empty_milestones(milestones)
}

#' @import log4r
get_closed_milestone_objects <- function(owner, repo) {
  debug(.le$logger, glue::glue("Retrieving closed milestones in organization {owner}, repo {repo}..."))

  milestones <- gh::gh("GET /repos/:owner/:repo/milestones", .api_url = Sys.getenv("GHQC_API_URL"), owner = owner, repo = repo, state = "closed", .limit = Inf)
  info(.le$logger, glue::glue("Retrieved {length(milestones)} closed milestone(s) in repo {repo}"))
  non_empty_milestones <- filter_for_non_empty_milestones(milestones)
}

#' @import log4r
get_all_milestone_objects <- function(owner, repo) {
  gh::gh("GET /repos/:owner/:repo/milestones", owner = owner, repo = repo, .api_url = Sys.getenv("GHQC_API_URL"), state = "all", .limit = Inf)
}

#' @import log4r
get_open_milestone_names <- function(org, repo) {

  tryCatch({
  milestones <- get_open_milestone_objects(org, repo)
  purrr::map_chr(milestones, "title")
  }, error = function(e) {
    error(.le$logger, glue::glue("Failed to retrieve open milestone names for organization {org} and {repo}."))
    rlang::abort(e$message)
  })
}

#' @import log4r
get_closed_milestone_names <- function(org, repo) {
  tryCatch({
    milestones <- get_closed_milestone_objects(org, repo)
    purrr::map_chr(milestones, "title")
  }, error = function(e) {
    error(.le$logger, glue::glue("Failed to retrieve closed milestone names for organization {org} and {repo}."))
    rlang::abort(e$message)
  })
}

#' @import log4r
#' @export
list_milestones <- function(org, repo) {
  debug(.le$logger, glue::glue("Retrieving milestones in organization {org}, repo {repo}..."))
  milestones <- get_all_milestone_objects(org, repo)
  info(.le$logger, glue::glue("Retrieved {length(milestones)} total milestone(s) in repo {repo}"))
  non_empty_milestones <- filter_for_non_empty_milestones(milestones)
  info(.le$logger, glue::glue("Retrieved {length(non_empty_milestones)} non-empty milestone(s) in repo {repo}"))
  res <- purrr::map_chr(non_empty_milestones, "title")
  return(res)
}

get_remote_name <- function(remote_url) {
  # remove .git and extract name
  remote_repo_name <- stringr::str_extract(remote_url, "(?<=/)[^/]+(?=\\.git$)")
  info(.le$logger, glue::glue("Retrieved remote repository name: {remote_repo_name}"))
  return(remote_repo_name)
}



get_remote_url <- function(remote) {
  org_url <- dirname(remote$url)
  api_url <- dirname(org_url)

  debug(.le$logger, glue::glue("Setting GHQC_API_URL environment variable: {api_url}..."))
  info(.le$logger, glue::glue("Connected to remote repository url: {api_url}"))

  Sys.setenv("GHQC_API_URL" = api_url)
  info(.le$logger, glue::glue("Set GHQC_API_URL environment variable: {Sys.getenv(\"GHQC_API_URL\")}"))

  return(api_url)
}

#' @import log4r
#' @export
get_remote <- function(remote_list) {

  debug(.le$logger, glue::glue("Retrieving local repo path..."))
  repo_path <- gert::git_find()
  debug(.le$logger, glue::glue("Retrieved local repo path: {repo_path}"))

  debug(.le$logger, glue::glue("Retrieving list of remotes..."))
  remote_list <- gert::git_remote_list(repo = repo_path)

  debug_remote_list <- apply(remote_list, 1, function(row) {
    glue::glue("name: {row['name']}, url: {row['url']}")
  })
  debug_remote_list_str <- glue::glue_collapse(debug_remote_list, sep = "\n")
  debug(.le$logger, glue::glue("Retrieved list of remotes: \n{debug_remote_list_str}"))

  debug(.le$logger, glue::glue("Selecting remote from list..."))
  remote <- {
    ### FIRST: check if there's a single remote,
    num_remotes <- nrow(remote_list)
    if (num_remotes == 1) {
      debug(.le$logger, glue::glue("Single remote: Selected only remote in list"))
      remote_list[1, ]
    } # FIRST

    ### SECOND: check if env var set
    else if (Sys.getenv("GHQC_REMOTE_NAME") != "") {
      # else, there are multiple or zero remotes
      info(.le$logger, "Multiple remote names detected")

      remote_env_var <- Sys.getenv("GHQC_REMOTE_NAME")

      info(.le$logger, glue::glue("Retrieving remote name from GHQC_REMOTE_NAME environment variable: {remote_env_var}"))
      # if in list of remotes
      if (remote_env_var %in% remote_list$name) {
        remote_list[remote_list$name == remote_env_var, ][1, ]
      }
      else {
        error(.le$logger, glue::glue("{remote_env_var} not in list of remotes"))
        rlang::abort(glue::glue("{remote_env_var} not in list of remotes"))
      }
    } # SECOND

    ### THIRD: check if origin exists in remote list
    else if ("origin" %in% remote_list$name) {
      info(.le$logger, "Multiple remote names detected")
      info(.le$logger, "No GHQC_REMOTE_NAME environment variable found. Using \"origin\" from list of remotes.")
      remote_list[remote_list$name == "origin", ][1, ]
    } # THIRD

    ### LAST: try to get first remote
    else {
      tryCatch({
        info(.le$logger, "Multiple remote names detected")
        info(.le$logger, glue::glue("No GHQC_REMOTE_NAME environment variable found. Using first remote from list of remotes: {remote_list$name[1]}"))
        remote_list[1, ]
        # error if no remote urls
      }, error = function(e) {
        error(.le$logger, glue::glue("No remote repository found"))
        rlang::abort(e$message)
      })
    } # LAST
  } # remote

  return(remote)
}

#' @import log4r
#' @export
get_current_repo <- function(remote = get_remote()) {
  tryCatch({
  debug(.le$logger, glue::glue("Connecting to repository..."))

  }, error = function(e) {
    error(.le$logger, glue::glue("No local git repository found."))
    rlang::abort(e$message)
  })
  remote_repo_name <- get_remote_name(remote$url)
}

#' @import log4r
get_organization_name_from_url <- function(remote_url) {
  # https url
  matches <- {
    if (grepl("https://", remote_url)) {
      regmatches(remote_url, regexec("https://[^/]+/([^/]+)", remote_url)) #/[^/]+
    }
    # ssh url
    else if (grepl("git@", remote_url)) {
      regmatches(remote_url, regexec("git@[^:]+:([^/]+)/[^/]+", remote_url))
    }
    else {
      NULL
      stop("Unknown remote url format")
    }
  } # matches

  # if could match org
  if (length(matches[[1]]) < 2) {
    stop("Unable to parse organization from url")
  }
  else {
    return(matches[[1]][2])
  }
} # get_organization_name_from_url

#' @import log4r
get_organization <- function() {
  tryCatch({
  debug(.le$logger, glue::glue("Connecting to organization..."))

  remote <- get_remote()

  # remote url
  debug(.le$logger, glue::glue("Retrieving remote url..."))
  remote_url <- dirname(remote$url)
  debug(.le$logger, glue::glue("Retrieved remote url: {remote_url}"))

  # org name
  debug(.le$logger, glue::glue("Retrieving organization name from remote url..."))

  org_name <- get_organization_name_from_url(remote_url)

  info(.le$logger, glue::glue("Connected to organization: {org_name}"))

  return(org_name)
  }, error = function(e) {
    error(.le$logger, "Failed to connect to organization. Ensure the repository exists and that remotes are correctly configured.")
    rlang::abort(e$message)
  })
}

#' @import log4r
get_issue <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number", .api_url = Sys.getenv("GHQC_API_URL"),
         owner = owner, repo = repo, issue_number = issue_number)
} # get_issue

#' @import log4r
get_issue_comments <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number/comments", .api_url = Sys.getenv("GHQC_API_URL"),
         owner = owner, repo = repo, issue_number = issue_number)
} # get_issue_comments

#' @import log4r
get_issue_events <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number/events", .api_url = Sys.getenv("GHQC_API_URL"),
         owner = owner, repo = repo, issue_number = issue_number)
} # get_issue_events

#' @import log4r
get_issue_timeline <- function(owner, repo, issue_number) {
  gh::gh("GET /repos/:owner/:repo/issues/:issue_number/timeline", .api_url = Sys.getenv("GHQC_API_URL"),
         owner = owner, repo = repo, issue_number = issue_number)
}

#' @import log4r
get_issues <- function(owner, repo, milestone) {
  params <- c(owner, repo)
  gh::gh("GET /repos/:owner/:repo/issues", .api_url = Sys.getenv("GHQC_API_URL"),
         owner = owner, repo = repo, milestone = milestone_number, state = "all")
}

#' @import log4r
get_all_issues_in_repo <- function(owner, repo) {
  debug(.le$logger, glue::glue("Retrieving all issues from repo: {repo}..."))
  open_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues", .api_url = Sys.getenv("GHQC_API_URL"),
                  owner = owner,
                  repo = repo,
                  state = "open",
                  per_page = 100,
                  page = page)

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    open_issues <- c(open_issues, res)

    # next page
    page <- page + 1
  }

  # closed issues
  closed_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues", .api_url = Sys.getenv("GHQC_API_URL"),
                  owner = owner,
                  repo = repo,
                  state = "closed",
                  per_page = 100,
                  page = page)

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    closed_issues <- c(closed_issues, res)

    # next page
    page <- page + 1
  }

  issues <- c(open_issues, closed_issues)
  num_issues <- length(issues)
  info(.le$logger, glue::glue("Retrieved {num_issues} issue(s) from repo: {repo}"))
  return(issues)

}

# sort by open/closed
#' @import log4r
get_all_issues_in_milestone <- function(owner, repo, milestone_name) {
  debug(.le$logger, glue::glue("Retrieving all issues from milestone: {milestone_name}..."))
  # get milestone number from name
  milestone_number <- look_up_existing_milestone_number(list(owner = owner, repo = repo, title = milestone_name))

  # if the milestone dne, there are no issues in the milestone, return an empty vector
  if (is.null(milestone_number)) {
    info(.le$logger, glue::glue("milestone: {milestone_name} doesn't yet exist"))
    return(c())
  }

  open_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues", .api_url = Sys.getenv("GHQC_API_URL"),
                  owner = owner,
                  repo = repo,
                  milestone = milestone_number,
                  state = "open",
                  per_page = 100,
                  page = page)

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    open_issues <- c(open_issues, res)

    # next page
    page <- page + 1
  }

  # closed issues
  closed_issues <- list()
  page <- 1

  repeat {
    res <- gh::gh("GET /repos/:owner/:repo/issues", .api_url = Sys.getenv("GHQC_API_URL"),
                  owner = owner,
                  repo = repo,
                  milestone = milestone_number,
                  state = "closed",
                  per_page = 100,
                  page = page)

    # break if no more issues
    if (length(res) == 0) break

    # append to list
    closed_issues <- c(closed_issues, res)

    # next page
    page <- page + 1
  }

  issues <- c(open_issues, closed_issues)
  info(.le$logger, glue::glue("Retrieved {length(issues)} issue(s) from milestone: {milestone_name}"))
  return(issues)
}

#' @import log4r
get_milestone_url <- function(owner, repo, milestone_name) {
  milestone_number <- get_milestone_number(list(owner = owner, repo = repo, title = milestone_name))
  milestone <- gh::gh(
    "GET /repos/{owner}/{repo}/milestones/{milestone_number}", .api_url = Sys.getenv("GHQC_API_URL"),
    owner = owner,
    repo = repo,
    milestone_number = milestone_number
  )

  milestone$html_url
}

#' @import log4r
get_milestone_list_url <- function() {
  remote_url <- dirname(get_remote()$url)
  remote_repo <- get_current_repo()
  # will look something like:
  # https://ghe-experiments.dev.a2-ai.cloud/gsk-cpmsprojects/test_ghqc_9005/milestones
  milestones_url <- file.path(remote_url, remote_repo, "milestones")
}

#' @import log4r
get_collaborators <- function(owner = get_organization(), repo = get_current_repo()) {
  tryCatch({
    query <- gh::gh("GET /repos/{owner}/{repo}/collaborators", .api_url = Sys.getenv("GHQC_API_URL"), .limit = Inf, owner = owner, repo = repo)
    members_list <- purrr::map(query, ~ get_names_and_usernames(.x$login))
    members_df <- purrr::map_df(members_list, ~ as.data.frame(t(.x), stringsAsFactors = FALSE))
    return(members_df)
  }, error = function(e) {
    error(.le$logger, glue::glue("No collaborators found from: {owner} and {repo}"))
    rlang::abort(e$message)
  })
}
