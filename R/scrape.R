create_qc_data_section <- function(issue_creation_time, issue_creator, issue_title, issue_number, milestone_title, milestones) {
  sections <- c()
  # get qc_initializer
  humanized_creation_time <- humanize_time(issue_creation_time)
  qc_initializer <- glue::glue("{issue_creator} at {humanized_creation_time}")

  # get file author
  authors <- get_authors(issue_title)
  latest_author <- authors$latest
  author_section <- glue::glue("* **File Author:** {latest_author}")
  sections <- c(sections, author_section)

  # get collaborators
  collaborators <- authors$collaborators
  if (length(collaborators) != 0) {
    collaborators <- glue::glue_collapse(collaborators, sep = ", ")
    collaborators_section <- glue::glue("* **Other file collaborators:** {collaborators}")
    sections <- c(sections, collaborators_section)
  }

  # issue number and qc_init sections
  qc_init_section <- glue::glue("* **QC initializer:** {qc_initializer}")
  issue_number_section <- glue::glue("* **Issue number:** {issue_number}")
  milestone_section <- create_milestone_section(milestone_title, milestones)
  sections <- c(sections, qc_init_section, issue_number_section, milestone_section)

  # create body
  issue_body <- glue::glue_collapse(sections, sep = "\n\n")
  issue_section <- create_section("QC Data", issue_body)
}

create_milestone_section <- function(milestone_title, milestones) { # issue$milestone$title
  if (!is.null(milestone_title)) {
    milestone_body <- {
      description <- get_milestone_description(milestone_title, milestones)
      if (!is.null(description) && description != "") {
        glue::glue("* **Milestone:** {milestone_title}
                   * **Milestone description:** {description}")
      }
      else {
        glue::glue("* **Milestone:** {milestone_title}")
      }
    }
    #create_section("Milestone", milestone_body)
  }
  else ""
}

create_assignees_section <- function(assignees) {
  assignees_list <- sapply(assignees, function(assignee) glue::glue("- {assignee$login}"))
  assignees_body <- glue::glue_collapse(assignees_list, sep = "\n\n")
  assignees_section <- create_section("Assigned QCers", assignees_body)
}
create_comments_section <- function(issue_comments) {
  comments_list <- process_comments(issue_comments)
  comments_body <- glue::glue_collapse(comments_list, sep = "\n\n")
  comments_section <- create_section("Comments", comments_body)
}

create_events_section <- function(events_list) {
  events_body <- glue::glue_collapse(events_list, sep = "\n")
  events_section <- create_section("Events", events_body)
}

create_status_section <- function(events_list, issue_state) {
  # status
  closures <- events_list[grep("closed", events_list)]

  status <- {
    if (length(closures) == 0) {issue_state} # should be open (in theory)
    else {
      # get the last time it was closed, which is the current status
      last_closure <- closures[length(closures)]
      gsub("- ", "", last_closure)
    }
  }
  status_section <- create_section("Issue Status", status)
}

create_timeline_section <- function(timeline) {
  timeline_list <- get_timeline_list(timeline)
  timeline_body <- glue::glue_collapse(timeline_list, sep = "\n")
  timeline_section <- create_section("Detailed Timeline", timeline_body)
}

issue_to_markdown <- function(owner, repo, issue_number) {
  # collect issue info
  issue <- get_issue(owner, repo, issue_number)
  milestones <- get_all_milestone_objects(owner, repo)

  issue_comments <- get_issue_comments(owner, repo, issue_number)

  issue_events <- get_issue_events(owner, repo, issue_number)
  events_list <- get_events_list(issue_events)

  timeline <- get_issue_timeline(owner, repo, issue_number)

  # create sections
  issue_section <- create_qc_data_section(issue_creation_time = issue$created_at,
                                          issue_creator = issue$user$login,
                                          issue_title = issue$title,
                                          issue_number = issue$number,
                                          milestone_title = issue$milestone$title,
                                          milestones = milestones)

  assignees_section <- create_assignees_section(issue$assignees)

  status_section <- create_status_section(events_list, issue$state)

  checklist_section <- create_section("Issue Body", issue$body)

  comments_section <- create_comments_section(issue_comments)

  events_section <- create_events_section(events_list)

  timeline_section <- create_timeline_section(timeline)

  # put it all together
  paste0(
    issue_section,
    assignees_section,
    status_section,
    checklist_section,
    comments_section,
    events_section,
    timeline_section
  )
} # issue_to_markdown

get_pdf_name <- function(input_name, milestone_names, just_tables, repo) {
  browser()
  milestone_str <- glue::glue_collapse(milestone_names, "-")

  base_name <- {
    if (is.null(input_name) || input_name == "") {
      if (just_tables) {
        glue::glue("tables-{repo}-{milestone_str}")
      }
      else {
        glue::glue("{repo}-{milestone_str}")
      }

    }
    else { # remove .pdf if at the end
      stringr::str_remove(input_name, "\\.pdf$")
    }
  }

  # cleaning up:

  # check num chars
  if (nchar(base_name) > 60) {
    base_name <- substr(base_name, 1, 60)
  }

  # replace spaces and _ with -
  clean_name <- stringr::str_replace_all(base_name, "[ _]", "-")

  # remove special characters except for dashes and numbers
  clean_name <- stringr::str_remove_all(clean_name, "[^0-9A-Za-z\\-]")

  # make lowercase
  clean_name <- tolower(clean_name)

  pdf_name <- glue::glue("{clean_name}.pdf")
  return(pdf_name)
}

#' @import log4r
markdown_to_pdf <- function(rmd_content, repo, milestone_names, just_tables, location, pdf_name) {
  debug(.le$logger, "Creating report pdf...")

  # create temporary rmd
  rmd <- tempfile(fileext = ".Rmd")
  fs::file_create(rmd)
  # delete temporary rmd when it's time
  #suppressMessages({withr::defer_parent(fs::file_delete(rmd))})
  writeLines(rmd_content, con = rmd)

  # create pdf from rmd
  location <- normalizePath(location)
  pdf_path <- file.path(location, pdf_name)
  suppressWarnings(rmarkdown::render(rmd, output_file = pdf_path, quiet = TRUE))
  suppressMessages({withr::defer_parent(unlink(dirname(rmd)))})

  pdf_path_abs <- get_simple_path(normalizePath(pdf_path))

  info(.le$logger, "Converted rmd to pdf")
  info(.le$logger, glue::glue("Created report pdf: {pdf_path_abs}"))

  return(pdf_path_abs)
} # markdown_to_pdf

scrape_issue <- function(owner, repo, issue_number) {
  rmd_contents <- issue_to_markdown(owner, repo, issue_number)
  markdown_to_pdf(rmd_contents, repo, issue_number)
} # scrape

get_summary_table_col_vals <- function(issue) {
  metadata <- {
    tryCatch({
      get_metadata(issue$body)
    }, error = function(e) {
      # rename file path to issue title if not a ghqc issue

      list(
        `qc type` = "NA"
      )
    })
  }


  close_data <- get_close_info(issue)

  authors <- get_authors(issue$title)
  latest_author <- authors$latest

  file_path <- issue$title
  author <- ifelse(!is.null(latest_author), latest_author, "NA")
  qc_type <- ifelse(!is.null(metadata$`qc type`), metadata$`qc type`, ifelse(!is.null(metadata$`qc_type`), "NA"))
  #file_name <- basename(file_path)
  #git_sha <- ifelse(!is.null(metadata$git_sha), metadata$git_sha, NA)
  qcer <- ifelse(length(issue$assignees) > 0, issue$assignees[[1]], "NA")
  issue_closer <- ifelse(!is.null(close_data$closer), close_data$closer, "NA")
  close_date <- ifelse(!is.null(close_data$closed_at), close_data$closed_at, "NA")

  c(
    file_path = file_path,
    author = author,
    qc_type = qc_type,
    #file_name = file_name,
    #git_sha = git_sha,
    qcer = qcer,
    issue_closer = issue_closer,
    close_date = close_date
  )
}

get_summary_df <- function(issues) {
  col_vals <- lapply(issues, get_summary_table_col_vals)
  list_of_vectors <- lapply(col_vals, function(vec) {
    as.data.frame(as.list(vec))
  })

  df <- dplyr::bind_rows(list_of_vectors)
}


create_big_section <- function(section_title, contents) {
  glue::glue("# {section_title}\n\n{contents}\n\n\\newpage\n\n", .trim = FALSE)
} # create_section

create_medium_section <- function(section_title, contents) {
  glue::glue(
    "## {section_title}\n\n{contents}\n\n\\newpage\n\n", .trim = FALSE)
} # create_section

insert_breaks <- function(text, width) {
  sapply(text, function(x) {
    if (nchar(x) > width) {
      # insert spaces into long words
      paste(strsplit(x, paste0("(?<=.{", width, "})"), perl = TRUE)[[1]], collapse = " ")
    } else {
      x
    }
  })
}

create_summary_csv <- function(issues, env) {
  summary_df <- get_summary_df(issues)
  # wrap file paths
  summary_df$file_path <- insert_breaks(summary_df$file_path, 20)
  summary_csv <- tempfile(fileext = ".csv")
  suppressMessages({withr::defer(fs::file_delete(summary_csv), env)})
  write.csv(summary_df, file = summary_csv, row.names = FALSE)
  return(summary_csv)
}

create_intro <- function(repo, milestone_names, header_path) {
  author <- Sys.info()[["user"]]
  date <- format(Sys.Date(), '%B %d, %Y')
  milestone_names_list <- glue::glue_collapse(milestone_names, sep = ", ")

  intro <- glue::glue(
    "---
  title: \"QC Report: {milestone_names_list}\"
  subtitle: \"Git repository: {repo}\"
  author: {author}
  date: {date}
  output:
    pdf_document:
      latex_engine: xelatex
      pandoc_args: --listings
      toc: true
      toc_depth: 1
      includes:
        in_header: {header_path}
  ---

  \\newpage

  ")
}

create_header <- function() {
  header_path <- system.file("header.tex", package = "ghqc")
  image_path <- file.path(.lci$client_repo_path, "logo.png")

  header_tex <- paste0(
    "\\usepackage{fancyhdr}\n",
    "\\pagestyle{fancy}\n",
    "\\fancyhead[R]{\\includegraphics[width=2cm]{", image_path, "}}\n",
    "\\fancyhead[C]{}\n",
    "\\fancyhead[L]{}\n",
    "\\setlength{\\headheight}{30pt}\n",
    "\\fancypagestyle{plain}{%\n",
    "    \\fancyhead[R]{\\includegraphics[width=2cm]{", image_path, "}}\n",
    "    \\renewcommand{\\headrulewidth}{0.4pt}\n",
    "}\n",
    "\\fancyfoot[C]{Page \\thepage\\ of \\pageref{LastPage}}\n",
    "\\usepackage{lastpage}\n",
    "\\lstset{\nbreaklines=true\n}"
  )
  writeLines(header_tex, header_path)

  return(header_path)
}

set_up_chunk <- function() {
  glue::glue(
    "```{{r setup, include=FALSE}}
  library(knitr)
  library(dplyr)
  library(flextable)
  knitr::opts_chunk$set(eval=FALSE, warning = FALSE)\n```\n\n")
}

create_summary_table_section <- function(summary_csv) {
  glue::glue(
    "```{{r, include=FALSE, eval=TRUE}}
  summary_df <- read.csv(\"{summary_csv}\")\n
  summary_df <- summary_df %>%
  mutate(across(everything(), ~ ifelse(is.na(.), \"NA\", .)))
  invisible(summary_df)\n```\n",

    "## Summary Table\n```{{r, eval=TRUE, echo=FALSE, warning=FALSE, message=FALSE}}
  ft <- flextable::flextable(summary_df)
  dimensions <- dim_pretty(ft)
  col_widths <- dimensions$widths * 0.8
  #row_heights <- dimensions$heights * 0.8
  ft <- ft %>%
  set_header_labels(file_path = \"File Path\",
  author = \"Author\",
  qc_type = \"QC Type\",
  # file_name = \"File Name\",
  # git_sha = \"Git SHA\",
  qcer = \"QCer\",
  issue_closer = \"Issue Closer\",
  close_date = \"Close Date\") %>%

  set_table_properties(width = 1.0) %>% # , align = \"left\"
  #width(j = seq_along(col_widths), width = col_widths) %>%

  padding(padding = 35, part = \"all\") %>%
  fit_to_width(9) %>%
  #fontsize(size = 9, part = 'all') %>%
  theme_vanilla()

  ft <- ft %>% autofit()

  ft <- width(ft, width = dim(ft)$widths*6.5 /(flextable_dim(ft)$widths))
  ft <- width(ft, j = 1, 1.5)
  ft\n```\n\\newpage\n",
    .trim = FALSE)
}

create_set_of_issue_sections <- function(issues, owner, repo) {
  issue_numbers <- sapply(issues, function(issue) issue$number)
  issue_markdown_strings <- sapply(issues, function(issue) issue_to_markdown(owner, repo, issue$number))
  issue_titles <- sapply(issues, function(issue) issue$title)

  issue_section_strs <- mapply(create_medium_section, section_title = issue_titles, contents = issue_markdown_strings)
  issue_sections <- glue::glue_collapse(issue_section_strs, sep = "\n\\newpage\n")
}

#' @import log4r
create_milestone_report_section <- function(owner, repo, milestone_name, env, just_tables = FALSE) {
  debug(.le$logger, glue::glue("Creating section for milestone: {milestone_name}..."))
  issues <- get_all_issues_in_milestone(owner, repo, milestone_name)

  debug(.le$logger, glue::glue("Creating summary table for milestone: {milestone_name}..."))
  # summary table
  summary_csv <- create_summary_csv(issues, env)
  summary_table_section <- create_summary_table_section(summary_csv)
  info(.le$logger, glue::glue("Created summary table for milestone: {milestone_name}"))
  # issues
  issue_sections <- create_set_of_issue_sections(issues, owner, repo)

  res <- {
    if (just_tables) {
      summary_table_section
    }
    else { # put it all together
      paste0(summary_table_section, issue_sections)
    }
  }
  info(.le$logger, glue::glue("Created section for milestone: {milestone_name}"))
  return(res)

} # create_milestone_report_section

clean_input <- function(milestones_in) {
  # remove all quotes if any
  milestones_in_clean <- gsub('"', '', milestones_in)
  # make comma-separated str into vector
  milestones_list <- strsplit(milestones_in_clean, ",\\s*")
  unlist(milestones_list)
}

get_inputted_milestone_names <- function(owner, repo) {
  # gate with interactive() to avoid hanging
  if (interactive()) {

    milestones <- list_milestones(owner, repo)
    print(glue::glue("Non-empty milestones in {repo}:\n"))
    print(milestones)
    valid_input <- FALSE
    while (!valid_input) {
      # read in milestones
      user_input <- readline(prompt = glue::glue("\nInput milestones: e.g. milestone1, milestone2: "))
      clean_input <- clean_input(user_input)

      # check they exist and are non-empty
      result <- tryCatch({
        check_milestones(clean_input, owner, repo)
        TRUE
      },
      warning = function(w) {
        warning(w$message)
        FALSE
      },
      error = function(e) {
        cat("Error:", e$message, "\n")
        FALSE
      })

      # Check if the conversion was successful
      if (result) {
        cat("You entered valid milestones:", glue::glue_collapse(clean_input, sep = ", "), "\n")
        valid_input <- TRUE
      }
      else {
        cat("Invalid input. Please try again.\n")
      }
    }
    return(clean_input)
  }
} # get_inputted_milestone_names

check_milestones <- function(milestone_names, owner, repo) {
  # check that each milestone exists and is non-empty
  lapply(milestone_names, function(milestone_name) {
    exists <- milestone_exists(milestone_name, owner, repo)
    if (!exists) {
      stop(glue::glue("\"{milestone_name}\" is not a milestone in {repo}"))
    }
    milestone <- get_milestone_from_name(owner, repo, milestone_name)
    non_empty <- check_that_milestone_is_non_empty(milestone)
    if (!non_empty) {
      stop(glue::glue("\"{milestone_name}\" in {repo} is an empty milestone (no issues)"))
    }
  })
}

#' @export
#' @import log4r
ghqc_report <- function(milestone_names = NULL,
                        input_name = NULL,
                        just_tables = FALSE,
                        location = ".",
                        owner = get_organization(),
                        repo = get_current_repo()) {

  # get user input if milestone_names not inputted (check existence here)
  if (is.null(milestone_names)) {
    milestone_names <- get_inputted_milestone_names(owner, repo)
  }
  else {
    # check that milestones exist and are non-empty
    check_milestones(milestone_names, owner, repo)
  }


  if (fs::is_file(location)) {
    error(.le$logger, glue::glue("Inputted directory {location} is a file path. Input an existing directory."))
    rlang::abort(message = glue::glue("Inputted directory {location} is a file path.<br>Input an existing directory."))
  }

  # check location exists
  if (!fs::dir_exists(location)) {
    error(.le$logger, glue::glue("Inputted directory {location} doesn't exist. Input an existing directory."))
    rlang::abort(message = glue::glue("Inputted directory {location} doesn't exist.<br>Input an existing directory."))
  }

  debug(.le$logger, "Creating report introduction...")
  # intro
  header_path <- create_header()
  intro <- create_intro(repo, milestone_names, header_path)
  set_up_chunk <- set_up_chunk()
  info(.le$logger, "Created report introduction")

  debug(.le$logger, "Creating milestone sections...")
  # create milestone sections
  milestone_sections <- lapply(milestone_names, function(milestone_name) {
    milestone_body <- create_milestone_report_section(owner, repo, milestone_name, parent.frame(n = 2), just_tables)
    create_big_section(milestone_name, milestone_body)
  })
  info(.le$logger, "Created milestone sections")

  # appendix

  rmd_content <- glue::glue_collapse(c(intro, set_up_chunk, milestone_sections), sep = "")

  pdf_name <- get_pdf_name(input_name = input_name,
                           milestone_names = milestone_names,
                           just_tables = just_tables,
                           repo = repo)

  # create pdf from markdown

  debug(.le$logger, "Converting rmd to pdf...")

  markdown_to_pdf(rmd_content = rmd_content,
                  repo = repo,
                  milestone_names = milestone_names,
                  just_tables = just_tables,
                  location = location,
                  pdf_name = pdf_name)
}

get_simple_path <- function(working_dir = gert::git_find()) {
  home_dir <- Sys.getenv("HOME")
  simple_path <- stringr::str_replace(working_dir,
                                      stringr::fixed(home_dir),
                                      "~")
  return(simple_path)
}
