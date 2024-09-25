#' @export
get_checklists <- function() {
  #checklists_path <- system.file("checklists", package = "ghqc")
  checklists_path <- file.path(.lci$client_repo_path, "checklists")
  yaml_checklists <- list.files(checklists_path, pattern = "\\.ya?ml$", full.names = TRUE)
  checklists_data <- sapply(yaml_checklists, function(yaml_checklist) {
    yaml::read_yaml(yaml_checklist)
  }, USE.NAMES = FALSE)
  return(checklists_data)
}

create_file_data_structure <- function(file_name, assignees = NULL, checklist_type, checklists = get_checklists()) {
  # if checklist_type wasn't given, make it the file ext
  # if (is.null(checklist_type)) {
  #   file_extension <- tools::file_ext(file_name)
  #   checklist_type <- file_extension
  #
  # }

  file_data <- list(
    name = file_name,
    checklist_type = checklist_type
  )

  if (!is.null(assignees)) {file_data$assignees = assignees}

  #file_data$items <- checklists[[checklist_type]]

  file_data
}

# files should be a list of lists like this:
# list(
#   list(
#     name = name1,
#     assignees = assignees1,
#.    checklist_type = type1,
#     items = items1
#   ),
#   list(
#     name = name2,
#     assignees = assignees2,
#.    checklist_type = type2,
#     items = items2
#   ),
#   list(
#     name = name3,
#     assignees = assignees3,
#.    checklist_type = type3,
#     items = items3
#   ),
#  ...
# )

#### Example output

# owner: A2-Ai
# repo: test-qc-api
# files:
# - name: file1.cpp
#   checklist_type: cpp
#   assignees: jenna-a2ai
#   items:
#   - code is easily readable
#   - code follows style guidelines
#   - comments explain non-standard coding conventions
# - name: file1.R
#   checklist_type: vpc
#   assignees: jenna-a2ai
#   items:
#   - confidence interval is 95%
#   - 500 simulations given
#   - generated figures match
create_yaml <- function(name,
                        org,
                        repo,
                        files, # files must be a list of lists not a vector of lists
                        milestone = NULL,
                        description = NULL
                        ) {
  # hard code owner
  #owner <- get_organization() #"a2-ai-tech-training"

  data <- list(
    owner = org,
    repo = repo,
    files = files # files must be a list of lists not a vector of lists
  )

  if (!is.null(milestone) && milestone != "") {data$milestone = milestone}
  if (!is.null(description) && description != "") {data$description = description}

  # make into yaml string
  yaml_string <- yaml::as.yaml(data)
  # validate contents
  #validate_yaml_contents(data) # error
  # make path
  yaml_path <- file.path(paste0(name, ".yaml"))
  withr::defer_parent(fs::file_delete(yaml_path))
  # create yaml
  write(yaml_string, file = yaml_path)

  yaml_path
}
