
test_that("convert_list_to_ui handles list with named elements correctly", {
  index <- which(names(get_checklists()) == "NCA")
  list <- get_checklists()[[index]]

  result <- convert_list_to_ui(list)

  expect_true(is.list(list))
  expect_true(is.list(result))

  expect_true(length(result) > 0)
  expect_true(any(sapply(result, function(x) inherits(x, "shiny.tag"))))

  has_named_elements <- any(sapply(result, function(x) is.list(x) && !is.null(names(x))))
  expect_true(has_named_elements)
})

test_that("convert_list_to_ui handles a character vector correctly", {
  index <- which(names(get_checklists()) == "R script")
  chr_vector <- get_checklists()[[index]]
  result <- convert_list_to_ui(chr_vector)

  expect_true(is.character(chr_vector))
  expect_true(is.list(result))

  expect_equal(length(result), length(chr_vector))
  expect_true(all(sapply(result, function(x) inherits(x, "shiny.tag"))))

  expect_true(all(sapply(result, function(x) grepl("<li>", as.character(x)))))
})

test_that("convert_list_to_ui handles unsupported types correctly", {
  unsupported_input <- 42
  expect_error(convert_list_to_ui(unsupported_input), "Unsupported type of checklist")

  unsupported_input <- TRUE
  expect_error(convert_list_to_ui(unsupported_input), "Unsupported type of checklist")

  unsupported_input <- 3.14
  expect_error(convert_list_to_ui(unsupported_input), "Unsupported type of checklist")
})
