context("multivalues")

test_that("Multivalues are properly identified", {
  # Garbage in -> FALSE out
  expect_false(isMultivalued(NULL))
  # No duplicates allowed!
  expect_false(isMultivalued(c("steve", "steve")))
  # Fall in line, people! - Gotta have the -ds- indicator in all entries
  expect_false(isMultivalued(c("steve-ds-42", "fred-64")))
  # Nobody? Really??? - Like I said, ALL ENTRIES!
  expect_false(isMultivalued(c("steve", "fred")))
  # Must have integer ids (well, integer when casted)
  expect_true(isMultivalued(c("steve-ds-4", "fred-ds-5")))
  expect_true(isMultivalued(c("steve-ds-4.0", "fred-ds-5")))
  expect_false(isMultivalued(c("steve-ds-4.2", "fred-ds-5")))
})

test_that("IDs of multivalued presets are parsed correctly", {
  ## Already id'd
  presets <- c("Sepal.Length-ds-4")
  choices <- list(Sepal.Length = "Sepal.Length", Sepal.Width = "Sepal.Width")

  # Empty named list (gotta be a better way...)
  foo <- list(bar = 42)
  foo$bar <- NULL

  # Subset of choices (multivalued = TRUE)
  expect_equal(
    presetsWithIds(presets, choices, multivalued = TRUE),
    list(values = list(Sepal.Length = "Sepal.Length"), ids = "4")
  )
  # Subset of choices (multivalued = FALSE)
  expect_equal(
    presetsWithIds(presets, choices, multivalued = FALSE),
    list(values = foo, ids = logical(0))
  )

  ## Not already id'd
  presets <- c("steve", "fred")
  choices <- list(steve = "Steve", fred = "Fred")

  # IDs returned should be 1...length(presets) when multivalued is TRUE
  expect_equal(
    presetsWithIds(presets, choices, multivalued = TRUE),
    list(values = choices, ids = c(1, 2))
  )
  # IDs returned should be NA when multivalued is FALSE
  expect_equal(
    presetsWithIds(presets, choices, multivalued = FALSE),
    list(values = choices, ids = c(NA, NA))
  )

  # No presets and no choices
  expect_equal(
    presetsWithIds(NULL, choices, multivalued = TRUE),
    list(values = NULL, ids = NULL)
  )
  expect_equal(
    presetsWithIds(NULL, choices, multivalued = FALSE),
    list(values = NULL, ids = NULL)
  )
  expect_equal(
    presetsWithIds(NULL, NULL, multivalued = TRUE),
    list(values = NULL, ids = NULL)
  )
  expect_equal(
    presetsWithIds(NULL, NULL, multivalued = FALSE),
    list(values = NULL, ids = NULL)
  )
})
