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
