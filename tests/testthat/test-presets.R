context("presets")

test_that("IDs are extracted from presets correctly", {
  expect_equal(getIds(list(values = "Sepal.Length-ds-4"), multivalued = TRUE),
               list(values = "Sepal.Length", ids = "4"))
  expect_equal(getIds(list(values = "Sepal.Length-ds-4"), multivalued = FALSE),
               list(values = "Sepal.Length-ds-4", ids = NA))
  expect_equal(getIds(list(values = c("Sepal.Length-ds-4", "foo")), multivalued = TRUE),
               list(values = c("Sepal.Length-ds-4", "foo"), ids = c(1, 2)))
})

test_that("Preset options are parsed correctly", {
  expect_equal(parseOption(list(values = "foo"), "locked"),
               list(values = "foo"))

  # Selected is a bit of a special case
  expect_equal(parseOption(list(values = "foo", selected = "foo"), "selected"),
               list(values = "foo", selected = "ds-selected"))
  expect_equal(parseOption(list(values = "foo", selected = "foo"), "selected"),
               list(values = "foo", selected = "ds-selected"))
  expect_equal(parseOption(list(values = c("foo", "bar"), selected = "bar"), "selected"),
               list(values = c("foo", "bar"), selected = c(NA, "ds-selected")))
  expect_error(parseOption(list(values = c("foo", "bar"), selected = c("foo", "bar")), "selected"),
               "Only one selected value allowed!")

  # Let's try one more - invisible
  expect_equal(parseOption(list(values = "foo", invisible = "foo"), "invisible"),
               list(values = "foo", invisible = "ds-invisible"))
  expect_equal(parseOption(list(values = c("foo", "bar"), invisible = "foo"), "invisible"),
               list(values = c("foo", "bar"), invisible = c("ds-invisible", NA)))
  expect_warning(parseOption(list(values = c("foo", "bar"), invisible = "steve"), "invisible"), NULL)
})

test_that("Presets and options are parsed correctly", {
  choices <- list(foo = "Foo", bar = "Bar")
  expect_equal(presetsWithOptions(c("foo", "bar"), choices, FALSE),
               list(values = choices, ids = c(NA, NA)))

  # Preset value not in choices
  expect_warning(presetsWithOptions(c("foo", "steve"), choices, FALSE), NULL)
  expect_equal(suppressWarnings(presetsWithOptions(c("foo", "steve"), choices, FALSE)),
               list(values = list(foo = "Foo"), ids = NA))

  # Selected
  expect_equal(presetsWithOptions(list(values = c("foo", "bar"), selected = "bar"), choices, FALSE),
               list(values = choices, selected = c(NA, "ds-selected"), ids = c(NA, NA)))

  # Let's go crazy!
  presets <- list(values = c("foo-ds-2", "bar-ds-4"),
                  selected = "bar-ds-4",
                  invisible = "foo-ds-2")
  expect_equal(presetsWithOptions(presets, choices, multivalued=TRUE),
               list(values = choices,
                    selected = c(NA, "ds-selected"),
                    invisible = c("ds-invisible", NA),
                    ids = c("2", "4")))
})
