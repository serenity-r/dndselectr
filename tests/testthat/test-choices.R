context("choices")

# Borrowed from shiny/tests/testthat/test-bootstrap.R
test_that("Choices are correctly assigned names", {
  # Unnamed vector
  expect_identical(
    choicesWithNames(c("a","b","3")),
    list(a="a", b="b", "3"="3")
  )
  # Unnamed list
  expect_identical(
    choicesWithNames(list("a","b",3)),
    list(a="a", b="b", "3"="3")
  )
  # Vector, with some named, some not
  expect_identical(
    choicesWithNames(c(A="a", "b", C="3", "4")),
    list(A="a", "b"="b", C="3", "4"="4")
  )
  # List, with some named, some not
  expect_identical(
    choicesWithNames(list(A="a", "b", C=3, 4)),
    list(A="a", "b"="b", C="3", "4"="4")
  )
  # Unnamed list of tags
  expect_identical(
    choicesWithNames(list(div(), p())),
    list("<div></div>" = div(), "<p></p>" = p())
  )
  # Unnamed list of HTML
  expect_identical(
    choicesWithNames(list(HTML("<div></div>"), HTML(4))),
    list("<div></div>" = HTML("<div></div>"), "4" = HTML(4))
  )
  # Error if choice entry is list
  expect_error(
    choicesWithNames(list(a = "1", b = list())),
    "Individual choices cannot be vectors or lists."
  )
})
