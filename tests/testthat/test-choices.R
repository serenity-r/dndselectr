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
  # List, named, with a sub-vector
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=c("d", "e"))),
    list(A="a", B="b", C=list(d="d", e="e"))
  )
  # List, named, with a sub-vector with numeric elements
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=c(1, 2))),
    list(A="a", B="b", C=list(`1`="1", `2`="2"))
  )
  # List, named, with sublist
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=list("d", "e"))),
    list(A="a", B="b", C=list(d="d", e="e"))
  )
  # List, named, with sublist with numeric elements
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=list(1, 2))),
    list(A="a", B="b", C=list(`1`="1", `2`="2"))
  )
  # List, named, with a named sub-vector of length 1
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=c(D="d"))),
    list(A="a", B="b", C=list(D="d"))
  )
  # List, named, with a named sub-vector of length 1 with a numeric element
  expect_identical(
    choicesWithNames(list(A="a", B="b", C=c(D=1))),
    list(A="a", B="b", C=list(D="1"))
  )
  # List, some named, with sublist
  expect_identical(
    choicesWithNames(list(A="a", "b", C=list("d", E="e"))),
    list(A="a", b="b", C=list(d="d", E="e"))
  )
  # Deeper nesting
  expect_identical(
    choicesWithNames(list(A="a", "b", C=list(D=list("e", "f"), G=c(H="h", "i")))),
    list(A="a", b="b", C=list(D=list(e="e", f="f"), G=list(H="h", i="i")))
  )
  # Error when sublist is unnamed
  expect_error(choicesWithNames(list(A="a", "b", list(1,2))))
})
