context("zones")

test_that("Drag zones are working properly", {
  expect_warning(dragZone(choices = "foo"), "Using generic 'dragzone' as id. HTML element may not be unique!")
  expect_error(dragZone("dragzone"), "You must specify choices for this dragZone.")
})

test_that("Dragula zone items are formatted correctly", {
  expect_error(dragulaZoneItems('foo'), "foo is not a valid container type. Dragula container type must be either 'drag' or 'drop'")
  expect_error(dragulaZoneItems('drag', 'stuff'), "stuff is not a valid item type. Item type must be either 'options' or 'presets'")

  # Drag correct
  expect_match(
    as.character(dragulaZoneItems('drag', 'options', list(foo = "Foo"))),
    "<div data-value=\"foo\" data-instance class=\"ds-dragitem\">Foo</div>"
    )
  # Drop correct
  expect_match(
    as.character(dragulaZoneItems('drop', 'options', list(foo = "Foo"))),
    "<div data-value=\"foo\" data-instance class=\"ds-dropoption\">Foo</div>"
  )
  # ids working
  expect_match(
    as.character(dragulaZoneItems('drop', 'presets', list(foo = "Foo"), c(42))),
    "<div data-value=\"foo\" data-instance=\"42\" class=\"ds-dropoption\">Foo</div>"
  )
  # Non-list for items
  expect_match(
    as.character(dragulaZoneItems('drop', 'options', c("foo"))),
    "<div data-value=\"foo\" data-instance class=\"ds-dropoption\">foo</div>"
  )
})

test_that("Dropzone selections are doing their thing", {
  expect_equal(parseSelected(selected = NULL, presets = "foo"), NULL)
  expect_equal(parseSelected(selected = "foo", presets = NULL), NULL)
  expect_warning(parseSelected(selected = "foo", presets = "bar"), NULL)
  expect_error(parseSelected(selected = c("foo", "bar"), presets = "bar"), "Only one selected value allowed!")
  expect_equal(parseSelected(selected = "bar", presets = c("foo", "bar")), c(NA, "selected"))
})
