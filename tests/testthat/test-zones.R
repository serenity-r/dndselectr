context("zones")

test_that("Drag zones are working properly", {
  expect_warning(dragZone(choices = "foo"), "Using generic 'dragzone' as id. HTML element may not be unique!")
  expect_error(dragZone("dragzone"), "You must specify choices for this dragZone.")
})

test_that("Zone items are formatted correctly", {
  expect_error(zoneItems('foo'), "foo is not a valid zone type. Zone type must be either 'drag' or 'drop'")
  expect_error(zoneItems('drag', 'stuff'), "stuff is not a valid item type. Item type must be either 'options' or 'presets'")

  # Drag correct
  expect_match(
    as.character(zoneItems('drag', 'options', list(foo = "Foo"))),
    "<div data-value=\"foo\" data-instance class=\"ds-dragitem\">Foo</div>"
    )
  # Drop correct
  expect_match(
    as.character(zoneItems('drop', 'options', list(foo = "Foo"))),
    "<div data-value=\"foo\" data-instance class=\"ds-dropoption\">Foo</div>"
  )
  # ids working
  expect_match(
    as.character(zoneItems('drop', 'presets', list(foo = "Foo"), c(42))),
    "<div data-value=\"foo\" data-instance=\"42\" class=\"ds-dropoption\">Foo</div>"
  )
  # Non-list for items
  expect_match(
    as.character(zoneItems('drop', 'options', c("foo"))),
    "<div data-value=\"foo\" data-instance class=\"ds-dropoption\">foo</div>"
  )
})

test_that("Drop zones are working properly", {
  # Simple (no presets)
  expect_match(
    as.character(
      dropZoneInput("dropzone",
                    choices = list(foo = "Foo", bar = "Bar")
                    )),
    "<div id=\"dropzone\" class=\"form-control ds-dropzone\" data-select-on-drop=\"false\" data-remove-on-spill=\"true\" data-direction=\"vertical\" data-max-input=\"Infinity\" data-server=\"false\">\n  <span class=\"ds-placeholder hidden\"></span>\n  <div class=\"ds-dropzone-options\">\n    <div data-value=\"foo\" data-instance class=\"ds-dropoption\">Foo</div>\n    <div data-value=\"bar\" data-instance class=\"ds-dropoption\">Bar</div>\n  </div>\n</div>"
  )

  # Server UI
  ui_func <- function(id, server=FALSE) {
    div(class="test-class", id)
  }
  expect_match(
    as.character(
      dropZoneInput("dropzone",
                    choices = list(foo = "Foo"),
                    presets = c("foo"),
                    server = ui_func
                    )),
    "<div id=\"dropzone\" class=\"form-control ds-dropzone\" data-select-on-drop=\"false\" data-remove-on-spill=\"true\" data-direction=\"vertical\" data-max-input=\"Infinity\" data-server=\"true\">\n  <span class=\"ds-placeholder hidden\"></span>\n  <div class=\"ds-dropzone-options\">\n    <div data-value=\"foo\" data-instance class=\"ds-dropoption\">Foo</div>\n  </div>\n  <div data-value=\"foo\" data-instance class=\"ds-dropoption\">\n    <div class=\"test-class\">foo</div>\n  </div>\n</div>"
  )

  # Make sure icons match preset state
  expect_match(
    as.character(
      dropZoneInput("dropzone",
                    choices = list(foo = "Foo"),
                    presets = list(values = "foo", invisible = "foo"),
                    togglevis = TRUE)),
    "<div id=\"dropzone\" class=\"form-control ds-dropzone\" data-select-on-drop=\"false\" data-remove-on-spill=\"true\" data-direction=\"vertical\" data-max-input=\"Infinity\" data-server=\"false\">\n  <span class=\"ds-placeholder hidden\"></span>\n  <div class=\"ds-dropzone-options\">\n    <div data-value=\"foo\" data-instance class=\"ds-dropoption\">\n      Foo\n      <div class=\"ds-toggle-visible\">\n        <i class=\"fa fa-eye\" role=\"presentation\" aria-label=\"eye icon\"></i>\n      </div>\n    </div>\n  </div>\n  <div data-value=\"foo\" data-instance class=\"ds-dropoption ds-invisible\">\n    Foo\n    <div class=\"ds-toggle-visible\">\n      <i class=\"fa fa-eye-slash\" role=\"presentation\" aria-label=\"eye-slash icon\"></i>\n    </div>\n  </div>\n</div>"
  )

  # Only one ds-dropoption!!
  expect_match(
    as.character(
      dropZoneInput("dropzone",
                    choices = list(foo = "Foo"),
                    presets = list(values = "foo",
                                   selected = "foo",
                                   invisible = "foo"))),
    "<div id=\"dropzone\" class=\"form-control ds-dropzone\" data-select-on-drop=\"false\" data-remove-on-spill=\"true\" data-direction=\"vertical\" data-max-input=\"Infinity\" data-server=\"false\">\n  <span class=\"ds-placeholder hidden\"></span>\n  <div class=\"ds-dropzone-options\">\n    <div data-value=\"foo\" data-instance class=\"ds-dropoption\">Foo</div>\n  </div>\n  <div data-value=\"foo\" data-instance class=\"ds-dropoption ds-selected ds-invisible\">Foo</div>\n</div>"
  )

  # Let's go crazy
  expect_match(
    as.character(
      dropZoneInput("dropzone",
                    choices = list(foo = "Foo", bar = "Bar"),
                    presets = list(values = c("foo-ds-2", "bar-ds-4"),
                                   selected = "bar-ds-4",
                                   invisible = "foo-ds-2"),
                    multivalued = TRUE,
                    removeOnSpill = FALSE,
                    direction = "horizontal")),
    "<div id=\"dropzone\" class=\"form-control ds-dropzone ds-multivalued\" data-select-on-drop=\"false\" data-remove-on-spill=\"false\" data-direction=\"horizontal\" data-max-input=\"Infinity\" data-server=\"false\">\n  <span class=\"ds-placeholder hidden\"></span>\n  <div class=\"ds-dropzone-options\">\n    <div data-value=\"foo\" data-instance class=\"ds-dropoption\">Foo</div>\n    <div data-value=\"bar\" data-instance class=\"ds-dropoption\">Bar</div>\n  </div>\n  <div data-value=\"foo\" data-instance=\"2\" class=\"ds-dropoption ds-invisible\">Foo</div>\n  <div data-value=\"bar\" data-instance=\"4\" class=\"ds-dropoption ds-selected\">Bar</div>\n</div>"
  )
})

test_that("Number of presets is at or below maximum allowed",
  expect_error(dropZoneInput("dropzone",
                             choices = list(foo = "Foo", bar = "Bar"),
                             presets = c("foo", "bar"),
                             maxInput = 1), NULL)
)
