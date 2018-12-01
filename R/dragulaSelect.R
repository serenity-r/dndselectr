#' Create a Dragula dragzone container
#'
#' @param id The container id.
#' @param choices List of values to select from.
#'
#' @export
#'
#' @examples
dragZone <- function(id, choices) {
  inputTag <- div(
    id = id,
    class = 'ds-dragzone',
    tagList(
      purrr::map2(names(choices),
                  choices,
                  ~ createContainerChoices(.x, .y, 'drag')
      )
    )
  )

  attachDependencies(inputTag)
}

#' Create a Dragula dropzone input
#'
#' @param inputId The \code{input} slot that will be used to acces the value.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param choices List of acceptable values with their associated labels. Note that
#'   the labels can be arbitrary HTML, as long as they are wrapped in a \code{tagList}.
#' @param hidden Should the selected items be hidden? This is useful to represent
#'   a reactive or event trigger.
#' @param highlight Highlights the container on dragover. Useful when \code{hidden} is active.
#' @param multivalued Allow multiple items with the same value?
#'
#' @export
#'
#' @examples
#' dropZoneInput("drake", choices = list(one = "1",
#'                                       two = "2",
#'                                       three = "3",
#'                                       four = "4"))
#'
#' @import shiny
dropZoneInput <- function(inputId, label, choices, hidden=FALSE, highlight=FALSE, multivalued=FALSE) {
  inputTag <- div(
    id = inputId,
    class = trimws(paste('ds-dropzone', opts2class(list(hidden = hidden,
                                                        highlight = highlight,
                                                        multivalued = multivalued))), "right"),
    div(
      class = 'ds-dropzone-options',
      tagList(
        purrr::map2(names(choices),
                    choices,
                    ~ createContainerChoices(.x, .y, 'drop')
        )
      )
    )
  )

  attachDependencies(inputTag)
}

# UTILS ----

#' Attach javascript dependencies
#'
#' \code{attachDependencies} attaches the javascript dependencies.Specifically,
#' the Dragula JS package is attached, as well as the javascript wrapper and
#' input bindings.
attachDependencies <- function(...) {
  deps <- list(
    htmltools::htmlDependency(name = "dragula", version = "3.7.2",
                              src = "www/dragula-3.7.2",
                              script = "dragula.min.js",
                              stylesheet = "dragula.min.css"
    )
  )
  htmltools::attachDependencies(..., deps)
}

#' Converts options to class names
#'
#' @param varArgs Named list of options for dropzone
#'
#' @return String
#'
opts2class <- function(varArgs) {
  varArgsNames <- names(varArgs)[vapply(varArgs, isTRUE, logical(1))]
  paste(purrr::map(varArgsNames, ~ paste0('ds-', .)), collapse = ' ')
}

#' Create options for dragzones/dropzones
#'
#' In Dragula, the drag and drop areas are called "containers". This function
#' creates the individual draggable items in these containers.
#'
#' @param value Value for allowable item/option (unique identifier)
#' @param label Label for allowable item/option (what the user sees)
#' @param class One of 'ds-dropoption' or 'ds-dragitem'
#'
#' @return div element
#'
createContainerChoices <- function(value, label, type) {
  if (!(type %in% c('drag', 'drop'))) {
    stop(type, " is not a valid container type. Dragula container type must be either 'drag' or 'drop'")
  }
  div(
    "data-value" = value,
    class = paste0('ds-', ifelse(type=='drop', 'dropoption', 'dragitem')),
    label
  )
}
