dragZone <- function(id, choices) {
}

#' Create a dropzone input
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import shiny
dropZoneInput <- function(inputId, label, choices, hidden=FALSE, highlight=FALSE, multivalued=FALSE) {
  inputTag <- div(
    id = inputId,
    class = paste('ds-dropzone', opts2class(hidden, highlight, multivalued)),
    style = style,
    div(
      class = 'ds-dropzone-options'
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
    htmlDependency(name = "dragula", version = "3.7.2",
                   src = "www/dragula-3.7.2",
                   script = "dragula.min.js",
                   stylesheet = "dragula.min.css"
    )
  )
  htmltools::attachDependencies(..., deps)
}

#' Converts options to class names
#'
#' @return String
#'
opts2class <- function(...) {
  opts_enq <- enquos(...)
  paste(purrr::map2(list(...), opts_enq, ~ ifelse(!!.x, paste0('ds-', rlang::quo_text(.y)), '')), collapse = ' ')
}
