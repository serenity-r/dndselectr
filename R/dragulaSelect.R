#' dragulaSelectR: Utilizes Dragula JS library to implement a drag-and-drop Shiny select input.
#'
#' Utilizes Dragula JS library to implement a drag-and-drop Shiny select input.
#' Dragula is a drag-and-drop javascript library that is very lightweight and robust.
#' This implementation creates a Shiny input that replicates much of the functionality of
#' selectInput. Multiple zones for dragging and dropping are allowed. See the Dragula JS
#' library for more information (https://github.com/bevacqua/dragula).
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' # basic example
#' shinyApp(
#'   ui = fluidPage(
#'     column(6,
#'       dragZone("dragzone", choices = list(one = "One",
#'                                           two = "Two",
#'                                           three = "Three",
#'                                           four = "Four"))
#'       ),
#'     column(6,
#'       dropZoneInput("dropzone", choices = list(one = "1",
#'                                                two = "2",
#'                                                three = "3",
#'                                                four = "4"),
#'                                 highlight = TRUE)
#'       )
#'   ),
#'   server = function(input, output) {
#'   }
#' )
#' }
#'
#' @docType package
#'
#' @import shiny
#'
#' @name dragulaSelectR
#'
#' @seealso \code{\link{dragZone}}, \code{\link{dropZoneInput}}
#'
NULL

#' Create a Dragula dragzone container
#'
#' @param id The container id.
#' @param choices List of values to select from.
#'
#' @export
#'
#' @examples
#' dragZone("dragzone", choices = list(one = "One",
#'                                     two = "Two",
#'                                     three = "Three",
#'                                     four = "Four"))
#'
#' @seealso \code{\link{dragulaSelectR}}
#'
dragZone <- function(id, choices) {

  # Resolve names
  choices <- choicesWithNames(choices)

  inputTag <- div(
    id = id,
    class = 'ds-dragzone',
    tagList(
      mapply(names(choices),
             choices,
             FUN = function(name, choice) { dragulaZoneOptions('drag', name, choice) },
             SIMPLIFY = FALSE, USE.NAMES = FALSE
      )
    )
  )

  attachDependencies(inputTag)
}

#' Create a Dragula dropzone input
#'
#' @param inputId The \code{input} slot that will be used to acces the value.
#' @param choices List of acceptable values with their associated labels. Note that
#'   the labels can be arbitrary HTML, as long as they are wrapped in a \code{tagList}.
#' @param hidden Should the selected items be hidden? This is useful to represent
#'   a reactive or event trigger.
#' @param placeholder If hidden is true, insert placeholder text.
#' @param highlight Highlights the container on dragover. Useful when \code{hidden} is active.
#' @param multivalued Allow multiple items with the same value?
#'
#' @export
#'
#' @examples
#' dropZoneInput("dropzone", choices = list(one = "1",
#'                                          two = "2",
#'                                          three = "3",
#'                                          four = "4"))
#'
#' @seealso \code{\link{dragulaSelectR}}
#'
dropZoneInput <- function(inputId, choices, hidden=FALSE, placeholder=NULL,
                          highlight=FALSE, multivalued=FALSE) {

  # Resolve names
  choices <- choicesWithNames(choices)

  inputTag <- div(
    id = inputId,
    class = trimws(paste('ds-dropzone', opts2class(list(hidden = hidden,
                                                        highlight = highlight,
                                                        multivalued = multivalued))), "right"),
    insertPlaceholder(inputId, ifelse(hidden, placeholder, NA)),
    div(
      class = 'ds-dropzone-options',
      tagList(
        mapply(names(choices),
               choices,
               FUN = function(name, choice) { dragulaZoneOptions('drop', name, choice) },
               SIMPLIFY = FALSE, USE.NAMES = FALSE
        )
      )
    )
  )

  attachDependencies(inputTag)
}

#' Run dragulaSelectR Example Applications
#'
#' Launch dragulaSelectR example applications, and optionally, your system's web browser.
#'
#' @param example The name of the example to run, or \code{NA} (the default) to
#'   list the available examples.
#' @param port The TCP port that the application should listen on. Defaults to
#'   choosing a random port.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to true in
#'   interactive sessions only.
#' @param host The IPv4 address that the application should listen on. Defaults
#'   to the \code{shiny.host} option, if set, or \code{"127.0.0.1"} if not.
#' @param display.mode The mode in which to display the example. Defaults to
#'   \code{showcase}, but may be set to \code{normal} to see the example without
#'   code or commentary.
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   # List all available examples
#'   runExample()
#'
#'   # Run one of the examples
#'   runExample("01_basic")
#'
#'   # Print the directory containing the code for all examples
#'   system.file("examples", package="dragulaSelectR")
#' }
#' @export
#'
#' @references \code{\link[shiny]{runExample}}
#'
runExample <- function(example=NA,
                       port=NULL,
                       launch.browser=getOption('shiny.launch.browser',
                                                interactive()),
                       host=getOption('shiny.host', '127.0.0.1'),
                       display.mode=c("auto", "normal", "showcase")) {
  examplesDir <- system.file('examples', package='dragulaSelectR')
  dir <- resolve(examplesDir, example)
  if (is.null(dir)) {
    if (is.na(example)) {
      errFun <- message
      errMsg <- ''
    }
    else {
      errFun <- stop
      errMsg <- paste('Example', example, 'does not exist. ')
    }

    errFun(errMsg,
           'Valid examples are "',
           paste(list.files(examplesDir), collapse='", "'),
           '"')
  }
  else {
    runApp(dir, port = port, host = host, launch.browser = launch.browser,
           display.mode = display.mode)
  }
}

# HELPERS ----

#' Attach javascript dependencies
#'
#' \code{attachDependencies} attaches the javascript dependencies.Specifically,
#' the Dragula JS package is attached, as well as the javascript wrapper and
#' input bindings.
#'
attachDependencies <- function(...) {
  deps <- list(
    htmltools::htmlDependency(name = "dragula", version = "3.7.2",
                              package = "dragulaSelectR",
                              src = "www/dragula-3.7.2",
                              script = "dragula.min.js",
                              stylesheet = "dragula.min.css"
    ),
    htmltools::htmlDependency(name = "dragula-select", version = "0.0.0.9000",
                              package = "dragulaSelectR",
                              src = "www",
                              script = "dragula-select.js",
                              stylesheet = "dragula-select.css"
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
  paste(lapply(varArgsNames, function(opt) { paste0('ds-', opt) }), collapse = ' ')
}

#' Create options for dragzones/dropzones
#'
#' In Dragula, the drag and drop areas are called "containers". This function
#' creates the individual draggable items in these containers.
#'
#' @param type  Container type: either \code{drop} or \code{drag}
#' @param value Value for allowable item/option (unique identifier)
#' @param label Label for allowable item/option (what the user sees)
#'
#' @return div element
#'
dragulaZoneOptions <- function(type, value, label=NULL) {
  if (!(type %in% c('drag', 'drop'))) {
    stop(type, " is not a valid container type. Dragula container type must be either 'drag' or 'drop'")
  }
  div(
    "data-value" = value,
    class = paste0('ds-', ifelse(type=='drop', 'dropoption', 'dragitem')),
    label %||% value
  )
}

#' Convert unique values to multivalues
#'
#' (Multivalued dropzones only) This will drop the added unique counter ids
#' to multivalued inputs.
#'
#' @param uniqueIds Values returned by dropzone input
#'
#' @return Multivalues with unique id stripped away.
#'
#' @export
multivalues <- function(uniqueIds) {
  if (is.null(uniqueIds)) {
    return(NULL)
  } else {
    sapply(strsplit(uniqueIds, '-'),
           FUN = function(x) {
             paste(x[-length(x)], collapse = '-')
           }, simplify = "array")
  }
}
