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

  if (missing(id)) {
    warning("Using generic 'dragzone' as id. HTML element may not be unique!")
    id <- "dragzone"
  }

  if (missing(choices)) {
    stop("You must specify choices for this dragZone. ")
  }

  # Resolve names
  choices <- choicesWithNames(choices)

  inputTag <- div(
    id = id,
    class = 'ds-dragzone',
    dragulaZoneItems('drag', 'options', choices)
  )

  attachDependencies(inputTag)
}

#' Create a Dragula dropzone input
#'
#' @param inputId The \code{input} slot that will be used to acces the value.
#' @param choices List of acceptable values with their associated labels. Note that
#'   the labels can be arbitrary HTML, as long as they are wrapped in a \code{tagList}.
#' @param presets Array or list of preset values.
#' @param hidden Should the selected items be hidden? This is useful to represent
#'   a reactive or event trigger.
#' @param placeholder If hidden is true, insert placeholder text.
#' @param highlight Highlights the container on dragover. Useful when \code{hidden} is active.
#' @param multivalued Allow multiple items with the same value?
#' @param selectable Are the items in this dropzone selectable? Default is \code{false}. Use
#'   Shiny input \code{input$<inputId>_selected} to access selected items.
#' @param selectOnDrop Should new dropped items be automatically selected?
#' @param togglevis Add an icon to allow toggling items between visible/invisible. Default is
#'   \code{false}. Use Shiny input \code{input$<inputId>_invisible} to access invisible items.
#' @param togglelock Add an icon to allow toggling items between locked/unlocked. Locked items
#'   are not draggable. Default is \code{false}. Use Shiny input \code{input$<inputId>_locked}
#'   to access locked items.
#' @param removeOnSpill Remove items when dragged outside dropzone? Default is \code{true}.
#' @param direction Direction (\code{horizontal} or \code{vertical}) to consider when
#'   determining where an element would be dropped. Default is \code{vertical}.
#' @param maxInput Maximum allowable dropped items.
#' @param replaceOnDrop Replace item on drop when at maximum allowable items?
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
dropZoneInput <- function(inputId, choices, presets=NULL, hidden=FALSE, placeholder=NULL,
                          highlight=FALSE, multivalued=FALSE, selectable=FALSE,
                          selectOnDrop=FALSE, togglevis=FALSE, togglelock=FALSE,
                          removeOnSpill=TRUE, direction="vertical", maxInput=Inf,
                          replaceOnDrop=FALSE) {

  # Resolve names
  choices <- choicesWithNames(choices)

  # Manage presets
  presets <- presetsWithOptions(presets, choices, multivalued)

  # Make sure number of preset values obeys maxInput setting
  if (length(presets$values) > maxInput) {
    stop("Number of preset values (", length(presets$values), ") exceeds the maximum allowable (", maxInput,")")
  }

  inputTag <- div(
    id = inputId,
    class = trimws(paste('ds-dropzone', opts2class(list(hidden = hidden,
                                                        highlight = highlight,
                                                        multivalued = multivalued,
                                                        selectable = selectable,
                                                        `max-input` = (length(presets$values) == maxInput),
                                                        `replace-on-drop` = replaceOnDrop))), "right"),
    "data-select-on-drop" = tolower(selectOnDrop),
    "data-remove-on-spill" = tolower(removeOnSpill),
    "data-direction" = tolower(direction),
    "data-max-input" = ifelse(is.infinite(maxInput), "Infinity", maxInput),
    insertPlaceholder(inputId, ifelse(hidden, placeholder, NA)),
    div(
      class = 'ds-dropzone-options',
      dragulaZoneItems('drop', 'options', choices,
                       togglevis = togglevis,
                       togglelock = togglelock)
    ),
    dragulaZoneItems('drop', 'presets',
                     items = presets$values,
                     ids = presets$ids,
                     selected = presets$selected,
                     invisible = presets$invisible,
                     locked = presets$locked,
                     freeze = presets$freeze,
                     togglevis = togglevis,
                     togglelock = togglelock)
  )

  attachDependencies(inputTag)
}

#' Create items for dragzones/dropzones
#'
#' In Dragula, the drag and drop areas are called "containers". This function
#' creates the individual draggable items and options in these containers.
#'
#' @param zone  Container zone type: either \code{drop} or \code{drag}
#' @param type  Are these \code{options} or \code{presets}?
#' @param items List of item labels, with names corresponding to values
#' @param ids If multivalued, these will be unique ids
#' @param selected  Selected items (array length of items - either NA or ds-selected)
#' @param invisible Invisible items (array length of items - either NA or ds-invisible)
#' @param locked  Locked items (array length of items - either NA or ds-locked)
#' @param freeze No items allowed before these. Analogous to freezing the first few
#'   columns of a spreadsheet (array length of items - either NA or ds-freeze). Makes
#'   since only for first initial items, and when used in conjunction with locked.
#'   Frozen items also cannot be toggled.
#' @param togglevis Add an icon to allow toggling items between visible/invisible.
#' @param togglelock Add an icon to allow toggling items between locked/unlocked.
#'
#' @return div element
#'
dragulaZoneItems <- function(zone, type, items, ids=rep(NA, length(items)), selected=NULL,
                             invisible=NULL, locked=NULL, freeze=NULL, togglevis=FALSE, togglelock=FALSE) {
  if (!(zone %in% c('drag', 'drop'))) {
    stop(zone, " is not a valid container type. Dragula container type must be either 'drag' or 'drop'")
  }
  if (!(type %in% c('options', 'presets'))) {
    stop(type, " is not a valid item type. Item type must be either 'options' or 'presets'")
  }
  values <- names(items)
  tagList(
    lapply(seq_along(items),
           FUN = function(values, labels, ids, selected, invisible, locked, freeze, togglevis, togglelock, i) {
             div(
               "data-value" = values[[i]] %||% labels[[i]],
               "data-instance" = ids[[i]],
               class = trimws(paste(paste0('ds-', ifelse(zone=='drop', 'dropoption', 'dragitem')),
                                    paste(keepTruthy(c(selected[[i]], invisible[[i]], locked[[i]], freeze[[i]])), collapse = ' '))
                              ),
               labels[[i]] %||% values[[i]],
               switch(togglevis && (zone == 'drop') && !isTruthy(freeze[[i]]),
                      div(class = "ds-toggle-visible",
                          ifelse(isTruthy(invisible[[i]]), tagList(icon("eye-slash")), tagList(icon("eye")))),
                      NULL),
               switch(togglelock && (zone == 'drop') && !isTruthy(freeze[[i]]),
                      div(class = "ds-toggle-lock",
                          ifelse(isTruthy(locked[[i]]), tagList(icon("lock")), tagList(icon("lock-open")))),
                      NULL)
             )
           }, values = values, labels = items, ids = ids, selected = selected, invisible = invisible,
           locked = locked, freeze = freeze, togglevis = togglevis, togglelock = togglelock
    )
  )
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
#' @param ... Shiny tag object
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

#' Input is set of possible unique ids from multivalued inputs
#'
#' @param values Values returned by dropzone input
#'
#' @return logical
#'
#' @export
isMultivalued <- function(values) {
  nvals <- length(values)
  if (is.null(values))
    return(FALSE)

  # Not unique values
  if (length(unique(values)) != nvals)
    return(FALSE)

  # Doesn't have the -ds- separator in all entries
  if (length(grep('-ds-', values)) != nvals)
    return(FALSE)

  # Check individual id entries - are they integers when casted?
  ids <- multivalues(values, ids=TRUE)
  if (anyNAOrFalse(isWholeNum(suppressWarnings(as.double(ids)))))
    return(FALSE)

  return(TRUE)
}

#' Convert unique values to multivalues
#'
#' (Multivalued dropzones only) This will drop the added unique counter ids
#' and convert to multivalued inputs.
#'
#' @param values Values returned by dropzone input
#' @param ids Return unique ids rather than multivalues?
#'
#' @return Multivalues with unique id stripped away.
#'
#' @export
multivalues <- function(values, ids=FALSE) {
  if (!is.null(values)) {
    sapply(strsplit(values, '-ds-'),
           FUN = function(x) {
             if (length(x) == 1) {
               return(ifelse(ids, NA, x))
             } else {
               return(ifelse(ids, x[length(x)], paste(x[-length(x)], collapse = '-ds-')))
             }
           }, simplify = "array")
  }
}

#' Entangle two dropzones
#'
#' Create observe events that entangle two Shiny dropzones. Useful for
#' hidden dropzones that take drops but display options elsewhere.
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param dropZoneOneId  The \code{id} of the first dropzone.
#' @param dropZoneTwoId  The \code{id} of the second dropzone.
#'
#' @return Expression including the two observe events.
#' @export
entangle <- function(session, dropZoneOneId, dropZoneTwoId) {
  return({
    observeEvent(session$input[[dropZoneOneId]], {
      if (!isTRUE(all.equal(session$input[[dropZoneOneId]], session$input[[dropZoneTwoId]]))) {
        entangleSourceToTarget(session, sourceId = dropZoneOneId, targetId = dropZoneTwoId)
      }
    }, ignoreNULL = FALSE)

    observeEvent(session$input[[dropZoneTwoId]], {
      if (!isTRUE(all.equal(session$input[[dropZoneOneId]], session$input[[dropZoneTwoId]]))) {
        entangleSourceToTarget(session, sourceId = dropZoneTwoId, targetId = dropZoneOneId)
      }
    }, ignoreNULL = FALSE)
  })
}

#' Update target dropzone entangled with source dropzone
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param sourceId  The \code{id} of the source dropzone.
#' @param targetId  The \code{id} of the target dropzone.
#'
entangleSourceToTarget <- function(session, sourceId, targetId) {
  message <- dropNulls(list(action = "entangle", sourceId = session$ns(sourceId)))
  session$sendInputMessage(targetId, message)
}
