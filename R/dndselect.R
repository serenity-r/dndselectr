#' dndselectr: Implements a drag-and-drop Shiny select input
#'
#' Implements a drag-and-drop Shiny select input. This implementation creates a Shiny input that replicates
#' much of the functionality of selectInput. Multiple zones for dragging and dropping are allowed.
#' Currently utilizes Dragula JS library, https://github.com/bevacqua/dragula.
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
#' @name dndselectr
#'
#' @seealso \code{\link{dragZone}}, \code{\link{dropZoneInput}}
#'
NULL

#' Create a dragzone container
#'
#' @param id The container id.
#' @param choices List of values to select from.
#' @param ... Additional arguments passed along to tags$div, such as class
#'
#' @export
#'
#' @examples
#' dragZone("dragzone", choices = list(one = "One",
#'                                     two = "Two",
#'                                     three = "Three",
#'                                     four = "Four"))
#'
#' @seealso \code{\link{dndselectr}}
#'
dragZone <- function(id, choices, ...) {

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
    zoneItems('drag', 'options', choices),
    ...
  )

  attachDependencies(inputTag)
}

#' Create a dropzone input
#'
#' @param inputId The \code{input} slot that will be used to acces the value.
#' @param choices List of acceptable values with their associated labels. Note that
#'   the labels can be arbitrary HTML, as long as they are wrapped in a \code{tagList}.
#' @param presets Array or list of preset values.
#' @param hidden Should the selected items be hidden? This is useful to represent
#'   a reactive or event trigger.
#' @param placeholder Insert placeholder text.
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
#' @param flex Use flex container for dropzone. Items are set to wrap, and flex direction is
#'   given by the \code{direction} argument (defaults to \code{vertical}).
#' @param server Function or function name as a string that will be used for
#'   server-side creation of UI for dropzone items. This is needed only when the
#'   dropzone items contain Shiny inputs and/or outputs. You must also include
#'   the \code{\link{dropZoneServer}} function in the server portion of your
#'   Shiny app.
#' @param ... Additional arguments passed along to \code{tags$div}, such as class
#'
#' @export
#'
#' @examples
#' dropZoneInput("dropzone", choices = list(one = "1",
#'                                          two = "2",
#'                                          three = "3",
#'                                          four = "4"))
#'
#' @seealso \code{\link{dndselectr}}
#'
dropZoneInput <- function(inputId, choices, presets=NULL, hidden=FALSE, placeholder=NULL,
                          highlight=FALSE, multivalued=FALSE, selectable=FALSE,
                          selectOnDrop=FALSE, togglevis=FALSE, togglelock=FALSE,
                          removeOnSpill=TRUE, direction="vertical", maxInput=Inf,
                          replaceOnDrop=FALSE, flex=FALSE, server=NULL, ...) {

  # Resolve names
  choices <- choicesWithNames(choices)

  # Manage presets
  presets <- presetsWithOptions(presets, choices, multivalued, server)

  # Make sure number of preset values obeys maxInput setting
  if (length(presets$values) > maxInput) {
    stop("Number of preset values (", length(presets$values), ") exceeds the maximum allowable (", maxInput,")")
  }

  inputTag <- div(
    id = inputId,
    class = trimws(paste('form-control ds-dropzone', opts2class(list(hidden = hidden,
                                                                     highlight = highlight,
                                                                     multivalued = multivalued,
                                                                     selectable = selectable,
                                                                     flex = flex,
                                                                     `max-input` = (length(presets$values) == maxInput),
                                                                     `replace-on-drop` = replaceOnDrop))), "right"),
    "data-select-on-drop" = tolower(selectOnDrop),
    "data-remove-on-spill" = tolower(removeOnSpill),
    "data-direction" = tolower(direction),
    "data-max-input" = ifelse(is.infinite(maxInput), "Infinity", maxInput),
    "data-server" = tolower(ifelse(!is.null(server), TRUE, FALSE)),
    insertPlaceholder(placeholder, hidden = is.null(placeholder) || (!hidden && (length(presets$values) > 0))),
    div(
      class = 'ds-dropzone-options',
      zoneItems('drop', 'options', choices,
                togglevis = togglevis,
                togglelock = togglelock)
    ),
    zoneItems('drop', 'presets',
              items = presets$values,
              ids = presets$ids,
              selected = presets$selected,
              invisible = presets$invisible,
              locked = presets$locked,
              freeze = presets$freeze,
              togglevis = togglevis,
              togglelock = togglelock),
    ...
  )

  attachDependencies(inputTag)
}

#' Create items for dragzones/dropzones
#'
#' Creates the individual draggable items and options in drag and drop zones.
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
zoneItems <- function(zone, type, items, ids=rep(NA, length(items)), selected=NULL,
                             invisible=NULL, locked=NULL, freeze=NULL, togglevis=FALSE, togglelock=FALSE) {
  if (!(zone %in% c('drag', 'drop'))) {
    stop(zone, " is not a valid zone type. Zone type must be either 'drag' or 'drop'")
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

#' Change the choices of a dragzone on the client
#'
#' @param session The session object passed to function given to shinyServer.
#' @inheritParams dragZone
#'
#' @export
updateDragZone <- function(session = getDefaultReactiveDomain(), id, choices=NULL)
{
  if (missing(id)) {
    warning("Using generic 'dragzone' as id. HTML element may not be unique!")
    id <- "dragzone"
  }

  selector <- paste0('#', id)

  removeUI(paste(selector, '.ds-dragitem'),
           multiple = TRUE,
           session = session)

  if (!is.null(choices)) {
    insertUI(selector,
             ui = zoneItems('drag', 'options', choicesWithNames(choices)),
             session = session)
  }
}

#' Change the values or settings of a dropzone on the client
#'
#' The set of presets can be cleared by using presets=character(0).
#'
#' @param session The session object passed to function given to shinyServer.
#' @inheritParams dropZoneInput
#'
#' @export
updateDropZoneInput <- function(session = getDefaultReactiveDomain(),
                                inputId, presets=NULL, choices=NULL, placeholder=NULL)
{
  # Make sure dropzone has been initialized first
  if (!is.null(session$input[[paste0(inputId, "_settings")]])) {
    update_choices <- !is.null(choices)
    update_presets <- !is.null(presets)
    if (!update_choices) { # If not updating choices, grab current choices
      choices <- session$input[[paste0(inputId, "_settings")]]$choices
    } else
      if (!update_presets) { # Need this to check if current presets are legit with new choices
        presets <- session$input[[inputId]]
      }
    choices <- choicesWithNames(choices)
    multivalued <- session$input[[paste0(inputId, "_settings")]]$multivalued
    maxInput <- session$input[[paste0(inputId, "_settings")]]$maxInput

    # NULL means do nothing; NA or "" means delete all options
    if (update_presets || update_choices) {
      preset_choices <- choicesWithNames(names(choices))
      # Manage presets
      presets <- withCallingHandlers(
        presetsWithOptions(presets, preset_choices, multivalued),
        warning = function(w) {
          update_presets <<- TRUE # There was a problem with presets and new choices - force update
          invokeRestart("muffleWarning")
        }
      )

      # Make sure number of preset values obeys maxInput setting
      if (!is.null(maxInput) && (length(presets$values) > maxInput)) {
        stop("Number of preset values (", length(presets$values), ") exceeds the maximum allowable (", maxInput,")")
      }
    }

    # First, handle updating choices - can do this server-side right here
    #  since doesn't affect input directly (although can affect indirectly
    #  through changes to presets, which is handled below)
    if (update_choices) {
      selector <- paste0('#', session$ns(inputId), ' .ds-dropzone-options')

      removeUI(paste(selector, '.ds-dropoption'),
               multiple = TRUE,
               immediate = TRUE,
               session = session)

      if (!is.null(choices)) {
        insertUI(selector,
                 ui = zoneItems('drop', 'options', choices),
                 immediate = TRUE,
                 session = session)
      }
    }

    # Refactor - handle presets server-side (like dropZoneInput)
    # Note: Need to send choices update message in order to update settings input
    message <- dropNulls(list(presets = switch(update_presets, presets),
                              choices = switch(update_choices, TRUE),
                              placeholder = placeholder))
    session$sendInputMessage(inputId, message)
  }
}

#' Run dndselectr Example Applications
#'
#' Launch dndselectr example applications, and optionally, your system's web browser.
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
#'   system.file("examples", package="dndselectr")
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
  examplesDir <- system.file('examples', package='dndselectr')
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
#' \code{attachDependencies} attaches the javascript dependencies. Specifically,
#' the Dragula JS package is attached, as well as the javascript wrapper and
#' input bindings.
#'
#' @param ... Shiny tag object
attachDependencies <- function(...) {
  deps <- list(
    htmltools::htmlDependency(name = "dragula", version = "3.7.2",
                              package = "dndselectr",
                              src = "www/dragula-3.7.2",
                              script = "dragula.min.js",
                              stylesheet = "dragula.min.css"
    ),
    htmltools::htmlDependency(name = "dndselectr", version = "0.0.0.9000",
                              package = "dndselectr",
                              src = "www",
                              script = "dndselectr.js",
                              stylesheet = "dndselectr.css"
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

#' Create server code to handle server-side creation of UI for dropzone items
#'
#' This function will create the necessary event handlers that will call
#'   the UI function specified in the \code{server} argument of the
#'   \code{\link{dropZoneInput}} function.
#' @param session     The \code{session} object passed to function given to \code{shinyServer}.
#' @param dropZoneId  The \code{id} of the first dropzone.
#' @param server      Function or function name as a string that will be used for
#'   server-side creation of UI for dropzone items. This is needed only when the
#'   dropzone items contain Shiny inputs and/or outputs.
#'
#' @return Expression including an observe event handler.
#' @export
dropZoneServer <- function(session, dropZoneId, server) {
  return({
    newserver <- list()
    newserver[[dropZoneId]] <- switch(class(server), "character" = eval(parse(text = server)), "function" = server)
    session$userData$server <- c(session$userData$server, newserver)

    observeEvent(session$input[[paste0(dropZoneId, '_server')]], {
      ui_func <- session$userData$server[[dropZoneId]]
      ui <- do.call(ui_func,
                    dropNulls(
                      list(
                        session$input[[paste0(dropZoneId, '_server')]]$value,
                        server = switch("server" %in% names(formals(ui_func)), TRUE, NULL)
                      )
                    )
      )

      # Make sure client-side code doesn't insert HTML within the selector element
      insertUI(
        selector = session$input[[paste0(dropZoneId, '_server')]]$selector,
        ui = ui,
        session = session
      )
    })
  })
}

#' Entangle inputs
#'
#' Create observe events that entangle multiple Shiny inputs. Useful for
#' hidden dropzones that take drops but display options elsewhere.
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param ...  The \code{ids} of the inputs
#'
#' @return Expressions including the \code{observeEvent}s.
#' @export
entangleInputs <- function(session, ...) {
  entangle <- function(from, to, len, type) {
    clear_input <- switch(type,
                          "DropZone" = rlang::expr(character(0)),
                          "Picker" = rlang::expr(""))
    # Create update function call (couldn't directly implement in returned expr below;
    #   didn't like !! as argument name in function call).  Thus, creating function
    #   call expression "manually" and using fact that call type is basically a list.
    #   See https://adv-r.hadley.nz/expressions.html#calls
    # Refactor: Learn quasiquotation
    ufunc <- rlang::call2(
      rlang::parse_expr(
        paste0(switch(type,
                      "DropZone" = "dndselectr",
                      "Picker" = "shinyWidgets"), '::', # namespace
               "update", type, "Input") # function
      ),
      rlang::expr(session),
      to,
      rlang::expr(session$input[[!!from]] %||% !!clear_input)
    )
    names(ufunc) <- c("", "session", "inputId", switch(type, "DropZone" = "presets", "Picker" = "selected"))

    return(rlang::expr(
      observeEvent(session$input[[!!from]], {
        session$userData$entangled <- sign(session$userData$entangled + 1) * ((session$userData$entangled + 1) %% !!len)
        if (session$userData$entangled > 0) {
          !!ufunc
        }
      }, ignoreNULL = FALSE, ignoreInit = TRUE, label = !!from)
    ))
  }

  session$userData$entangled <- 0 # Temporary variable used for chain updating

  id <- as.list(match.call(expand.dots=FALSE))$...

  if (is.null(names(id))) {
    type <- rep("DropZone", length(id))
    id <- unlist(id)
  } else {
    type <- unlist(id)
    id <- names(id)
  }

  for (i in 1:length(id)) {
    j <- (i %% length(id)) + 1
    eval(entangle(id[i], id[j], length(id), type[j]))
  }
}

#' Append item to end of specified dropzone
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param value The value to clone from dragzoneId and append to dropzoneId.
#' @param dropzoneId The \code{id} of the dropzone.
#'
#' @export
appendToDropzone <- function(session, value, dropzoneId) {
  message <- dropNulls(list(action = "append", value = value))
  session$sendInputMessage(dropzoneId, message)
}

#' Delete selected item in specified dropzone
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param dropzoneId The \code{id} of the dropzone.
#'
#' @export
removeSelected <- function(session, dropzoneId) {
  message <- dropNulls(list(action = "remove_selected"))
  session$sendInputMessage(dropzoneId, message)
}

#' Remove selection
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param dropzoneId The \code{id} of the dropzone.
#'
#' @export
unselect <- function(session, dropzoneId) {
  message <- dropNulls(list(action = "unselect"))
  session$sendInputMessage(dropzoneId, message)
}

#' Select element
#'
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param value The value of the item to select.
#' @param dropzoneId The \code{id} of the dropzone.
#'
#' @export
select <- function(session, value, dropzoneId) {
  message <- dropNulls(list(action = "select",
                            val = multivalues(value),
                            id = switch(isMultivalued(value), multivalues(value, ids=TRUE), NULL)))
  session$sendInputMessage(dropzoneId, message)
}
