# Defaults for NULL values
`%||%` <- function(a, b) if (is.null(a)) b else a

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

keepTruthy <- function(x) {
  x[vapply(x, isTruthy, FUN.VALUE=logical(1))]
}

# Useful for multivalued check
isWholeNum <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

anyNAOrFalse <- function(x) {
  any(is.na(x) | !x)
}

# Used in runExample ----

# Attempt to join a path and relative path, and turn the result into a
# (normalized) absolute path. The result will only be returned if it is an
# existing file/directory and is a descendant of dir.
#
# Example:
# resolve("/Users/jcheng", "shiny")  # "/Users/jcheng/shiny"
# resolve("/Users/jcheng", "./shiny")  # "/Users/jcheng/shiny"
# resolve("/Users/jcheng", "shiny/../shiny/")  # "/Users/jcheng/shiny"
# resolve("/Users/jcheng", ".")  # NULL
# resolve("/Users/jcheng", "..")  # NULL
# resolve("/Users/jcheng", "shiny/..")  # NULL
#
resolve <- function(dir, relpath) {
  abs.path <- file.path(dir, relpath)
  if (!file.exists(abs.path))
    return(NULL)
  abs.path <- normalizePath(abs.path, winslash='/', mustWork=TRUE)
  dir <- normalizePath(dir, winslash='/', mustWork=TRUE)
  # trim the possible trailing slash under Windows (#306)
  if (isWindows()) dir <- sub('/$', '', dir)
  if (nchar(abs.path) <= nchar(dir) + 1)
    return(NULL)
  if (substr(abs.path, 1, nchar(dir)) != dir ||
      substr(abs.path, nchar(dir)+1, nchar(dir)+1) != '/') {
    return(NULL)
  }
  return(abs.path)
}

# On a Windows machine?
isWindows <- function() .Platform$OS.type == 'windows'

# Input utils ----
# See shiny/input-utils.R

insertPlaceholder <- function(placeholder, hidden = FALSE) {
  class <- paste(c("ds-placeholder", switch(hidden, "hidden", NULL)), collapse = " ")
  placeholder %AND% tags$span(class = class, placeholder)
}

# Takes a vector or list, and adds names (same as the value) to any entries
# without names.
choicesWithNames <- function(choices) {
  # Take a vector or list, and convert to list. If the list is unnamed,
  # convert it to a named list with blank names.
  listify <- function(obj) {
    # If a list/vector is unnamed, give it blank names
    makeNamed <- function(x) {
      if (is.null(names(x))) names(x) <- character(length(x))
      x
    }

    res <- lapply(obj, function(val) {
      if (any(class(val) %in% c("html", "shiny.tag", "shiny.tag.list"))) {
        return(val)
      } else if (length(val) == 1) {
        return(as.character(val))
      } else
        stop("Individual choices cannot be vectors or lists.")
    })

    makeNamed(res)
  }

  choices <- listify(choices)
  if (length(choices) == 0) return(choices)

  # default missing names to choice values
  # Note: Need to use mapply since vectorized as.character(choices) does bad
  #  things to a list of shiny.tag objects
  names(choices) <- mapply(function(choice, name) {
    if (name == "") {
      return(as.character(choice))
    } else {
      return(name)
    }
  }, choices, names(choices), USE.NAMES=FALSE)

  choices
}

# multivalued and selectable must be given by dropZoneInput
presetsWithOptions <- function(presets, choices, multivalued) {
  if (is.atomic(presets)) presets <- list(values = presets)
  for (option in c("selected", "locked", "invisible", "freeze")) {
    presets <- parseOption(presets, option)
  }
  presets <- getIds(presets, multivalued)

  # Make sure they are valid choices
  valid_choices <- presets$values %in% names(choices)
  if (anyNAOrFalse(valid_choices)) {
    warning("Preset value(s) ", paste(presets$values[!valid_choices], sep=", "), " not allowed for this dropzone.")
  }
  # Subset accordingly
  presets <- lapply(presets, function(value, valid_choices) { value[valid_choices] }, valid_choices = valid_choices)
  presets$values <- choices[presets$values]

  presets
}

getIds <- function(presets, multivalued) {
  if (multivalued) {
    if (isMultivalued(presets$values)) {
      presets$ids <- multivalues(presets$values, ids=TRUE)
      presets$values <- multivalues(presets$values)
    } else {
      presets$ids <- seq(1, length(presets$values))
    }
  } else {
    presets$ids <- rep(NA, length(presets$values))
  }
  presets
}

parseOption <- function(presets, option) {
  if (is.null(presets$values) || is.null(presets[[option]])) {
    return(presets)
  }
  if ((option == "selected") && (length(presets$selected) > 1)) {
    stop("Only one selected value allowed!")
  }
  if (anyNAOrFalse(presets[[option]] %in% presets$values)) {
    warning(.simpleCap(option), " value ", paste(presets[[option]][!(presets[[option]] %in% presets$values)], sep=", "), " not in presets.")
  }
  classnames <- rep(NA, length(presets$values))
  classnames[presets$values %in% presets[[option]]] <- paste0('ds-', option)
  presets[[option]] <- classnames
  return(presets)
}

# From toupper help
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
