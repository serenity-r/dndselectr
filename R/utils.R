# Defaults for NULL values
`%||%` <- function(a, b) if (is.null(a)) b else a

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0
}
# Given a vector or list, drop all the NULL items in it
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE=logical(1))]
}

# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
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

insertPlaceholder <- function(zoneId, placeholder) {
  placeholder %AND% tags$span(class = "ds-placeholder", placeholder)
}

# Takes a vector or list, and adds names (same as the value) to any entries
# without names. Coerces all leaf nodes to `character`.
choicesWithNames <- function(choices) {
  # Take a vector or list, and convert to list. Also, if any children are
  # vectors with length > 1, convert those to list. If the list is unnamed,
  # convert it to a named list with blank names.
  listify <- function(obj) {
    # If a list/vector is unnamed, give it blank names
    makeNamed <- function(x) {
      if (is.null(names(x))) names(x) <- character(length(x))
      x
    }

    res <- lapply(obj, function(val) {
      if (is.list(val))
        listify(val)
      else if (length(val) == 1 && is.null(names(val)))
        as.character(val)
      else
        makeNamed(as.list(val))
    })

    makeNamed(res)
  }

  choices <- listify(choices)
  if (length(choices) == 0) return(choices)

  # Recurse into any subgroups
  choices <- mapply(choices, names(choices), FUN = function(choice, name) {
    if (!is.list(choice)) return(choice)
    if (name == "") stop('All sub-lists in "choices" must be named.')
    choicesWithNames(choice)
  }, SIMPLIFY = FALSE)

  # default missing names to choice values
  missing <- names(choices) == ""
  names(choices)[missing] <- as.character(choices)[missing]

  choices
}

# This does a little more than just splitting preset ids. It also
# assigns the label of the corresponding option.
splitPresets <- function(presets, choices, multivalued=FALSE) {
  ids <- NULL
  if (!nullOrEmpty(presets)) {
    if (multivalued) {
      if (isMultivalued(presets)) {
        ids <- multivalues(presets, ids=TRUE)
        presets <- multivalues(presets)
      } else {
        ids <- seq(1, length(presets))
      }
    } else {
      ids <- rep(NA, length(presets))
    }

    # Make sure they are valid choices
    if (anyNAOrFalse(presets %in% names(choices))) {
      warning("Preset value(s) ", paste(presets[!(presets %in% names(choices))], sep=", "), " not allowed for this dropzone.")
    }
    # Subset accordingly
    ids <- ids[presets %in% names(choices)]
    presets <- dropNulls(choices[presets])
  }

  list(
    values = presets,
    ids = ids
  )
}

# Given the result of splitPresets, reconstruct the id'd unique value.
# Throws away the label for the preset.
# Note: Consider refactoring and making this an inverse function of multivalues
#       The function multivalues could also return a list of values and ids,
#       like what splitPresets is doing.
combinePresets <- function(presets) {
  sapply(seq_along(names(presets$values)),
         FUN = function(value, id, i) {
           ifelse(is.na(id[i]), value[i], paste0(value[i], '-ds-', id[i]))
         }, value = names(presets$values), id = presets$ids)
}

# If valid selected item present, will return array of NAs for non-selected items,
# and string "selected" for the selected item.
parseSelected <- function(selected, presets) {
  if (is.null(presets) || is.null(selected)) {
    return(NULL)
  }
  if (length(selected) > 1) {
    stop("Only one selected value allowed!")
  }
  if (anyNAOrFalse(selected %in% presets)) {
    warning("Selected value ", paste(selected[!(selected %in% presets)], sep=", "), " not in presets.")
  }
  selections <- rep(NA, length(presets))
  selections[presets == selected] <- "selected"
  return(selections)
}
