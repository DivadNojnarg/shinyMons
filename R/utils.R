#' Extract selected key from lists
#'
#' @param l List to search in.
#' @param key List key to extract. Default to name.
#' @param type Needed by \code{vapply} as FUN.VALUE parameter.
#' Default to character.
#'
#' @return A list.
#' @keywords internal
extract_from_list <- function(l, key = "name", type = character(1)) {
  vapply(
    l,
    `[[`,
    key,
    FUN.VALUE = type
  )
}

#' Remove NULL elements from list
#'
#' @param x List to cleanup
#'
#' @return A list.
#' @keywords internal
dropNulls <- function(x) { #nolint
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' Recursively find pokemon evolutions
#' 
#' When length(tmp$evolves_to) is 0, the function returns
#' a list with aggregated pokemon names, ids, trigger, levels ...
#'
#' @param tmp, id, entry_point, trigger, level, object Internal parameters
#'
#' @return A list.
#' @keywords internal
find_evol <- function(tmp, id, entry_point, trigger = NULL, level = NULL, object = NULL) {
  # handle first gen (Onyx can evolve but not in first gen)
  tmp_id <- fromJSON(tmp$species$url, simplifyVector = FALSE)$id
  if (tmp_id >= 151) {
    return(NULL)
  }

  # handle evolution details: levels, trigger, ...
  details <- tmp$evolution_details[[1]]
  id <- c(id, tmp_id)
  entry_point <- c(entry_point, tmp$species$name)
  trigger <- c(trigger, details$trigger$name)
  level <- c(level, if (is.null(details$min_level)) NA else details$min_level)
  object <- c(object, if (details$trigger$name == "use-item") details$item$name else NA)

  if (length(tmp$evolves_to)) {
    find_evol(tmp$evolves_to[[1]], id, entry_point, trigger, level, object)
  }  else {
    return(list(
      chain = entry_point,
      id = id,
      trigger = c(trigger, NA),
      level = c(level, NA),
      object = c(object, NA)
    ))
  }
}

#' Given a pokemon entry_point return the entire list
#'
#' @param l Pokemon evolution chain.
#'
#' @return A list.
#' @keywords internal
find_evols <- function(l) {
  if (!length(l$evolves_to) && length(l$species$name) == 1) return(NULL)
  id <- fromJSON(l$species$url, simplifyVector = FALSE)$id
  entry_point <- l$species$name
  # Handle evee (3 evolutions of stage 1)
  evols <- l$evolves_to
  if (length(evols) > 1) {
    dropNulls(mclapply(evols, \(tmp) {
      find_evol(tmp, id, entry_point)
    }))
  } else {
    tmp <- evols[[1]]
    # remove eggs (pikachu, magmar, ...)
    if (l$is_baby) {
      entry_point <- tmp$species$name
      id <- fromJSON(tmp$species$url, simplifyVector = FALSE)$id
      if (length(tmp$evolves_to)) {
        tmp <- tmp$evolves_to[[1]]
      } else {
        return(NULL)
      }
    }
    find_evol(tmp, id, entry_point)
  }
}