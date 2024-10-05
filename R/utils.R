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
  # handle evolution details: levels, trigger, ...
  details <- tmp$evolution_details[[1]]
  entry_point <- c(entry_point, tmp$species$name)
  trigger <- c(trigger, details$trigger$name)
  level <- c(level, if (is.null(details$min_level)) NA else details$min_level)
  object <- c(object, if (details$trigger$name == "use-item") details$item$name else NA)

  tmp_id <- fromJSON(tmp$species$url, simplifyVector = FALSE)$id
  if (tmp_id > 151) return(NULL)

  res <- list(
    chain = entry_point,
    id = c(id, tmp_id),
    trigger = c(trigger, NA),
    level = c(level, NA),
    object = c(object, NA)
  )

  if (length(tmp$evolves_to)) {
    evol_id <- fromJSON(tmp$evolves_to[[1]]$species$url, simplifyVector = FALSE)$id
    if (evol_id > 151) return(res)
    id <- c(id, tmp_id)
    find_evol(tmp$evolves_to[[1]], id, entry_point, trigger, level, object)
  }  else {
    return(res)
  }
}

#' Given a pokemon entry_point return the entire list
#'
#' @param l Pokemon evolution chain.
#'
#' @return A list.
#' @keywords internal
find_evols <- function(l) {
  # Handle pokemons that just can't evolve at all
  if (!length(l$evolves_to) && length(l$species$name) == 1) return(NULL)
  id <- fromJSON(l$species$url, simplifyVector = FALSE)$id
  # Handle Hitmonchan and cie (that has a lower evolution but not from a baby ...)
  if (id > 151) return(NULL)
  evols <- l$evolves_to
  evol_id <- fromJSON(evols[[1]]$species$url, simplifyVector = FALSE)$id
  # handle cases like onyx, ... that can evolve in second gen
  if (evol_id > 151) return(NULL)

  entry_point <- l$species$name
  # Handle evee (3 evolutions of stage 1)
  dropNulls(lapply(evols, \(tmp) {
    # Handle babies like for pichu, ...
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
  }))
}

build_network_props <- function(tmp, poke_nodes, poke_edges, i) {
  list(
    nodes = data.frame(
      id = tmp$id,
      label = tmp$chain,
      group = rep(i, length(tmp$id)),
      image = vapply(tmp$id, \(el) {
        poke_data[[el]]$sprites$front_default
      }, FUN.VALUE = character(1))
    ),
    edges = data.frame(
      from = c(tmp$id[1], if (length(tmp$id) > 2) tmp$id[2]),
      to = c(tmp$id[2], if (length(tmp$id) > 2) tmp$id[3]),
      title = c(
        sprintf(
          "At level: %s; trigger: %s, object: %s",
          tmp$level[1],
          tmp$trigger[1],
          tmp$object[1]
        ),
        if (length(tmp$id) > 2) {
          sprintf(
            "At level: %s; trigger: %s, object: %s",
            tmp$level[2],
            tmp$trigger[2],
            tmp$object[2]
          )
        }
      )
    )
  )
}

build_poke_families <- function() {
  i <- 1
  poke_nodes <- data.frame()
  poke_edges <- data.frame()

  while (i <= length(poke_data)) {
    tmp <- poke_data[[i]]$evolutions
    if (length(tmp)) {
      # build nodes
      for (evol in tmp) {
        if (length(poke_nodes$id) == 0 || !all((evol$id %in% poke_nodes$id))) {
          res <- build_network_props(evol, poke_nodes, poke_edges, i)
          poke_nodes <- rbind(poke_nodes, res$nodes)
          poke_edges <- rbind(poke_edges, res$edges)
        }
      }
      if (length(tmp) > 1) {
        i <- i + length(tmp)
      } else {
        i <- i + length(tmp[[1]]$id)
      }
    } else {
      i <- i + 1
    }
  }

  # In case of multiple evolutions like for evee
  # we must remove the dupliacted evee that will
  # appear for each evolution.
  to_remove <- which(duplicated(poke_nodes$id) == TRUE)
  poke_nodes <- poke_nodes[-to_remove, ]

  poke_nodes$shape = rep("image", length(poke_nodes$id))
  poke_nodes$value = rep(10, length(poke_nodes$id))
  poke_edges$arrows = rep("to", length(poke_edges$title))

  list(
    poke_nodes = poke_nodes,
    poke_edges = poke_edges
  ) 
}