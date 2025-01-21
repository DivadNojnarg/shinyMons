#' UI module for generating the pokemon type
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
#' @rdname poke-types
poke_types_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h1("Types"),
    uiOutput(ns("poke_types"))
  )
}

#' Server module generating the pokemon types info boxes
#'
#' @param selected Input containing the selected pokemon index.
#'
#' @import tablerDash
#'
#' @export
#' @rdname poke-types
poke_types_server <- function(id, selected) {
  moduleServer(
    id,
    function(input, output, session) {
      # render infoBoxes
      output$poke_types <- renderUI({
        req(selected())
        types <- poke_data[[selected()]]$types

        lapply(types, FUN = function(type) {
          type_name <- type$name
          type_slot <- type$slot
          res <- type$damage_relations
          tagList(
            customTablerTable(
              title = paste(type_name, " type damages"),
              status = get_type_color(type_name),
              width = 12,
              # card content
              data = list(
                list(
                  Damages = "2X",
                  From = lapply(
                    extract_from_list(res$double_damage_from),
                    \(name) {
                      tablerTag(name, color = get_type_color(name))
                    }
                  ),
                  To = lapply(
                    extract_from_list(res$double_damage_to),
                    \(name) {
                      tablerTag(name, color = get_type_color(name))
                    }
                  )
                ),
                list(
                  Damages = "1/2",
                  From = lapply(
                    extract_from_list(res$half_damage_from),
                    \(name) {
                      tablerTag(name, color = get_type_color(name))
                    }
                  ),
                  To = lapply(
                    extract_from_list(res$half_damage_to),
                    \(name) {
                      tablerTag(name, color = get_type_color(name))
                    }
                  )
                ),
                list(
                  Damages = "0",
                  From = lapply(extract_from_list(res$no_damage_from), \(name) {
                    tablerTag(name, color = get_type_color(name))
                  }),
                  To = lapply(extract_from_list(res$no_damage_to), \(name) {
                    tablerTag(name, color = get_type_color(name))
                  })
                )
              )
            )
          )
        })
      })
    }
  )
}
