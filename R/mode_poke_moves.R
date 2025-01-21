#' UI module for sorting pokemon
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
#' @rdname poke-moves
poke_moves_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h1("Moves"),
    uiOutput(ns("poke_moves"))
  )
}

#' Server module generating the pokemon interface
#'
#' @param selected Input containing the selected pokemon index.
#'
#' @import tablerDash
#'
#' @export
#' @rdname poke-moves
poke_moves_server <- function(id, selected) {
  # generate the card
  moduleServer(
    id,
    function(input, output, session) {
      output$poke_moves <- renderUI({
        req(selected())
        poke_moves <- poke_data[[selected()]]$moves
        customTablerTable(
          title = paste(
            "Attacks that",
            selected(),
            "may learn during its growth."
          ),
          width = 12,
          # card content
          data = lapply(poke_moves, FUN = function(move) {
            list(
              name = move$name,
              type = tablerTag(move$type, color = get_type_color(move$type)),
              power = tablerProgress(
                100 *
                  move$power /
                  max(unlist(dropNulls(lapply(poke_attacks, `[[`, "power"))))
              ),
              description = move$text
            )
          })
        )
      })
    }
  )
}
