#' UI module for sorting pokemon
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
#' @rdname poke-moves
poke_moves_ui <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_moves"), class = "col-sm-12")
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

        tablerCard(
          title = paste0(selected(), " Moves"),
          statusSide = "top",
          collapsible = FALSE,
          closable = FALSE,
          zoomable = FALSE,
          width = 12,

          # card content
          lapply(poke_moves, FUN = function(move) {
            fluidRow(
              tagAppendAttributes(
                tablerTag(
                  move$name,
                  href = NULL,
                  rounded = FALSE,
                  color = NULL
                ),
                class = "mx-2"
              ),
              move$text
            )
          })
        )
      })
    }
  )
}
