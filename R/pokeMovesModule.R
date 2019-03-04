#' UI module for sorting pokemon
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeMoveUi <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_moves"), class = "col-sm-12")
}




#' Server module generating the pokemon interface
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param selected Input containing the selected pokemon index.
#' @param moves Contains preprocessed pokemon moves.
#'
#' @import tablerDash
#'
#' @export
pokeMove <- function(input, output, session, selected, moves) {

  # take the whole ability dataframe
  pokeMoves <- reactive({
    req(!is.null(selected()))
    moves[[selected()]]
  })

  # generate the card
  output$poke_moves <- renderPrint({

    tablerCard(
      title = paste0(selected(), " Moves"),
      statusSide = "top",
      collapsible = FALSE,
      closable = FALSE,
      zoomable = FALSE,
      width = 12,

      # card content
      lapply(seq_along(pokeMoves()), FUN = function(i) {
        moveName <- pokeMoves()[[i]]$name
        moveSlot <- pokeMoves()[[i]]$moveSlot
        moveEffect <- pokeMoves()[[i]]$moveEffect
        moveId <- pokeMoves()[[i]]$id

        fluidRow(
          paste("Slot: ", moveSlot),
          tagAppendAttributes(
            tablerTag(
              paste(moveId, moveName),
              href = NULL,
              rounded = FALSE,
              color = NULL
            ),
            class = "mx-2"
          ),
          moveEffect
        )

      })
    )
  })

}
