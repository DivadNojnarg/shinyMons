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
#' @param mainData Object containing the main pokemon data.
#' @param details Object containing extra pokemon details.
#' @param selected Input containing the selected pokemon index.
#' @export
pokeMove <- function(input, output, session, mainData, details, selected) {

  # take the whole ability dataframe
  abilities <- reactive({
    req(!is.null(selected()))
    mainData[[selected()]]$abilities
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
      lapply(seq_along(abilities()$slot), FUN = function(i) {
        moveName <- abilities()$ability$name[[i]]
        moveSlot <- abilities()$slot[[i]]
        moveUrl <- abilities()$ability$url[[i]]

        # potentially bottleneck
        moveDetails <- fromJSON(moveUrl)

        moveEffect <- moveDetails$effect_entries$short_effect
        moveId <- moveDetails$id

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
