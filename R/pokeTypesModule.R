#' UI module for generating the pokemon type
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeTypeUi <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_types"))
}




#' Server module generating the pokemon types info boxes
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param mainData Object containing the main pokemon data.
#' @param selected Input containing the selected pokemon index.
#' @export
pokeType <- function(input, output, session, mainData, selected) {


  types <- reactive({
    req(!is.null(selected()))
    mainData[[selected()]]$types
  })

  # render infoBoxes
  output$poke_types <- renderUI({

    lapply(seq_along(rev(types()$slot)), FUN = function(i) {

      typeName <- types()$type[["name"]][[i]]
      typeSlot <- types()$slot[[i]]

      # set up colors according to the pokemon type original color
      # fortunataley tabler dash has tons of colors available
      pokeColor <- switch(
        typeName,
        "normal" = "gray-lightest",
        "fighting" = "red",
        "flying" = "indigo",
        "poison" = "purple-light",
        "ground" = "yellow-lighter",
        "rock" = "yellow-darker",
        "bug" = "green-lighter",
        "ghost" = "purple-dark",
        "fire" = "orange",
        "water" = "azure",
        "grass" = "green",
        "electric" = "yellow",
        "psychic" = "pink",
        "ice" = "azure-lighter",
        "dragon" = "purple-darker"
      )

      tablerInfoCard(
        value = paste(typeSlot, typeName),
        status = pokeColor,
        icon = NULL,
        description = NULL,
        width = 12
      )
    })
  })
}
