#' UI module for generating the pokemon location card
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeLocationUi <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_locations"))
}




#' Server module generating the pokemon location card
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param mainData Object containing the main pokemon data.
#' @param details Object containing extra pokemon details.
#' @param selected Input containing the selected pokemon index.
#' @export
pokeLocation <- function(input, output, session, mainData, details, selected) {

  locations <- reactive({
    req(!is.null(selected()))
    locationUrl <- fromJSON(mainData[[selected]]$location_area_encounters)
    locationUrl$location_area$name
    # there are locations not related to the first generation ...
  })

  output$poke_locations <- renderUI({

  })
}
