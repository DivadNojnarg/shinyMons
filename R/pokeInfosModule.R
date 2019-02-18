#' UI module for generating the pokemon info cards
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeInfosUi <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_infos"))
}



#' Server module generating the pokemon info cards
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param mainData Object containing the main pokemon data.
#' @param details Object containing extra pokemon details.
#' @param pokeNames Object containing pokemon names.
#' @export
pokeInfos <- function(input, output, session, mainData, details, pokeNames) {

  #generate the profile cards (as many as the number of selected pokemons)
  output$poke_infos <- renderUI({
    fluidRow(
      lapply(seq_along(pokeNames()), FUN = function(i) {
        tablerProfileCard(
          title = pokeNames()[[i]],
          subtitle = details()[[i]]$flavor_text_entries$flavor_text[54],
          background = NULL,
          src = mainData()[[i]]$sprites$front_default,
          socials = NULL,
          width = 4
        )
      })
    )
  })
}
