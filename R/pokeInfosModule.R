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
#' @param selected Input containing the selected pokemon index.
#' @export
pokeInfos <- function(input, output, session, mainData, details, pokeNames, selected) {

  #generate the profile cards (as many as the number of selected pokemons)
   output$poke_infos <- renderUI({

     # there is a quirk in the raw data: in details, names are in lower case
     # whereas in the main pokemon list, names start with a capital letter...

     fluidRow(
       tablerProfileCard(
         title = selected(),
         subtitle = details[[selected()]]$flavor_text_entries$flavor_text[54],
         background = NULL,
         src = mainData[[selected()]]$sprites$front_default,
         socials = NULL,
         width = 12
       )
     )
   })
}
