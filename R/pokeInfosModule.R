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
#' @param selected Input containing the selected pokemon index.
#' @param shiny Whether to display a shiny version. FALSE by default.
#' @export
pokeInfos <- function(input, output, session, mainData, details, selected, shiny) {

  #generate the profile cards (as many as the number of selected pokemons)
   output$poke_infos <- renderUI({

     # there is a quirk in the raw data: in details, names are in lower case
     # whereas in the main pokemon list, names start with a capital letter...

     req(!is.null(selected()))

     habitats <- unique(unlist(lapply(1:151, function(i) details[[i]]$habitat$name)))
     habitatColor <- switch (details[[selected()]]$habitat$name,
       "grassland" = "lime",
       "mountain" = "orange",
       "waters-edge" = "azure",
       "forest" = "green",
       "rough-terrain" = "yellow",
       "cave" = "gray-dark",
       "urban" = "gray",
       "sea" = "blue",
       "rare" = "purple"
     )

     tagList(
       fluidRow(
         tablerProfileCard(
           title = selected(),
           subtitle = details[[selected()]]$flavor_text_entries$flavor_text[54],
           background = NULL,
           src = if (!shiny()) {
             mainData[[selected()]]$sprites$front_default
           } else {
             mainData[[selected()]]$sprites$front_shiny
           },
           socials = NULL,
           width = 12
         )
       ),
       tablerTagList(
         align = "center",
         tablerTag(name = details[[selected()]]$shape$name, rounded = TRUE, color = "default"),
         tablerTag(name = details[[selected()]]$habitat$name, rounded = TRUE, color = habitatColor)
       ),
       br()
     )
   })
}
