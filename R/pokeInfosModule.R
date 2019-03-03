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
#'
#' @import tablerDash
#'
#' @export
pokeInfos <- function(input, output, session, mainData, details, selected, shiny) {

  #generate the profile cards (as many as the number of selected pokemons)
   output$poke_infos <- renderUI({

     # there is a quirk in the raw data: in details, names are in lower case
     # whereas in the main pokemon list, names start with a capital letter...

     pokeNames <- names(mainData)
     sprites <- vapply(seq_along(pokeNames), FUN = function(i) {
       paste0("http://www.pokestadium.com/sprites/xy/", mainData[[i]]$name, ".gif")
     }, FUN.VALUE = character(1))
     names(sprites) <- pokeNames

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
           subtitle = tagList(
             align = "center",
             details[[selected()]]$flavor_text_entries$flavor_text[54],
             tablerTagList(
               tablerTag(name = details[[selected()]]$shape$name, rounded = TRUE, color = "default"),
               tablerTag(name = details[[selected()]]$habitat$name, rounded = TRUE, color = habitatColor)
             )
           ),
           background = "https://pngimage.net/wp-content/uploads/2018/06/pokemon-background-png.png",
           src = if (!shiny()) {
             sprites[[selected()]]
           } else {
             mainData[[selected()]]$sprites$front_shiny
           },
           socials = tablerSocialLinks(
             tablerSocialLink(
               name = "pokeApi",
               href = paste0("https://pokeapi.co/api/v2/pokemon/", tolower(selected())),
               icon = "at"
             ),
             tablerSocialLink(
               name = "Bulbapedia",
               href = paste0("https://bulbapedia.bulbagarden.net/wiki/", selected(), "_(Pok\u00e9mon)"),
               icon = "address-card"
             )
           ),
           width = 12
         )
       ),
       br()
     )
   })
}
