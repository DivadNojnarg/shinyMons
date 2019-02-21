#' UI module for generating the pokemon evolution timeline
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeEvolveUi <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_evolve"))
}




#' Server module generating the pokemon evolution timeline
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param mainData Object containing the main pokemon data.
#' @param details Object containing extra pokemon details.
#' @param selected Input containing the selected pokemon index.
#' @param shiny Whether to display a shiny version. FALSE by default.
#' @export
pokeEvolve <- function(input, output, session, mainData, details, selected, shiny) {

  # extract the data
  evolve_chain <- reactive({
    req(!is.null(selected()))
    fromJSON(details[[selected()]]$evolution_chain$url, flatten = TRUE)
  })

  # treat data and generate the timeline
  output$poke_evolve <- renderUI({

    req(!is.null(selected()))
    fromSpecie <- details[[selected()]]$evolves_from_species
    chain <- evolve_chain()$chain

    if (!is.null(fromSpecie)) {
      # handle a possible second evolution
      minLevelBis <- chain$evolves_to$evolves_to[[1]]$evolution_details[[1]]$min_level
      if (!is.null(minLevelBis)) {
        triggerBis <- chain$evolves_to$evolves_to[[1]]$evolution_details[[1]]$trigger.name
        evolutionBis <- chain$evolves_to$evolves_to[[1]]$species.name
        evolutionBis <- stringr::str_to_title(evolutionBis) # title case

        # handle the case of the last family member which cannot evolve in itself
        if (selected() != evolutionBis) {
          # take the sprite
          if (shiny()) {
            evolSpriteBis <- mainData[[evolutionBis]]$sprites$front_shiny
          } else {
            evolSpriteBis <- mainData[[evolutionBis]]$sprites$front_default
          }

          # the timeline
          tablerTimeline(
            tablerTimelineItem(
              title = paste0("Evolves to: ", evolutionBis),
              status = "green",
              date = paste0("At level: ", minLevelBis),
              img(src = evolSpriteBis),
              triggerBis
            )
          )
        } else {
          tablerAlert(
            title = "Alert",
            "This Pokemon cannot evolve.",
            icon = "alert-triangle",
            status = "danger"
          )
        }

      }
    } else {
      # continue only if at least 1 evolution is found
      if (length(chain$evolves_to) > 0) {
        evolution <- chain$evolves_to$species.name
        evolution <- stringr::str_to_title(evolution) # title case
        minLevel <- chain$evolves_to$evolution_details[[1]]$min_level
        trigger <- chain$evolves_to$evolution_details[[1]]$trigger.name

        # take the sprite
        if (shiny()) {
          evolSprite <- mainData[[evolution]]$sprites$front_shiny
        } else {
          evolSprite <- mainData[[evolution]]$sprites$front_default
        }

        # the timeline
        tablerTimeline(
          tablerTimelineItem(
            title = paste0("Evolves to: ", evolution),
            status = "green",
            date = paste0("At level: ", minLevel),
            img(src = evolSprite),
            trigger
          )
        )
      }
    }

  })

}
