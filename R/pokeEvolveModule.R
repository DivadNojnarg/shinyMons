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
#' @param evolutions Preprocessed pokemon evolutions data.
#'
#' @import tablerDash
#'
#' @export
pokeEvolve <- function(input, output, session, mainData, details, selected, shiny, evolutions) {

  # extract the data
  evolve_chain <- reactive({
    req(!is.null(selected()))
    evolutions[[selected()]]
  })

  # treat data and generate the timeline
  output$poke_evolve <- renderUI({

    req(!is.null(selected()))
    fromSpecie <- details[[selected()]]$evolves_from_species
    chain <- evolve_chain()

    if (!is.null(fromSpecie)) {
      # handle a possible second evolution
      minLevelBis <- chain$evolves_to$evolves_to[[1]]$evolution_details[[1]]$min_level
      if (!is.null(minLevelBis)) {
        triggerBis <- chain$evolves_to$evolves_to[[1]]$evolution_details[[1]]$trigger.name
        evolutionBis <- chain$evolves_to$evolves_to[[1]]$species.name[[1]]
        evolutionBis <- stringr::str_to_title(evolutionBis) # title case

        # handle the case of the last family member which cannot evolve in itself
        if (selected() != evolutionBis) {
          # take the sprite
          if (shiny()) {
            evolSpriteBis <- mainData[[evolutionBis]]$sprites$front_shiny
          } else {
            evolSpriteBis <- mainData[[evolutionBis]]$sprites$front_default
          }

          # recover item
          if (triggerBis != "level-up") {
            if (triggerBis == "use-item") {
              triggerBisUrl <- chain$evolves_to$evolves_to[[1]]$evolution_details[[1]]$item.url
              # fromJSON could be a bottlneck
              triggerBisImage <- jsonlite::fromJSON(triggerBisUrl)$sprites$default
              triggerBisName <- chain$evolves_to$evolves_to[[1]]$evolution_details[[1]]$item.name
            } else {
              triggerBisUrl <- chain$evolves_to$evolves_to[[1]]$evolution_details[[1]]$trigger.url
              # fromJSON could be a bottlneck
              triggerBisImage <- jsonlite::fromJSON(triggerBisUrl)$sprites$default
              triggerBisName <- chain$evolves_to$evolves_to[[1]]$evolution_details[[1]]$trigger.name
            }
          }

          # handle the case where the evolution might not be in the first gen
          if (evolutionBis %in% names(mainData)) {
            tablerTimelineItem(
              title = paste0("Evolves to: ", evolutionBis),
              status = "green",
              date = if (triggerBis == "level-up") paste0("At level: ", minLevelBis),
              img(src = evolSpriteBis),
              if (triggerBis == "level-up") triggerBis else tagList(triggerBisName, img(src = triggerBisImage))
            )
          } else {
            tablerAlert(
              title = "Alert",
              "This Pokemon can evolve but not in the first generation.",
              icon = "alert-triangle",
              status = "danger"
            )
          }
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
        # handle Eevee that has 3 base evolutions instead of 1
        if (selected() == "Eevee") {
          evolutions <- chain$evolves_to$species.name[c(1:3)]
          evolutions <- stringr::str_to_title(evolutions) # title case
          # the timeline
          tablerTimeline(
            lapply(seq_along(evolutions), function(i) {
              # take the sprite
              if (shiny()) {
                evolSprite <- mainData[[evolutions[[i]]]]$sprites$front_shiny
              } else {
                evolSprite <- mainData[[evolutions[[i]]]]$sprites$front_default
              }
              trigger <- chain$evolves_to$evolution_details[[i]]$trigger.name
              minLevel <- chain$evolves_to$evolution_details[[i]]$min_level

              # recover item
              if (trigger != "level-up") {
                if (trigger == "use-item") {
                  triggerUrl <- chain$evolves_to$evolution_details[[i]]$item.url
                  # fromJSON could be a bottlneck here (however this only applies for Eevee)
                  triggerImage <- jsonlite::fromJSON(triggerUrl)$sprites$default
                  triggerName <- chain$evolves_to$evolution_details[[i]]$item.name
                } else {
                  triggerUrl <- chain$evolves_to$evolution_details[[i]]$trigger.url
                  # fromJSON could be a bottlneck here (however this only applies for Eevee)
                  triggerImage <- jsonlite::fromJSON(triggerUrl)$sprites$default
                  triggerName <- chain$evolves_to$evolution_details[[i]]$trigger.name
                }
              }

              tablerTimelineItem(
                title = paste0("Evolves to: ", evolutions[[i]]),
                status = "green",
                date = if (trigger == "level-up") paste0("At level: ", minLevel),
                img(src = evolSprite),
                if (trigger == "level-up") trigger else tagList(triggerName, img(src = triggerImage))
              )

            })
          )
        } else {
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

          # recover item
          if (trigger != "level-up") {
            if (trigger == "use-item") {
              triggerUrl <- chain$evolves_to$evolution_details[[1]]$item.url
              # fromJSON could be a bottlneck here (however this only applies for Eevee)
              triggerImage <- jsonlite::fromJSON(triggerUrl)$sprites$default
              triggerName <- chain$evolves_to$evolution_details[[1]]$item.name
            } else {
              triggerUrl <- chain$evolves_to$evolution_details[[1]]$trigger.url
              # fromJSON could be a bottlneck here (however this only applies for Eevee)
              triggerImage <- jsonlite::fromJSON(triggerUrl)$sprites$default
              triggerName <- chain$evolves_to$evolution_details[[1]]$trigger.name
            }
          }

          if (evolution %in% names(mainData)) {
            tablerTimelineItem(
              title = paste0("Evolves to: ", evolution),
              status = "green",
              date = if (trigger == "level-up") paste0("At level: ", minLevel),
              img(src = evolSprite),
              if (trigger == "level-up") trigger else tagList(triggerName, img(src = triggerImage))
            )
            # handle the case where the evolution is not in the first generation
          } else {
            tablerAlert(
              title = "Alert",
              "This Pokemon can evolve but not in the first generation.",
              icon = "alert-triangle",
              status = "danger"
            )
          }

        }
      }
    }

  })

}
