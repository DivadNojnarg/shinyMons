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
#' @param evolutions Preprocessed evolution data.
#' @param types Object containing the preprocessed pokemon types.
#'
#' @import tablerDash
#'
#' @export
pokeInfos <- function(input, output, session, mainData, details, selected, shiny,
                      evolutions, types) {

  ns <- session$ns

  # evolutions module
  callModule(
    module = pokeEvolve,
    id = "evol",
    mainData = mainData,
    details = details,
    selected = selected,
    shiny = shiny,
    evolutions = evolutions
  )

  # types modules
  callModule(
    module = pokeType,
    id = "types",
    types = types,
    selected = selected
  )

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
    habitatColor <- switch (
      details[[selected()]]$habitat$name,
      "grassland" = "lime",
      "mountain" = "orange",
      "waters-edge" = "azure",
      "forest" = "green",
      "rough-terrain" = "yellow",
      "cave" = "black",
      "urban" = "gray",
      "sea" = "blue",
      "rare" = "purple"
    )

    f7SocialCard(
      # sprite
      author_img = if (!shiny()) {
        sprites[[selected()]]
      } else {
        mainData[[selected()]]$sprites$front_shiny
      },
      author = selected(),
      pokeEvolveUi(id = ns("evol")),
      hr(),
      pokeTypeUi(id = ns("types")),
      hr(),
      # text content
      details[[selected()]]$flavor_text_entries$flavor_text[54],
      br(),
      br(),
      # habitat details
      f7Flex(
        f7Chip(label = details[[selected()]]$shape$name, status = NULL),
        f7Chip(label = details[[selected()]]$habitat$name, status = habitatColor)
      )
    )
  })
}
