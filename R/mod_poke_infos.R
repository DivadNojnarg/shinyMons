#' UI module for generating the pokemon info cards
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
poke_infos_ui <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_infos"))
}

#' Server module generating the pokemon info cards
#'
#' @param id Module id.
#' @param selected Input containing the selected pokemon index.
#' @param shiny Whether to display a shiny version. FALSE by default.
#'
#' @import tablerDash
#'
#' @export
poke_infos_server <- function(id, selected, shiny) {
  moduleServer(
    id,
    function(input, output, session) {
      # generate the profile cards (as many as the number of selected pokemons)
      output$poke_infos <- renderUI({
        # there is a quirk in the raw data: in details, names are in lower case
        # whereas in the main pokemon list, names start with a capital letter...
        req(!is.null(selected()))

        habitat <- poke_data[[selected()]]$habitat 

        tablerProfileCard(
          title = selected(),
          subtitle = tagList(
            poke_data[[selected()]]$description,
            tablerTagList(
              align = "center",
              tablerTag(name = poke_data[[selected()]]$shape, rounded = TRUE, color = "default"),
              tablerTag(name = habitat, rounded = TRUE, color = get_habitat_color(habitat))
            )
          ),
          background = get_habitat_landscape(habitat),
          src = get_front_sprites(shiny())[[selected()]],
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
      })
    }
  )
}
