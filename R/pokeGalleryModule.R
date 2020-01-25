#' UI module for generating the gallery of the first 151 pokemons
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeGalleryUi <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    f7Slider(
      inputId = ns("pokeRange"),
      label = h3("Selected Pokemons"),
      min = 1,
      max = 151,
      value = c(1, 151)
    ),
    uiOutput(ns("poke_gallery"))
  )
}



#' Server module generating the pokemon gallery interface
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param mainData Object containing the main pokemon data.
#' @param details Object containing extra pokemon details.
#' @param shiny Whether to display a shiny version. FALSE by default.
#'
#' @importFrom tablerDash tablerMediaCard
#'
#' @export
pokeGallery <- function(input, output, session, mainData, details, shiny) {

  ns <- session$ns
  range <- reactive(mainData[input$pokeRange[1]:input$pokeRange[2]])

  output$poke_gallery <- renderUI({
    req(!is.null(shiny()))
    lapply(seq_along(range()), FUN = function(i) {
      cardTag <- f7ExpandableCard(
        id = ns(paste0("pokegallery_", i)),
        title = range()[[i]]$name,
        fullBackground = TRUE,
        img = if (!shiny()) {
          range()[[i]]$sprites$front_default
        } else {
          range()[[i]]$sprites$front_shiny
        },
        subtitle = paste0("Pokemon: ", range()[[i]]$id)
      )
      cardTag
    })
  })
}
