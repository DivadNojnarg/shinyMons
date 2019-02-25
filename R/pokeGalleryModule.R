#' UI module for generating the gallery of the first 151 pokemons
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeGalleryUi <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        align = "center",
        sliderInput(
          inputId = ns("pokeRange"),
          label = h3("Selected Pokemons"),
          min = 1,
          max = 151,
          value = c(1, 9)
        )
      )
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
#' @export
pokeGallery <- function(input, output, session, mainData, details, shiny) {

  range <- reactive(mainData[input$pokeRange[1]:input$pokeRange[2]])

  output$poke_gallery <- renderUI({
    fluidRow(
      lapply(seq_along(range()), FUN = function(i) {
        cardTag <- tablerMediaCard(
          title = range()[[i]]$name,
          date = NULL,
          href = NULL,
          src = if (!shiny()) {
            range()[[i]]$sprites$front_default
          } else {
            range()[[i]]$sprites$front_shiny
          },
          avatarUrl = if (!shiny()) {
            range()[[i]]$sprites$back_default
          } else {
            range()[[i]]$sprites$back_shiny
          },
          width = 4,
          paste0("Pokemon n°: ", range()[[i]]$id)
        )
        cardTag$children[[1]] <- tagAppendAttributes(cardTag$children[[1]], class = "galleryCard")
        cardTag
      })
    )
  })
}
