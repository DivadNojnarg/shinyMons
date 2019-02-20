#' UI module for sorting pokemon
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
    fluidRow(uiOutput(ns("poke_gallery")))
  )
}



#' Server module generating the pokemon interface
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param raw_data Object containing the main pokemon data.
#' @param raw_details Object containing extra pokemon details.
#' @export
pokeGallery <- function(input, output, session, raw_data, raw_details) {

  range <- reactive(raw_data[input$pokeRange[1]:input$pokeRange[2]])

  output$poke_gallery <- renderUI({
    lapply(seq_along(range()), FUN = function(i) {
      column(
        width = 4,
        tablerMediaCard(
          title = range()[[i]]$name,
          date = NULL,
          href = "https://www.google.com",
          src = range()[[i]]$sprites$front_shiny,
          avatarUrl = range()[[i]]$sprites$back_shiny,
          width = 12,
          "Other elements"
        )
      )
    })
  })
}
