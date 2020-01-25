#' UI module for sorting pokemon
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeInputUi <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("pokeChoice"))
}




#' Server module generating the pokemon interface
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param mainData Object containing the main pokemon data.
#' @param sprites Object containing pokemon images.
#' @param details Object containing extra pokemon details.
#' @param selected Object containing the selected pokemon in the network, if not NULL.
#'
#' @import shinyWidgets
#'
#' @export
pokeInput <- function(input, output, session, mainData, sprites, details, selected) {

  ns <- session$ns

  pokeNames <- names(mainData)

  # pokemon selector
  output$pokeChoice <- renderUI({
    fluidRow(
      align = "center",
      f7SmartSelect(
        inputId = ns("pokeSelect"),
        label = "Select a pokemon",
        choice = sprintf("<img src=\'%s\' width=20 style=\'vertical-align:top;\'></img> %s", sprites, pokeNames),
        selected = pokeNames[[1]],
        type = "sheet"
      ),
      # because it's a shiny app ;)
      tagAppendAttributes(
        prettySwitch(
          inputId = ns("pokeShiny"),
          label = "Shiny?",
          value = FALSE,
          status = "primary",
          slim = TRUE,
          width = NULL
        ),
        class = "m-2"
      )
    )
  })

  observe({
    req(!is.null(selected()))
    updatePickerInput(
      session,
      inputId = "pokeSelect",
      selected = pokeNames[selected()]
    )
  })

  return(
    list(
      pokeSelect = reactive(input$pokeSelect),
      pokeShiny = reactive(input$pokeShiny)
    )
  )

}
