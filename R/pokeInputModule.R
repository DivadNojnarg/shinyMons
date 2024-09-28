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
#' @param currentTab Currently selected tab (input$tabset).
#'
#' @importFrom shinyjs hide
#'
#' @export
pokeInput <- function(input, output, session, mainData, sprites, details, selected, currentTab) {

  ns <- session$ns

  pokeNames <- names(mainData)

  # pokemon selector
  output$pokeChoice <- renderUI({
    f7Flex(
      align = "center",
      f7AutoComplete(
        inputId = ns("pokeSelect"),
        label = "Select a pokemon",
        choices = pokeNames,#sprintf("<img src=\'%s\' width=20 style=\'vertical-align:top;\'></img> %s", sprites, pokeNames),
        openIn = "dropdown"
      ),
      # because it's a shiny app ;)
      f7Toggle(
        inputId = ns("pokeShiny"),
        label = "Shiny?",
        checked = FALSE,
        color = "blue"
      )
    )
  })

  observeEvent(currentTab(), {
    if (currentTab() != "infos") {
      hide(id = "subnavbar")
    }
  })

  observe({
    req(!is.null(selected()))
    updateF7AutoComplete(
      session,
      inputId = "pokeSelect",
      value = pokeNames[selected()]
    )
  })

  return(
    list(
      # tranform selected since it contains an image tag
      pokeSelect = reactive(input$pokeSelect),
      pokeShiny = reactive(input$pokeShiny)
    )
  )

}
