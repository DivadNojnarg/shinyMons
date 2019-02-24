#' UI module for sorting pokemon
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeDataUi <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("pokeChoice"))
}




#' Server module generating the pokemon interface
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param raw_data Object containing the main pokemon data.
#' @param raw_details Object containing extra pokemon details.
#' @export
pokeData <- function(input, output, session, raw_data, raw_details) {

  ns <- session$ns

  # filter pokemon data according to the slider input
  pokemons <- raw_data
  details <- raw_details
  pokeNames <- names(pokemons)
  sprites <- vapply(seq_along(pokeNames), FUN = function(i) pokemons[[i]]$sprites$front_default, FUN.VALUE = character(1))

  # pokemon selector
  output$pokeChoice <- renderUI({
    fluidRow(
      pickerInput(
        inputId = ns("pokeSelect"),
        width = NULL,
        options = list(style = "btn-primary"),
        multiple = FALSE,
        choices = pokeNames,
        choicesOpt = list(
          content = sprintf("<img src=\'%s\' width=20 style=\'vertical-align:top;\'></img> %s", sprites, pokeNames)
        ),
        selected = pokeNames[[1]]
      ),
      # because it's a shiny app ;)
      tagAppendAttributes(
        prettySwitch(
          inputId = ns("pokeShiny"),
          label = "Shiny?",
          value = FALSE,
          status = "primary",
          slim = TRUE
        ),
        class = "m-2"
      )
    )
  })

  return(
    list(
      pokemons = pokemons,
      details = details,
      pokeNames = pokeNames,
      sprites = sprites,
      pokeSelect = reactive(input$pokeSelect),
      pokeShiny = reactive(input$pokeShiny)
    )
  )

}
