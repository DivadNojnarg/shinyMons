#' UI module for sorting pokemon
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeDataUi <- function(id) {

  ns <- shiny::NS(id)

  #fluidRow(
  #  column(
  #    width = 12,
  #    align = "center",
  #    sliderInput(
  #      inputId = ns("pokeSelect"),
  #      label = h3("Selected Pokemons"),
  #      min = 1,
  #      max = 151,
  #      value = c(1, 9)
  #    )
  #  )
  #)

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

  # pokemon skills dataframe
  skills <- reactive({

    req(input$pokeSelect)

    data.frame(
      x = pokemons[[input$pokeSelect]]$stats$stat$name,
      y = pokemons[[input$pokeSelect]]$stats$base_stat
    )
  })


  # pokemon selector
  output$pokeChoice <- renderUI({
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
    )
  })

  return(
    list(
      pokemons = pokemons,
      details = details,
      pokeNames = pokeNames,
      skills = skills,
      pokeSelect = reactive(input$pokeSelect)
    )
  )

}
