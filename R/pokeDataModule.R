#' UI module for sorting pokemon
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeDataUi <- function(id) {

  ns <- shiny::NS(id)

  fluidRow(
    column(
      width = 12,
      align = "center",
      sliderInput(
        inputId = ns("pokeSelect"),
        label = h3("Selected Pokemons"),
        min = 1,
        max = 151,
        value = c(1, 9)
      )
    )
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
pokeData <- function(input, output, session, raw_data, raw_details) {

  # filter pokemon data according to the slider input
  pokemons <- reactive(raw_data[c(input$pokeSelect[1]:input$pokeSelect[2])])
  details <- reactive(raw_details[c(input$pokeSelect[1]:input$pokeSelect[2])])
  pokeNames <- reactive(names(pokemons()))

  # pokemon skills dataframe
  skills <- reactive({
    lapply(seq_along(pokeNames()), FUN = function(i) {
      data.frame(
        x = pokemons()[[i]]$stats$stat$name,
        y = pokemons()[[i]]$stats$base_stat
      )
    })
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
