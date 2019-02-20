#' UI module for generating the pokemon stats chart
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeStatsUi <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("pokeStatsCard"))
}

# make R CMD check happy
globalVariables("x")
globalVariables("y")

#' Server module generating the pokemon stats chart
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param skills Object containing pokemon statistics.
#' @param pokeNames Object containing pokemon names.
#' @param selected Input containing the selected pokemon index.
#' @export
pokeStats <- function(input, output, session, skills, pokeNames, selected) {

  ns <- session$ns

  # generate radar chart for pokemons
  output$pokeStats <- renderEcharts4r({

    req(skills())

    skills() %>%
      e_charts(x) %>%
      e_radar(y, name = paste0(selected(), " Stats")) %>%
      e_tooltip(trigger = "item")
  })

  output$pokeStatsCard <- renderUI({
    fluidRow(
      tablerCard(
        title = paste0(selected(), " Stats"),
        options = NULL,
        footer = NULL,
        status = "info",
        statusSide = "left",
        collapsible = FALSE,
        closable = FALSE,
        zoomable = FALSE,
        width = 12,
        overflow = FALSE,
        echarts4rOutput(outputId = ns("pokeStats"))
      )
    )
  })

}
