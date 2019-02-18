#' UI module for generating the pokemon stats chart
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeStatsUi <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_stats"))
}

#' Server module generating the pokemon stats chart
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param skills Object containing pokemon statistics.
#' @param pokeNames Object containing pokemon names.
#' @export
pokeStats <- function(input, output, session, skills, pokeNames) {

  ns <- session$ns

  # generate radar chart for pokemons
  lapply(seq_along(pokeNames()), FUN = function(i) {
    output[[paste0(pokeNames()[[i]], "_stats")]] <- renderEcharts4r({
      skills()[[i]] %>%
        e_charts(x) %>%
        e_radar(y, name = paste0(pokeNames()[[i]], " Stats")) %>%
        e_tooltip(trigger = "item")
    })
  })



  statCards <- reactive({
    lapply(seq_along(pokeNames()), FUN = function(i) {
      tablerCard(
        title = paste0(pokeNames()[[i]], " Stats"),
        options = NULL,
        footer = NULL,
        status = "info",
        statusSide = "left",
        collapsible = TRUE,
        collapsed = FALSE,
        closable = TRUE,
        zoomable = TRUE,
        width = 6,
        overflow = FALSE,
        echarts4rOutput(outputId = ns(paste0(pokeNames()[[i]], "_stats")))
      )
    })
  })

  output$poke_stats <- renderUI(statCards())

}
