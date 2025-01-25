library(shiny)
library(shinyjs)
library(tablerDash)
library(shinyWidgets)
library(shinyEffects)
library(pushbar)
library(shinyMons)
library(waiter)

tabler_deps <- htmltools::findDependencies(tablerDashPage())

ui <- function(request) {
  tagList(
    div(
      class = "container-fluid my-5",
      poke_select_ui("select"),
      fluidRow(
        class = "my-5",
        column(
          width = 3,
          poke_infos_ui("infos"),
          poke_stats_ui("stats")[[1]],
          poke_locations_ui("location")
        ),
        column(
          width = 5,
          poke_stats_ui("stats")[c(2, 3)],
          poke_evol_ui("evol"),
        ),
        column(
          width = 4,
          poke_types_ui("types")
        )
      ),
      poke_moves_ui("moves")
    ),
    tabler_deps
  )
}

server <- function(input, output, session) {
  rv <- reactiveValues(network_selected = NULL)
  # main module (data)
  main <- poke_select_server(
    "select",
    selected = reactive(rv$network_selected)
  )

  # infos module
  poke_infos_server(
    "infos",
    selected = main$poke_select,
    shiny = main$is_shiny
  )
  # stats module
  poke_stats_server("stats", selected = main$poke_select)
  # types modules
  poke_types_server("types", selected = main$poke_select)
  # moves module
  poke_moves_server("moves", selected = main$poke_select)
  # location
  poke_locations_server("location", selected = main$poke_select)

  # evolutions module
  evol_out <- poke_evol_server(
    "evol",
    selected = main$poke_select,
    shiny = main$is_shiny
  )
  observeEvent(evol_out$selected(), {
    rv$network_selected <- as.numeric(evol_out$selected())
  })
}

shinyApp(ui, server)
