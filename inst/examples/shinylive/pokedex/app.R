webr::install(
  "shinyMons",
  repos = c(
    "https://rinterface.github.io/rinterface-wasm-cran/",
    "https://repo.r-wasm.org"
  )
)

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
      div(
        class = "row my-5",
        div(
          class = "col-lg-3 col-md-6",
          poke_infos_ui("infos"),
          poke_stats_ui("stats")[[1]],
          poke_locations_ui("location")
        ),
        div(
          class = "col-lg-5 col-md-6",
          poke_stats_ui("stats")[c(2, 3)],
          poke_evol_ui("evol"),
        ),
        div(
          class = "col-lg-4 col-md-12",
          poke_types_ui("types")
        )
      ),
      poke_moves_ui("moves")
    ),
    useWaiter(),
    waiterShowOnLoad(html = tagList(spin_fading_circles(), "Loading ...")),
    tabler_deps
  )
}

server <- function(input, output, session) {
  waiter_hide()
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
