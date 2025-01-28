#' UI module for generating the pokemon evolution timeline
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
#' @rdname poke_evolve
poke_evol_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h1("Evolution family"),
    visNetworkOutput(ns("poke_evolve_network"))
  )
}

#' Server module generating the pokemon evolution timeline
#'
#' @param selected Input containing the selected pokemon index.
#' @param shiny Whether to display a shiny version. FALSE by default.
#'
#' @export
#' @rdname poke_evolve
poke_evol_server <- function(id, selected, shiny) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      # treat data and generate the timeline

      evol_chain <- reactive({
        req(selected())
        poke_data[[selected()]]$evolutions[[1]]
      })

      output$poke_evolve_network <- renderVisNetwork({
        filtered_nodes <- poke_network$poke_nodes[
          poke_network$poke_nodes$id %in% evol_chain()$id,
          colnames(poke_network$poke_nodes) != "value"
        ]

        filtered_edges <- poke_network$poke_edges[
          poke_network$poke_edges$from %in% evol_chain()$id,
        ]

        visNetwork(
          nodes = cbind(
            size = 80,
            filtered_nodes
          ),
          edges = cbind(
            filtered_edges,
            arrows = "middle",
            color = "#494f56"
          ),
        ) |>
          visOptions(nodesIdSelection = TRUE) |>
          visEdges(length = 300) |>
          visInteraction(dragView = FALSE, zoomView = FALSE) |>
          visLayout(randomSeed = 123)
      })

      return(
        list(
          selected = reactive({
            req(nchar(input$poke_evolve_network_selected) > 0)
            input$poke_evolve_network_selected
          })
        )
      )
    }
  )
}
