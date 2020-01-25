#' UI module for generating the pokeNetwork section
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeNetworkUi <- function(id) {
  ns <- shiny::NS(id)
  visNetworkOutput(ns("pokeNet"), height = "900px")
}


#' Server module for generating the pokeNetwork section
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param mainData All pokemon main data.
#' @param details Object containing extra pokemon details.
#' @param families List containg all pokemon connections.
#' @param groups List containing data for grouping pokemons by evolution family.
#' @param mobile Shiny input checking if the app is running on a cellphone/tablet.
#' @param networkOptions Slot for the pokeNetworkInputModule inputs.
#'
#' @import visNetwork
#'
#' @export
pokeNetwork <- function(input, output, session, mainData, details, families, groups, mobile, networkOptions) {

  ns <- session$ns
  #-------------------------------------------------------------------------
  # Network: nodes, edges, events, ...
  #-------------------------------------------------------------------------
  nodes <- reactive({

    req(!is.null(networkOptions$pokeNodesShape()), !is.null(networkOptions$pokeNodesSize()))

    df <- data.frame(
      id = 1:length(mainData),
      group = groups,
      shape = networkOptions$pokeNodesShape(),
      label = pokeNames,
      #fixed = list("x" = FALSE, "y" = FALSE),
      size = networkOptions$pokeNodesSize(),
      physics = TRUE,
      hidden = rep(FALSE, length(mainData)),
      stringsAsFactors = FALSE
    )

    if (networkOptions$pokeNodesShape() == "image") df$image <-  sprites

    return(df)
  })

  edges <- reactive({

    req(!is.null(networkOptions$pokeEdgesWidth()), !is.null(networkOptions$displayEdges()))

    if (networkOptions$displayEdges()) {
      data.frame(
        width = networkOptions$pokeEdgesWidth(),
        color = list(color = c(rep("black", length(families$from))), highlight = "blue"),
        dashes = TRUE,
        smooth = FALSE,
        hidden = FALSE,
        from = families$from,
        to = families$to,
        stringsAsFactors = FALSE
      )
    }
  })

  pokeNames <- names(mainData)
  sprites <- vapply(seq_along(pokeNames), FUN = function(i) mainData[[i]]$sprites$front_default, FUN.VALUE = character(1))

  # below is a test to see if gif are supported (lag)
  #sprites <- vapply(seq_along(pokeNames), FUN = function(i) {
  #  paste0("http://www.pokestadium.com/sprites/xy/", mainData[[i]]$name, ".gif")
  #}, FUN.VALUE = character(1))


  output$pokeNet <- renderVisNetwork({

    req(!is.null(networkOptions$nodesInterp()),
        !is.null(networkOptions$pokeNodesDrag()),
        !is.null(networkOptions$dragView()),
        !is.null(networkOptions$zoomView()),
        !is.null(networkOptions$nodeDistance()),
        !is.null(networkOptions$centralGravity()),
        !is.null(networkOptions$springLength())
    )

    visNetwork(nodes(), edges(), width = "100%") %>%
      visEvents(selectNode = paste0("function(nodes) { Shiny.setInputValue('", ns("current_node_id"), "', nodes.nodes); }")) %>%
      # add the doubleclick for nodes (zoom view)
      visNetwork::visEvents(doubleClick = paste0("function(nodes) { Shiny.setInputValue('", ns("current_node_id_zoom"), "', nodes.nodes); }")) %>%
      visEvents(deselectNode = paste0(
        "function(nodes) {
          Shiny.setInputValue('", ns("current_node_id"), "', 'null');
          //Shiny.setInputValue('", ns("current_node_id_zoom"), "', 'null');
         }
        "
      )
      ) %>%
      visNodes(
        shapeProperties =
          list(
            useBorderWithImage = FALSE,
            interpolation = networkOptions$nodesInterp() # time consumming
          )
      ) %>%
      visEdges(arrows = "to") %>%
      visOptions(
        highlightNearest = FALSE,
        clickToUse = FALSE,
        manipulation = FALSE, # to manually add nodes and edges. Could be interesting ...
        collapse = list(enabled = FALSE, clusterOptions = list(shape = "square")),
        autoResize = TRUE,
        nodesIdSelection = FALSE,
        selectedBy = "group"
      ) %>%
      visInteraction(
        hover = TRUE,
        hoverConnectedEdges = FALSE,
        selectConnectedEdges = FALSE,
        multiselect = FALSE,
        dragNodes = networkOptions$pokeNodesDrag(),
        dragView = networkOptions$dragView(),
        zoomView = networkOptions$zoomView(),
        navigationButtons = FALSE,
        selectable = TRUE
      ) %>%
      visPhysics(
        stabilization = TRUE,
        solver = "repulsion",
        repulsion = list(
          nodeDistance = networkOptions$nodeDistance(),
          centralGravity = networkOptions$centralGravity(),
          springLength = networkOptions$springLength()
        ),
        enabled = TRUE
      )
  })


  # increase the current node size on selection
  observeEvent(input$current_node_id, {

    selected_node <- input$current_node_id
    nodes <- nodes()

    # javascript returns null and not NULL like R
    if (!identical(selected_node, "null")) {
      nodes$size[selected_node] <- nodes$size[1] * 5
      #nodes$hidden[-selected_node] <- rep(TRUE, length(nodes()$hidden) - 1)
      visNetworkProxy(ns("pokeNet"), session) %>%  # then reset the graph
        visUpdateNodes(nodes = nodes)
    } else {
      nodes$size <- networkOptions$pokeNodesSize()
      #nodes$hidden <- rep(FALSE, length(nodes$hidden))
      visNetworkProxy(ns("pokeNet"), session) %>%  # then reset the graph
        visUpdateNodes(nodes = nodes)
    }
  })


  # double click on node

  observeEvent(input$current_node_id_zoom, {

    selected <- input$current_node_id_zoom

    names <- data.frame(
      languages = details[[selected]]$names$language$name,
      name = details[[selected]]$names$name
    )

    #showModal(
    #  modalDialog(
    #    title = fluidRow(
    #      column(
    #        width = 2,
    #        align = "left",
    #        tablerAvatar(url = sprites[[selected]])
    #      ),
    #      column(
    #        width = 8,
    #        align = "center",
    #        paste0(names(mainData)[[selected]], " 's names")
    #      ),
    #      column(
    #        width = 2,
    #        align = "right",
    #        HTML('<a href="#" data-dismiss="modal" class="btn btn-outline-primary">Close</a>')
    #      )
    #    ),
    #    tablerTable(
    #      lapply(seq_along(names$languages), function(i) {
    #        tablerTableItem(
    #          left = names$languages[[i]],
    #          right = names$name[[i]]
    #        )
    #      }),
    #      stacked = FALSE
    #    ),
    #    easyClose = TRUE,
    #    footer = NULL
    #  )
    #)
  })

  return(list(selected = reactive(input$current_node_id_zoom)))

}
