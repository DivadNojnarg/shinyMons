#' UI module for generating the pokeNetwork section
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeNetworkUi <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("open"), "Network Options"),
    pushbar(
      #style = "padding:20px;",
      from = "bottom",
      id = ns("myPushbar"),

      # content
      h4("Hi")
    ),
    visNetworkOutput(ns("pokeNet"), height = "900px")
  )
}


#' Server module for generating the pokeNetwork section
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param mainData All pokemon main data.
#' @param families List containg all pokemon connections.
#' @export
pokeNetwork <- function(input, output, session, mainData, families) {

  ns <- session$ns

  #-------------------------------------------------------------------------
  # Pushbar setup, events, ...
  #-------------------------------------------------------------------------
  setup_pushbar(session, blur = TRUE) # setup

  observeEvent(input$open, {
    pushbar_open(session, id = ns("myPushbar"))
  })

  #-------------------------------------------------------------------------
  # Network: nodes, edges, events, ...
  #-------------------------------------------------------------------------

  nodes <- reactive({
    data.frame(
      id = 1:length(mainData),
      shape = rep("image", length(mainData)),
      image = sprites,
      label = pokeNames,
      fixed = list("x" = FALSE, "y" = FALSE),
      size = rep(100, length(mainData)),
      physics = rep(TRUE, length(mainData)),
      hidden = rep(FALSE, length(mainData)),
      stringsAsFactors = FALSE
    )
  })

  edges <- reactive({
    data.frame(
      width = 10,
      color = list(color = c(rep("black", length(families$from))), highlight = "yellow"),
      dashes = TRUE,
      smooth = TRUE,
      hidden = FALSE,
      from = families$from,
      to = families$to,
      stringsAsFactors = FALSE
    )
  })

  pokeNames <- names(mainData)
  sprites <- vapply(seq_along(pokeNames), FUN = function(i) mainData[[i]]$sprites$front_default, FUN.VALUE = character(1))

  output$pokeNet <- renderVisNetwork({

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
            interpolation = FALSE
          )
      ) %>%
      visOptions(
        highlightNearest = FALSE,
        clickToUse = FALSE,
        manipulation = FALSE, # to manually add nodes and edges. Could be interesting ...
        collapse = FALSE,
        autoResize = TRUE,
        nodesIdSelection = TRUE
      ) %>%
      visInteraction(
        hover = TRUE,
        hoverConnectedEdges = FALSE,
        selectConnectedEdges = FALSE,
        multiselect = FALSE,
        dragNodes = TRUE,
        dragView = TRUE,
        zoomView = TRUE,
        navigationButtons = FALSE,
        selectable = TRUE
      ) %>%
      visPhysics(stabilization = TRUE, enabled = TRUE)
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
      nodes$size <- 100
      #nodes$hidden <- rep(FALSE, length(nodes$hidden))
      visNetworkProxy(ns("pokeNet"), session) %>%  # then reset the graph
        visUpdateNodes(nodes = nodes)
    }
  })
}
