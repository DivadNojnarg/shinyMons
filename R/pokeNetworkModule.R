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
    visNetworkOutput(ns("pokeNet"))
  )
}


#' Server module for generating the pokeNetwork section
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param attacks Data containing all pokemon abilities.
#' @export
pokeNetwork <- function(input, output, session, mainData) {

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

  pokeNames <- names(mainData)
  sprites <- vapply(seq_along(pokeNames), FUN = function(i) mainData[[i]]$sprites$front_default, FUN.VALUE = character(1))

  output$pokeNet <- renderVisNetwork({

    nodes <- data.frame(
      id = 1:length(mainData),
      shape = rep("image", length(mainData)),
      image = sprites,
      label = pokeNames,
      fixed = list("x" = FALSE, "y" = FALSE),
      size = rep(100, length(mainData)),
      #physics = rep(FALSE, 16)
      hidden = rep(FALSE, length(mainData)),
      stringsAsFactors = FALSE
    )
    edges <- NULL

    visNetwork(nodes, edges, width = "100%") %>%
      visEvents(selectNode = paste0("function(nodes) { Shiny.setInputValue('", ns("current_node_id"), "', nodes.nodes); }")) %>%
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
        autoResize = TRUE
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
      )
  })
}
