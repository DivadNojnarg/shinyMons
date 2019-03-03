#' UI module for generating the pokeNetwork section
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeNetworkUi <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      column(
        width = 1,
        align = "left",
        actionButton(ns("open"), "Network Options")
      ),
      column(
        width = 11,
        align = "center",
        visNetworkOutput(ns("pokeNet"), height = "900px")
      )
    ),
    pushbar(
      from = "bottom",
      id = ns("myPushbar"),
      # content
      uiOutput(ns("pushbarContent"))
    )
  )
}



pushBarContent <- NULL


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
#'
#' @import shinyWidgets visNetwork pushbar tablerDash
#'
#' @export
pokeNetwork <- function(input, output, session, mainData, details, families, groups, mobile) {

  ns <- session$ns

  #-------------------------------------------------------------------------
  # Pushbar setup, events, ...
  #-------------------------------------------------------------------------
  setup_pushbar(blur = TRUE, overlay = TRUE) # setup

  # the pushbar orientation depends if we are on mobile or not...
  output$pushbarContent <- renderUI({
    req(!is.null(mobile()))

    content_desktop <- fluidRow(
      column(
        width = 3,
        align = "center",
        # nodes shape
        shinyWidgets::prettyRadioButtons(
          inputId = ns("pokeNodesShape"),
          label = "Nodes shape:",
          thick = TRUE,
          inline = TRUE,
          selected = "image",
          choices = c("Circles" = "circle", "Sprites" = "image"),
          animation = "pulse",
          status = "info"
        ),
        # can we drag nodes?
        shinyWidgets::prettySwitch(
          inputId = ns("pokeNodesDrag"),
          label = "Drag nodes?",
          value = TRUE,
          status = "default",
          slim = FALSE,
          fill = TRUE,
          bigger = TRUE,
          inline = FALSE,
          width = NULL
        )
      ),
      column(
        width = 3,
        align = "center",
        # nodes size
        shiny::numericInput(
          inputId = ns("pokeNodesSize"),
          label = "Size of nodes:",
          value = 200,
          min = 100,
          max = NA,
          step = 10,
          width = NULL
        ),
        # edges width
        shiny::numericInput(
          inputId = ns("pokeEdgesWidth"),
          label = "Width of edges:",
          value = 10,
          min = 5,
          max = NA,
          step = 1,
          width = NULL
        )
      ),
      column(
        width = 3,
        align = "center",
        prettyToggle(
          inputId = ns("dragView"),
          label_on = "DragView on",
          label_off = "DragView off",
          value = TRUE,
          status_on = "success",
          status_off = "danger",
          shape = "curve",
          outline = TRUE,
          animation = "pulse"
        ),
        prettyToggle(
          inputId = ns("zoomView"),
          label_on = "ZoomView on",
          label_off = "ZoomView off",
          value = TRUE,
          status_on = "success",
          status_off = "danger",
          shape = "curve",
          outline = TRUE,
          animation = "pulse"
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("nodesInterp"),
          label = "Nodes interpolation?",
          value = FALSE,
          status = "primary",
          slim = TRUE,
          fill = FALSE,
          bigger = TRUE,
          inline = FALSE
        )
      ),
      column(
        width = 3,
        align = "center",
        sliderInput(
          inputId = ns("nodeDistance"),
          label = "Distance between nodes:",
          min = 50,
          value = 500,
          max = 500
        ),
        sliderInput(
          inputId = ns("centralGravity"),
          label = "Central gravity:",
          min = 0,
          value = 0,
          max = 1
        ),
        sliderInput(
          inputId = ns("springLength"),
          label = "Spring lenght:",
          min = 50,
          value = 200,
          max = 600
        )
      )
    )

    content_mobile <- tagList(
      fluidRow(
        align = "center",
        # nodes shape
        shinyWidgets::prettyRadioButtons(
          inputId = ns("pokeNodesShape"),
          label = "Nodes shape:",
          thick = TRUE,
          inline = TRUE,
          selected = "image",
          choices = c("Circles" = "circle", "Sprites" = "image"),
          animation = "pulse",
          status = "info"
        ),
        # can we drag nodes?
        shinyWidgets::prettySwitch(
          inputId = ns("pokeNodesDrag"),
          label = "Drag nodes?",
          value = TRUE,
          status = "default",
          slim = FALSE,
          fill = TRUE,
          bigger = TRUE,
          inline = FALSE,
          width = NULL
        )
      ),
      # nodes size
      shiny::numericInput(
        inputId = ns("pokeNodesSize"),
        label = "Size of nodes:",
        value = 200,
        min = 100,
        max = NA,
        step = 10,
        width = NULL
      ),
      # edges width
      shiny::numericInput(
        inputId = ns("pokeEdgesWidth"),
        label = "Width of edges:",
        value = 10,
        min = 5,
        max = NA,
        step = 1,
        width = NULL
      ),
      prettyToggle(
        inputId = ns("dragView"),
        label_on = "DragView on",
        label_off = "DragView off",
        value = TRUE,
        status_on = "success",
        status_off = "danger",
        shape = "curve",
        outline = TRUE,
        animation = "pulse"
      ),
      prettyToggle(
        inputId = ns("zoomView"),
        label_on = "ZoomView on",
        label_off = "ZoomView off",
        value = TRUE,
        status_on = "success",
        status_off = "danger",
        shape = "curve",
        outline = TRUE,
        animation = "pulse"
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("nodesInterp"),
        label = "Nodes interpolation?",
        value = FALSE,
        status = "primary",
        slim = TRUE,
        fill = FALSE,
        bigger = TRUE,
        inline = FALSE
      ),
      sliderInput(
        inputId = ns("nodeDistance"),
        label = "Distance between nodes:",
        min = 50,
        value = 500,
        max = 500
      ),
      sliderInput(
        inputId = ns("centralGravity"),
        label = "Central gravity:",
        min = 0,
        value = 0,
        max = 1
      ),
      sliderInput(
        inputId = ns("springLength"),
        label = "Spring lenght:",
        min = 50,
        value = 200,
        max = 600
      )
    )

    if (mobile()) {
      content_mobile
    } else {
      content_desktop
    }
  })

  observeEvent(input$open, {
    pushbar_open(id = ns("myPushbar"))
  })

  observe({
    req(!is.null(mobile()))
    if (mobile()) {
      shinyjs::runjs("$('#myPushbar').removeClass('from_bottom');")
      shinyjs::runjs("$('#myPushbar').addClass('from_right');")
    }
  })

  #-------------------------------------------------------------------------
  # Network: nodes, edges, events, ...
  #-------------------------------------------------------------------------

  nodes <- reactive({

    req(!is.null(input$pokeNodesShape), !is.null(input$pokeNodesSize))

    df <- data.frame(
      id = 1:length(mainData),
      group = groups,
      shape = input$pokeNodesShape,
      label = pokeNames,
      #fixed = list("x" = FALSE, "y" = FALSE),
      size = input$pokeNodesSize,
      physics = TRUE,
      hidden = rep(FALSE, length(mainData)),
      stringsAsFactors = FALSE
    )

    if (input$pokeNodesShape == "image") df$image <-  sprites

    return(df)
  })

  edges <- reactive({

    req(!is.null(input$pokeEdgesWidth))

    data.frame(
      width = input$pokeEdgesWidth,
      color = list(color = c(rep("black", length(families$from))), highlight = "blue"),
      dashes = TRUE,
      smooth = FALSE,
      hidden = FALSE,
      from = families$from,
      to = families$to,
      stringsAsFactors = FALSE
    )
  })

  pokeNames <- names(mainData)
  sprites <- vapply(seq_along(pokeNames), FUN = function(i) mainData[[i]]$sprites$front_default, FUN.VALUE = character(1))

  # below is a test to see if gif are supported (lag)
  #sprites <- vapply(seq_along(pokeNames), FUN = function(i) {
  #  paste0("http://www.pokestadium.com/sprites/xy/", mainData[[i]]$name, ".gif")
  #}, FUN.VALUE = character(1))


  output$pokeNet <- renderVisNetwork({

    req(!is.null(input$nodesInterp),
        !is.null(input$pokeNodesDrag),
        !is.null(input$dragView),
        !is.null(input$zoomView),
        !is.null(input$nodeDistance),
        !is.null(input$centralGravity),
        !is.null(input$springLength)
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
            interpolation = input$nodesInterp # time consumming
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
        dragNodes = input$pokeNodesDrag,
        dragView = input$dragView,
        zoomView = input$zoomView,
        navigationButtons = FALSE,
        selectable = TRUE
      ) %>%
      visPhysics(
        stabilization = TRUE,
        solver = "repulsion",
        repulsion = list(
          nodeDistance = input$nodeDistance,
          centralGravity = input$centralGravity,
          springLength = input$springLength
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
      nodes$size <- input$pokeNodesSize
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

    showModal(modalDialog(
      title = fluidRow(
        column(
          width = 2,
          align = "left",
          tablerAvatar(url = sprites[[selected]])
        ),
        column(
          width = 8,
          align = "center",
          paste0(names(mainData)[[selected]], " 's names")
        ),
        column(width = 2, align = "right", modalButton("X"))
      ),
      tablerTable(
        lapply(seq_along(names$languages), function(i) {
          tablerTableItem(
            left = names$languages[[i]],
            right = names$name[[i]]
          )
        }),
        stacked = FALSE
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  return(list(selected = reactive(input$current_node_id_zoom)))

}
