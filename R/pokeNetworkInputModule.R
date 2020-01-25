#' UI module for controling pokeNetwork options
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeNetworkInputUi <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h2("Nodes"),
    # nodes shape
    f7Radio(
      inputId = ns("pokeNodesShape"),
      label = "Nodes shape:",
      selected = "image",
      choices = c("Circles" = "circle", "Sprites" = "image")
    ),
    # can we drag nodes?
    f7Toggle(
      inputId = ns("pokeNodesDrag"),
      label = "Drag nodes?",
      checked = TRUE
    ),
    br(),
    br(),
    # nodes size
    f7Stepper(
      inputId = ns("pokeNodesSize"),
      label = "Size of nodes:",
      value = 200,
      min = 100,
      max = 1000,
      step = 10
    ),
    br(),
    br(),
    f7Toggle(
      inputId = ns("nodesInterp"),
      label = "Nodes interpolation?",
      checked = FALSE
    ),
    f7Slider(
      inputId = ns("nodeDistance"),
      label = "Distance between nodes:",
      min = 50,
      value = 500,
      max = 500,
      scale = TRUE
    ),

    hr(),
    h2("Edges"),
    # edges width
    f7Stepper(
      inputId = ns("pokeEdgesWidth"),
      label = "Width of edges:",
      value = 10,
      min = 5,
      max = 50,
      step = 1
    ),
    br(),
    br(),
    f7Toggle(
      inputId = ns("displayEdges"),
      label = "Display edges?",
      checked = TRUE
    ),
    f7Slider(
      inputId = ns("springLength"),
      label = "Spring lenght:",
      min = 50,
      value = 200,
      max = 600,
      scale = TRUE
    ),

    hr(),
    h2("Others"),
    f7Toggle(
      inputId = ns("dragView"),
      label = "DragView",
      checked = TRUE
    ),
    br(),
    br(),
    f7Toggle(
      inputId = ns("zoomView"),
      label = "ZoomView",
      checked = TRUE
    ),
    br(),
    br(),
    f7Slider(
      inputId = ns("centralGravity"),
      label = "Central gravity:",
      min = 0,
      value = 0,
      max = 1
    )
  )
  #uiOutput(ns("networkInputs"))
}


#' Server module for controling the pokeNetwork section
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param currentTab Currently selected tab (input$tabset).
#' @export
pokeNetworkInput <- function(input, output, session, currentTab) {

  ns <- session$ns

  output$networkInputs <- renderUI({
    #req(currentTab == "network")

  })


  return(
    list(
      pokeNodesShape = reactive(input$pokeNodesShape),
      pokeNodesDrag = reactive(input$pokeNodesDrag),
      pokeNodesSize = reactive(input$pokeNodesSize),
      nodesInterp = reactive(input$nodesInterp),
      nodeDistance = reactive(input$nodeDistance),
      pokeEdgesWidth = reactive(input$pokeEdgesWidth),
      displayEdges = reactive(input$displayEdges),
      springLength = reactive(input$springLength),
      dragView = reactive(input$dragView),
      zoomView = reactive(input$zoomView),
      centralGravity = reactive(input$centralGravity)
    )
  )
}
