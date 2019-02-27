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
      h4("HELLO"),
      actionButton(ns("close"), "Close pushbar")
    )#,
    #uiOutput(ns("poke_network"))
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
  setup_pushbar(session, blur = TRUE) # setup

  ns <- session$ns

  observeEvent(input$open, {
    pushbar_open(session, id = ns("myPushbar"))
  })

  observeEvent(input$close, {
    pushbar_close(session)
  })
}
