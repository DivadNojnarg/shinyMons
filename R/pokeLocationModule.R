#' UI module for generating the pokemon location card
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeLocationUi <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_locations"), class = "col-sm-12")
}




#' Server module generating the pokemon location card
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param selected Input containing the selected pokemon index.
#' @param locations Contains preprocessed data of the selected pokemon location
#'
#' @importFrom tablerDash tablerCard
#'
#' @export
pokeLocation <- function(input, output, session, selected, locations) {

  pokeLocations <- reactive({
    req(!is.null(selected()))
    locations[[selected()]]$name
  })

  output$poke_locations <- renderUI({

    req(!is.null(selected()))

    f7Card(
      title = paste0("Where to find ",  selected(), " ?"),
      if (!is.null(pokeLocations())) {
        lapply(seq_along(pokeLocations()), function(i) f7Flex(paste(i, ":", pokeLocations()[[i]])))
      } else {
        "This pokemon cannot be find in the wild."
      }
    )
  })
}
