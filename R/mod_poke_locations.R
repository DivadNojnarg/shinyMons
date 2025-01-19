#' UI module for generating the pokemon location card
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
#' @rdname poke-locations
poke_locations_ui <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_locations"), class = "col-sm-12")
}

#' Server module generating the pokemon location card
#'
#' @param selected Input containing the selected pokemon index.
#'
#' @importFrom tablerDash tablerCard
#'
#' @export
#' @rdname poke-locations
poke_locations_server <- function(id, selected) {
  moduleServer(
    id,
    function(input, output, session) {
      output$poke_locations <- renderUI({
        req(selected())

        poke_locations <- poke_data[[selected()]]$locations

        tablerCard(
          title = paste0("Where to find ", selected(), " ?"),
          collapsible = FALSE,
          closable = FALSE,
          zoomable = FALSE,
          statusSide = "top",
          width = 12,
          if (!is.null(poke_locations)) {
            lapply(
              seq_along(poke_locations),
              function(i) fluidRow(paste(i, ":", poke_locations[[i]]))
            )
          } else {
            "This pokemon cannot be find in the wild."
          }
        )
      })
    }
  )
}
