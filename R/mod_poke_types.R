#' UI module for generating the pokemon type
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
#' @rdname poke-types
poke_types_ui <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_types"))
}

#' Server module generating the pokemon types info boxes
#'
#' @param selected Input containing the selected pokemon index.
#'
#' @import tablerDash
#'
#' @export
#' @rdname poke-types
poke_types_server <- function(id, selected) {

  moduleServer(
    id,
    function(input, output, session) {
      # render infoBoxes
  output$poke_types <- renderUI({
    req(selected())
    types <- poke_data[[selected()]]$types

    lapply(types, FUN = function(type) {
      type_name <- type$name
      type_slot <- type$slot

      tagList(
        tablerInfoCard(
          value = paste(type_slot, type_name),
          status = get_type_color(type_name),
          icon = NULL,
          description = NULL,
          width = 12
        ),
        fluidRow(
          column(
            width = 6,
            align = "left",
            h5("Damages from:"), br(),
            HTML(paste0(tablerTag(name = "2X", rounded = FALSE, color = "red"), " ")),
            lapply(seq_along(type$double_damage_from), FUN = function(j) type$double_damage_from[[j]]), br(),
            HTML(paste0(tablerTag(name = "0.5X", rounded = FALSE, color = "green"), " ")),
            lapply(seq_along(type$half_damage_from), FUN = function(j) type$half_damage_from[[j]]), br(),
            HTML(paste0(tablerTag(name = "0", rounded = FALSE, color = "default"), " ")),
            lapply(seq_along(type$no_damage_from), FUN = function(j) type$no_damage_from[[j]])
          ),
          column(
            width = 6,
            align = "left",
            h5("Damages to:"), br(),
            HTML(paste0(tablerTag(name = "2X", rounded = FALSE, color = "green"), " ")),
            lapply(seq_along(type$double_damage_to), FUN = function(j) type$double_damage_to[[j]]), br(),
            HTML(paste0(tablerTag(name = "0.5X", rounded = FALSE, color = "red"), " ")),
            lapply(seq_along(type$half_damage_to), FUN = function(j) type$half_damage_to[[j]]), br(),
            HTML(paste0(tablerTag(name = "0", rounded = FALSE, color = "default"), " ")),
            lapply(seq_along(type$no_damage_to), FUN = function(j) type$no_damage_to[[j]])
          )
        ),
        br()
      )
    })
  })
    }
  )
}
