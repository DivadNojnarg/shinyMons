#' UI module for generating the pokemon type
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeTypeUi <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_types"))
}




#' Server module generating the pokemon types info boxes
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param types Object containing the preprocessed pokemon types.
#' @param selected Input containing the selected pokemon index.
#' @export
pokeType <- function(input, output, session, types, selected) {


  pokeTypes <- reactive({
    req(!is.null(selected()))
    types[[selected()]]
  })

  # render infoBoxes
  output$poke_types <- renderUI({

    lapply(seq_along(pokeTypes()), FUN = function(i) {

      typeName <- pokeTypes()[[i]]$name
      typeSlot <- pokeTypes()[[i]]$slot

      double_damage_from <- pokeTypes()[[i]]$double_damage_from
      double_damage_to <- pokeTypes()[[i]]$double_damage_to
      half_damage_from <- pokeTypes()[[i]]$half_damage_from
      half_damage_to <- pokeTypes()[[i]]$half_damage_to
      no_damage_from <- pokeTypes()[[i]]$no_damage_from
      no_damage_to <- pokeTypes()[[i]]$no_damage_to

      # set up colors according to the pokemon type original color
      # fortunataley tabler dash has tons of colors available
      pokeColor <- switch(
        typeName,
        "normal" = "gray-lightest",
        "fighting" = "red",
        "flying" = "indigo",
        "poison" = "purple-light",
        "ground" = "yellow-lighter",
        "rock" = "yellow-darker",
        "bug" = "green-lighter",
        "ghost" = "purple-dark",
        "fire" = "orange",
        "water" = "azure",
        "grass" = "green",
        "electric" = "yellow",
        "psychic" = "pink",
        "ice" = "azure-lighter",
        "dragon" = "purple-darker"
      )

      tagList(
        tablerInfoCard(
          value = paste(typeSlot, typeName),
          status = pokeColor,
          icon = NULL,
          description = NULL,
          width = 12
        ),
        fluidRow(
          column(
            width = 6,
            align = "center",
            h5("Damages from:"), br(),
            HTML(paste0(tablerTag(name = "2X", rounded = FALSE, color = "red"), " ")),
            lapply(seq_along(double_damage_from), FUN = function(j) double_damage_from[[j]]), br(),
            HTML(paste0(tablerTag(name = "0.5X", rounded = FALSE, color = "green"), " ")),
            lapply(seq_along(half_damage_from), FUN = function(j) half_damage_from[[j]]), br(),
            HTML(paste0(tablerTag(name = "∅", rounded = FALSE, color = "default"), " ")),
            lapply(seq_along(no_damage_from), FUN = function(j) no_damage_from[[j]])
          ),
          column(
            width = 6,
            align = "center",
            h5("Damages to:"), br(),
            HTML(paste0(tablerTag(name = "2X", rounded = FALSE, color = "green"), " ")),
            lapply(seq_along(double_damage_to), FUN = function(j) double_damage_to[[j]]), br(),
            HTML(paste0(tablerTag(name = "0.5X", rounded = FALSE, color = "red"), " ")),
            lapply(seq_along(half_damage_to), FUN = function(j) half_damage_to[[j]]), br(),
            HTML(paste0(tablerTag(name = "∅", rounded = FALSE, color = "default"), " ")),
            lapply(seq_along(no_damage_to), FUN = function(j) no_damage_to[[j]])
          )
        ),
        br()
      )
    })
  })
}
