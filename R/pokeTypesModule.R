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
#'
#' @import tablerDash
#'
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
        "normal" = "white",
        "fighting" = "red",
        "flying" = "blue",
        "poison" = "purple",
        "ground" = "gray",
        "rock" = "orange",
        "bug" = "teal",
        "ghost" = "deeppurple",
        "fire" = "deeporange",
        "water" = "default",
        "grass" = "green",
        "electric" = "yellow",
        "psychic" = "pink",
        "ice" = "lightblue",
        "dragon" = "black"
      )

      f7Card(
        title = tagList(
          paste(typeSlot, typeName),
          f7Badge(color = pokeColor)
        ),
        h5("Damages from:"), br(),
        # double
        if (!is.null(double_damage_from)) {
          f7Chip(
            label = lapply(
              seq_along(double_damage_from),
              FUN = function(j) double_damage_from[[j]]
            ),
            status = "red"
          )
        },
        # half
        if (!is.null(half_damage_from)) {
          f7Chip(
            label = lapply(
              seq_along(half_damage_from),
              FUN = function(j) half_damage_from[[j]]
            ),
            status = "green"
          )
        },
        # none
        if (!is.null(no_damage_from)) {
          f7Chip(
            label = lapply(
              seq_along(no_damage_from),
              FUN = function(j) no_damage_from[[j]]
            ),
            status = "gray"
          )
        },
        br(), br(),

        h5("Damages to:"), br(),
        # double
        if (!is.null(double_damage_to)) {
          f7Chip(
            label = lapply(
              seq_along(double_damage_to),
              FUN = function(j) double_damage_to[[j]]
            ),
            status = "green"
          )
        },
        # half
        if (!is.null(half_damage_to)) {
          f7Chip(
            label = lapply(
              seq_along(half_damage_to),
              FUN = function(j) half_damage_to[[j]]
            ),
            status = "red"
          )
        },
        # none
        if (!is.null(no_damage_to)) {
          f7Chip(
            label = lapply(
              seq_along(no_damage_to),
              FUN = function(j) no_damage_to[[j]]
            ),
            status = "gray"
          )
        }
      )
    })
  })
}
