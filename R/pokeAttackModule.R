#' UI module for generating the pokeAttacks section
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeAttackUi <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("poke_attack"))
}


#' Server module for generating the pokeAttacks section
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param attacks Data containing all pokemon abilities.
#' @export
pokeAttack <- function(input, output, session, attacks) {

  ns <- session$ns

  output$poke_attack <- renderUI({
    lapply(seq_along(attacks), function(i) {
      tablerBlogCard(
        title = attacks[[i]]$name,
        author = attacks[[i]]$type$name,
        date = attacks[[i]]$target$name,
        href = NULL,
        src = NULL,
        avatarUrl = NULL,
        width = 6,
        attacks[[i]]$flavor_text_entries$flavor_text[44]
      )
    })
  })


}
