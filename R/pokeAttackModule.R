#' UI module for generating the pokeAttacks section
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeAttackUi <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        align = "center",
        multiInput(
          inputId = ns("pokeAttackSelect"),
          label = "Select pokemon abilities:",
          choices = names(pokeAttacks),
          selected = names(pokeAttacks)[c(1:10)],
          width = "350px"
        )
      )
    ),
    uiOutput(ns("poke_attack"))
  )
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

    req(input$pokeAttackSelect)

    fluidRow(
      lapply(seq_along(input$pokeAttackSelect), function(i) {

        selected <- input$pokeAttackSelect[[i]]

        tablerBlogCard(
          title = attacks[[selected]]$name,
          author = attacks[[selected]]$type$name,
          date = attacks[[selected]]$target$name,
          href = NULL,
          src = NULL,
          avatarUrl = NULL,
          width = 6,
          tablerTable(
            title = "Main Stats",
            width = 4,
            tablerTableItem(
              right = "Power",
              left = attacks[[selected]]$power
            ),
            tablerTableItem(
              right = "PP",
              left = attacks[[selected]]$pp
            ),
            tablerTableItem(
              right = "Accuracy",
              left = attacks[[selected]]$accuracy
            ),
            tablerTableItem(
              right = "Priority",
              left = attacks[[selected]]$priority
            )
          ),
          attacks[[i]]$flavor_text_entries$flavor_text[44]
        )
      })
    )
  })


}
