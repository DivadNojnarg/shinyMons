#' UI module for generating the pokeAttacks section
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeAttackUi <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    uiOutput(ns("poke_attack_select")),
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


  output$poke_attack_select <- renderUI({
    fluidRow(
      column(
        width = 12,
        align = "center",
        multiInput(
          inputId = ns("pokeAttackSelect"),
          label = "Select pokemon abilities:",
          choices = names(attacks),
          selected = names(attacks)[c(1:10)],
          width = "350px"
        )
      )
    )
  })


  output$poke_attack <- renderUI({

    req(input$pokeAttackSelect)

    fluidRow(
      lapply(seq_along(input$pokeAttackSelect), function(i) {

        selected <- input$pokeAttackSelect[[i]]

        # here some colors are not supported by tags. Need to fix it
        typeColor <- switch(
          attacks[[selected]]$type$name,
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

        tablerBlogCard(
          title = attacks[[selected]]$name,
          author = tablerTag(
            name = "Type",
            rounded = FALSE,
            color = "default",
            addon = attacks[[selected]]$type$name,
            addonColor = typeColor
          ),
          date = tablerTag(
            name = "Target",
            rounded = FALSE,
            color = "default",
            addon = attacks[[selected]]$target$name,
            addonColor = NULL
          ),
          href = NULL,
          src = NULL,
          avatarUrl = NULL,
          width = 6,
          tablerTable(
            title = "Main Stats",
            width = 4,
            tablerTableItem(
              left = tablerTag(name = "Power", rounded = TRUE, color = "pink"),
              right = h3(attacks[[selected]]$power)
            ),
            tablerTableItem(
              left = tablerTag(name = "PP", rounded = TRUE, color = "yellow"),
              right = h3(attacks[[selected]]$pp)
            ),
            tablerTableItem(
              left = tablerTag(name = "Accuracy", rounded = TRUE, color = "orange"),
              right = h3(attacks[[selected]]$accuracy)
            ),
            tablerTableItem(
              left = tablerTag(name = "Priority", rounded = TRUE, color = "blue"),
              right = h3(attacks[[selected]]$priority)
            )
          ),
          paste0("Description: ", attacks[[i]]$flavor_text_entries$flavor_text[44]), br(),
          tablerTag(
            name = "Type of damages",
            rounded = FALSE,
            color = "default",
            addon = attacks[[selected]]$damage_class$name,
            addonColor = "red"
          )
        )
      })
    )
  })


}
