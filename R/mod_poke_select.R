#' UI module for sorting pokemon
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
poke_select_ui <- function(id) {
  ns <- shiny::NS(id)
  div(
    class = "d-flex justify-content-center",
    pickerInput(
      inputId = ns("poke_select"),
      width = NULL,
      options = list(style = "btn-primary"),
      choices = list(),
      multiple = FALSE
    ),
    tagAppendAttributes(
      prettySwitch(
        inputId = ns("is_shiny"),
        label = "Shiny?",
        value = FALSE,
        status = "primary",
        slim = TRUE,
        width = NULL
      ),
      class = "m-2"
    )
  )
}

#' Server module generating the pokemon interface
#'
#' @param id Module id.
#' @param selected Object containing the selected pokemon in the network, if not NULL.
#'
#' @import shinyWidgets
#'
#' @export
poke_select_server <- function(id, selected) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      pokeNames <- names(poke_data)

      # Update choices
      observeEvent(c(input$is_shiny, selected()), {
        updatePickerInput(
          session,
          inputId = "poke_select",
          choices = pokeNames,
          choicesOpt = list(
            content = sprintf(
              "<img src=\'%s\' width=20 style=\'vertical-align:top;\'></img> %s",
              get_front_sprites(input$is_shiny),
              pokeNames
            )
          ),
          selected = if (!is.null(selected())) pokeNames[selected()] else pokeNames[[1]]
        )
      })

      return(
        list(
          pokeSelect = reactive(input$poke_select),
          pokeShiny = reactive(input$is_shiny)
        )
      )
    } 
  )
}
