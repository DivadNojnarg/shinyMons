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

      poke_names <- names(poke_data)
      # Update choices
      observeEvent(c(input$is_shiny, selected()), {
        updatePickerInput(
          session,
          inputId = "poke_select",
          choices = poke_names,
          choicesOpt = list(
            content = sprintf(
              "<img src=\'%s\' width=20 style=\'vertical-align:top;\'></img> %s",
              get_front_sprites(input$is_shiny),
              poke_names
            )
          ),
          selected = if (!is.null(selected())) {
            if (poke_names[selected()] != input$poke_select)
              poke_names[selected()]
          } else {
            if (!is.null(input$poke_select)) input$poke_select else
              poke_names[[1]]
          }
        )
      })

      return(
        list(
          poke_select = reactive(input$poke_select),
          is_shiny = reactive(input$is_shiny)
        )
      )
    }
  )
}
