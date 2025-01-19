#' UI module for generating other stats chart
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeOtherUi <- function(id) {
  ns <- shiny::NS(id)
  fluidRow(
    uiOutput(ns("poke_distrib"), class = "col-sm-6"),
    uiOutput(ns("poke_types_distrib"), class = "col-sm-6")
  )
}


#' Server module generating other stats chart
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param mainData Object containing the main pokemon data.
#' @param details Object containing extra pokemon details.
#'
#' @import echarts4r tablerDash
#'
#' @export
pokeOther <- function(input, output, session, mainData, details) {
  ns <- session$ns

  # ########################################
  # Classic stats: global height and weight
  # ########################################

  # Height distribution
  output$distribPlot <- renderEcharts4r({
    heights <- vapply(
      X = seq_along(names(mainData)),
      FUN = function(i) {
        mainData[[i]]$height * 10
      },
      FUN.VALUE = numeric(1)
    )

    weights <- vapply(
      X = seq_along(names(mainData)),
      FUN = function(i) {
        mainData[[i]]$weight / 10
      },
      FUN.VALUE = numeric(1)
    )


    n <- seq_along(names(mainData))
    df <- data.frame(n = n, h = heights, w = weights)

    req(!is.null(input$distribChoice))

    if (input$distribChoice == "Height") {
      df %>%
        e_charts() %>%
        e_histogram(h) %>%
        e_tooltip()
    } else {
      df %>%
        e_charts() %>%
        e_histogram(w) %>%
        e_tooltip()
    }
  })


  output$poke_distrib <- renderUI({
    tablerCard(
      title = paste("All Pokemons Distribution"),
      options = tagList(
        prettyRadioButtons(
          inputId = ns("distribChoice"),
          label = "Choose a distribution:",
          choices = c("Height (cm)" = "Height", "Weight (kg)" = "Weight"),
          selected = "Height",
          animation = "pulse",
          inline = TRUE
        )
      ),
      footer = NULL,
      status = "info",
      statusSide = "left",
      collapsible = FALSE,
      closable = FALSE,
      zoomable = FALSE,
      width = 12,
      overflow = FALSE,
      echarts4rOutput(outputId = ns("distribPlot"))
    )
  })


  # ########################################
  # Types stats: global height and weight
  # ########################################


  output$typesDistrib <- renderEcharts4r({
    types <- unlist(lapply(seq_along(names(mainData)), FUN = function(i) mainData[[i]]$types$type$name))
    types <- table(types)
    # n <- seq_along(types)
    df <- data.frame(t = types)
    names(df) <- c("type", "n")
    df %>%
      e_charts(type) %>%
      e_pie(n, roseType = "radius") %>%
      e_tooltip()
  })


  output$poke_types_distrib <- renderUI({
    tablerCard(
      title = paste("All Pokemons Types"),
      options = NULL,
      footer = NULL,
      status = "info",
      statusSide = "left",
      collapsible = FALSE,
      closable = FALSE,
      zoomable = FALSE,
      width = 12,
      overflow = FALSE,
      echarts4rOutput(outputId = ns("typesDistrib"))
    )
  })
}


# make R CMD check happy
globalVariables("h")
globalVariables("w")
globalVariables("type")
