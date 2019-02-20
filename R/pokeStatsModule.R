#' UI module for generating the pokemon stats chart
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeStatsUi <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    fluidRow(
      column(width = 3, uiOutput(ns("pokeHappy"))),
      column(width = 3, uiOutput(ns("baseXp"))),
      column(width = 3, uiOutput(ns("pokeHeight"))),
      column(width = 3, uiOutput(ns("pokeWeight")))
    ),
    uiOutput(ns("pokeStatsCard"))
  )
}

# make R CMD check happy
globalVariables("x")
globalVariables("y")

#' Server module generating the pokemon stats chart
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param mainData Object containing the main pokemon data.
#' @param details Object containing extra pokemon details.
#' @param skills Object containing pokemon statistics.
#' @param selected Input containing the selected pokemon index.
#' @export
pokeStats <- function(input, output, session, mainData, details, skills, selected) {

  ns <- session$ns

  # TO DO: add capture rate/ growth rate

  # happiness
  output$pokeHappy <- renderUI({

    req(!is.null(selected()))

    tablerStatCard(
      value = details[[selected()]]$base_happiness,
      title = "Base Happiness",
      width = 12
    )
  })

  # pokemon height
  output$pokeHeight <- renderUI({

    req(!is.null(selected()))

    tablerStatCard(
      value = paste(0, mainData[[selected()]]$weight, sep = "."),
      title = "Height in cms",
      width = 12
    )
  })


  # pokemon weight
  output$pokeWeight <- renderUI({

    req(!is.null(selected()))

    tablerStatCard(
      value = mainData[[selected()]]$weight / 10,
      title = "Weight in Kgs",
      width = 12
    )
  })


  # base experience
  output$baseXp <- renderUI({

    req(!is.null(selected()))

    tablerStatCard(
      value = mainData[[selected()]]$base_experience,
      title = "Base Xp",
      width = 12
    )
  })

  # generate radar chart for pokemons
  output$pokeStats <- renderEcharts4r({

    req(!is.null(skills()))

    skills() %>%
      e_charts(x) %>%
      e_radar(y, name = paste0(selected(), " Stats")) %>%
      e_tooltip(trigger = "item")
  })


  # card wrapper for the charts
  output$pokeStatsCard <- renderUI({

    req(!is.null(selected()))

    fluidRow(
      tablerCard(
        title = paste0(selected(), " Stats"),
        options = NULL,
        footer = NULL,
        status = "info",
        statusSide = "left",
        collapsible = FALSE,
        closable = FALSE,
        zoomable = FALSE,
        width = 12,
        overflow = FALSE,
        echarts4rOutput(outputId = ns("pokeStats"))
      )
    )
  })

}
