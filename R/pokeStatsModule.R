#' UI module for generating the pokemon stats chart
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
pokeStatsUi <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    uiOutput(ns("basic_stats")),
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
#' @param selected Input containing the selected pokemon index.
#'
#' @import tablerDash
#'
#' @export
pokeStats <- function(input, output, session, mainData, details, selected) {

  ns <- session$ns

  # ################################################################
  # Basic Stats
  # ################################################################

  # mean values
  names_means <- c(
    "capture_rate",
    "base_happiness",
    "height",
    "weight",
    "base_experience"
  )

  means <- round(
    unlist(
      lapply(seq_along(names_means), function(i) {
        current_mean <- names_means[[i]]
        if (i %in% c(3:5)) {
          mean(sapply(seq_along(mainData), function(j) {
            mainData[[j]][[current_mean]]
          }))
        } else {
          mean(sapply(seq_along(details), function(j) {
            details[[j]][[current_mean]]
          }))
        }
      })
    )
  )

  # height and weight need to be scaled
  means[[3]] <- means[[3]] * 10
  means[[4]] <- means[[4]] / 10
  names(means) <- paste0("mean_", names_means)

  means <- c(NA, means)

  # basic stats
  outputNames <- c(
    "pokeGrowth",
    "pokeCapture",
    "pokeHappy",
    "pokeHeight",
    "pokeWeight",
    "baseXp"
  )

  basicStats <- reactive({

    req(!is.null(selected()))

    list(
      values = tagList(
        switch(
          details[[selected()]]$growth_rate$name,
          "slow" = tablerProgress(value = 0, size = "xs", status = "danger"),
          "medium-slow" = tablerProgress(value = 25, size = "xs", status = "warning"),
          "medium" = tablerProgress(value = 60, size = "xs", status = "yellow"),
          "fast" = tablerProgress(value = 90, size = "xs", status = "success")
        ),
        details[[selected()]]$capture_rate,
        details[[selected()]]$base_happiness,
        mainData[[selected()]]$height * 10,
        mainData[[selected()]]$weight / 10,
        mainData[[selected()]]$base_experience
      ),
      titles = c(
        "Growth Rate",
        "Capture Rate",
        "Base Happiness",
        "Height in cm",
        "Weight in Kg",
        "Base Xp"
      )
    )
  })

  lapply(seq_along(outputNames), FUN = function(i) {

    output[[outputNames[[i]]]] <- renderUI({

      # calculate trends in %
      trend <- if (i == 1) NA else round(100 * (basicStats()$values[[i]] - means[[i]]) / means[[i]])

      tablerStatCard(
        value = basicStats()$values[[i]],
        title = basicStats()$titles[[i]],
        width = 12,
        trend = if (!is.na(trend)) trend else NULL
      )
    })
  })


  output$basic_stats <- renderUI({
    req(!is.null(input$pokeBasicStats))
    if (input$pokeBasicStats) {
      tagList(
        fluidRow(
          column(width = 4, uiOutput(ns("pokeHappy"))),
          column(width = 4, uiOutput(ns("pokeHeight"))),
          column(width = 4, uiOutput(ns("pokeWeight")))
        ),
        fluidRow(
          column(width = 4, uiOutput(ns("baseXp"))),
          column(width = 4, uiOutput(ns("pokeGrowth"))),
          column(width = 4, uiOutput(ns("pokeCapture")))
        )
      )
    }
  })


  # ################################################################
  # Skills
  # ################################################################

  # pokemon skills dataframe
  skills <- reactive({
    req(!is.null(selected()))

    data.frame(
      x = mainData[[selected()]]$stats$stat$name,
      y = mainData[[selected()]]$stats$base_stat
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

    tablerCard(
      title = paste0(selected(), " Stats"),
      options = tagList(
        shinyWidgets::prettySwitch(
          inputId = ns("pokeBasicStats"),
          label = "Display Basic Stats?",
          value = TRUE,
          status = "default",
          slim = TRUE,
          fill = FALSE,
          bigger = TRUE,
          inline = FALSE
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
      echarts4rOutput(outputId = ns("pokeStats"))
    )
  })

}
