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
    "Growth Rate",
    "Capture Rate",
    "Base Happiness",
    "Height in cm",
    "Weight in Kg",
    "Base Xp"
  )

  progressValue <- reactive({
    req(selected())
    switch(
      details[[selected()]]$growth_rate$name,
      "slow" = f7Progress(id = ns("slow_pg"), value = 0, color = "red"),
      "medium-slow" = f7Progress(id = ns("slow_pg"), value = 25, color = "orange"),
      "medium" = f7Progress(id = ns("slow_pg"), value = 60, color = "yellow"),
      "fast" = f7Progress(id = ns("slow_pg"), value = 90, color = "green")
    )
  })

  basicStats <- reactive({
    req(!is.null(selected()))
    c(
      NA,
      details[[selected()]]$capture_rate,
      details[[selected()]]$base_happiness,
      mainData[[selected()]]$height * 10,
      mainData[[selected()]]$weight / 10,
      mainData[[selected()]]$base_experience
    )
  })



    output$basic_stats <- renderUI({
      req(input$pokeBasicStats)
      listItems <- lapply(seq_along(outputNames), FUN = function(i) {

        # calculate trends in %
        trend <- if (i == 1) {
          NA
          } else {
            res <- round(100 * (basicStats()[i] - means[[i]]) / means[[i]])
            if (res < 0){
              HTML(paste0(res, "%", f7Icon("arrow_down_right")))
            } else {
              HTML(paste0(res, "%", f7Icon("arrow_up_right")))
            }
          }


        f7ListItem(
          media = f7Icon("info"),
          if (i == 1) progressValue() else basicStats()[i],
          header = outputNames[i],
          right = if (!is.na(trend)) trend else NULL
        )
      })

      f7List(
        inset = TRUE,
        listItems
      )
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

    f7Card(
      title = tagList(
        paste0(selected(), " Stats"),
        f7Toggle(
          inputId = ns("pokeBasicStats"),
          label = NULL,
          checked = TRUE,
          color = "default"
        )
      ),
      echarts4rOutput(outputId = ns("pokeStats")),
      footer = NULL
    )
  })

}
