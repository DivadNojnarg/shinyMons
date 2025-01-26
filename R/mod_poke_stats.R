get_max_stats <- function(stat = NULL) {
  res <- lapply(
    poke_data,
    function(data) {
      stats <- data$stats
      tmp <- extract_from_list(
        stats,
        "base_stat",
        numeric(1)
      )
      names(tmp) <- extract_from_list(stats)
      tmp
    }
  )

  # Merge lists into tibble
  res <- do.call(rbind, res) |>
    as_tibble()
  if (is.null(stat)) res else pull(res, stat)
}

get_max_of_max <- function(data = get_max_stats()) {
  cols <- colnames(data)
  data |>
    # get max of each stat
    summarise(across(all_of(cols), max)) |>
    rowwise() |>
    # Max of max
    max()
}

process_pokemon_stats <- function(stats) {
  data.frame(
    x = extract_from_list(stats),
    y = extract_from_list(stats, "base_stat", numeric(1))
  )
}

create_radar_stats <- function(pokemon) {
  # R CMD check stop crying ...
  x <- y <- z <- NULL

  stats <- pokemon$stats
  # Prepare data
  data <- process_pokemon_stats(stats)

  p <- data |>
    e_charts(x) |>
    e_radar(y, name = paste0(pokemon$name, " stats"), max = get_max_of_max()) |>
    e_tooltip(trigger = "item") |>
    e_legend(textStyle = list(color = "#000"))

  # Also adds previous pokemon stats to compare
  # Check that the evolution belongs to the first 151 pkmns ...
  evolutions <- pokemon$evolutions

  if (length(evolutions) > 0) {
    last_pokemon_evol <- tail(evolutions[[1]]$chain, n = 1)

    if (pokemon$name != last_pokemon_evol) {
      evolution <- which(names(poke_data) == pokemon$name) + 1
      tmp <- process_pokemon_stats(
        poke_data[[evolution]]$stats
      )
      p$x$data[[1]]$z <- tmp$y
      p <- p |>
        e_radar(z, name = paste0(poke_data[[evolution]]$name, " stats"))
    }
  }
  p
}

#' UI module for generating the pokemon stats chart
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
poke_stats_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    uiOutput(ns("basic_stats")),
    h1("Stats"),
    echarts4rOutput(ns("poke_stats"))
  )
}

#' Server module generating the pokemon stats chart
#'
#' @param id Module id.
#' @param selected Input containing the selected pokemon index.
#'
#' @export
poke_stats_server <- function(id, selected) {
  moduleServer(
    id,
    function(input, output, session) {
      other_stats_names <- reactive({
        req(selected())
        names(poke_data[[selected()]]$other_stats)
      })

      output$basic_stats <- renderUI({
        tablerTable(
          title = "Basic stats",
          width = 12,
          lapply(other_stats_names(), function(stat) {
            tablerTableItem(
              left = div(
                class = "d-flex justify-content-between",
                switch(
                  stat,
                  "height" = icon("up-down"),
                  "weight" = icon("weight-scale"),
                  "base_happiness" = icon("face-smile"),
                  "capture_rate" = icon("bowling-ball"),
                  "growth_rate" = icon("up-long")
                ),
                if (stat == "base_happiness") {
                  "happiness"
                } else if (stat == "capture_rate") {
                  "capture rate"
                } else {
                  stat
                }
              ),
              right = poke_data[[selected()]]$other_stats[[stat]]
            )
          })
        )
      })

      output$sum_stats <- renderText({
        poke_data[[selected()]]$sum_stats
      })

      # Generate radar chart for pokemons
      output$poke_stats <- renderEcharts4r({
        req(selected())
        create_radar_stats(poke_data[[selected()]])
      })
    }
  )
}
