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
    uiOutput(ns("poke_attack")),
    uiOutput(ns("poke_attacks_types"))
  )
}


#' Server module for generating the pokeAttacks section
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param attacks Data containing all pokemon abilities in the first generation.
#'
#' @import tablerDash echarts4r
#' @importFrom stats rnorm
#'
#' @export
pokeAttack <- function(input, output, session, attacks) {

  ns <- session$ns

  output$poke_attack_select <- renderUI({
    f7Select(
      inputId = ns("pokeAttackSelect"),
      label = "Select an attack:",
      choices = names(attacks),
      selected = names(attacks)[1]
    )
  })


  # below we calculate the mean values for each stat
  # mean values
  names_stats <- c(
    "power",
    "pp",
    "accuracy"
  )

  stats <- lapply(seq_along(names_stats), function(i) {
    current_stat <- names_stats[[i]]
    unlist(
      lapply(seq_along(attacks), function(j) {
        attacks[[j]][[current_stat]]
      })
    )
  })

  names(stats) <- names_stats


  output$attackMeans <- renderEcharts4r({

    df <- data.frame(
      x = c(
        stats$power,
        stats$pp,
        stats$accuracy
      ),
      grp = c(
        rep(names_stats[1], length(stats$power)),
        rep(names_stats[2], length(stats$pp)),
        rep(names_stats[3], length(stats$accuracy))
      )
    )

    df %>%
      group_by(grp) %>%
      e_charts() %>%
      e_boxplot(x) %>%
      e_title("Global Stats Means")

  })


  # radar chart of attacks
  output$attackStats <- renderEcharts4r({

    req(input$pokeAttackSelect)
    selected <- input$pokeAttackSelect

    name <- attacks[[selected]]$name

    accuracy <- attacks[[selected]]$accuracy
    power <- attacks[[selected]]$power
    pp <- attacks[[selected]]$pp

    # if power is NULL, we stop here
    req(!is.null(power))

    df <- data.frame(
      x = c("accuracy", "power", "pp"),
      y = c(accuracy, power, pp)
    )

    df %>%
      e_charts(x) %>%
      e_radar(y, name = paste0(name, " Stats")) %>%
      e_tooltip(trigger = "item")

  })

  # box containing radar chart and other elements
  output$poke_attack <- renderUI({

    req(input$pokeAttackSelect)
    selected <- input$pokeAttackSelect

    # for the radar chart
    name <- attacks[[selected]]$name
    power <- attacks[[selected]]$power

    # here some colors are not supported by tags. Need to fix it
    typeColor <- switch(
      attacks[[selected]]$type$name,
      "normal" = "white",
      "fighting" = "red",
      "flying" = "blue",
      "poison" = "purple",
      "ground" = "gray",
      "rock" = "orange",
      "bug" = "teal",
      "ghost" = "deeppurple",
      "fire" = "deeporange",
      "water" = "default",
      "grass" = "green",
      "electric" = "yellow",
      "psychic" = "pink",
      "ice" = "lightblue",
      "dragon" = "black"
    )

    tagList(
      f7Card(
        title = tagList(
          attacks[[selected]]$name,
          f7Chip(
            label = paste0("Type: ", attacks[[selected]]$type$name),
            status = typeColor
          ),
          f7Chip(label = paste0("Target: ", attacks[[selected]]$target$name))
        ),
        f7List(
          f7ListItem(
            title = tablerTag(name = "Power", rounded = TRUE, color = "pink"),
            right = h3(attacks[[selected]]$power)
          ),
          f7ListItem(
            title = tablerTag(name = "PP", rounded = TRUE, color = "yellow"),
            right = h3(attacks[[selected]]$pp)
          ),
          f7ListItem(
            title = tablerTag(name = "Accuracy", rounded = TRUE, color = "orange"),
            right = h3(attacks[[selected]]$accuracy)
          ),
          f7ListItem(
            title = tablerTag(name = "Priority", rounded = TRUE, color = "blue"),
            right = h3(attacks[[selected]]$priority)
          )
        ),
        footer = tagList(
          paste0("Description: ", attacks[[selected]]$flavor_text_entries$flavor_text[44]),
          f7Chip(
            label = paste0("Type: ", attacks[[selected]]$damage_class$name),
            status = typeColor
          )
        )
      ),


      f7Card(
        title = "Global Stats",
        echarts4rOutput(ns("attackMeans"))
      ),

      if (!is.null(power)) {
        f7Card(
          echarts4rOutput(outputId = ns("attackStats"))
        )
      }
    )
  })


  # treemap of attack types
  attackTypes <- sapply(seq_along(attacks), function(i) attacks[[i]]$type$name)

  df <- data.frame(
    parent = attackTypes,
    child = names(attacks),
    value = ceiling(rnorm(length(attacks), 10, 2))
  )

  output$attackTypes <- renderEcharts4r({
    df %>%
      e_charts() %>%
      e_treemap(parent, child, value)
  })


  output$poke_attacks_types <- renderUI({
    f7Card(
      title = paste("Attack Types"),
      echarts4rOutput(outputId = ns("attackTypes"))
    )
  })
}


# make R CMD check happy
globalVariables("parent")
globalVariables("child")
globalVariables("value")
globalVariables("grp")
