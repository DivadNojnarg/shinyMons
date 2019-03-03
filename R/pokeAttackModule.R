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
    fluidRow(
      uiOutput(ns("poke_attack"), class = "col-sm-6"),
      uiOutput(ns("poke_attack_stats"), class = "col-sm-6")
    ),
    uiOutput(ns("poke_attacks_types"), class = "col-sm-12")
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
    fluidRow(
      column(
        width = 12,
        align = "center",
        selectInput(
          inputId = ns("pokeAttackSelect"),
          label = "Select pokemon abilities:",
          choices = names(attacks),
          selected = names(attacks)[1],
          width = "350px"
        )
      )
    )
  })


  output$poke_attack <- renderUI({

    req(input$pokeAttackSelect)
    selected <- input$pokeAttackSelect

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
      width = 12,
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
      fluidRow(
        # index 44 corresponds to English
        paste0("Description: ", attacks[[selected]]$flavor_text_entries$flavor_text[44]), br(),
        tablerTag(
          name = "Type of damages",
          rounded = FALSE,
          color = "default",
          addon = attacks[[selected]]$damage_class$name,
          addonColor = "red"
        )
      )
    )
  })


  # treemap of attack types
  attackTypes <- sort(sapply(seq_along(attacks), function(i) attacks[[i]]$type$name))

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
    tablerCard(
      title = paste("Attack Types"),
      options = NULL,
      footer = NULL,
      status = "info",
      statusSide = "left",
      collapsible = FALSE,
      closable = FALSE,
      zoomable = FALSE,
      width = 12,
      overflow = FALSE,
      echarts4rOutput(outputId = ns("attackTypes"))
    )
  })

  # radar chart of attacks
  output$attackStats <- renderEcharts4r({

    req(input$pokeAttackSelect)
    selected <- input$pokeAttackSelect

    name <- attacks[[selected]]$name

    accuracy <- attacks[[selected]]$accuracy
    power <- attacks[[selected]]$power
    pp <- attacks[[selected]]$pp

    if (!is.null(power)) {
      tablerAlert(
        title = "Alert",
        "This attack has undetermind power.",
        icon = "alert-triangle",
        status = "warning"
      )
    }

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


  output$poke_attack_stats <- renderUI({

    req(input$pokeAttackSelect)
    selected <- input$pokeAttackSelect

    name <- attacks[[selected]]$name
    power <- attacks[[selected]]$power

    if (is.null(power)) {
      tablerAlert(
        title = "Alert",
        "This attack has undetermind power.",
        icon = "alert-triangle",
        status = "warning"
      )
    } else {
      tablerCard(
        title = paste0(name," Stats"),
        options = NULL,
        footer = NULL,
        status = "info",
        statusSide = "left",
        collapsible = FALSE,
        closable = FALSE,
        zoomable = FALSE,
        width = 12,
        overflow = FALSE,
        echarts4rOutput(outputId = ns("attackStats"))
      )
    }
  })

}


# make R CMD check happy
globalVariables("parent")
globalVariables("child")
globalVariables("value")
