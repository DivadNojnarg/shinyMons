library(shiny)
library(shinyjs)
library(tablerDash)
library(shinyWidgets)
library(shinyEffects)
library(pushbar)
library(shinyMons)
library(waiter)

source("pokeNames.R")

# main data
pokeMain <- readRDS("pokeMain")
pokeDetails <- readRDS("pokeDetails")

# subdata from main
pokeLocations <- readRDS("pokeLocations")
pokeMoves <- readRDS("pokeMoves")
pokeTypes <- readRDS("pokeTypes")
pokeEvolutions <- readRDS("pokeEvolutions")
pokeAttacks <- readRDS("pokeAttacks")
pokeEdges <- readRDS("pokeEdges")
pokeGroups <- readRDS("pokeGroups")

# pokemon sprites
pokeSprites <- vapply(
  seq_along(pokeNames),
  FUN = function(i) {
    pokeMain[[i]]$sprites$front_default
  },
  FUN.VALUE = character(1)
)

# shiny app code
shiny::shinyApp(
  ui = f7Page(
    init = f7Init(
      skin = "ios",
      theme = "dark",
      color = "orange",
      filled = FALSE,
      hideNavOnPageScroll = FALSE,
      hideTabsOnPageScroll = FALSE,
      serviceWorker = "service-worker.js",
      iosTranslucentBars = TRUE
    ),
    f7TabLayout(
      panels = tagList(
        f7Panel(
          inputId = "panelLeft",
          title = "Left Panel",
          side = "left",
          theme = "light",
          "Blabla",
          effect = "reveal"
        ),
        f7Panel(
          title = "Right Panel",
          side = "right",
          theme = "dark",
          "Blabla",
          effect = "cover"
        )
      ),
      navbar = f7Navbar(
        title = "shinyMons",
        subtitle = "for Shiny",
        hairline = TRUE,
        shadow = TRUE,
        left_panel = TRUE,
        right_panel = TRUE,
        bigger = TRUE,
        transparent = TRUE
      ),
      f7Tabs(
        id = "tabset",
        animated = FALSE,
        swipeable = TRUE,
        pokeInfosUi(id = "infos")
      )
    )
  ),
  server = function(input, output, session) {
    # Network module: network stores a potential selected node in the
    # network and pass it to the pickerInput function in the main
    # module to update its value
    #network <- callModule(
    #  module = pokeNetwork,
    #  id = "network",
    #  mainData = pokeMain,
    #  details = pokeDetails,
    #  families = pokeEdges,
    #  groups = pokeGroups,
    #  mobile = isMobile
    #)

    # main module (data)
    main <- callModule(
      module = pokeInput,
      id = "input",
      mainData = pokeMain,
      sprites = pokeSprites,
      details = pokeDetails,
      selected = NULL #network$selected
    )

    # infos module
    callModule(
      module = pokeInfos,
      id = "infos",
      mainData = pokeMain,
      details = pokeDetails,
      selected = main$pokeSelect,
      shiny = main$pokeShiny
    )

    # stats module
    #callModule(module = pokeStats, id = "stats", mainData = pokeMain, details = pokeDetails, selected = main$pokeSelect)
    # types modules
    #callModule(module = pokeType, id = "types", types = pokeTypes, selected = main$pokeSelect)
    # moves module
    #callModule(module = pokeMove, id = "moves", selected = main$pokeSelect, moves = pokeMoves)

    # evolutions module
    #callModule(
    #  module = pokeEvolve,
    #  id = "evol",
    #  mainData = pokeMain,
    #  details = pokeDetails,
    #  selected = main$pokeSelect,
    #  shiny = main$pokeShiny,
    #  evolutions = pokeEvolutions
    #)

    # fights module
    #callModule(
    #  module = pokeFight,
    #  id = "fights",
    #  mainData = pokeMain,
    #  sprites = pokeSprites,
    #  attacks = pokeAttacks,
    #  types = pokeTypes
    #)

    # location
    #callModule(module = pokeLocation, id = "location", selected = main$pokeSelect, locations = pokeLocations)
    # gallery module
    #callModule(module = pokeGallery, id = "gallery", mainData = pokeMain, details = pokeDetails, shiny = main$pokeShiny)
    # pokemon attacks
    #callModule(module = pokeAttack, id = "attacks", attacks = pokeAttacks)
    # other elements
    #callModule(module = pokeOther, id = "other", mainData = pokeMain, details = pokeDetails)

  }
)
