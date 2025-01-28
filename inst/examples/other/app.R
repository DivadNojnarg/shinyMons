library(shiny)
library(shinyjs)
library(tablerDash)
library(shinyWidgets)
library(shinyEffects)
library(pushbar)
library(shinyMons)
library(waiter)

deps <- htmltools::findDependencies(tablerDashPage())

# shiny app code
shiny::shinyApp(
  ui = tablerDashPage(
    enable_preloader = TRUE,
    loading_duration = 4,
    navbar = tablerDashNav(
      id = "mymenu",
      src = "https://www.ssbwiki.com/images/9/9c/Master_Ball_Origin.png",
      navMenu = tablerNavMenu(
        tablerNavMenuItem(
          tabName = "PokeList",
          icon = "box",
          "PokeList"
        ),
        tablerNavMenuItem(
          tabName = "PokeAttacks",
          icon = "box",
          "PokeAttacks"
        ),
        tablerNavMenuItem(
          tabName = "PokeNetwork",
          icon = "box",
          "PokeNetwork"
        ),
        tablerNavMenuItem(
          tabName = "PokeFight",
          icon = "box",
          "PokeFight"
        ),
        tablerNavMenuItem(
          tabName = "PokeOther",
          icon = "box",
          "PokeOther"
        )
      ),
      tablerDropdown(
        tablerDropdownItem(
          title = NULL,
          href = "https://pokeapi.co",
          url = "https://pokeapi.co/static/logo-6221638601ef7fa7c835eae08ef67a16.png",
          status = "success",
          date = NULL,
          "This app use pokeApi by Paul Hallet and PokéAPI contributors."
        )
      )
    ),
    footer = tablerDashFooter(
      copyrights = "Disclaimer: this app is purely intended for learning purpose. @David Granjon, 2019"
    ),
    title = "Gotta Catch'Em (Almost) All",
    body = tablerDashBody(
      # load pushbar dependencies
      pushbar_deps(),
      # laad the waiter dependencies
      use_waiter(),
      # load shinyjs
      useShinyjs(),

      # custom jquery to hide some inputs based on the selected tag
      # actually tablerDash would need a custom input/output binding
      # to solve this issue once for all
      tags$head(
        # test whether mobile or not
        tags$script(
          "$(document).on('shiny:connected', function(event) {
            var isMobile = /iPhone|iPad|iPod|Android/i.test(navigator.userAgent);
            Shiny.onInputChange('isMobile', isMobile);
          });
          "
        )
      ),

      # custom shinyWidgets skins
      chooseSliderSkin("Round"),

      # use shinyEffects
      shinyEffects::setShadow(class = "galleryCard"),
      shinyEffects::setZoom(class = "galleryCard"),

      tablerTabItems(
        tablerTabItem(
          tabName = "PokeList",
          pokeGalleryUi(id = "gallery")
        ),
        tablerTabItem(
          tabName = "PokeAttacks",
          pokeAttackUi(id = "attacks")
        ),
        tablerTabItem(
          tabName = "PokeNetwork",
          pokeNetworkUi(id = "network")
        ),
        tablerTabItem(
          tabName = "PokeFight",
          pokeFightUi(id = "fights")
        ),
        tablerTabItem(
          tabName = "PokeOther",
          pokeOtherUi(id = "other")
        )
      )
    )
  ),
  server = function(input, output, session) {
    # determine whether we are on mobile or not
    # relies on a simple Shiny.onInputChange
    isMobile <- reactive(input$isMobile)

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

    # fights module
    #callModule(
    #  module = pokeFight,
    #  id = "fights",
    #  mainData = pokeMain,
    #  sprites = pokeSprites,
    #  attacks = pokeAttacks,
    #  types = pokeTypes
    #)

    # gallery module
    #callModule(module = pokeGallery, id = "gallery", mainData = pokeMain, details = pokeDetails, shiny = main$pokeShiny)
    # pokemon attacks
    #callModule(module = pokeAttack, id = "attacks", attacks = pokeAttacks)
    # other elements
    #callModule(module = pokeOther, id = "other", mainData = pokeMain, details = pokeDetails)
  }
)
