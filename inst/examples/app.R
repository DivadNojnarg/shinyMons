library(shiny)
library(shinyjs)
library(tablerDash)
library(shinyWidgets)
library(shinyEffects)
library(pushbar)
library(shinyMons)
library(waiter)

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
          tabName = "PokeInfo",
          icon = "home",
          "PokeInfo"
        ),
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

      poke_select_ui("select"),

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
        tags$script(
          "$(function () {
            $('#mymenu .nav-item a').click(function(){
              var tab = $(this).attr('id');
              if (tab == 'tab-PokeInfo' || tab == 'tab-PokeList') {
                $('#input-pokeChoice').show();
              } else {
                $('#input-pokeChoice').hide();
              }
            });
           });"
        ),

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
      setShadow(class = "galleryCard"),
      setZoom(class = "galleryCard"),

      tablerTabItems(
        tablerTabItem(
          tabName = "PokeInfo",
          fluidRow(
            column(
              width = 4,
              poke_infos_ui("infos"),
              poke_types_ui("types"),
              pokeEvolveUi(id = "evol")
            ),
            column(
              width = 8,
              poke_stats_ui("stats"),
              pokeMoveUi(id = "moves"),
              pokeLocationUi(id = "location")
            )
          )
        ),
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

    # main module (data)
    main <- poke_select_server("select", selected = reactive(NULL)) #network$selected

    # infos module
    poke_infos_server("infos", selected = main$poke_select, shiny = main$is_shiny)

    # stats module
    poke_stats_server("stats", selected = main$poke_select)
    # types modules
    poke_types_server("types", selected = main$poke_select)
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
