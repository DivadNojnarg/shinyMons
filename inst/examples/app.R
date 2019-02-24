library(jsonlite)
library(shiny)
library(tablerDash)
library(shinyWidgets)
library(shinyEffects)
library(echarts4r)
library(parallel)
library(stringr)
library(dplyr)

source("pokeNames.R")

pokeMain <- readRDS("pokeMain")
pokeDetails <- readRDS("pokeDetails")
firstGen <- readRDS("firstGen")

pokeLocations <- readRDS("pokeLocations")
pokeMoves <- readRDS("pokeMoves")
pokeTypes <- readRDS("pokeTypes")
pokeEvolutions <- readRDS("pokeEvolutions")


#pokeTypes <- firstGen$types
#pokeMoves <- firstGen$moves

# shiny app code
shiny::shinyApp(
  ui = tablerDashPage(
    navbar = tablerDashNav(
      id = "mymenu",
      src = "https://www.ssbwiki.com/images/9/9c/Master_Ball_Origin.png",
      navMenu = tablerNavMenu(
        tablerNavMenuItem(
          tabName = "PokeFilter",
          icon = "home",
          "PokeFilter"
        ),
        tablerNavMenuItem(
          tabName = "PokeGroup",
          icon = "box",
          "PokeGroup"
        )
      ),

      pokeDataUi(id = "data"),

      tablerDropdown(
        tablerDropdownItem(
          title = "Item 1 title",
          href = "http://google.com",
          url = "https://image.flaticon.com/icons/svg/1301/1301804.svg",
          status = "danger",
          date = "now",
          "This is the first dropdown item"
        ),
        tablerDropdownItem(
          url = "https://image.flaticon.com/icons/svg/1301/1301809.svg",
          status = "warning",
          "This is the second dropdown item",
          date = "yesterday"
        ),
        tablerDropdownItem(
          title = "Item 3 title",
          "This is the third dropdown item"
        )
      )
    ),
    footer = tablerDashFooter(copyrights = "@David Granjon, 2019"),
    title = "The fucking Pokemon App",
    body = tablerDashBody(

      # custom shinyWidgets skins
      chooseSliderSkin("Nice"),

      tablerTabItems(
        tablerTabItem(
          tabName = "PokeFilter",
          fluidRow(
            column(
              width = 4,
              pokeInfosUi(id = "infos"),
              pokeTypeUi(id = "types"),
              pokeEvolveUi(id = "evol")
            ),
            column(
              width = 8,
              pokeStatsUi(id = "stats")
            )
          ),
          pokeMoveUi(id = "moves"),
          pokeLocationUi(id = "location")
        ),
        tablerTabItem(
          tabName = "PokeGroup",
          pokeGalleryUi(id = "gallery")
        )
      )
    )
  ),
  server = function(input, output, session) {

    # main module (data)
    main <- callModule(module = pokeData, id = "data", raw_data = pokeMain, raw_details = pokeDetails)

    # infos module
    callModule(
      module = pokeInfos,
      id = "infos",
      mainData = main$pokemons,
      details = main$details,
      selected = main$pokeSelect,
      shiny = main$pokeShiny
    )

    # stats module
    callModule(
      module = pokeStats,
      id = "stats",
      mainData = main$pokemons,
      details = main$details,
      skills = main$skills,
      selected = main$pokeSelect
    )

    # types modules
    callModule(
      module = pokeType,
      id = "types",
      types = pokeTypes,
      selected = main$pokeSelect
    )

    # moves module
    callModule(
      module = pokeMove,
      id = "moves",
      selected = main$pokeSelect,
      moves = pokeMoves
    )

    # evolutions module
    callModule(
      module = pokeEvolve,
      id = "evol",
      mainData = main$pokemons,
      details = main$details,
      selected = main$pokeSelect,
      shiny = main$pokeShiny,
      evolutions = pokeEvolutions
    )

    # location
    callModule(
      module = pokeLocation,
      id = "location",
      selected = main$pokeSelect,
      locations = pokeLocations
    )

    # gallery module
    callModule(
      module = pokeGallery,
      id = "gallery",
      raw_data = pokeMain,
      raw_details = pokeDetails,
      shiny = main$pokeShiny
    )
  }
)
