library(jsonlite)
library(shiny)
library(tablerDash)
library(shinyWidgets)
library(shinyEffects)
library(echarts4r)

source("pokeNames.R")

pokeMain <- readRDS("pokeMain")
pokeDetails <- readRDS("pokeDetails")
firstGen <- readRDS("firstGen")

pokeTypes <- firstGen$types
pokeMoves <- firstGen$moves

# shiny app code
shiny::shinyApp(
  ui = tablerDashPage(
    navbar = tablerDashNav(
      id = "mymenu",
      src = "https://www.ssbwiki.com/images/9/9c/Master_Ball_Origin.png",
      navMenu = tablerNavMenu(
        tablerNavMenuItem(
          tabName = "Home",
          icon = "home",
          "Home"
        ),
        tablerNavMenuItem(
          tabName = "Test",
          icon = "box",
          "Test"
        )
      ),
      tablerIcon(name = "fr", lib = "flag"),
      tablerIcon(name = "ch", lib = "flag"),
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
    footer = tablerDashFooter(
      tablerIcon(name = "maestro", lib = "payment"),
      tablerIcon(name = "mastercard", lib = "payment"),
      copyrights = "@David Granjon, 2019"
    ),
    title = "The fucking Pokemon App",
    body = tablerDashBody(

      # custom shinyWidgets skins
      chooseSliderSkin("Nice"),

      tablerTabItems(
        tablerTabItem(
          tabName = "Home",
          pokeDataUi(id = "data"),
          pokeInfosUi(id = "infos")
        ),
        tablerTabItem(
          tabName = "Test",
          pokeStatsUi(id = "stats")
        )
      )
    )
  ),
  server = function(input, output, session) {
    main <- callModule(module = pokeData, id = "data", raw_data = pokeMain, raw_details = pokeDetails)
    callModule(module = pokeInfos, id = "infos", mainData = main$pokemons, details = main$details, pokeNames = main$pokeNames)
    callModule(module = pokeStats, id = "stats", skills = main$skills, pokeNames = main$pokeNames)
  }
)
