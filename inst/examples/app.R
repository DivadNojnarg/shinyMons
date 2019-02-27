library(jsonlite)
library(shiny)
library(tablerDash)
library(shinyWidgets)
library(shinyEffects)
library(echarts4r)
library(parallel)
library(stringr)
library(dplyr)
library(pushbar)
library(visNetwork)

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
          tabName = "PokeOther",
          icon = "box",
          "PokeOther"
        )
      ),

      pokeInputUi(id = "input"),

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
    title = "The fucking Pokemon App",
    body = tablerDashBody(

      # load pushbar dependencies
      pushbar_deps(),

      # custom jquery to hide some inputs based on the selected tag
      # actually tablerDash would need a custom input/output binding
      # to solve this issue once for all
      tags$head(
        tags$script(
          "$(function () {
            $('#mymenu .nav-item a').click(function(){
              var tab = $(this).attr('id');
              console.log(tab);
              if (tab == 'tab-PokeFilter' || tab == 'tab-PokeGroup') {
                $('#input-pokeChoice').show();
              } else {
                $('#input-pokeChoice').hide();
              }
            });
           });"
         )
      ),

      # custom shinyWidgets skins
      chooseSliderSkin("Nice"),

      # use shinyEffects
      setShadow(class = "galleryCard"),
      setZoom(class = "galleryCard"),

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
          fluidRow(
            pokeMoveUi(id = "moves"),
            pokeLocationUi(id = "location")
          )
        ),
        tablerTabItem(
          tabName = "PokeGroup",
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
          tabName = "PokeOther",
          pokeOtherUi(id = "other")
        )
      )
    )
  ),
  server = function(input, output, session) {

    # main module (data)
    main <- callModule(module = pokeInput, id = "input", mainData = pokeMain, details = pokeDetails)

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
    callModule(module = pokeStats, id = "stats", mainData = pokeMain, details = pokeDetails, selected = main$pokeSelect)
    # types modules
    callModule(module = pokeType, id = "types", types = pokeTypes, selected = main$pokeSelect)
    # moves module
    callModule(module = pokeMove, id = "moves", selected = main$pokeSelect, moves = pokeMoves)

    # evolutions module
    callModule(
      module = pokeEvolve,
      id = "evol",
      mainData = pokeMain,
      details = pokeDetails,
      selected = main$pokeSelect,
      shiny = main$pokeShiny,
      evolutions = pokeEvolutions
    )

    # location
    callModule(module = pokeLocation, id = "location", selected = main$pokeSelect, locations = pokeLocations)
    # gallery module
    callModule(module = pokeGallery, id = "gallery", mainData = pokeMain, details = pokeDetails, shiny = main$pokeShiny)
    # pokemon attacks
    callModule(module = pokeAttack, id = "attacks", attacks = pokeAttacks)
    # Network module
    callModule(module = pokeNetwork, id = "network", mainData = pokeMain)
    # other elements
    callModule(module = pokeOther, id = "other", mainData = pokeMain, details = pokeDetails)

  }
)
