library(jsonlite)
library(parallel)
library(shiny)
library(tablerDash)
library(shinyWidgets)
library(shinyEffects)

source("pokeNames.R")

# create the pokemon object
pokeApi <- "https://pokeapi.co/api/v2/pokemon/"
pokemons <- mclapply(seq_along(pokeNames), FUN = function(i) {
  fromJSON(paste0(pokeApi, i, "/"))
})
names(pokemons) <- pokeNames

# take the first example bulbasaur
#bulbasaur <- pokemons[[1]]
#front_face <- bulbasaur$sprites$front_default
#browseURL(front_face)

# generate the profile cards
profileCards <- mclapply(seq_along(pokeNames), FUN = function(i) {
  tablerProfileCard(
    title = pokeNames[[i]],
    subtitle = NULL,
    background = NULL,
    src = pokemons[[i]]$sprites$front_default,
    socials = NULL,
    width = 4
  )
})


# shiny app code
shiny::shinyApp(
  ui = tablerDashPage(
    navbar = tablerDashNav(
      id = "mymenu",
      src = "https://preview.tabler.io/demo/brand/tabler.svg",
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
    title = "tablerDash",
    body = tablerDashBody(

      setZoom(class = "card"),
      chooseSliderSkin("Nice"),
      tablerTabItems(
        tablerTabItem(
          tabName = "Home",
          fluidRow(profileCards)
        ),
        tablerTabItem(
          tabName = "Test",
          "Item 2"
        )
      )
    )
  ),
  server = function(input, output, session) {}
)
