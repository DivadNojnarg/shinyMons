
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyMons <img src="man/figures/shinymons_hex.svg" width=200 align="right" alt="shinyMons hex sticker"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/shinyMons)](https://cran.r-project.org/package=shinyMons)
[![R-CMD-check](https://github.com/DivadNojnarg/shinyMons/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DivadNojnarg/shinyMons/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> shiny app for pokemons

<img src="man/figures/shinyMons.png" alt="shinyMons interface">

## Purpose of this App:

This app was designed for two reasons:

- propose something original for the upcoming [shiny
  contest](https://blog.rstudio.com/2019/01/07/first-shiny-contest/)
  event
- serve as a gallery for the RinteRface
  [tablerDash](https://github.com/RinteRface/tablerDash) dashboard
  template

## Original Data

The original data are taken from the amazing
[pokeApi](https://pokeapi.co). I built my own data from what I found
relevant in the API (see `data-waw` folder). Moreover, I chose to deal
only with the first 151 pokemons (because I played with them as a
child). Such a restriction is actually not as straightforward as it
seems to handle, since each element need to belong to the first pokemon
generation (while the API provides unfiltered data). There are features
that exist now but not in the first games (namely red/blue):

- eggs
- special objects
- day/night cycle
- mega evolutions
- …

## App skeleton

This app is packaged into 11 shiny modules that tightly interact
together to pass useful informations, such as the currently selected
pokemon. All outputs are mainly generated with the excellents
[echarts4r](https://github.com/JohnCoene/echarts4r) and
[visNetwork](https://datastorm-open.github.io/visNetwork/).

## Interface

This app is built on top of the Bootstrap 4
[tablerDash](https://github.com/RinteRface/tablerDash) template.
Furthermore, it includes the pushbar.js library implementation by [John
Coene](https://github.com/JohnCoene/pushbar). Combined together, this
allows to develop clean templates, especially for mobile devices.

## Further projects

- Develop a fight system based on pokemon’s stats
