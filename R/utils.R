# # Run this script to rebuild a clean pokemon file
# library(dplyr)
# library(parallel)
# source("pokeNames.R")
#
# # create the pokemon object
# pokeApi <- "https://pokeapi.co/api/v2/pokemon/"
# pokeMain <- mclapply(seq_along(pokeNames), FUN = function(i) {
#   fromJSON(paste0(pokeApi, i, "/"))
# })
# names(pokeMain) <- pokeNames
#
# saveRDS(pokeMain, file = "pokeMain")
#
# # contains more pokedex details than the pokemons object itself
# pokeDetails <- mclapply(seq_along(pokeNames), FUN = function(i) {
#   url <- pokeMain[[i]]$species$url
#   fromJSON(url)
# })
#
# saveRDS(pokeDetails, file = "pokeDetails")
# names(pokeDetails) <- pokeNames


# there are locations not related to the first generation ...
#locationUrl <- mclapply(names(pokeMain), function(i) {
#  locationUrl <- fromJSON(pokeMain[[i]]$location_area_encounters)
#  if (length(locationUrl) > 0) {
#    locationArea <- locationUrl$location_area
#    locationArea  %>%
#      filter(str_detect(name, pattern = "kanto")) %>%
#      select(name)
#  } else {
#    NULL
#  }
#})
#
#names(locationUrl) <- pokeNames
#saveRDS(locationUrl, file = "pokeLocations")

# preprocess abilities
#pokeMoves <- lapply(names(pokeMain), function(i) {
#  abilities <- pokeMain[[i]]$abilities
#  lapply(seq_along(abilities$slot), function(j) {
#    moveUrl <- abilities$ability$url[[j]]
#    # potentially bottleneck
#    moveDetails <- fromJSON(moveUrl)
#    moveEffect <- moveDetails$effect_entries$short_effect
#    moveId <- moveDetails$id
#    moveSlot <- abilities$slot[[j]]
#    cbind(abilities$ability[j, ], moveId, moveEffect, moveSlot)
#  })
#})
#names(pokeMoves) <- pokeNames
#saveRDS(pokeMoves, file = "pokeMoves")

#
# # In what follows we need data only for the first generation of pokemons
# firstGen <- fromJSON("https://pokeapi.co/api/v2/generation/1/")
# saveRDS(firstGen, file = "firstGen")
#
# # pokemon types
# pokeTypes <- firstGen$types
#
# # pokemon moves
# pokeMoves <- firstGen$moves
#
# # locations (maybe useful)
# kantoLocations <- fromJSON(firstGen$main_region$url)
