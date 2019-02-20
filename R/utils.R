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
