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


# preprocess types
#pokeTypes <- lapply(names(pokeMain), function(i) {
#  types <- pokeMain[[i]]$types
#  lapply(seq_along(types$slot), function(j) {
#    typeName <- types$type[["name"]][[j]]
#    typeSlot <- types$slot[[j]]
#    damageRelations <- fromJSON(types$type[["url"]][[j]])$damage_relations
#    double_damage_from <- damageRelations$double_damage_from$name
#    double_damage_to <- damageRelations$double_damage_to$name
#    half_damage_from <- damageRelations$half_damage_from$name
#    half_damage_to <- damageRelations$half_damage_to$name
#    no_damage_from <- damageRelations$no_damage_from$name
#    no_damage_to <- damageRelations$no_damage_to$name
#
#    list(
#      name = typeName,
#      slot = typeSlot,
#      double_damage_from = double_damage_from,
#      double_damage_to = double_damage_to,
#      half_damage_from = half_damage_from,
#      half_damage_to = half_damage_to,
#      no_damage_from = no_damage_from,
#      no_damage_to = no_damage_to
#    )
#  })
#})
#names(pokeTypes) <- pokeNames
#saveRDS(pokeTypes, file = "pokeTypes")

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
