# # # Run this script to rebuild a clean pokemon file
#  library(dplyr)
#  library(parallel)
#  source("pokeNames.R")
#
#  # create the pokemon object
#  pokeApi <- "https://pokeapi.co/api/v2/pokemon/"
#  pokeMain <- mclapply(seq_along(pokeNames), FUN = function(i) {
#    fromJSON(paste0(pokeApi, i, "/"))
#  })
#  names(pokeMain) <- pokeNames
#
#  saveRDS(pokeMain, file = "pokeMain")
#
#
#
# # contains more pokedex details than the pokemons object itself
# pokeDetails <- mclapply(seq_along(pokeNames), FUN = function(i) {
#   url <- pokeMain[[i]]$species$url
#   fromJSON(url)
# })
# names(pokeDetails) <- pokeNames
# saveRDS(pokeDetails, file = "pokeDetails")
#
#
#
# # there are locations not related to the first generation ...
# locationUrl <- mclapply(names(pokeMain), function(i) {
#   locationUrl <- fromJSON(pokeMain[[i]]$location_area_encounters)
#   if (length(locationUrl) > 0) {
#     locationArea <- locationUrl$location_area
#     locationArea  %>%
#       filter(str_detect(name, pattern = "kanto")) %>%
#       select(name)
#   } else {
#     NULL
#   }
# })
#
# names(locationUrl) <- pokeNames
# saveRDS(locationUrl, file = "pokeLocations")
#
#
#
# # preprocess abilities
# pokeMoves <- lapply(names(pokeMain), function(i) {
#   abilities <- pokeMain[[i]]$abilities
#   lapply(seq_along(abilities$slot), function(j) {
#     moveUrl <- abilities$ability$url[[j]]
#     # potentially bottleneck
#     moveDetails <- fromJSON(moveUrl)
#     moveEffect <- moveDetails$effect_entries$short_effect
#     moveId <- moveDetails$id
#     moveSlot <- abilities$slot[[j]]
#     cbind(abilities$ability[j, ], moveId, moveEffect, moveSlot)
#   })
# })
# names(pokeMoves) <- pokeNames
# saveRDS(pokeMoves, file = "pokeMoves")
#
#
#
# # preprocess types
# pokeTypes <- lapply(names(pokeMain), function(i) {
#   types <- pokeMain[[i]]$types
#   lapply(seq_along(types$slot), function(j) {
#     typeName <- types$type[["name"]][[j]]
#     typeSlot <- types$slot[[j]]
#     damageRelations <- fromJSON(types$type[["url"]][[j]])$damage_relations
#     double_damage_from <- damageRelations$double_damage_from$name
#     double_damage_to <- damageRelations$double_damage_to$name
#     half_damage_from <- damageRelations$half_damage_from$name
#     half_damage_to <- damageRelations$half_damage_to$name
#     no_damage_from <- damageRelations$no_damage_from$name
#     no_damage_to <- damageRelations$no_damage_to$name
#
#     list(
#       name = typeName,
#       slot = typeSlot,
#       double_damage_from = double_damage_from,
#       double_damage_to = double_damage_to,
#       half_damage_from = half_damage_from,
#       half_damage_to = half_damage_to,
#       no_damage_from = no_damage_from,
#       no_damage_to = no_damage_to
#     )
#   })
# })
# names(pokeTypes) <- pokeNames
# saveRDS(pokeTypes, file = "pokeTypes")
#
#
#
# # preprocess evolutions
# pokeEvolutions <- lapply(names(pokeDetails), function(i) {
#   fromJSON(pokeDetails[[i]]$evolution_chain$url, flatten = TRUE)$chain
# })
# names(pokeEvolutions) <- pokeNames
# saveRDS(pokeEvolutions, file = "pokeEvolutions")
#
#
# # preprocess attacks
# firstGen <- readRDS("firstGen")
# pokeAttacks <- mclapply(seq_along(firstGen$moves$url), function(i) {
#   fromJSON(firstGen$moves$url[[i]])
# })
# names(pokeAttacks) <- firstGen$moves$name
# saveRDS(pokeAttacks, file = "pokeAttacks")
#
#
#
# # # In what follows we need data only for the first generation of pokemons
# # firstGen <- fromJSON("https://pokeapi.co/api/v2/generation/1/")
# # saveRDS(firstGen, file = "firstGen")
# #
# # # pokemon types
# # pokeTypes <- firstGen$types
# #
# # # pokemon moves
# # pokeMoves <- firstGen$moves
# #
# # # locations (maybe useful)
# # kantoLocations <- fromJSON(firstGen$main_region$url)
#
#
# # build pokemon families (preprocess)
# pokeMain <- readRDS("pokeMain")
# family <-  vector("list", length = length(pokeNames))
# evolutions <- readRDS("pokeEvolutions")
# details <- readRDS("pokeDetails")
#
#
# for (i in seq_along(pokeNames)) {
#   pokemon <- pokeNames[[i]]
#
#   if (pokemon == "Eevee") {
#     family[[i]]$from <- rep(pokemon, 3)
#     family[[i]]$to <- stringr::str_to_title(evolutions[[i]]$evolves_to$species.name[c(1:3)])
#     # increment i from 3 species
#     i <- i + 3
#   } else {
#
#     from_specie <- details[[pokemon]]$evolves_from_species$name
#     # handle the case for pokemon that have babies (pikachu ...)
#     if(!is.null(from_specie)) {
#       if (from_specie == "pichu" | from_specie == "cleffa" | from_specie == "igglybuff" |
#           from_specie == "smoochum" | from_specie == "elekid" | from_specie == "magby") {
#         is_starter <- TRUE
#       } else {
#         is_starter <- FALSE
#       }
#     } else {
#       is_starter <- TRUE
#     }
#
#     # if the pokemon is a starter, we try to find its evolution(s)
#     if (is_starter) {
#       evolution_1 <- evolutions[[i]]$evolves_to$species.name
#
#       # check for the first evolution
#       if (!is.null(evolution_1)) {
#         evolution_1 <- stringr::str_to_title(evolution_1)
#
#         # handle the case of multiple evolutions at first stage
#         # not possible in the first gen
#         if (length(evolution_1) > 1) evolution_1 <- evolution_1[1]
#         # check if the first evolution belongs to the first gen
#         if (evolution_1 %in% pokeNames) {
#           evolution_2 <- evolutions[[i]]$evolves_to$evolves_to[[1]]$species.name
#
#           # check for second evolution,
#           if (!is.null(evolution_2)) {
#             # also look for a third evolution
#             # which is not possible in the first gen
#             if (length(evolution_2) == 1) {
#               evolution_2 <- stringr::str_to_title(evolution_2)
#               # check if evolution 2 belongs to the first gen
#               if (evolution_2 %in% pokeNames) {
#                 if (pokemon != evolution_1) {
#                   family[[i]]$pokemons <- c(pokemon, evolution_1, evolution_2)
#                   family[[i]]$from <- c(pokemon, evolution_1)
#                   family[[i]]$to <- c(evolution_1, evolution_2)
#                 } else {
#                   family[[i]]$pokemons <- c(pokemon, evolution_2)
#                   family[[i]]$from <- pokemon
#                   family[[i]]$to <- evolution_2
#                 }
#               } else {
#                 if (pokemon != evolution_1) {
#                   family[[i]]$pokemons <- c(pokemon, evolution_1)
#                   family[[i]]$from <- pokemon
#                   family[[i]]$to <- evolution_1
#                 }
#               }
#             } else {
#               evolution_2 <- stringr::str_to_title(evolution_2[1])
#               # check if evolution 2 belongs to the first gen
#               if (evolution_2 %in% pokeNames) {
#                 family[[i]]$pokemons <- c(pokemon, evolution_1, evolution_2)
#                 family[[i]]$from <- c(pokemon, evolution_1)
#                 family[[i]]$to <- c(evolution_1, evolution_2)
#               } else {
#                 family[[i]]$pokemons <- c(pokemon, evolution_1)
#                 family[[i]]$from <- pokemon
#                 family[[i]]$to <- evolution_1
#               }
#             }
#           } else {
#             # handle Jynx, Electabuzz and Magmar
#             if (pokemon != evolution_1) {
#               family[[i]]$pokemons <- c(pokemon, evolution_1)
#               family[[i]]$from <- pokemon
#               family[[i]]$to <- evolution_1
#             }
#           }
#         }
#       }
#     }
#   }
# }
#
#
# family <- family[which(!sapply(family, is.null))]
#
#
# # transform name in ids
# family <- lapply(seq_along(family), function(i) {
#     m <- sapply(seq_along(family[[i]]$from), function(j) {
#       pokeIdFrom <- family[[i]]$from[j]
#       pokeIdTo <- family[[i]]$to[j]
#       c(pokeMain[[pokeIdFrom]]$id, pokeMain[[pokeIdTo]]$id)
#     })
#   list(from = m[1, ], to = m[2, ])
# })
#
#
# familyFrom <- unlist(lapply(seq_along(family), function(i) family[[i]]$from))
# familyTo <- unlist(lapply(seq_along(family), function(i) family[[i]]$to))
#
# family <- data.frame(from = familyFrom, to = familyTo)
#
# saveRDS(family, file = "pokeEdges")
