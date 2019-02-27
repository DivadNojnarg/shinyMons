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

# preprocess evolutions
#pokeEvolutions <- lapply(names(pokeDetails), function(i) {
#  fromJSON(pokeDetails[[i]]$evolution_chain$url, flatten = TRUE)$chain
#})
#names(pokeEvolutions) <- pokeNames
#saveRDS(pokeEvolutions, file = "pokeEvolutions")


# preprocess attacks
#firstGen <- readRDS("firstGen")
#pokeAttacks <- mclapply(seq_along(firstGen$moves$url), function(i) {
#  fromJSON(firstGen$moves$url[[i]])
#})
#names(pokeAttacks) <- firstGen$moves$name
#saveRDS(pokeAttacks, file = "pokeAttacks")

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


# build pokemon families (preprocess)
source("pokeNames.R")
pokeMain <- readRDS("pokeMain")
family <-  vector("list", length = 9)
evolutions <- readRDS("pokeEvolutions")
details <- readRDS("pokeDetails")

# parallel for loop
for (i in 1:9) {

  print(i)
  # we start to build Bulbasaur's family and we know it has 2 evolutions
  # this is cheating ;)
  if (i == 1) {

    starter <- pokeNames[[i]]
    evolution_1 <- evolutions[[i]]$evolves_to$species.name

    # check for the first evolution
    if (!is.null(evolution_1)) {
      evolution_2 <- evolutions[[i]]$evolves_to$evolves_to[[1]]$species.name
      # check for second evolution
      if (!is.null(evolution_2)) {
        family[[i]]$from <- rep(starter, 2)
        family[[i]]$to <- stringr::str_to_title(c(evolution_1, evolution_2))
      }
    } else {
      family[[i]] <- NULL
    }

    # after building the first family
  } else {
    starter <- pokeNames[[i]]

    # check whether the starter is already included in the previous family
    # except the current one
    for (j in 1:(i-1)) {
      print(starter %in% unlist(family[[j]]))
      if (!is.na(family[j])) {
        if (starter %in% unlist(family[[j]])) {
          # then it means that this pokemon is included in the previous family
          # we do not include it
          family[[j + 1]] <- NA
        } else {
          # need to check for Eevee's family
          if (starter == "Eevee") {
            family[[i]]$from <- rep(starter, 3)
            family[[i]]$to <- evolutions[[i]]$evolves_to$species.name[c(1:3)]
          } else if (starter == "Vaporeon") {
            family[[i]] <- NA
          } else if (starter == "Jolteon") {
            family[[i]] <- NA
          } else if (starter == "Flareon") {
            family[[i]] <- NA
          } else {
            evolution_1 <- evolutions[[i]]$evolves_to$species.name

            # check for the first evolution
            if (!is.null(evolution_1)) {
              evolution_1 <- stringr::str_to_title(evolution_1)

              # handle the case of multiple evolutions at first stage
              # not possible in the first gen
              if (length(evolution_1) > 1) {
                evolution_1 <- evolution_1[1]
              }
              # check if the first evolution belongs to the first gen
              if (!evolution_1 %in% pokeNames) {
                family[[i]] <- NA
              } else {
                evolution_2 <- evolutions[[i]]$evolves_to$evolves_to[[1]]$species.name

                # check for second evolution,
                if (!is.null(evolution_2)) {
                  # also look for a third evolution
                  # which is not possible in the first gen
                  if (length(evolution_2) == 1) {
                    evolution_2 <- stringr::str_to_title(evolution_2)
                    # check if evolution 2 belongs to the first gen
                    if (evolution_2 %in% pokeNames) {
                      family[[i]]$from <- rep(starter, 2)
                      family[[i]]$to <- c(evolution_1, evolution_2)
                    } else {
                      family[[i]]$from <- starter
                      family[[i]]$to <- evolution_1
                    }
                  } else {
                    evolution_2 <- stringr::str_to_title(evolution_2[1])
                    # check if evolution 2 belongs to the first gen
                    if (evolution_2 %in% pokeNames) {
                      family[[i]]$from <- rep(starter, 2)
                      family[[i]]$to <- c(evolution_1, evolution_2)
                    } else {
                      family[[i]]$from <- starter
                      family[[i]]$to <- evolution_1
                    }
                  }
                } else {
                  family[[i]]$from <- starter
                  family[[i]]$to <- evolution_1
                }
              }
            } else {
              family[[i]] <- NA
            }
          }
        }
      }
    }
  }
}

# only get other family members
family <- lapply(seq_along(pokeNames), function(i) {
  if (pokeNames[[i]] %in% family[[i]]) {
    id <- match(pokeNames[[i]], family[[i]])
    if (!is.na(id)) family[[i]] <- family[[i]][-id]
  }
  unique(family[[i]])
})

# transform name in ids
family <- lapply(seq_along(pokeNames), function(i) {
  if (!is.na(family[i])) {
    unlist(
      lapply(seq_along(family[[i]]), function(j) {
        pokeId <- family[[i]][[j]]
        family[[i]][j] <- pokeMain[[pokeId]]$id
      })
    )
  }
})


saveRDS(family, file = "pokeEdges")
