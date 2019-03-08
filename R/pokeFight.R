#' UI module for generating the pokeFight section
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
#' @import shinyWidgets
pokeFightUi <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      tagAppendAttributes(actionButton(ns("go"), "Goooo!!!!"), class = "btn-outline-primary mx-2"),
      prettyRadioButtons(
        inputId = ns("pokeDifficulty"),
        label = "Difficulty",
        thick = TRUE,
        choices = c("Easy" = "easy", "Medium" = "medium", "No Way" = "noway"),
        selected = "medium",
        inline = TRUE,
        animation = "pulse",
        status = "info"
      )
    ),
    br(), br(),
    fluidRow(
      uiOutput(ns("poke_1"), class = "col-sm-6"),
      uiOutput(ns("poke_2"), class = "col-sm-6")
    )
  )
}


#' Function that calculate a given stat of a pokemon
#' based on its level.
#' @param level Current pokemon level.
#' @param base_stat Stat to be scaled.
#' @importFrom stats runif
#' @note To be used in \link{generate_pokemons}.
compute_stat <- function(level, base_stat) {
  # generate a variable stat number so that if
  # there are 2 different pokemons,
  # they do not have the same stats
  # It is of course rounded...
  # Formula taken from https://bulbapedia.bulbagarden.net/wiki/Statistic
  stat_var <- round(runif(1, 0, 20))
  stat <- round(( (base_stat + stat_var) * 2 * level ) / 100 + 10 + level)
  return(stat)
}



#' Function that generate 2 random pokemons.
#' Levels are between 1 and 100 and ids are between
#' 1 and 151
#' @param mainData Object containing the main pokemon data.
#' @param sprites Object containing pokemon images.
#' @param difficulty Game difficulty.
#' @param attacks Object containing pokemon attacks.
#' @importFrom stats runif
generate_pokemons <- function(mainData, sprites, difficulty, attacks) {

  # set up the game difficulty
  # might be adjusted ...
  coef <- switch(difficulty,
    "easy" = 1,
    "medium" = 1.2,
    "noway" = 2
  )

  poke_ids <- round(runif(n = 2, min = 1, max = 151))
  poke_lvl1 <- round(runif(n = 1, min = 1, max = 100))

  # handle the case where the first pokemon is at the upper border
  # the level cannot be higher than 100
  poke_lvl2 <- round(coef * poke_lvl1)
  if (poke_lvl2 >= 100) poke_lvl2 <- 100

  poke_lvls <- c(poke_lvl1, poke_lvl2)

  # pokemon 1
  id_1 <- poke_ids[[1]]

  defense_1_base <- mainData[[id_1]]$stats$base_stat[5]
  defense_1 <- compute_stat(poke_lvl1, defense_1_base)
  attack_1_base <- mainData[[id_1]]$stats$base_stat[4]
  attack_1 <- compute_stat(poke_lvl1, attack_1_base)
  hp_1_base <- mainData[[id_1]]$stats$base_stat[6]
  hp_1 <- compute_stat(poke_lvl1, hp_1_base)

  pokemon1 <- list(
    id = id_1,
    name = mainData[[id_1]]$name,
    type = mainData[[id_1]]$type,
    attacks = select_attacks(mainData, attacks, current_pokemon_id = id_1),
    sprite = sprites[[id_1]],
    lvl = poke_lvls[[1]],
    defense = defense_1,
    attack = attack_1,
    hp = hp_1,
    # hp_0 is the basal HP, do not update. Update hp instead
    hp_0 = hp_1
  )

  # pokemon 2
  id_2 <- poke_ids[[2]]

  defense_2_base <- mainData[[id_2]]$stats$base_stat[5]
  defense_2 <- compute_stat(poke_lvl2, defense_2_base)
  attack_2_base <- mainData[[id_2]]$stats$base_stat[4]
  attack_2 <- compute_stat(poke_lvl2, attack_2_base)
  hp_2_base <- mainData[[id_2]]$stats$base_stat[6]
  hp_2 <- compute_stat(poke_lvl2, hp_2_base)

  pokemon2 <- list(
    id = id_2,
    name = mainData[[id_2]]$name,
    type = mainData[[id_2]]$type,
    attacks = select_attacks(mainData, attacks, current_pokemon_id = id_2),
    sprite = sprites[[id_2]],
    lvl = poke_lvls[[2]],
    defense = defense_2,
    attack = attack_2,
    hp = hp_2,
    # hp_0 is the basal HP, do not update. Update hp instead
    hp_0 = hp_2
  )

  return(list(pokemon1, pokemon2))
}


#' Function that list all possible learnable moves for a pokemon
#' and select only 4 moves.
#' @param mainData Object containing the main pokemon data.
#' @param attacks Object containing all pokemon attacks data.
#' @param current_pokemon_id Id of the randomly generated pokemon.
#' @importFrom stats runif
#' @importFrom stringr str_split
#' @note Limitations: only physical moves are considered for the moment. This function
#' has to be called inside \link{generate_pokemons}.
select_attacks <- function(mainData, attacks, current_pokemon_id) {
  temp_moves <- mainData[[current_pokemon_id]]$moves

  # remove all moves not in the first gen (165 first attacks)
  moves_urls <- temp_moves$move$url
  bad_moves <- unlist(
    lapply(seq_along(moves_urls), function(i) {
      url <- moves_urls[[i]]
      url_split <- str_split(url, "/")[[1]]
      if (as.numeric(url_split[7]) > 165) i
    })
  )

  # moves name that can be learn by bulbasaur
  temp_moves<- temp_moves$move[-c(bad_moves), ]

  # randomly select 4 moves
  temp_moves_id <- round(runif(4, min = 1, max = length(temp_moves$name)))

  # select the corresponding 4 attacks in the attacks data object
  # to access all stats ...
  temp_attacks <- attacks[c(temp_moves_id)]
  return(temp_attacks)
}





#' Function that calculates the damages of an attack on a given opponent
#' @param current_attack The currently selected attack.
#' @param current_pokemon Id of the randomly generated pokemon who is attacking.
#' @param opponent Opponent type. Useful for effectiveness calculations.
#' @param types Object containing all pokemons types strenght and weaknesses.
#' @note As already explained, I only consider physical attacks...
#' @importFrom stats runif
#' @importFrom stringr str_to_title
calculate_damages <- function(current_attack, current_pokemon, opponent, types) {

  attack_target <- current_attack$target$name
  attack_type <- current_attack$type$name

  random <- runif(n = 1, min = 0.85, max = 1)


  targets <- if (attack_target == "all-opponents") {
    0.75
  } else if (attack_target == "selected-pokemon") {
    1
  }

  stab <- if (attack_type %in% opponent_types) {
    1.5
    # adaptability is missing from raw data
  } else {
    1
  }


  # Below we determine if the current attack is effecient
  # against the opponent. For that, we extract the opponent resistances
  # and weaknesses and see if the attack type is in one of these data.
  opponent_types <- types[[str_to_title(opponent$name)]]
  double_damage_from <- unlist(
    lapply(seq_along(opponent_types), function(i) {
      opponent_types[[i]]$double_damage_from
    })
  )
  half_damage_from <- unlist(
    lapply(seq_along(opponent_types), function(i) {
      opponent_types[[i]]$half_damage_from
    })
  )
  no_damage_from <- unlist(
    lapply(seq_along(opponent_types), function(i) {
      opponent_types[[i]]$no_damage_from
    })
  )


  type <- if (attack_type %in% double_damage_from) {
    2
  } else if (attack_type %in% half_damage_from) {
    0.5
  } else if (attack_type %in% no_damage_from) {
    0
  }

  # modifyer coefficient (pretty simple in the first gen)
  modifiyer <- targets * random * stab * type

  # the main formula
  # I think some attacks use special-stats instead of classic stats (psychic)
  # Here it is not the case.
  # Taken from https://pokemon.fandom.com/wiki/Damage_Calculation
  level <- current_pokemon$lvl
  power <- current_attack$power
  attack <- current_pokemon$attack
  defense <- opponent$defense

  damages <- ( ( ( (2 * level) / 5 + 2 ) * power * attack / defense ) / 50 + 2 ) * modifiyer

  # Below I decided to integrate the accuracy stat since it is crucial
  # An attack with 50% accuracy will not hit its target in 50% of times...
  accuracy <- current_attack$accuracy
  hit_prop <- c(rep(1, accuracy), rep(0, 100 - accuracy))
  prob_to_hit <- sample(hit_prop, 1)

  damages <- damages * prob_to_hit

  return(damages)
}





#' Server module for generating the pokeFight section
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param mainData Object containing the main pokemon data.
#' @param sprites Object containing pokemon images.
#' @param attacks Object containing pokemon attacks.
#'
#' @import tablerDash echarts4r
#' @importFrom stats rnorm
#'
#' @export
pokeFight <- function(input, output, session, mainData, sprites, attacks) {

  ns <- session$ns

  start_fight <- FALSE

  # create the pokemons when click on go
  pokemons <- eventReactive(input$go, {
    generate_pokemons(
      mainData,
      sprites,
      difficulty = input$pokeDifficulty,
      attacks = attacks
    )
  })

  observe({
    #print(pokemons()[[1]]$attack)
    #print(pokemons()[[1]]$defense)
    #print(pokemons()[[1]]$hp)
  })


  # when HP is 0, the game is lost
  observe({
    pokemons <- pokemons()
    if (pokemons[[1]]$hp <= 0 | pokemons[[2]]$hp <= 0) {
      if (pokemons[[1]]$hp <= 0) {
        tablerAlert(
          title = "Wasted",
          "You lost!",
          icon = "alert-triangle",
          status = "danger"
        )
      } else {
        tablerAlert(
          title = "Congrats",
          "You won!",
          icon = "alert-triangle",
          status = "success"
        )
      }
    }
  })

  # progress bar for HP
  # dynamically updated
  lapply(1:2, function(i) {
    output[[paste0("pokeHP_", i)]] <- renderUI({

      hp <- round(pokemons()[[i]]$hp / pokemons()[[i]]$hp_0 * 100)

      tablerProgress(
        value = hp,
        status = if (75 < hp & hp <= 100) {
          "green"
        } else if (50 < hp & hp <= 75) {
          "yellow"
        } else if (25 < hp & hp <= 50) {
          "orange"
        } else if (0 < hp & hp <= 25) {
          "red"
        },
        size = "md"
      )
    })
  })


  # render the first pokemon box
  lapply(1:2, function(i) {
    output[[paste0("poke_", i)]] <- renderUI({

      pokemons <- pokemons()
      sprite <- pokemons[[i]]$sprite
      name <- pokemons[[i]]$name
      lvl <- pokemons[[i]]$lvl
      hp <- pokemons[[i]]$hp
      attacks <- pokemons()[[i]]$attacks

      attacks <- sapply(seq_along(attacks), function(i) attacks[[i]]$name)

      tablerCard(
        title = NULL,
        options = NULL,
        footer = NULL,
        status = "red",
        statusSide = "top",
        collapsible = FALSE,
        collapsed = FALSE,
        closable = FALSE,
        zoomable = FALSE,
        width = 12,
        overflow = FALSE,
        fluidRow(
          column(
            width = 6,
            align = "center",
            tablerAvatar(
              url = sprite,
              size = "xxl"
            ),
            paste(name, "lvl:", lvl)
          ),
          column(
            width = 6,
            uiOutput(ns(paste0("pokeHP_", i)))
          )
        )
      )
    })
  })

}

