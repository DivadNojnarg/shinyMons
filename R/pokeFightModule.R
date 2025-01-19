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
  stat <- round(((base_stat + stat_var) * 2 * level) / 100 + 10 + level)
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

  defense_spe_1_base <- mainData[[id_1]]$stats$base_stat[2]
  defense_spe_1 <- compute_stat(poke_lvl1, defense_spe_1_base)
  attack_spe_1_base <- mainData[[id_1]]$stats$base_stat[3]
  attack_spe_1 <- compute_stat(poke_lvl1, attack_spe_1_base)

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
    defense_spe = defense_spe_1,
    attack = attack_1,
    attack_spe = attack_spe_1,
    hp = hp_1,
    # hp_0 is the basal HP, do not update. Update hp instead
    hp_0 = hp_1
  )

  # pokemon 2
  id_2 <- poke_ids[[2]]

  defense_spe_2_base <- mainData[[id_2]]$stats$base_stat[2]
  defense_spe_2 <- compute_stat(poke_lvl2, defense_spe_2_base)
  attack_spe_2_base <- mainData[[id_2]]$stats$base_stat[3]
  attack_spe_2 <- compute_stat(poke_lvl2, attack_spe_2_base)

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
    defense_spe = defense_spe_2,
    attack = attack_2,
    attack_spe = attack_spe_2,
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
  good_moves <- unlist(
    lapply(seq_along(moves_urls), function(i) {
      url <- moves_urls[[i]]
      url_split <- str_split(url, "/")[[1]]
      move_id <- as.numeric(url_split[7])
      if (move_id <= 165) {
        move_name <- temp_moves$move$name[[i]]
        # counter attack need previous damages to work
        # this is not supported right now
        if (move_name != "counter") {
          accuracy <- attacks[[move_name]]$accuracy
          if (!is.null(accuracy)) {
            # for the moment we do not select attack altering the status
            # this is not supported right now.
            damage_class <- attacks[[move_name]]$damage_class$name
            if (damage_class != "status") move_id
            # TO DO: need to handle attacks like flying or dig which take 2 turns to launch
          }
        }
      }
    })
  )

  # randomly select 4 moves
  temp_moves_id <- sample(good_moves, 4)

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
  damage_class <- current_attack$damage_class$name

  opponent_types <- types[[str_to_title(opponent$name)]]

  opponent_types_names <- unlist(
    lapply(seq_along(opponent_types), function(i) {
      opponent_types[[i]]$name
    })
  )

  random <- runif(n = 1, min = 0.85, max = 1)

  targets <- if (attack_target == "all-opponents" | attack_target == "all-other-pokemon") {
    0.75
  } else if (attack_target == "selected-pokemon" | attack_target == "random-opponent") {
    1
  }

  stab <- if (attack_type %in% opponent_types_names) {
    1.5
    # adaptability is missing from raw data
  } else {
    1
  }


  # Below we determine if the current attack is effecient
  # against the opponent. For that, we extract the opponent resistances
  # and weaknesses and see if the attack type is in one of these data.
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

  # TO DO: return a text saying: "it was super efficient",
  # "it was not efficient" to know what happend...
  type <- if (attack_type %in% double_damage_from) {
    2
  } else if (attack_type %in% half_damage_from) {
    0.5
  } else if (attack_type %in% no_damage_from) {
    0
  } else {
    1
  }

  # modifyer coefficient (pretty simple in the first gen)
  modifiyer <- targets * random * stab * type

  # the main formula taken from https://pokemon.fandom.com/wiki/Damage_Calculation
  level <- current_pokemon$lvl
  power <- if (!is.null(current_attack$power)) {
    current_attack$power
  } else {
    # handle variable power attacks
    # The bad new is that it depends on the current attack
    # There is not predifined rule
    if (current_attack$name == "super-fang") {
      opponent$hp / 2
    } else if (current_attack$name == "seismic-toss") {
      opponent$lvl
    } else if (current_attack$name == "low-kick") {
      50
    } else if (current_attack$name == "psywave") {
      level * round(runif(1, min = 1, max = 1.5), 1)
    }
  }
  attack <- current_pokemon$attack
  defense <- opponent$defense

  # handle the case where the attack involves special stats like psychic
  damages <- if (damage_class == "special") {
    attack_spe <- current_pokemon$attack_spe
    defense_spe <- opponent$defense_spe
    ((((2 * level) / 5 + 2) * power * attack_spe / defense_spe) / 50 + 2) * modifiyer
  } else {
    ((((2 * level) / 5 + 2) * power * attack / defense) / 50 + 2) * modifiyer
  }

  # Below I decided to integrate the accuracy stat since it is crucial
  # An attack with 50% accuracy will not hit its target in 50% of times...
  accuracy <- current_attack$accuracy
  hit_prop <- c(rep(1, accuracy), rep(0, 100 - accuracy))
  prob_to_hit <- sample(hit_prop, 1)

  # handle attacks that one shot
  ko <- current_attack$meta$category$name
  is_ko <- (ko == "ohko")

  damages <- if (is_ko) {
    opponent$hp
  } else {
    # handle multiple shot attacks such as fury attack
    max_hits <- current_attack$meta$max_hits
    if (!is.null(max_hits)) {
      min_hits <- current_attack$meta$min_hits
      n_hits <- round(runif(n = 1, min = min_hits, max = max_hits))
      damages <- rep(round(damages * prob_to_hit), n_hits)
      sum(damages)
    } else {
      round(damages * prob_to_hit)
    }
  }

  return(damages)
}




#' Function that generates a timelineItem for each attack the pokemon does.
#'
#' @param attacking Id of the randomly generated pokemon who is attacking.
#' @param opponent Opponent type. Useful for effectiveness calculations.
#' @param current_attack The currently selected attack.
#' @param damages Result of \link{calculate_damages} witht the current_attack.
#' TRUE in this case.
fight_History <- function(attacking, opponent, current_attack, damages) {
  # insert alert to send user feedback on the current attack results
  pokeName1 <- attacking$name
  pokeName2 <- opponent$name

  insertUI(
    selector = paste0("#", pokeName1, "_fightCard"),
    where = "afterBegin",
    ui = if (damages > 0) {
      status <- "success"
      tablerTimelineItem(
        title = "Event",
        date = Sys.time(),
        status = status,
        tablerAlert(
          paste(current_attack, "dealt", damages, "damages to", pokeName2),
          icon = "alert-triangle",
          status = status
        )
      )
    } else if (damages == opponent$hp) {
      status <- "purple"
      tablerTimelineItem(
        title = "Event",
        date = Sys.time(),
        status = status,
        tablerAlert(
          paste(pokeName2, "has been one shot by", current_attack),
          icon = "alert-triangle",
          status = status
        )
      )
    } else {
      status <- "danger"
      tablerTimelineItem(
        title = "Event",
        date = Sys.time(),
        status = status,
        tablerAlert(
          paste(current_attack, "missed its target"),
          icon = "alert-triangle",
          status = status
        )
      )
    }
  )
}



#' Server module for generating the pokeFight section
#'
#' @param input Shiny inputs.
#' @param output Shiny outputs.
#' @param session Shiny session.
#' @param mainData Object containing the main pokemon data.
#' @param sprites Object containing pokemon images.
#' @param attacks Object containing pokemon attacks.
#' @param types Object containing all pokemon types.
#'
#' @import tablerDash echarts4r waiter
#' @importFrom stats rnorm
#'
#' @export
pokeFight <- function(input, output, session, mainData, sprites, attacks, types) {
  ns <- session$ns

  # Randomly selects who starts the fight and init variables
  # This booleans will be update all along the current fight
  # to create a turn by turn system...
  who_starts <- sample(1:2, 1)
  rv <- reactiveValues(turn = who_starts, poke1 = NULL, poke2 = NULL, endGame = FALSE)


  # create the pokemons when click on go
  observeEvent(input$go,
    {
      # generate pokemons
      pokemons <- generate_pokemons(
        mainData,
        sprites,
        difficulty = input$pokeDifficulty,
        attacks = attacks
      )

      rv$poke1 <- pokemons[[1]]
      rv$poke2 <- pokemons[[2]]


      # Explicitly says who starts: delayed by the time of loader
      confirmSweetAlert(
        session = session,
        inputId = ns("startFight"),
        btn_labels = "Confirm",
        title = NULL,
        text = if (rv$turn == 1) {
          fluidRow(
            img(src = rv$poke1$sprite),
            paste(rv$poke1$name, "starts!")
          )
        } else {
          fluidRow(
            img(src = rv$poke2$sprite),
            paste(rv$poke2$name, "starts!")
          )
        },
        html = TRUE
      )


      # show a spinner
      show_waiter(
        color = "#1e90ff",
        tagList(
          spin_folding_cube(),
          "Loading your fight..."
        )
      )
      Sys.sleep(2)
      hide_waiter()
    },
    priority = 100000,
    ignoreInit = TRUE
  )


  # Fighting engine for pokemon 1
  lapply(seq_along(attacks), function(i) {
    current_attack <- names(attacks)[[i]]

    # depending on which pokemon starts...

    # event for the first pokemon
    observeEvent(input[[paste0("poke1_", current_attack)]], {
      if (!rv$endGame) {
        if (rv$turn == 1) {
          # calculate damages
          damages <- calculate_damages(
            current_attack = attacks[[current_attack]],
            current_pokemon = rv$poke1,
            opponent = rv$poke2,
            types = types
          )

          print(damages)
          print(current_attack)

          # put items in the timeline
          fight_History(
            attacking = rv$poke1,
            opponent = rv$poke2,
            current_attack = current_attack,
            damages = damages
          )

          # disable pokemon 1 attacks
          lapply(seq_along(rv$poke1$attacks), function(i) {
            selected <- paste0("poke1_", rv$poke1$attacks[[i]]$name)
            shinyjs::disable(selected)
          })

          # update pokemon 2 life accordingly
          rv$poke2$hp <- rv$poke2$hp - damages

          confirmSweetAlert(
            session = session,
            btn_labels = "Confirm",
            inputId = ns("poke2Confirm"),
            type = "warning",
            text = fluidRow(
              img(src = rv$poke2$sprite),
              paste("It's", rv$poke2$name, "turn!")
            ),
            danger_mode = TRUE
          )
        }
      }
    })
  })

  observeEvent(input$poke2Confirm, {
    req(input$poke2Confirm)
    if (input$poke2Confirm) rv$turn <- 2
  })


  # Fighting engine for pokemon 2
  observeEvent(c(input$startFight, input$poke2Confirm), {
    if (!rv$endGame) {
      if (rv$turn == 2) {
        # handle the case the attacking pokemon is controlled by the computer
        rand_id <- sample(seq_along(rv$poke2$attacks), 1)
        current_attack <- rv$poke2$attacks[[rand_id]]$name

        # calculate damages
        damages <- calculate_damages(
          current_attack = attacks[[current_attack]],
          current_pokemon = rv$poke2,
          opponent = rv$poke1,
          types = types
        )

        print(damages)
        print(current_attack)

        # put items in the timeline
        fight_History(
          attacking = rv$poke2,
          opponent = rv$poke1,
          current_attack = current_attack,
          damages = damages
        )

        # update pokemon 1 life accordingly
        rv$poke1$hp <- rv$poke1$hp - damages

        # enable pokemon 1 attacks
        lapply(seq_along(rv$poke1$attacks), function(i) {
          selected <- paste0("poke1_", rv$poke1$attacks[[i]]$name)
          shinyjs::enable(selected)
        })

        # delay a little bit since the calculation is instantaneou
        shinyjs::delay(2000, {
          confirmSweetAlert(
            session = session,
            inputId = ns("poke1Confirm"),
            btn_labels = "Confirm",
            type = "warning",
            text = fluidRow(
              img(src = rv$poke1$sprite),
              paste("It's", rv$poke1$name, "turn!")
            ),
            danger_mode = TRUE
          )
        })
      }
    }
  })


  observeEvent(input$poke1Confirm, {
    req(input$poke1Confirm)
    if (input$poke1Confirm) rv$turn <- 1
  })



  # when HP is 0, the game is lost or won depending on the pokemon
  observe({
    req(input$go > 0)
    if (rv$poke1$hp <= 0 | rv$poke2$hp <= 0) {
      if (rv$poke1$hp <= 0) {
        confirmSweetAlert(
          session = session,
          inputId = ns("restart"),
          btn_labels = "Reset",
          title = "Wasted",
          text = fluidRow(
            img(src = rv$poke2$sprite),
            paste(rv$poke2$name, "won")
          ),
          html = TRUE
        )
      } else {
        sendSweetAlert(
          session = session,
          btn_labels = "Reset",
          title = "Congrats",
          text = fluidRow(
            img(src = rv$poke1$sprite),
            paste(rv$poke1$name, "won")
          ),
          html = TRUE
        )
      }
      shinyjs::delay(2000, {
        shinyjs::click(id = "go")
      })
    }
  })

  # progress bar for HP
  # dynamically updated
  lapply(1:2, function(i) {
    output[[paste0("pokeHP_", i)]] <- renderUI({
      req(input$go > 0)

      hp <- round(rv[[paste0("poke", i)]]$hp / rv[[paste0("poke", i)]]$hp_0 * 100)

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
      req(input$go > 0)

      sprite <- rv[[paste0("poke", i)]]$sprite
      name <- rv[[paste0("poke", i)]]$name
      lvl <- rv[[paste0("poke", i)]]$lvl
      hp_0 <- rv[[paste0("poke", i)]]$hp_0
      hp <- rv[[paste0("poke", i)]]$hp
      attacks <- rv[[paste0("poke", i)]]$attacks

      attacks <- sapply(seq_along(attacks), function(i) attacks[[i]]$name)

      # generate the 4 attacks buttons
      # We consider unlimited number of attacks right now.
      # This make sense since a single fight does not last enough
      # to lose all pp. However, if later we decide to
      # chain fights with the same pokemon, it will be relevant.
      # Do not create any buttons for the second pokemon since all attacks
      # will be randomly selected... Surprise...
      attackBttns <- if (i == 1) {
        lapply(seq_along(attacks), function(j) {
          fluidRow(
            tagAppendAttributes(
              actionBttn(
                inputId = ns(paste0("poke1_", attacks[[j]])),
                label = attacks[[j]],
                style = "simple",
                block = TRUE,
                color = "warning"
              ),
              # we add a different class for each pokemon
              # used later by jQuery to identify on which side
              # a given button was clicked.
              class = "m-2 btn-outline-warning"
            )
          )
        })
      }

      # better layout
      attackBttns <- tagList(
        fluidRow(
          column(width = 6, attackBttns[[1]]),
          column(width = 6, attackBttns[[2]])
        ),
        fluidRow(
          column(width = 6, attackBttns[[3]]),
          column(width = 6, attackBttns[[4]])
        )
      )

      # pokemon fight card
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
            width = 4,
            align = "center",
            tablerAvatar(
              url = sprite,
              size = "xxl"
            ),
            br(),
            paste(name, "lvl:", lvl)
          ),
          column(
            width = 8,
            uiOutput(ns(paste0("pokeHP_", i))),
            div(align = "center", paste0(hp, "/", hp_0)),
            if (i == 1) attackBttns
          )
        ),
        tablerTimeline(id = paste0(name, "_fightCard"), style = "max-height: 400px; overflow-y: auto;")
      )
    })
  })
}
