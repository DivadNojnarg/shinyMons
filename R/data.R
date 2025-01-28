#' Pokemon global data
#'
#' Aggregated list of pokemon data
#'
#' @format ## `who`
#' A list with `r length(poke_data)` elements:
#' \describe{
#'   \item{name}{Pokemon name}
#'   \item{description}{Pokemon description}
#'   \item{shape}{Pokemon shape}
#'   ...
#' }
#' @source <https://pokeapi.co/docs/v2>
"poke_data"

#' Pokemon attacks data
#'
#' Aggregated list of pokemon attacks data
#'
#' @format ## `who`
#' A list with `r length(poke_attacks)` elements.
#' @source <https://pokeapi.co/docs/v2>
"poke_attacks"

#' Pokemon network data
#'
#' Useful data to reconstruct with visNetwork htmlWidget.
#'
#' @format ## `who`
#' A list with nodes and edges dataframes. For the node dataframe:
#' \describe{
#'   \item{id}{Pokemon id}
#'   \item{label}{Pokemon name}
#'   \item{group}{Pokemon family}
#'   \item{image}{Pokemon sprite}
#'   \item{shape}{Node shape}
#'   \item{value}{Node value}
#' }
#' For edges:
#' \describe{
#'   \item{from}{Pokemon previous stage}
#'   \item{to}{Pokemon next stage}
#'   \item{title}{Evolution detail}
#'   \item{arrow}{Direction of arrows}
#' }
#' @source <https://pokeapi.co/docs/v2#evolution-section>
"poke_network"

globalVariables(c("poke_data", "poke_network", "poke_attacks"))
