#' @title Launch the pokemon app
#'
#' @description Unleash the pokemon app
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   run_pokemon()
#' }
run_pokemons <- function() {
  pkgs <- c(
    "jsonlite",
    "parallel",
    "shiny",
    "tablerDash",
    "shinyWidgets",
    "shinyEffects"
  )

  # handle missing packages
  lapply(seq_along(pkgs), FUN = function(i) {
    if (!requireNamespace(package = pkgs[[i]])) {
      message(paste0("Package '", pkgs[[i]], "' is required to run this function"))
    }
  })

  shiny::runApp(appDir = system.file("examples", package = "shinyMons", mustWork = TRUE))
}
