bulbasaur_details <- pokeDetails[[1]]
bulbasaur_evolve <- fromJSON(bulbasaur_details$evolution_chain)
bulbasaur_evolve$chain$evolves_to
bulbasaur_evolve$chain$evolves_to$species # give the parent
bulbasaur_evolve$chain$evolves_to$evolution_details # give details: min_level trigger


bulbasaur_details$evolves_from_species # useful for parents
