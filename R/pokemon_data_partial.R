#' Show chosen statistics of pokemons
#'
#' @param para specific characteristic you want to check about pokemon data, like "weight", "height", "HP"...,set default to "Weight"
#' @import  dplyr
#' @return A dataset containing statistics of pokemons that chosen by the user
#' @export


pokemon_data_partial <- function(para = "Weight") {

  requireNamespace("dplyr")

  df <- pokemon_data()
  paracol <- select(df, para)
  return(cbind(df$Pokemon_Name, paracol))
}
