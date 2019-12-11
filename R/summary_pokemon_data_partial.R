#' Show summary statistics of pokemons
#'
#' @param para specific characteristic you want to check about pokemon data, like "weight", "height", "HP"..., set default to "Weight"
#' @param summary TURE if you want to see the summary statistics, FALSE if you do not want to see, set default to TRUE
#' @import  dplyr
#' @return A dataset containing suammry statistics of pokemons that chosen by the user
#' @export



summary_pokemon_data_partial <- function(para = "Weight", summary = TRUE) {

  requireNamespace("dplyr")

  df <- pokemon_data()
  partial <- select(df, para)
  if (summary == TRUE) {
    return(summary(partial))
  }
}
