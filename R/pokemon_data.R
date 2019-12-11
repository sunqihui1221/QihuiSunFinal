#' Show statistics of pokemons, including weight, height and strength.
#'
#' @import httr
#' @import dplyr
#' @import tidyverse
#' @import jsonlite
#' @import xml2
#' @import rvest
#' @return A dataset containing statistics of pokemons
#' @export


pokemon_data <- function() {

  requireNamespace("httr")
  requireNamespace("dplyr")
  requireNamespace("tidyverse")
  requireNamespace("jsonlite")
  requireNamespace("xml2")
  requireNamespace("rvest")

  endpoint <- "https://pokeapi.co/api/v2/pokemon/?offset807&limit=807"
  data <- GET(endpoint)
  newdata <- fromJSON(content(data, as = "text"))
  pokemon <- newdata[[4]] %>% as.data.frame()
  detail=function(url){
    base=GET(url)
    infotb=fromJSON(content(base, as = "text"))
    weight=infotb['weight']
    height=infotb['height']
    fund=data.frame('weight'=weight,'height'=height)
    return (fund)
  }
  new_pk <- pokemon %>% mutate(df=map(url,detail))
  pokemon_final <- new_pk %>% unnest(df)
  pokemon_final %>% select(-url)
  getinfo <- function(name){
    website=paste0('https://pokemondb.net/pokedex/',name)
    g=GET(website)
    if(g$status_code!=200){return (NA)}
    poke <- read_html(website)

    stat <-html_nodes(poke,".vitals-table")
    table <- html_table(stat)[[4]]
    finaltable <- table%>%select(X1,X2)%>%pivot_wider(names_from = X1,values_from = X2)
    return (finaltable)
  }
  final <- pokemon_final %>% mutate(ability=map(name,getinfo))
  final_pokemon_data <- final %>% select(-url) %>% filter(!is.na(ability)) %>% unnest(ability) %>% rename(Pokemon_Name = name, Weight = weight, Height = height, Total_Ability = Total) %>% arrange(desc(Total_Ability))
  final_pokemon_data
}
