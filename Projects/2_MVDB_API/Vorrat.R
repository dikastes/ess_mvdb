unnest_published_items <- function(df) {
  names_repair = 'unique'
  df %>%
    unnest_longer(published_subitems, names_repair = names_repair)# %>%
  #unnest_longer('published_subitems$prints', names_repair = names_repair)# %>%
  #unnest_wider(sales, names_repair = names_repair) %>%
  #unnest_wider(maculation, names_repair = names_repair)
}