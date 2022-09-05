# Wir laden das Tidyverse

if(!require('tidyverse')) install.packages('tidyverse')
if(!require('jsonlite')) install.packages('jsonlite')
library(tidyverse)

# und deklarieren Funktionen, die uns das Suchen erleichtern

config <- function(path, size = 10000) {
  # str_c() konkateniert strings
  url <- str_c(path, '?size=', size)
  url
}

search <- function(query = '', fields = '', index = 'published_item') {
  path <- 'https://musikverlage.slub-dresden.de/api/search'
  url <- config(path)
  url <- str_c(url, '&index=', index)
  if (query!= '') {
    url <- str_c(url, '&query=', query)
  }
  if (fields != '') {
    url <- str_c(url, '&fields=', fields)
  }
  
  print(str_c('querying ', url))
  doc <- jsonlite::fromJSON(url)
  if (doc$hits$total$value == 0) {
    print('No results')
    NULL
  } else {
    as_tibble(doc$hits$hits) %>%
      unnest(c('_source')) %>%
      select(!'_index':'uid')
  }
}