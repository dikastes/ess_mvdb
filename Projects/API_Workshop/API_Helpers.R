if(!require('tidyverse')) install.packages('tidyverse')
if(!require('jsonlite')) install.packages('jsonlite')
if(!require('magrittr')) install.packages('magrittr')
if(!require('lubridate')) install.packages('lubridate')
library(tidyverse)

config <- function(path, size = 10000) {
  paste(path, '?size=', size)
}

search <- function(query = '', fields = '', index = 'published_item') {
  path <- 'https://musikverlage.slub-dresden.de/api/search'
  url <- config(path)
  url <- paste(url, '&index=', index)
  if (query!= '') {
    url <- paste(url, '&query=', query)
  }
  if (fields != '') {
    url <- paste(url, '&fields=', fields)
  }
  print(paste('querying ', url))
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

expand_economics <- function(df) {
    type <- ''
    repair <- 'unique'
    cols <- colnames(df)
    # type detection
    if ('published_subitems' %in% cols && 'works' %in% cols) {
        type <- 'published_item'
    } else if ('tonality' %in% cols) {
        type <- 'work'
    } else if ('instrumentations' %in% cols) {
        type <- 'instrument'
    } else if ('date_of_birth' %in% cols) {
        print(
            paste(
                'I detected a person. ',
                'I\'ll only expand economics via works',
                'For economics on editors or collection please expand manually.'
            )
        )
        type <- 'person'
    } else {
        type <- 'genre'
    }
    print(type)
    # conditional expansion
    if (type == 'instrument') {
        df %<>%
            filter(lengths(instrumentations) > 0) %>%
            unnest( instrumentations, names_repair = repair )
    }
    if (type %in% c('person', 'genre', 'instrument')) {
        df %<>%
            filter(lengths(works) > 0) %>%
            unnest( works, names_repair = repair )
    }
    df %>%
        filter(lengths(published_subitems) > 0) %>%
        unnest( published_subitems, names_repair = repair ) %>%
        filter(lengths(prints) > 0) %>%
        unnest( prints, names_repair = repair )
}

plot_timeseries <- function(df, color = '', facet = '', movavg = 0) {
    if (!('quantity' %in% colnames(df))) {
        df %<>% expand_economics
    }
    df %<>%
        mutate(Jahr = year(date_of_action))
    if (color) {
        if (facet) {
            df %<>%
                select(Jahr, {{color}}, {{facet}}) %>%
                group_by(Jahr, {{color}}, {{facet}})
        } else {
            df %<>%
                select(Jahr, {{facet}}) %>%
                group_by(Jahr, {{facet}})
        }
    } else {
        if (facet) {
            df %<>%
                select(Jahr, {facet}) %>%
                group_by(Jahr, {facet})
        } else (facet) {
            df %<>%
                select(Jahr) %>%
                group_by(Jahr)
        }
    }
    df %<>%
        summarise(Total = sum(quantity))
}

write_csvfile <- function(df, filename = 'out.csv') {
    if (!('quantity' %in% colnames(df))) {
        df %<>% expand_economics
    }
    write_csv(df, filename)
}
