if(!require('tidyverse')) install.packages('tidyverse')
if(!require('jsonlite')) install.packages('jsonlite')
if(!require('magrittr')) install.packages('magrittr')
if(!require('lubridate')) install.packages('lubridate')
if(!require('zoo')) install.packages('zoo')

library('tidyverse')
library('jsonlite')
library('magrittr')
library('lubridate')
library('zoo')

config <- function(path, size = 10000) {
  str_c(path, '?size=', size)
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
      select(!'_index':'_score')
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
                'I detected a person.',
                'I\'ll only expand economics via works.',
                'For economics on editors or collection please expand manually.'
            )
        )
        type <- 'person'
    } else {
        type <- 'genre'
    }
    # conditional expansion
    print(type)
    if (type == 'instrument') {
        df %<>%
            rename( uid_instrument = uid ) %>%
            filter( lengths(instrumentations) > 0 ) %>%
            unnest( instrumentations, names_repair = repair ) %>%
            rename( uid_instrumentations = uid )
    }
    if (type == 'work') {
        df %<>% rename( uid_work = uid )
    }
    if (type == 'person') {
        df %<>% rename( uid_person = uid )
    }
    if (type == 'published_item') {
        df %<>% rename( uid_publisheditem = uid )
    }
    if (type == 'genre') {
        df %<>% rename( uid_genre = uid )
    }
    if (type %in% c('person', 'genre', 'instrument')) {
        df %<>%
            filter(lengths(works) > 0) %>%
            unnest( works, names_repair = repair ) %>%
            rename( uid_work = uid )
    }
    df %>%
        filter(lengths(published_subitems) > 0) %>%
        unnest( published_subitems, names_repair = repair ) %>%
        rename( uid_published_subitem = uid ) %>%
        filter(lengths(prints) > 0) %>%
        unnest( prints, names_repair = repair ) %>%
        rename( uid_print = uid )
}

plot_timeseries <- function(df, color = '', facet = '', movavg = 0) {
    if (!('quantity' %in% colnames(df))) {
        df %<>% expand_economics
    }
    df %<>%
        mutate(Jahr = year(date_of_action))
    df %>% summarise(d = min(Jahr)) -> min_year
    df %>% summarise(d = max(Jahr)) -> max_year
    tibble(Jahr = min_year$d : max_year$d) -> year_span
    print("color")
    print(color)
    print("facet")
    print(facet)
    if (color != '') {
        aesthetics <- aes_string(x = "Jahr", y = "Total", color = color)
        if (facet != '') {
            crossing(year_span, df %>% ungroup %>% select({{color}}, {{facet}}) %>% unique) ->
                joiner
            df %<>%
                select(uid_print, quantity, Jahr, {{color}}, {{facet}}) %>%
                unique %>%
                group_by_("Jahr", color, facet) %>%
                summarise(Total = sum(quantity))
            if (movavg) {
                df %<>% mutate(Total = rollmean(Total, movavg, na.pad=TRUE))
            }
            df %<>%
                right_join(joiner) %>%
                replace_na(list(Total = 0)) %>%
                ggplot(aesthetics) +
                    geom_line() +
                    facet_grid(rows = {{facet}}) +
                    theme_minimal()
        } else {
            crossing(year_span, df %>% ungroup %>% select({{color}}) %>% unique) ->
                joiner
            df %<>%
                select(uid_print, quantity, Jahr, {{color}}) %>%
                unique %>%
                group_by_("Jahr", color) %>%
                summarise(Total = sum(quantity))
            if (movavg) {
                df %<>% mutate(Total = rollmean(Total, movavg, na.pad=TRUE))
            }
            df %<>%
                right_join(joiner) %>%
                replace_na(list(Total = 0)) %>%
                ggplot(aesthetics) +
                    geom_line() +
                    theme_minimal()
        }
    } else {
        aesthetics <- aes(x = Jahr, y = Total)
        if (facet != '') {
            crossing(year_span, df %>% ungroup %>% select({{facet}}) %>% unique) ->
                joiner
            df %<>%
                select(uid_print, quantity, Jahr, {{facet}}) %>%
                unique %>%
                group_by_("Jahr", facet) %>%
                summarise(Total = sum(quantity))
            if (movavg) {
                df %<>% mutate(Total = rollmean(Total, movavg, na.pad=TRUE))
            }
            df %<>%
                right_join(joiner) %>%
                replace_na(list(Total = 0)) %>%
                ggplot(aesthetics) +
                    geom_line() +
                    facet_grid(rows = {{facet}}) +
                    theme_minimal()
        } else {
            df %<>%
                select(uid_print, quantity, Jahr) %>%
                unique %>%
                group_by_("Jahr") %>%
                summarise(Total = sum(quantity))
            if (movavg) {
                df %<>% mutate(Total = rollmean(Total, movavg, na.pad=TRUE))
            }
            df %<>%
                ggplot(aesthetics) +
                    geom_line() +
                    theme_minimal()
        }
    }
}

write_csvfile <- function(df, filename = 'out.csv') {
    if (!('quantity' %in% colnames(df))) {
        df %<>% expand_economics
    }
    write_csv(df, filename)
}
