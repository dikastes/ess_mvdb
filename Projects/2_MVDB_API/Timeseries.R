library(lubridate)
library(magrittr)

# Zeitserien

# Zeige die Entwicklung der Auflagen von PE_00043
data <- search(q='PE_00043')
View(data)

#prints <- data %>%
#  mutate(prints = data %>% select(prints))
View(data)

repair = 'unique'
prints <- data %>%
  unnest('published_subitems', names_repair = repair) %>%
  unnest('prints', names_repair = repair) %>%
  select(date_of_action, Quantität = quantity) %>%
  mutate(Jahr = year(date_of_action)) %>%
  arrange(Jahr)
View(prints)

prints %>% ggplot(aes(x = Jahr, y = Quantität)) +
  #geom_point()
  #geom_col()
  #geom_violin()
  geom_line()

# Eine realistischere Darstellung im Line-Plot erhalten wir, indem wir
# für fehlende Jahre Nullen ergänzen.
# Hinweis: x <- x %>% y kann abgekürzt werden zu x %<>% y
rg <- tibble (Jahr = 1870:1930)
prints %<>% 
  full_join(rg) %>%
  replace_na(list(Quantität = 0))

# Über die Berechnung eines gleitenden Mittelwertes kann man die Kurve
# glätten
get_ma <- function(dataset, width) {
  dataset %>%
    mutate(
      Jahr = Jahr + width,
      Quantität = -Quantität
    ) %>%
    rbind( dataset ) %>%
    group_by( Jahr ) %>%
    summarise( Quantität = sum(Quantität) ) %>%
    mutate( glMW = cumsum(Quantität / width) )
}
get_ma_voice <- function(dataset, width) {
  dataset %>%
    mutate(
      Jahr = Jahr + width,
      Quantität = -Quantität
    ) %>%
    rbind( dataset ) %>%
    group_by( Jahr, Stimme ) %>%
    summarise( Quantität = sum(Quantität) ) %>%
    group_by( Stimme ) %>%
    mutate( glMW = cumsum(Quantität / width) )
}

prints_ma <- prints %>% get_ma(7)
prints_ma %>% ggplot(aes(x = Jahr, y = glMW)) +
  geom_line()

# Zeige die Entwicklung der Auflagen von PE_00023
data <- search(q='PE_00023')
View(data)

prints <- data %>%
  unnest('published_subitems', names_repair = repair) %>%
  unnest('prints', names_repair = repair) %>%
  select(
    date_of_action, 
    Quantität = quantity,
    Stimme = voice
  ) %>%
  mutate(Jahr = year(date_of_action)) %>%
  arrange(Jahr)
View(prints)

prints_null <- prints %>%
  full_join(rg) %>%
  complete(Jahr, Stimme, fill = list(Quantität = 0))
View(prints_null)

prints_sum <- prints %>%
  group_by(year) %>%
  summarise( Total = sum(Quantität) )
View(prints_sum)

prints_ma <- prints_null %>% get_ma_voice(7)
View(prints_ma)

# Dotplot
prints %>% ggplot(aes(x = Jahr, y = Stimme)) +
  geom_point(aes(size = Quantität)) +
  geom_line() +
  theme_minimal()

# 
prints_null %>% ggplot(aes(x = Jahr, y = Quantität)) +  
  #geom_point()
  #geom_point(aes(color = Stimme, alpha = .1))
  #geom_col(aes(fill = Stimme), position=position_dodge2())
  geom_line(aes(color = Stimme, alpha = .3))

# Lineplot mit gleitendem Mittelwert
prints_ma %>% ggplot(aes(x = Jahr, y = glMW)) +
  geom_line(aes(color = Stimme))
# 