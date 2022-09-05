#
# Korrelation
#

if(!require('corrr')) install.packages('corrr')
if(!require('ggcorrplot')) install.packages('ggcorrplot')
library(tidyverse)
library(lubridate)
library(corrr)
library(ggcorrplot)

# Gibt es einen Zusammenhang zwischen Auflagenhöhe und -zeitpunkt
# bei Rieter-Biedermann?
rb <- search(q='RB_*', i='published_item', f='id')
repair <- 'unique'
prints <- rb %>%
  unnest(published_subitems, names_repair = repair) %>%
  filter(lengths(prints) > 0) %>%
  unnest(prints, names_repair = repair) %>%
  mutate(Datum = date_of_action %>% as.POSIXct %>% as.numeric) %>%
  select(Datum, Quantität = quantity)

View(prints)

# Wir schauen zunächst die Verteilung der Auflagenhöhen an.
prints %>% ggplot(aes(x = Quantität)) +
  geom_histogram()
# .. und die Verteilung der Auflagezeitpunkte
prints %>% ggplot(aes(x = Datum)) +
  geom_histogram()

# Ob Datenpunkte, die auf der Auflagenhöhen-Achse besonders weit links
# liegen, auf der Zeitpunkt-Achse auch weit links (oder sogar weit rechts)
# liegen, sehen wir im Scatterplot
prints %>% ggplot(aes(x = Datum, y = Quantität)) +
  geom_point(alpha=.1, position = position_jitter(width = .1, height = .1)) #+
  #geom_smooth(method = lm)

prints %>% ggplot(aes(x = Datum, y = Quantität)) +
  scale_y_continuous(trans = 'pseudo_log') +
  geom_point(alpha=.1, position = position_jitter(width = .1, height = .1)) #+
  #geom_smooth(method = lm)
prints %>% correlate()

# Gibt es einen Zusammenhang zwischen der Lebensdauer von Komponisten
# und den absoluten Auflagezahlen ihrer Werke?
composers <- search(i='person') %>%
  unnest(works, names_repair = repair) %>%
  filter(lengths(published_subitems) > 0) %>%
  unnest(published_subitems, names_repair = repair) %>%
  filter(lengths(prints) > 0) %>%
  unnest(prints, names_repair = repair)
View(composers)

composer_data <- composers %>%
  mutate(Alter = interval(date_of_birth, date_of_death)/days(1)) %>%
  group_by(gnd_id...12, Alter) %>%
  summarise(Total = sum(quantity)) %>%
  filter(!is.na(Alter))

composer_data %>% ggplot(aes(x = Alter)) +
  geom_histogram()
composer_data %>% ggplot(aes(x = Total)) +
  scale_x_continuous(labels=scales::comma) +
  geom_histogram()

composer_data %>% ggplot(aes(x = Alter, y = Total)) +
  geom_point(alpha = .3, position = position_jitter(width = .1, height = .1)) +
  scale_y_continuous(labels = scales::comma) #+
  #geom_smooth(method='lm')

composer_data %>% select(Alter, Total) %>% correlate

composer_data %>% ggplot(aes(x = Alter, y = Total)) +
  geom_point(alpha = .3, position = position_jitter(width = .1, height = .1)) +
  scale_y_continuous(trans='pseudo_log', labels = scales::comma)

# Evtl. sollten wir Komponisten ausfiltern, für die nur wenige Daten vorliegen
composer_data %>%
  filter(Total > 50000) %>% 
  ggplot(aes(x = Alter, y = Total)) +
  geom_point(alpha = .3, position = position_jitter(width = .1, height = .1)) +
  scale_y_continuous(labels = scales::comma)# +
  #geom_smooth(method='lm')

composer_data %>%
  filter(Total > 50000) %>% 
  correlate

# Korrelationen können über mehrere Faktoren in Korrelogrammen
# zusammengestellt werden
composer_mult <- composers %>%
  group_by(
    GebDat = date_of_birth %>% as.POSIXct %>% as.numeric,
    StrbDat = date_of_death %>% as.POSIXct %>% as.numeric,
    Druckdatum = date_of_action %>% as.POSIXct %>% as.numeric
  ) %>%
  filter(
    !is.na(GebDat),
    !is.na(StrbDat),
    !is.na(Druckdatum)
  ) %>%
  summarise(
    Total = sum(quantity)
  ) %>%
  cor
composer_mult %>% ggcorrplot(method = 'circle', lab = TRUE)

#
# Aufgaben
#

# Gibt es einen Zusammenhang zwischen der Anzahl der Werke, die für ein
# Instrument erfasst sind, und dem Auflagentotal für dieses Instrument?
instruments <- search(...)
instrument_works <- instruments %>%
  unnest(...) %>%
  unnest(...)
works_per_instrument <- instrument_works %>%
  group_by(...) %>%
  summarise(...)
instrument_total <- instrument_works %>%
  unnest(...) %>%
  unnest(...) %>%
  group_by(...) %>%
  summarise(...)
works_total <- works_per_instrument %>%
  inner_join(instrument_total, by=uid) %>%
  select(...)

# Histogramm 1
works_total %>% ggplot(...) +
# Histogramm 2
works_total %>% ggplot(...) +
# Scatterplot
works_total %>% ggplot(...) +
# Korrelationskoeffizient  
works_total %>% correlate

a <- 3:8
data <- tibble(a, b = a*2)
  
# Gibt es einen Zusammenhang zwischen Geburtsdatum, Sterbedatum und Alter von
# Komponisten und der Anzahl der erfassten Werke?
composer_data %>%