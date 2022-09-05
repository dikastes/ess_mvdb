#
# Kennwerte der zentralen Tendenz
#

if(!require('tidyverse')) install.packages('tidyverse')
if(!require('knitr')) install.packages('knitr')
library(tidyverse)
library(knitr)

# Wie hoch ist die typische Auflagenhöhe von Bargiel-Werken?
bargiel_works <- search(q='bargiel', i='work', f='composers.name')
View(bargiel_works)

repair = 'unique'
bargiel_prints <- bargiel_works %>%
  unnest( published_subitems, names_repair = repair ) %>%
  unnest( prints, names_repair = repair ) %>%
  select( 
    Titel = generic_title,
    Band = part,
    Stimme = voice,
    Quantität = quantity
  )
View(bargiel_prints)

# zunächst ein Überblick über die mittlere Auflagenhöhe
bargiel_prints %>%
  summarise(
    Mittelwert = mean(Quantität),
    Median = median(Quantität)
  ) %>%
  arrange(Mittelwert) %>%
  kable

# dann gruppiert nach Werk
bargiel_prints %>%
  group_by(Titel) %>%
  summarise(
    Mittelwert = mean(Quantität),
    Median = median(Quantität)
  ) %>%
  arrange(Mittelwert) %>%
  kable

# alternativ gruppiert nach Stimme
bargiel_prints %>%
  group_by(Stimme) %>%
  summarise(
    Mittelwert = mean(Quantität),
    Median = median(Quantität)
  ) %>%
  arrange(desc(Mittelwert)) %>%
  kable

# gruppiert nach Werk, Band und Stimme
bargiel_prints %>%
  group_by(Titel, Band, Stimme) %>%
  summarise(
    Mittelwert = mean(Quantität),
    Median = median(Quantität)
  ) %>%
  arrange(Mittelwert) %>%
  kable

#
# Aufgaben
#

# Welches ist die am meisten vorkommende Auflagenhöhe (Modus) in den
# Bargiel-Drucken?
bargiel_prints %>%

# Was ist der Mittelwert und der Median der Gesamtauflagen für Beethovens Werke?
beethoven_works <- search(..., i='work') %>%
  unnest(..., names_repair=repair) %>%
  unnest(..., names_repair=repair)
beethoven_works_totalprints <- beethoven_works %>%
  group_by(...) %>%
  summarise(...) %>%
  ungroup
beethoven_works_totalprints %>%

# Was ist der Mittelwert und der Median der Gesamtauflagen für Verlagsartikel,
# in denen Mozart-Werke gedruckt werden?
mozart_works <- search()
mozart_published_items <- mozart_works %>%
mozart_published_items_totalprints <- mozart_published_items %>%
mozart_published_items_totalprints %>%
