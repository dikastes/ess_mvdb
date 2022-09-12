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
    filter(Quantität == max(Quantität)) %>%
    select(Quantität) %>%
    unique

# Was ist der Mittelwert und der Median der Gesamtauflagen für Beethovens Werke?
beethoven_works <- search(q='beethoven', f='composers.name', i='work') %>%
  unnest(published_subitems, names_repair=repair) %>%
  unnest(prints, names_repair=repair)
beethoven_works %>%
  group_by(gnd_id, generic_title) %>%
  summarise(total = sum(quantity)) %>%
  ungroup %>%
  summarise(
    mean = mean(total),
    median = median(total),
    Std_Abw = sd(total),
    iqb = IQR(total)
)

# Was ist der Mittelwert und der Median der Gesamtauflagen für Verlagsartikel,
# in denen Mozart-Werke gedruckt werden?
mozart_works <- search(q='Mozart', f='works.composers.name')
mozart_published_items <- mozart_works %>%
    unnest(published_subitems, names_repair = repair) %>%
    filter(lengths(prints) > 0) %>%
    unnest(prints, names_repair = repair)
mozart_published_items_totalprints <- mozart_published_items %>%
    group_by(id) %>%
    summarise(Total = sum(quantity))
mozart_published_items_totalprints %>%
    summarise(
      mean = mean(Total),
      median = median(Total),
      Std_Abw = sd(Total),
      iqb = IQR(Total)
    )

# Achtung! sollen Gesamtzahlen für Komponisten, Genres oder Instrumente ermittelt werden,
# müssen mehrfach vorkommende Verlagsteilartikel ignoriert werden. Verschiedene Werke
# eines Komponisten (eines Genres, mit einem Instrument) können im gleichen Teilartikel
# aufgelegt werden und eine naive Auswertung, die auf den unique-Schritt verzichtet,
# würde diese mit aufsummieren.
mozart <- search(q='Mozart', i='person')
mozart_works <- mozart %>%
    unnest(works, names_repair = repair) %>%
    unnest(published_subitems, names_repair = repair) %>%
    filter(lengths(prints) > 0) %>%
    unnest(prints, names_repair = repair)

mozart_works %>%
    select(name, gnd_id...2, uid...27, quantity) %>%
    # Hier werden doppelte Teilartikel aussortiert
    unique %>%
    summarise(total = sum(quantity))
