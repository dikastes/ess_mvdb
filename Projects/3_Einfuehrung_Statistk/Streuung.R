#
# Kennwerte der Streuung
#

library(tidyverse)
library(magrittr)
library(knitr)

bargiel_works <- search(q='bargiel', i='work', f='composers.name')
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

# Wie ist die Streuung der Auflagenhöhe von Bargiel-Werken?
bargiel_prints %>%
  summarise(
    Mittelwert = mean(Quantität),
    Median = median(Quantität),
    StdAbw = sd(Quantität),
    MAA = mad(Quantität),
    IQuartB = IQR(Quantität)
  ) %>%
  arrange(Mittelwert) %>%
  kable

# dann gruppiert nach Werk
bargiel_prints %>%
  group_by(Titel) %>%
  summarise(
    Mittelwert = mean(Quantität),
    Median = median(Quantität),
    StdAbw = sd(Quantität),
    MAA = mad(Quantität),
    IQuartB = IQR(Quantität)
  ) %>%
  arrange(Mittelwert) %>%
  kable

# alternativ gruppiert nach Stimme
bargiel_prints %>%
  group_by(Stimme) %>%
  summarise(
    Mittelwert = mean(Quantität),
    Median = median(Quantität),
    StdAbw = sd(Quantität),
    MAA = mad(Quantität),
    IQuartB = IQR(Quantität)
  ) %>%
  arrange(desc(Mittelwert)) %>%
  kable
