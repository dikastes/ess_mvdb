#
# Kennwerte der Streuung
#

library(magrittr)

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

# Beispiel z-Skalierung

mozart_prints <- search(q='mozart', i='person') %>%
  unnest(works, repair_names=repair) %>%
  unnest(published_subitems, repair_names=repair) %>%
  filter(lengths(prints) > 0) %>%
  unnest(prints, repair_names=repair)
mozart_prints %>%
  summarise(total = sum(quantity))
mozart_work_total <- mozart_prints %>%
  group_by(gnd_id1, generic_title) %>%
  summarise(total = sum(quantity)) %>%
  ungroup
mozart_work_total %>%
  summarise(
    MW = mean(total),
    StdA = sd(total)
  )
bach_prints <- search(q='"bach, johann sebastian"', i='person') %>%
  unnest(works, repair_names=repair) %>%
  unnest(published_subitems, repair_names=repair) %>%
  filter(lengths(prints) > 0) %>%
  unnest(prints, repair_names=repair)
bach_prints %>%
  summarise(total = sum(quantity))
bach_work_total <- bach_prints %>%
  group_by(gnd_id1, generic_title) %>%
  summarise(total = sum(quantity)) %>%
  ungroup
bach_work_total %>%
  summarise(
    MW = mean(total),
    StdA = sd(total)
  )
mozart_work_total %<>%
  mutate(zscore = (total - mean(total)) / sd(total))
bach_work_total %<>%
  mutate(zscore = (total - mean(total)) / sd(total))
mozart_work_total %>%
  filter(gnd_id1 == '30010782X')
bach_work_total %>%  
  filter(gnd_id1 == '300006667')