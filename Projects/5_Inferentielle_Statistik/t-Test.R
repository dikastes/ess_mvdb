# Beispiele inferentielle Statistik

# Produzieren deutsche Autoren mehr oder weniger Werke als andere?
repair <- 'unique'
composers <- search(i='person') %>%
  mutate(
    German = ifelse(grepl('XA-DE', geographic_area_code), TRUE, FALSE)
    ) %>%
  unnest(works, names_repair=repair) %>%
  group_by(gnd_id...2, German) %>%
  summarise(WorkCount = n())
View(composers)

composers %>%
  group_by(German) %>%
  summarise(
    mean = mean(WorkCount),
    sd = sd(WorkCount)
  )

composers %>% t.test(WorkCount ~ German, .)

# Spielt der Geburtsort eines Komponisten eine Rolle für sein Auflagentotal?
composers <- search(i='person') %>%
  unnest(works, names_repair = repair) %>%
  filter(lengths(published_subitems) > 0) %>%
  unnest(published_subitems, names_repair = repair) %>%
  filter(lengths(prints) > 0) %>%
  unnest(prints, names_repair = repair) %>%
  select(gnd_id...2, place_of_birth, uid...27, quantity) %>%
  unique %>%
  group_by(gnd_id...2, place_of_birth) %>%
  summarise(total = sum(quantity))
View(composers)
composers %>% aov(total ~ place_of_birth, .) %>% summary

# Das hochsignifikante Ergebnis überrascht nicht, weil für viele Orte nur ein
# einziger Komponist vorliegt. Wir können die Anzahl der Komponisten ermitteln,
# die am jeweiligen Ort geboren sind.
composers_per_place <- search(i='person') %>%
  group_by(place_of_birth) %>%
  summarise(composers = n()) %>%
  filter(
    composers > 1,
    place_of_birth != ''
  )

composers %>%
  inner_join(composers_per_place) %>%
  aov(total ~ place_of_birth, .) %>%
  summary
