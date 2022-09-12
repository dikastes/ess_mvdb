# Rs Funktionalität kann durch Bibliotheken erweitert werden.
# Bibliotheken müssen ggf. zunächst heruntergeladen und installiert
# werden. Die require-Funktion entscheidet, ob eine Bibliothek
# installiert ist.
if (!require('tidyverse')) {
  # Bibliotheken werden mit install.packages installiert
  install.packages('tidyverse')
}
# Bibliotheken werden mit der library-Funktion geladen
library(tidyverse)
library(magrittr)

# Mit dem Tidyverse steht uns der pipe-Operator %>% zur Verfügung.
# Damit können Funktionsaufrufe fun(param1) umgeschrieben werden als
# param1 %>% fun.
datapoints <- c(6, 6, 4, 6, 8, 6, 5)
double <- function(p) {
  p * 2
}
sum <- function(p) {
  # is.na gibt an, ob Wert definiert ist, in eckigen Klammern wird
  # ein Listeneintrag über seine Position gewählt
  if (is.na(p[2])) {
    p[1]
  } else {
    # summiere den ersten Eintrag mit der Summe über die restlichen
    # Einträge
    p[1] + sum(p[-1])
  }
}

# 3 Möglichkeiten, datapoints zunächst zu verdoppeln und dann
# aufzusummieren:

# 1. geschachtelte Funktionen
sum(double(datapoints))

# 2. Puffervariablen
buffer <- double(datapoints)
sum(buffer)

# 3. Piping
datapoints %>% double %>% sum

# Die zentrale Datenstruktur in Tidyverse ist der Tibble
data <- read_csv("dataset.csv")
View(data)

# Einzelne Spalten eines Tibbles werden mit $ adressiert
View(data$shoe_size)

# Einzelne Spalten eines Tibbles können verändert werden.
data %>% mutate(weight = weight * 2)

# mutate ist Teil des dplyr-Paketes im Tidyverse, siehe das dazugehörige
# Cheatsheet
# https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf

# Eigene Funktionen müssen dafür vektorisiert werden.
rename_green_single <- function(party) {
  if (party == 'Grüne') {
    'Bündnis 90'
  } else {
    party
  }
}
rename_green_single('CDU')
rename_green_single('Grüne')
rename_green_single(c('CDU', 'Grüne'))
data %>% mutate(party = rename_green_single(party))

rename_green <- Vectorize(rename_green_single)
data %>% mutate(party = rename_green(party))

# Zu vielen Anwendungsfällen existieren vorgefertigte vektorisierte Funktionen
c('Grüne', 'CDU') %>% str_replace('Grüne', 'Bündnis 90')
'Grüne' %>% str_replace('Grüne', 'Bündnis 90')
data %>% mutate(party = party %>% str_replace('Grüne', 'Bündnis 90'))
'Die Grünen' %>% str_replace('Grüne', 'Bündnis 90' )

# Einzelne oder mehrere Spalten eines Tibbles können zu einer
# Kennzahl zusammengefasst werden.
data %>% summarise(mean_income = income %>% mean)

# Vor dem Zusammenfassen können Gruppen gebildet werden.
data %>%
  mutate(age = age %>% round(-1)) %>%
  group_by(age) %>%
  summarise(mean_income = income %>% mean)

# Wir können uns einen Überblick über die Einkommensverteilung verschaffen.
data %>%
  mutate(income = income %>% round(-4)) %>%
  group_by(income) %>%
  summarise(total = n())

# Wir können Menschen mit besonders kleinen oder besonders großen
# Füßen aus der Analyse ausschließen.
min_size <- min(data$shoe_size)
max_size <- max(data$shoe_size)
data %>%
  filter(
    shoe_size != min_size,
    shoe_size != max_size
  ) %>%
  mutate(age = age %>% round(-1)) %>%
  group_by(age) %>%
  summarise(total = n())

##
## Aufgaben
##

# Berechne den BMI einzeln für alle Proband:innen.
bmi <- Vectorize(function(weight, height) {
  weight / ((height / 100) ^ 2)
})

bmi_dataset <- data %>%
    mutate(bmi = bmi(weight, height))
  
# Berechne den durchschnittlichen BMI aller Proband:innen.
bmi_mean <- bmi_dataset %>%
    summarise(bmi_mean = mean(bmi))
  
# Berechne den durchschnittlichen BMI nach Geschlecht.
bmi_gender <- bmi_dataset %>%
    group_by(gender) %>%
    summarise(bmi_mean = mean(bmi))
  
# Berechne den durchschnittlichen BMI nach Parteipräferenz.
bmi_party <- bmi_dataset %>%
    group_by(party) %>%
    summarise(bmi_mean = mean(bmi))
  
# Berechne den durchschnittlichen BMI nach Geschlecht UND Parteipräferenz.
bmi_gender_party <- bmi_dataset %>%
    group_by(party, gender) %>%
    summarise(bmi_mean = mean(bmi))
 
# Berechne die absoluten und Relativen Stimmanteile der Parteien in der Stichprobe.
sizeOfSample <- length(data$party)
absoluteShares <- data %>%
    group_by(party) %>%
    summarise(total = n())
relativeShares <- absoluteShares %>%
    mutate(relative = total / sizeOfSample)
  
# Berechne die Einkommensteuer einzeln für alle Proband:innen
income_tax_function <- Vectorize(function(salary) {
  if (salary <= 10347) {
    y = (salary - 10347) / 10000
    (1088.67 * y + 1400) * y
  } else if (salary <= 14926) {
    z = (salary - 14926) / 10000
    (206.43 * z + 2397) * z + 869.32
  } else {
    0.42 * salary - 9336.45
  }
})
income_tax <- data %>%
    mutate(Einkommensteuer = income_tax_function(income))
  
# Berechne die relativen Stimmanteile der Parteien in Abhängigkeit vom Geschlecht
data %>%
    group_by(party, gender) %>%
    summarise(relativ = n() * 2 / sizeOfSample) %>%
    arrange(gender, desc(relativ))
  
# Berechne die relativen Stimmanteile der Parteien in Abhängigkeit vom Lebensjahrzehnt
data %<>% mutate(decade = round(age, -1))
age_percentage <- data %>%
    group_by(decade) %>%
    summarise(n = n())
data %>%
    inner_join(age_percentage) %>%
    group_by(decade, party) %>%
    summarise(absolut = n() / n) %>%
    unique
