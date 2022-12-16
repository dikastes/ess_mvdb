if (!require('tidyverse')) { install.packages('tidyverse') }
if (!require('magrittr')) { install.packages('magrittr') }
library(tidyverse)
library(magrittr)

# Einführung R-REPL

# Die REPL (read eval print loop) nimmt Ausdrücke entgegen, evaluiert sie, 
# und gibt sie wieder aus. Ausdrücke können bspw. sein: Zahlen, Zeichen-
# ketten oder Listen. Skriptzeilen werden in RStudio mit Strg+Enter
# ausgeführt.

# Ganzzahl
1

# Gleitkommazahl
1.3

# Zeichenkette (String)
'hallo'

# Boolwert
TRUE
FALSE

# Liste
c(1, 2)

# Kommentare werden durch # am Zeilenanfang eingeleitet und beim
# Ausführen ignoriert.

# Neben einfachen Datentypen können auch komplexe Ausdrücke ausge-
# wertet werden. Komplexe Ausdrücke können bspw. durch Operatoren
# gebildet werden.

1 + 3
1.4 + 2.7
c(3, 5, 7) + c(4, 1, 2)
2 + c(4, 1, 2)
2^5

1 - 3
1 / 3
4 * 6
20 %% 3

# Werte können in Variablen gespeichert und später wieder abgerufen
# werden.
a
a <- 42
a

# Variablen werden beim Auswerten durch den gespeicherten Wert
# ersetzt. Dieser kann wie gewohnt benutzt werden.

a <- c(3, 5, 7)
b <- c(6, 4, 2)
a + b

# Funktionen sind Abbildungen von einem oder mehreren Eingabewert 
# (Parameter) auf einen Ausgabewert. Sie werden notiert:
#
# name <- function (parameter1, parameter2) { Ausdrücke }
#
# Der letzte Ausdruck ist der Rückgabewert der Funktion.
addIncrement <- function(arg1, arg2) {
  buffer <- arg1 + arg2
  buffer + 1
}
addIncrement(2, 3)
addIncrement(a, b)

# Beim Funktionsaufruf können die Parameter benannt und unbenannt
# übergeben werden.
addIncrement(2, 3)
addIncrement(arg1 = 2, arg2 = 3)
addIncrement(arg2 = 3, arg1 = 2)

# API-Anbindung

# Suche nach 'Bach' in Verlagsartikeln
search(q='bach')
# Suche nach 'Requiem' in Werken
search(q='requiem', i='work')
# Suche nach 'deutsch' mit freier Endung in Werken
search(q='deutsch*', i='work')$generic_title
# Suche nach 'Violine' im Feld 'name' von Instrumenten
search(q='violine', i='instrument', f='name')
# Suche nach 'Lied' in Genres
search(q='lied', i='genre')
# Suche nach 'Lied' m Feld 'name' von Genres
search(q='lied', f='name', i='genre')

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

# Zeitserien

# Zeige die Entwicklung der Auflagen von PE_00043
data <- search(q='PE_00043')
repair = 'unique'
prints <- data %>%
  unnest('published_subitems', names_repair = repair) %>%
  unnest('prints', names_repair = repair) %>%
  select(date_of_action, Quantität = quantity) %>%
  mutate(Jahr = year(date_of_action)) %>%
  arrange(Jahr)

prints %>% ggplot(aes(x = Jahr, y = Quantität)) +
  #geom_point()
  #geom_col()
  #geom_violin()
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
  complete(Jahr, Stimme, fill = list(Quantität = 0)) %>%
  filter(!is.na(Stimme))

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

# Gruppierung der Stimmen nach Instrumenten, Stimmen, Partitur
voices <- c('EStA', 'EStB', 'EStS', 'EStT', 'EStC')
prints_ma %>%
  mutate(
    Gruppe = ifelse(
      Stimme == 'P',
      'P', 
      ifelse(
        Stimme %in% voices, 
        'Stimme',
        'Instrument')
      )
    ) %>%
  ggplot(aes(x = Jahr, y = glMW)) +
  geom_line(aes(color = Stimme)) +
  facet_grid(rows = vars(Gruppe), scales='free')

# Einzelfacettierung
prints_ma %>% ggplot(aes(x = Jahr, y = glMW)) +
  geom_line() +
  facet_grid(rows = vars(Stimme))
