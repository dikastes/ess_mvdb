# I API-Anbindung

# Suche nach 'Bach' in Verlagsartikeln
bach <- search(q='bach')
# Suche nach 'Mozart' in Personen
mozart <- search(q='mozart', i='person')
# Suche nach 'Requiem' in Werken
requiem <- search(q='requiem', i='work')
# Suche nach 'deutsch' mit freier Endung in Werken
deutsch <- search(q='deutsch*', i='work')$generic_title
# Suche nach 'Violine' im Feld 'name' von Instrumenten
violine <- search(q='violine', i='instrument', f='name')
# Suche nach 'Lied' in Genres
lied <- search(q='lied', i='genre')
# Suche nach 'Lied' m Feld 'name' von Genres
liedlied <- search(q='lied', f='name', i='genre')

# II Beispiele
bach %>% expand_economics
bach %>% write_csvfile
bach %>% plot_timeseries
bach %>% plot_timeseries(color = 'is_piano_reduction')
bach %>% plot_timeseries(facet = 'title')
bach %>% plot_timeseries

requiem %>% expand_economics
violine %>% expand_economics
lied %>% expand_economics
liedlied %>% expand_economics


lied %>% plot_timeseries
requiem %>% plot_timeseries
lied %>% plot_timeseries
liedlied %>% plot_timeseries

# III Direktpipelines

search('mozart') %>% write_csvfile
search('violine') %>% plot_timeseries

# IV Eigene Datenmanipulation

# Mit unnest werden komplexe Zellen in die Tabelle expandiert. Es entstehen
# neue Zeilen und neue Spalten.
# Mit filter werden Zeilen anhand einer Bedingung aussortiert.
repair <- 'unique'
bach %>%
    unnest(works, repair = repair)
bach %>%
    unnest(works, repair = repair) %>%
    unnest(published_subitems, repair = repair) %>%
    filter(lengths(prints) > 0) %>%
    unnest(prints, repair = repair) ->
    bach_expanded

# Einzelne Spalten eines Tibbles können verändert werden.
bach_expanded %>% mutate(quantity = quantity * 2)

# Mozarts Problem
mozart %>%
    unnest(works, repair = repair) %>%
    unnest(published_subitems, repair = repair) %>%
    filter(lengths(prints) > 0) %>%
    unnest(prints, repair = repair) ->
    mozart_expanded
mozart_expanded %>%
    filter(
        (uid1 == 822 | uid1 == 823),
        uid3 == 4829
    )
mozart_expanded %>%
    summarise(Total = sum(quantity))
mozart_expanded %>%
    select(uid3, quantity) %>%
    unique %>%
    summarise(Total = sum(quantity))

# V Eigene Grafiken

# Zeige die Entwicklung der Auflagen von PE_00043
pe43 <- search(q='PE_00043')
pe43 %>%
    unnest('published_subitems', names_repair = repair) %>%
    unnest('prints', names_repair = repair) %>%
    select(date_of_action, Quantität = quantity) %>%
    mutate(Jahr = year(date_of_action)) %>%
    arrange(Jahr) ->
    pe43_prints

pe43_prints %>% ggplot(aes(x = Jahr, y = Quantität)) #+
  #geom_point()
  #geom_col()
  #geom_violin()
  #geom_line()

rg <- tibble(Jahr = 1870:1930)
pe43_prints %>% 
    full_join(rg) %>%
    replace_na(list(Quantität = 0)) ->
    pe43_prints_null

pe43_prints_null %>% ggplot(aes(x = Jahr, y = Quantität)) +
    geom_line()

pe43_prints_null %>% 
    mutate(glMw = rollmean(Quantität, 5, na.pad=TRUE)) %>%
    ggplot(aes(x = Jahr, y = glMw)) +
        geom_line()



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

# mutate ist Teil des dplyr-Paketes im Tidyverse, siehe das dazugehörige
# Cheatsheet
# https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf

# Einzelne oder mehrere Spalten eines Tibbles können zu einer
# Kennzahl zusammengefasst werden.
bach_expanded %>% summarise(mean_prints = quantity %>% mean)

# Vor dem Zusammenfassen können Gruppen gebildet werden.
bach_expanded %>%
  group_by(voice) %>%
  summarise(mean_prints = quantity %>% mean)

bach_expanded %>%
  group_by(part) %>%
  summarise(total = n())

# Zusammenfassung dplyr-Verben
# Wesentliche Funktionalitäten der Funktionalen Programmieren werden durch
# diese dplyr-Verben implementiert:
# *filter*: sortiere Zeilen anhand einer Bedingung aus
# *summarise*: ermittle einen Kennwert über alle Zeilen
# *mutate*: errechne eine Spalte auf der Grundlage von Informationen aus anderen
# *select*: sortiere einzelne Spalten aus
# Spalten
# Wichtige Hilfsfunktionen
# *group_by*: definiert Gruppen, über die Funktionen laufen können, nützlich
# in Verbindung mit summarise
# *unique*: lösche alle duplizierten Zeilen, s.u.
# *arrange*: sortiere Zeilen anhand einer Spalte, s.u.

