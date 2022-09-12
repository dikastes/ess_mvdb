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

# UND
TRUE & FALSE
# ODER
TRUE | FALSE
# NICHT
!TRUE
!FALSE

# Addition ist in R nicht für Zeichenketten oder unterschiedlich
# lange Listen definiert.
'hal' + 'lo'
c(3, 5, 7) + c(4, 1)

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
# function (parameter1, parameter2) { Ausdrücke }
#
# Der letzte Ausdruck ist der Rückgabewert der Funktion.

# Funktionen können unmittelbar aufgerufen werden durch Anhängen von ().
(function(arg1, arg2) {
  buffer <- arg1 + arg2
  buffer + 1
})(3, 4)

# Funktionen können auch in einer Variablen gespeichert und später
# aufgerufen werden.
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

# Parameter können bei der Funktionsdeklaration mit Default-Werten
# belegt werden.
addDouble <- function(arg1, arg2 = 0, arg3 = 0) {
  buffer <- arg1 + arg2
  buffer <- buffer * 2
  buffer + arg3
}
addDouble(2, 3, 4)
addDouble(2, 3)
addDouble(2)
addDouble(2, arg3 = 4)

# Funktionen können bedingte Ausdrücke und Zuweisungen enthalten und
# sich ggf. selbst aufrufen.
faculty <- function(n) {
  # Vergleiche auf Gleichheit werden mit == durchgeführt
  if (n == 0) {
    1
  } else {
    n * faculty(n-1)
  }
}
faculty(6)

# Bemerkung: Die Fakultätsfunktion wächst extrem stark und Variablen
# erhalten nur begrenzten Speicherplatz. Die oben definierte Funktion
# gibt ab 171 auf.
faculty(168)
faculty(169)
faculty(170)
faculty(171)

##
## Aufgaben
##

# Welche Ausgabe wird die REPL für folgende Zeilen erzeugen? Warum?
3 + 8
TRUE & TRUE
c(11, 22, 33) * 2
3 * 2 == 3 + 3
FALSE & 2^4 == 16
2 * 5 == 20 | 2^4 == 16

# Implementiere die Funktion isAMultipleOfThree.
isAMultipleOfThree <- function (param) {
    param %% 3 == 0
}

isAMultipleOfThree(10) # FALSE
isAMultipleOfThree(9)  # TRUE
isAMultipleOfThree(3)  # TRUE
isAMultipleOfThree(0)  # TRUE
isAMultipleOfThree(1)  # FALSE

# Implementiere die Funktion nthFibonacciNumber.
nthFibonacciNumber <- function (n) {
    if (n <= 2) {
        1
    } else {
        nthFibonacciNumber(n-1) + nthFibonacciNumber(n-2)
    }
}

nthFibonacciNumber(1)  # 1
nthFibonacciNumber(2)  # 1
nthFibonacciNumber(3)  # 2
nthFibonacciNumber(4)  # 3
nthFibonacciNumber(5)  # 5
nthFibonacciNumber(6)  # 8
nthFibonacciNumber(7)  # 13
nthFibonacciNumber(8)  # 21
nthFibonacciNumber(9)  # 34
nthFibonacciNumber(10) # 55
nthFibonacciNumber(20) # 6,765

# Implementiere die Funktion fibonacciSeriesToN
fibonacciSeriesToN <- function(n) {
    if (n == 1) {
       c(1)
    } else {
       c(fibonacciSeriesToN(n-1), nthFibonacciNumber(n))
    } 
}

fibonacciSeriesToN(1)  # c(1)
fibonacciSeriesToN(2)  # c(1, 1)
fibonacciSeriesToN(3)  # c(1, 1, 2)
fibonacciSeriesToN(4)  # c(1, 1, 2, 3)
fibonacciSeriesToN(5)  # c(1, 1, 2, 3, 5)

