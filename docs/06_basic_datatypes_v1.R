# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Intro RStudio, Vektoren in R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ==== Übersicht RStudio ====

#   * lo: Code
#   * ro: Terminal
#   * ru: Files, Plots & HELP
#   * lu: Environment ("Workspace")

# Fokus auf einzelne Fenster möglich
# Shift + Strg 1 bis 4

# Prinzipiell: UTF-8 als encodung wählen
# Tools - Global Options - Code - Saving


# PPT: Grundlegende arithmetische Operatoren in R

# Code in Console exekutieren:
#   Zeile bzw. Selektion ausführen Strg + Return
#   Alles: Strg * Shift + Return

2 * 2

# Hinweis auf Code-Completion in R-Studio bei Befehlen > 1 Inputparamter

sqrt(34)

# Kommentare mit RAUTE

# einfacher Output: File > compile Report // Knopf "compile Report"
# wo liegt diese Datei: gleichen Verzeichnis wie Skript > Files-Tab


# ==== einfache Funktionen ====

round(2.456789, digits = 2)
round(2.456789, 2)

floor(3.8)
ceiling(3.8)

# Verschachteln von Funktionen
round(sqrt(2),2)

# Variablen definieren
dieWurzel <- round(sqrt(2),2)
dieWurzel

# Variablen löschen
rm(dieWurzel)


# ==== Vektoren ====

# wir arbeiten meist nicht mit einzelnen Zahlen sondern mit Vektoren
# > PPT-Vektor

c(1, 1, 2, 3, 5, 8, 13, 21)

sample(0:500, 20)
# Details zur Funktion > Hilfe

# Alternativen:
1:100
seq(0,5,0.5)
seq(-5,5)

# Sortieren: default = aufsteigend
sort(sample(0:500, 20))
sort(sample(0:500,20), decreasing = TRUE)

# Funktionen auf Vektoren anwenden
length(sample(0:500,20))
max(sample(0:500,20))

# Vekoren ablegen
mySample <- sample(0:500,20)   # check Environment
mySample

# auf Werte von Vektoren zugreifen
mySample[1]
mySample[length(mySample)]


# ==== aus Vektoren Datentabellen bauen ====

laender <- data.frame(land = c("Österreich", "Deutschland", "Schweiz"),
                      area = c(83800,357500,41300),
                      ew = c(8.9,83.2,8.6))
# Environment > Data > laender & Doppelklick; alternativ:
View(laender)
str(laender)

### Basisinfos zum Table abgrufen:

# n Records
nrow(laender)
# n Spalten (= Variablen)
ncol(laender)

# auf Spalten (= Vektoren) zugreifen
laender$land
laender[,1]
mean(laender$ew)

# auf Records zugreifen
laender[1,]
laender[laender$land == "Schweiz",]

# auf einzelnen Wert direkt zugreifen: BSP Bev Deutschlands
laender[2,3]


### Datentablellen (aka Tables) manipulieren

# Neue Spalte berechnen: Bevölkerungsdichte
laender$bevdichte <- laender$ew/laender$area
str(laender)
# besser lesabr: Tsd. EW / KM2 >>> OBACHT: erneutes Zuweisen der Spalte überschreibt alle vorherigen Werte - no questions asked
laender$bevdichte <- (laender$ew*1000)/laender$area

# Neuen Record anhängen
laender[nrow(laender)+1,] = c("Seefahrender Gottesstaat Kamistan", 300, 0.4, 0.001)
laender

# Record löschen
laender[-c(4),]
# nicht ganz weil:
laender
# Warum? Dem dataframe leander muss die reduzierte Fassung seinerselbst erst erneut zugewiesen werden
laender <- laender[-c(4),]
laender
# Hinweis: Meist ist es ratsamer, die reduzierte Fassung des originalen dataframes als neue Variable abzulegen. Dadurch steigt die Nachvollziehbarkeit und die Möglichkeit Fehler nachträglich besser ausbügeln zu können.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         Skalen und dataframes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ==== Datentypen vs. Skalen in R ====

# Vorab: Datentypen und Skalen sind in R zwei verschiedene Konzepte

class(4)
typeof(4)

# aber:
typeof(1:100)

# Hintergrund: R ist sparsam (weniger Memory) und erzeugt selbst nur Integer, wenn das "ausreichend" (1:100) ist
# Eingaben der Benutzer werden vorsichtshalber immer als Double abgelegt

class(c(1, 2, 3))
typeof(c(1, 2, 3))
class(c("hallo", "welt"))
typeof(c("hallo", "welt"))

# Lust auf mehr? https://r-intro.tadaa-data.de/book/datentypen.html

# ==== nominale Daten in R abbilden ====

namen <- c("Hans", "Peter", "Klaus")
class(namen)

# Obacht: Groß-Kleinschreibung
"hans" == "Hans"
"Hans" == "Hans"

# geht nicht, da falsches Skalenniveau
mean(namen)

# Die sauberste Lösung für nominale Daten: Factor
# Eigenschaften von Faktoren:
#   * Vektor mit Level Ausprägungen
#   * Level: mögl. Ausprägung
#   * Label: textl. Maske für jede Ausprägung

geschlecht <- factor(c("man", "frau", "man", "frau"),
                     levels = c("man", "frau", "div"),
                     labels = c("männlich", "weiblich", "divers"))
# alternative Kodierung der Geschlechter: 0 = männlich, 1 = weiblich & 3 = divers
geschlecht2 <- factor(c(0,1,0,1),
                      levels = c(0,1,2),
                      labels = c("männlich", "weiblich", "divers"))

geschlecht
class(geschlecht)

str(geschlecht)
levels(geschlecht)
# Labels bei ausgabe unterdrücken > liefert Indizes der Levels
as.numeric(geschlecht)

# Factors auswerten: Häufigkeitsauswertung
table(geschlecht)
plot(geschlecht)


# ==== ordinale Daten in R abbilden ====

# Wieder die sauberste Lösung: ordered factor am Beispiel Schulnoten
noten <- factor(c(3,3,5,1,2,4,5),
                ordered = TRUE,
                levels = c(1,2,3,4,5),
                labels = c("Sehr gut", "Gut", "Befriedigend", "Genügend", "Nichtgenügend"))
noten
class(noten)
str(noten)
# Labels der Schulnoten unterdrücken > Indizes der Levels (= Schulnote)
as.numeric(noten)

# Ordered factors auswertern: Häufigkeitsauswertung
table(noten)
# als Tabelle formatiert
table(noten) %>%
  knitr::kable()

# Export nach Office V1: direkt als Table
table(noten) %>%
  write.table(., "clipboard", sep="\t", row.names=FALSE)

# alternativ:
table(noten) %>%
  knitr::kable("html") %>%
  writeClipboard(.)

# Export nach Office V2: mit flextable-Package
library(flextable)
table(noten) %>%
  as.tibble(.) %>%
  flextable()

# in Prozent
prop.table(table(noten))

# einfache graphische Ausgabe
plot(noten)
