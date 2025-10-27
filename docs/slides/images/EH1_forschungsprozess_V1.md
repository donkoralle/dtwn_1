---
title: "Empirie – Wozu & wie"
subtitle: "716408 | Sozialwiss. Methoden – How 2 do Things with Numbers"
author: "KMH"
date: "SS 22  |  EH 1 (updated: `r Sys.Date()`)"
output:
  xaringan::moon_reader:
    css: [mycss_metropolis_v1.css, metropolis, metropolis-fonts]
    lib_dir: libs
    nature:
      highlightStyle: github
      highlightLines: true
      countIncrementalSlides: false
---
```{r xaringanExtras, echo=FALSE}
xaringanExtra::use_share_again()  # für die Buttons zur Navigation
xaringanExtra::use_tile_view()    # für den Overview auf die Slides
```
class: zwischentitel, center, middle

# .emolarge[🤔]<br> Empirie - aber wie?

---

# Methoden! No, wait ...


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/EH1_forschungsprozess_V1_S2_1.png")
knitr::asis_output("</div>")
```

.quelle[(Überarbeitung von: Reuber & Pfaffenbach 2005:21)]

---

# Methoden als Bausteine etwas Größeren

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/ImageSlide_3.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Der Forschungsprozess

---

# Methoden als Bausteine etwas Größeren

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/ImageSlide_4.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# 1. Der Beginn: Themenwahl & Forschungsfrage(n)
* Oftmals Thema & Forschungsfrage(n) grob vorgegeben:
    + Abschlussarbeiten, Ausschreibungen etc.
    + 
* Nach der groben Themenauswahl:
    + Was will ich untersuchen?
        - Lesen & umhören: z.B. Was ist am Thema Umgang mit Naturgefahren interessant & fachspezifisch relevant?
Wer schreibt darüber -> Forschungsstand
        - Je umfangreicher der Forschungsstand -> je enger die Fragestellung

---

# 1. Fundierung der Forschungsfragen
* Vertiefung & Fundierung der Untersuchungsfragen:
    + Welche Theorien & Thesen (Relevanz & Aktualität) gibt es zum zentralen Gegenstand der Forschung? 
    + Welche Forschungsperspektiven werden mit diesen Theorien verbunden?
    + Auf welche Erkenntnisobjekte stellen diese Theorien ab?-> Definitionen & Operationalisierungen
    + 
    + BSP: „Umgang mit Naturgefahren“:
        - Indiv. Psychologische Verarbeitung von Ereignissen
        - Wahrnehmungstheoretisch: Aspekte der Wahrnehmung von Naturgefahren
        - Handlungstheoretisch: Rechtfertigung der (Nicht-)Berücksichtigung von Naturgefahren

---

# 1. Das Untersuchungsziel
* Deskription:nicht-kausalte Beschreibung eines Phänomens
* Exploration:Herleiten von Thesen, Hyothesen oder Typlogien zu Phänomenen
* Explanation:Überprüfen von Thesen oder Hypothesen über Phänomene
* Prognose:Verlauf von Prozessen abschätzen
* Evaluation:Bewertung von Maßnahmen und Programmen

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Exkurs zu den Zielen

---

# Die deskriptive Untersuchung

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/ImageSlide_8.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# @ Deskription
* Ziel:Auskunft über die Ausprägung und Verteilung von Merkmalen in Grundgesamtheiten
* Meist keine Erhebung der Grundgesamtheit möglich:
    + Grundgesamtheit ist (praktisch) unendlich (z.B. Zeitungen)
    + Grundgesamtheit ist manchmal (bestenfalls) teilweise bekannt (z.B. Medikamentensucht, Schwarzfahrer etc.)
    + Untersuchung würde Grundgesamtheit zu stark beeinträchtigen (z.B. Qualitätskontrollen)
    + Untersuchung der Grundgesamtheit ist zu aufwändig
* 👉 Erhebung von Stichproben

---

# Exkurs: Stichprobe & Grundgesamtheit


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/EH1_forschungsprozess_V1_S10_2.png")
knitr::asis_output("</div>")
```

.quelle[(Eigene Erstellung, 2021, CC BY)]

---

# @ Deskription in Action:
Räumliche Variabiltität von COVID-19 Impfquoten in Österreich (Stand: Oktober 2021)


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/EH1_forschungsprozess_V1_S11_3.png")
knitr::asis_output("</div>")
```

.quelle[(Eigene Erstellung, 2021, CC BY)]

---

# Das Untersuchungsziel
* Deskription:nicht-kausalte Beschreibung eines Phänomens
* Exploration:Herleiten von Thesen, Hyothesen oder Typlogien zu Phänomenen
* Explanation:Überprüfen von Thesen oder Hypothesen über Phänomene
* Prognose:Verlauf von Prozessen abschätzen
* Evaluation:Bewertung von Maßnahmen und Programmen

---

# Die explorative Untersuchung

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/ImageSlide_13.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# @ Exploration
* Aufbereitung & Darstellung quantitativer Daten👉 unentdeckte Muster & Regelmäßigkeiten
* Fokus nicht primär auf Datenerhebung
* Viele Variablen-> grafische und numersiche Verfahren:
    + Exploratory Data Analysis (EDA)
        - Stem-and-Leaf-Plots
        - Box-Plots
        - Scatter-Plots
    + Multivariate Explorationstechniken:
        - Cluster- & Faktorenanalyse

---

# 3. Empirisch-quantitative Exploration II

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/ImageSlide_15.png")
knitr::asis_output("</div>")
```
.quelle[Joxemai, 2011, CC BY-SA; Indon~commonswiki, 2007, CC BY-SA]

---

# Quant.(qual.) Exploration in Action


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/EH1_forschungsprozess_V1_S16_4.png")
knitr::asis_output("</div>")
```

.quelle[(Grüneis et al. 2018:391)]

---

# Das Untersuchungsziel
* Deskription:nicht-kausalte Beschreibung eines Phänomens
* Exploration:Herleiten von Thesen, Hyothesen oder Typlogien zu Phänomenen
* Explanation:Überprüfen von Thesen oder Hypothesen über Phänomene
* Prognose:Verlauf von Prozessen abschätzen
* Evaluation:Bewertung von Maßnahmen und Programmen

---

# @ Explanation

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/ImageSlide_18.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# @ Exlplanation
* Ziel:(Statistischer) Test von Annahmen über Zusammenhänge, Unterschiede & Veränderungen ausgewählter Merkmale zu untersuchender Phänomene
* Unterschied zu deskriptiven & explorativen Untersuchungen:
    + Vorkenntnisse notwendig:👉 VOR der Untersuchung untersuchbare Hypothese formulieren
        - „Untersuchbar“: Präzise formulierte Wirkrichtung & Effektgröße einer Hypothese-> sichert Überprüfbarkeit

---

# Deduktiv-nomologische Explanation
* Deduktiv-nomologisch: Von einem allgemeinen Gesetz (nomos) und dem Vorliegen einer Rahmenbedingung auf das zu erklärende Phänomen schließen (Deduktion)
* 
* -> „Hempel-Oppenheim“ Schema


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/EH1_forschungsprozess_V1_S20_5.png")
knitr::asis_output("</div>")
```

.quelle[(Meier Kruker & Rauh 2005:10)]

---

# Explanation in Action


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/EH1_forschungsprozess_V1_S21_6.png")
knitr::asis_output("</div>")
```

.quelle[(Höferl 2017)]

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Back to the story

---

# Methoden als Bausteine etwas Größeren

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/ImageSlide_22.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# 2. Definitionen & Operationalisierung
* Wichtig: Bei Forschungsfrage begriffliche Klarheit suchen
    + Realdefinitionen: z.B. Lawinen zählen zur Gruppe der Naturgefahren
    + Analyt. Definitionen: z.B. Unter Resilienz verstehen wir …
    + Operative Definitionen: Von Bedeutung v.a. im quant. Forschungsstil
        - Ziel: Thematische Gegenstände (Konstrukte) empirisch abbildbar zu machen
        -   


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/EH1_forschungsprozess_V1_S23_7.png")
knitr::asis_output("</div>")
```

.quelle[( vgl. Schirmer 2009:116)]

---

# 3. Auswahl der Untersuchungsgegenstände
* Ausgehend von Forschungsfrage & Erhebungsziel:Welche Personen(gruppen), Räume & Materialien eigenen sich als Datengrundlage?BSP „Umgang mit Naturgefahren“
    + Einstellungen & Erfahrungen von Entscheidern: Gespräch
    + Handlungen zur ex-ante Schadenvorsorge: (Plan-)Dokumente, Protokolle, Sekundärstatistiken etc.
    + Sinnstrukturen beim Umgang mit Naturgefahren: Journale, Sitzungsprotokolle, Gespräch etc.
    + 
* -> Benennung des Materials zur Untersuchung der Forschungsfrage

---

# 4. Auswahl Erhebungsinstrumente & Analyseverfahren
* = f (Forschungsfrage, Untersuchungs- & Erhebungsziel,   Untersuchungsgegenstände)
    + Finanzielle, personelle & zeitliche Ressourcen
    + Kapazitäten für Datenerhebung, -verarbeitung & -auswertung
    + Weiterführende Verwendung der Daten & Ergebnisse

---

# 4. Auswahl Erhebungsinstrumente & Analyseverfahren


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/EH1_forschungsprozess_V1_S26_8.png")
knitr::asis_output("</div>")
```

.quelle[(Überarbeitung von: Reuber & Pfaffenbach 2005:21)]

---

# Methoden als Bausteine etwas Größeren

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/ImageSlide_27.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# 5. Sampling
* Welche Personen(gruppen) befragen? Welche konkreten Materialien in Untersuchung aufnehmen?
* Oftmals: Eingrenzung (Zeitraum, Ort, Gruppe, Materialtyp etc.)
* Quant. Forschungsstil:
    + Grundgesamtheit zur der eine Aussage getroffen werden soll-> meist jedoch keine Gesamterhebung möglich
    + Teilgesamtheit („Stichprobe“) für Erhebung & Analyse auswählen

---

# 6. Erhebung
* Untersuchungsgegenstände -> Daten
    + Pragmatische Einflüsse: 
        - Zeitliche, finanzielle & personelle Ressourcen
    + 5 Regeln der Erhebung:
        - Sorgfalt: Dokumentation, Transkription etc.
        - Kontextbezug: Dokumentation des Kontextes
        - Dichte: Detailliertheit* der Beschreibung
        - Ausschöpfung: Relevantes vollständig dokumentieren & ordnen
        - Reflexion: der eigenen Rolle -> Einfluss Erhebungsziel auf Daten

---

# Methoden als Bausteine etwas Größeren

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/ImageSlide_30.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# 7. Datenaufbereitung
* Daten = Material das (soziale) Sachverhalte repräsentiert
* Meist nicht direkt analysierbar:
    + „das“ Interview, „der“ Fragebogen
    + 
* Quant. Forschungsstil:
    + Codierung (der Antwort-Items)
    + Digitalisierung
    + Fehlerkontrolle, -bereinigung & -kennzeichnung
    + Recodieren, Indexbildung

---

# 8 & 9: Analyse & Interpretation
* Analyse = interpretierendes Auswerten & Ordnen von Daten für eine Fragestellung
* 
* Aufwand:
    + Quant. Forschungsstil: weniger in der Analyseals in Instrumentenerstellung
* 
* Typ. Ablauf:
    + Ordnung & Beschreibung des Untersuchten
    + Vergleich zwischen 
        - Teilen des Untersuchten
        - (Von Teilen) des Untersuchten mit Theorien

---

# Methoden als Bausteine etwas Größeren

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/ImageSlide_33.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# 10. Ergebnispräsentation
* Art der Darstellung & Ausführlichkeit:
    + Abh. Vom Zielpublikum:
        - Sprache (Fachvokabular etc.), Komplexität & Elemente der Textgliederung etc.
    + Wiss. Bericht:
        - Motivation (Zweck & Fokus)
        - Theoret. Fundierung
        - Forschungsfrage
        - Art der Datengewinnung & Auswertemethoden
        - Ergebnisse (übersichtlich in Zusammenfassung)
    + Transparenz:
        - Hinweise auf Probleme im Forschungsprozess, Widersprüchlichkeiten etc.

---

# 11. Archivierung

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/ImageSlide_35.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# 11. Archivierung

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh1_forschungsprozess/ImageSlide_36.png")
knitr::asis_output("</div>")
```
.quelle[(Planemad, Wikimedia, CC0; GitHub, Wikimedia, CC0; GESIS, Wikimedia, CC0)]

---
