---
title: "Primärdatengewinnung durch Befragung Special: Die Stichprobe"
subtitle: "716408 | Sozialwiss. Methoden – How 2 do Things with Numbers"
author: "KMH"
date: "SS 22 (updated: `r Sys.Date()`)"
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

# .emolarge[🤔]<br> Wieso sind Stichproben wichtig?

---

# Motivation: Warum das Ziehen von Stichproben wichtig ist

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/ImageSlide_2.png")
knitr::asis_output("</div>")
```
.quelle[(Eigene Überarbeitung 2016 von: Meier-Kruker & Rauh 2005:86)]

---

# Die Stichprobenziehung

„Repräsentativ“:Jedes Element der Grundgesamtheit → gleiche Chance in Stichprobe zu gelangen→ Strukturgleichheit=Häufigkeitsverteilung „wichtiger“ Merkmale in der Grundgesamtheit exakt wiedergebenVoraussetzung für Rückschluss auf Grundgesamtheit (z.B. Spannweiten)

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S4_1.png")
knitr::asis_output("</div>")
```
.quelle[(Höferl, 2020, CC BY)]

---

# Die Stichprobenziehung

„Repräsentativ“:Jedes Element der Grundgesamtheit → gleiche Chance in Stichprobe zu gelangenStrukturgleichheit=Häufigkeitsverteilung „wichtiger“ Merkmale in der Grundgesamtheit exakt wiedergebenDas Gegenteil:Die „informative“ Stichprobekein Rückschluss auf die Grundgesamtheit

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S6_2.png")
knitr::asis_output("</div>")
```
.quelle[(Höferl, 2020, CC BY)]

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Wie geht das statistisch?

---

# Zentraler Grenzwertsatz der Statistik

* Unabhängig von der konkreten Ausgangsverteilung konvergiert die Verteilungsfunktion einer Summe von Stichproben gegen die Normalverteilung
* **BSP: **Canadian Survey of Labour and Income Dynamics (n=4.147)
	+ Min: 2,30
	+ Max: 49,92
	+ Mean: 15,55

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S7_3.png")
knitr::asis_output("</div>")
```
.quelle[(Hudec 2010)]

---

# Zentraler Grenzwertsatz der Statistik in Action

→ Mittelwert Grundgesamt: 15,55

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S8_4.png")
knitr::asis_output("</div>")
```
.quelle[(Hudec 2010)]

---

# Zentraler Grenzwertsatz der Statistik in Action

* Nicht 1 Stichprobe á 100
* **1.000 Zufallsstichproben á 100**
	+ Min: 13,37
	+ Max: 18,77
	+ **Mean: 15,53**
	+ **Mean Grundgesamt: 15,55**
durchschnittlicher Lohn

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S9_5.png")
knitr::asis_output("</div>")
```
.quelle[(Hudec 2010)]

---

# Schätzen relativer Häufigkeiten

* **Grenzwertsatz** der Statistik:
	+ Bei großem n (≥100) sind relative Häufigkeiten p annähernd normalverteilt mit dem Erwartungswert π

	**→ Aus Normalverteilung: **Konfidenzintervall zur Sicherheit 1-α in dem der Parameter π ausgehend vom Stichprobenergebnis p liegt

	+ Große Grundgesamt (~N>10.000) & große Stichprobe (n≥100):

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S11_6.png")
knitr::asis_output("</div>")
```
.quelle[(Quatember 2007:127)]

---

# Das Konfidenzintervall – es funktioniert!

* Die 95%-“Irrtumswahrscheinlichkeit“ als **Überdeckungswahrscheinlichkeit**:
	+ Annahme: 100 Stichproben aus gleicher Grundgesamt****
	+ **Verfahren wird in 95 Fällen den unbekannten Parameter in Grundgesamt überdecken**

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S13_7.png")
knitr::asis_output("</div>")
```
.quelle[(adaptiert von: Liero 2009:o.S.)]

---

# Das Ganze in Action:

* https://www.sora.at/fileadmin/downloads/projekte/Austria_Spread_of_SARS-CoV-2_Study_Report.pdf

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S14_8.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Quick and dirty?

---

# Die Stichprobenziehung


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S16_9.png")
knitr::asis_output("</div>")
```
.quelle[(Überarbeitung von: Schnell, Hill & Esser 1995:252)]

---

# Die Stichprobenziehung

Willkürliche Auswahl:Kein Grundgesamt definiertMassive Verzerrung → wiss. bedenklich„Typische“ Fälle:Problem: Definition von „charakteristisch“ ohne Wissen über Grundgesamt→ Redefinition von Grundgesamt„Extreme“ Fälle:Fälle mit „extremen“ Merkmalsausprägungen→ Redefinition von Grundgesamt

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S18_10.png")
knitr::asis_output("</div>")
```
.quelle[(Schnell, Hill & Esser 1995:252)]

---

# Die Stichprobenziehung

KonzentrationsprinzipFälle die fast die gesamte Verteilung eines Merkmals in Grundgesamt bestimmenSchneeballverfahren:Soziale Netzwerke→ Auswahl bei „seltenen“ Populationen

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S20_11.png")
knitr::asis_output("</div>")
```
.quelle[(Schnell, Hill & Esser 1995:252)]

---

# Die Stichprobenziehung

Quotenauswahl:

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S21_12.png")
knitr::asis_output("</div>")
```
.quelle[(Höferl, 2019, CC BY)]

---

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Repräsentative Stichproben - aber wie?

---

# Die Stichprobenziehung


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S24_13.png")
knitr::asis_output("</div>")
```
.quelle[(Überarbeitung von: Schnell, Hill & Esser 1995:252)]

---

# Die Stichprobenziehung

1. Wahrscheinlichkeits-auswahl:UrnenmodellLiefert bei Häufigkeiten:

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S26_14.png")
knitr::asis_output("</div>")
```

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S26_15.png")
knitr::asis_output("</div>")
```
.quelle[(Schnell, Hill & Esser 1995:252)]

---

# Die Stichprobenziehung

2. Geschichtete Stichproben:Jedes Element Grundgesamt→ gehört einer Schicht anVorteile:Untersch. Streuung in Schichten → genauere MessungAussagen zu den Schichten selbst von InteresseNachteile:Kenntnis/Schätzung Parameter GrundgesamtheitNachtr. Gewichtung*

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S28_16.png")
knitr::asis_output("</div>")
```
.quelle[(Schnell, Hill & Esser 1995:252)]

---

# Ein Beispiel: Die COVID-19 Prävalenz Studie (2020)

* Quelle: https://www.bmbwf.gv.at/Themen/Forschung/Aktuelles/Corona-Studien.html
* n=1.544 (Zufallsstichprobe, österreichweit)
* Testzeitraum: 1.4. bis 6.4.2020
* Bruttostichprobe:
	+ Zufallsauswahl von 249 Gemeinden*:
		- Schichtung entlang Bundesländer & Gemeindegröße
		- * … Wiener Bezirke = Gemeinde
	+ Innerhalb der Gemeinden: Zufallsauswahl Haushalte
		- Adressdaten: Telefonverzeichnis & RLD-Verfahren
		- ~ 2.197 / 0,77 = 2.850 kontaktierte HH
		- 23% Verweigerung bei HH
	+ Innerhalb der Haushalte: Zufallsauswahl Haushaltsmitglied
		- Aus 2.197 HH → 1.544 Tests
.quelle[(SORA, 2020)]

---

# @ 249 Gemeinden

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/ImageSlide_30.png")
knitr::asis_output("</div>")
```
.quelle[()]

---
.quelle[(SORA, 2020)]

---

# Die Stichprobenziehung

3. Klumpenstichproben:Auswahl nicht auf Grundgesamt sondern auf zusammengefasste Elemente (=Klumpen)→ wenn keine Liste Grundgesamt, aber der Klumpen vorhandenBSP:Zählsprengel, Rasterzellen etc.Nachteil:Unterschiede in Klumpen gehen nicht ein → Klumpeneffekt

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S32_17.png")
knitr::asis_output("</div>")
```
.quelle[(Schnell, Hill & Esser 1995:252)]

---

# Die Stichprobenziehung

4. Mehrstufige Auswahlen:Zufallsauswahl in mehreren Stufen→ wenn keine Listen Grundgesamt, aber Listen aggr. ElementeKlassiker: FlächenstichprobenFlächen=PrimäreinheitDarin Haushalte auswählen= SekundäreinheitenDarin Befragungsperson(en) auswählenProblem:Untersch. große Primäreinheiten→ propability proportinal to size

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh5-1_primaer_sampling/EH5-1_Primaer_4_Sampling_S34_18.png")
knitr::asis_output("</div>")
```
.quelle[(Schnell, Hill & Esser 1995:252)]

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> dl;dr

---

# Konklusio

* Sampling zentral für **Validität** der Daten
	+ repräsentativ: ermöglicht Rückschlüsse auf Grundgesamtheit
	+ informativ: keine Rückschlüsse
* **→ Aufwand-Ertrags-Abwägung** notwendig
* **Verfahren:**
	+ **einfache Zufallsstichprobe: **nur wenn Grundgesamtheit bekannt
	+ **geschichtete Zufallsstichprobe: **s.o.
	+ **Klumpenstichprobe: **kann verzerren
	+ **Probability Proportional to Size (PPS) Verfahren: **kein Klumpeneffekt
