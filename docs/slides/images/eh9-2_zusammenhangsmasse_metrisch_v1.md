---
title: "Everything hangs together: Zusammenhangsmaße"
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

# .emolarge[🤔]<br> Everything hangs together?

---

# Ein Überblick auf ausgewählte Maße


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-2_korrel/eh9-2_zusammenhangsmasse_metrisch_v1_S2_1.png")
knitr::asis_output("</div>")
```
.quelle[(Eigene Überarbeitung 2016 von Hager, 2011)]

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Normalverteit?

---

# Überprüfen der Normalverteilung metr. Variablen 1/3

* **Visuell: **Histogramm & Normalverteilungs-kurve

---

# Überprüfen der Normalverteilung metr. Variablen 2/3

* **Visuell: **Q(uantil)-Q(uantil) Plots

---

# Überprüfen der Normalverteilung metr. Variablen 2/3

* **Numerisch: **Kolmogorov-Smirnov-Anpassungstest
	+ BSP: Q15S – Monatliche Brutto-Mietkosten je m² Wohnfläche

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Metrische Zusammenhänge messen?

---

# Zusammenhang zwischen zwei metrischen Variablen

* **Streudigramm:**grafische Darstellung von Wertpaaren zweier metrischer Merkmale

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-2_korrel/eh9-2_zusammenhangsmasse_metrisch_v1_S7_2.png")
knitr::asis_output("</div>")
```
.quelle[(Eigene Überarbeitung 2018 von: Quatember 2007:667)]

---

# Zusammenhang zwischen zwei metrischen Variablen

**Ermittlung der Korrelation r:**

* Berechnen der Kovarianz Sxy
* Kovarianz (wie χ²-Wert) ohne obere & untere Beschränkung**→ Normierung auf Wertbereich -1 bis +1**
* **Korrelationskoeffizient r** [-1; +1] 

					

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-2_korrel/eh9-2_zusammenhangsmasse_metrisch_v1_S10_3.png")
knitr::asis_output("</div>")
```
.quelle[(Eigene Überarbeitung 2019 von: Quatember 2007:69)]

---

# Zusammenhang zwischen zwei metrischen Variablen

* **Interpretation** des Korrelationskoeffizienten:

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-2_korrel/eh9-2_zusammenhangsmasse_metrisch_v1_S11_4.png")
knitr::asis_output("</div>")
```
.quelle[(Eigene Überarbeitung 2019 von: Quatember 2007:71)]

---

# Pearsons r & Spearmans ρ („rho“) im Team nutzen 

* **Pearsons Problem: **linearer Zusammenhang
	+ Wirkung von Ausreißern
* **Umgang damit:**
	+ Zunächst: Prüfen der Normalverteilung der verwendeten Variablen
	+ Pearsons r & Spearmans ρ ermitteln→ „deutliche“ Unterschiede = Ausreißer verzerren Pearsons r
	+ Ggf. Ausreißer ausschließen & erneut Maßzahlen berechnen

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-2_korrel/eh9-2_zusammenhangsmasse_metrisch_v1_S12_5.png")
knitr::asis_output("</div>")
```
.quelle[(Matheguru, o.J.)]

---

# Spearmans ρ („rho“) für ordinale Zusammenhänge

n … Anzahl der Wertpaare

rg(xi bzw. yi) … Rang in Variable x bzw. y

* Misst **beliebige monotone Zusammenhänge:**
	+ Wertbereich: wie Pearsons r (-1 bis +1)
	+ Grundlage: Differenz in den Rängen eines Wertpaares
	+ Praktisch: Pearsons r auf Basis von Rängen
* **Problem: **Bindungen („Ties“)
	+ Daumenregel: unter 20% aller Wertpaare → Ränge über Mittelwerte
