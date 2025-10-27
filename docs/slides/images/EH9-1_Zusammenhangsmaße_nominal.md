---
title: "Everything hangs together: Zusammenhangsmaße"
subtitle: "716408 | Sozialwiss. Methoden – How 2 do Things with Numbers"
author: "KMH"
date: "SS 20 | KMH (updated: `r Sys.Date()`)"
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

# Auf den Zusammenhang kommt es an I


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/EH9-1_Zusammenhangsmaße_nominal_S2_1.png")
knitr::asis_output("</div>")
```
.quelle[(https://www.univie.ac.at/ksa/elearning/cp/quantitative/quantitative-113.html)]

---

# Auf den Zusammenhang kommt es an II


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/EH9-1_Zusammenhangsmaße_nominal_S3_2.png")
knitr::asis_output("</div>")
```
.quelle[(Cutter, Mitchell & Scott 2000)]

---

# Auf den Zusammenhang kommt es an III

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/ImageSlide_5.png")
knitr::asis_output("</div>")
```
.quelle[()]

---
.quelle[(National Geographic 2012)]

---

# Auf den Zusammenhang kommt es an IV

* Vier und mehr Merkmale: Zusammenhänge mittels paarweisen Scatterplots

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/EH9-1_Zusammenhangsmaße_nominal_S6_3.png")
knitr::asis_output("</div>")
```
.quelle[Charles Joseph Minard, Wikimedia, PD]

---

# Summary: Skalenniveaus quant. Daten

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/ImageSlide_7.png")
knitr::asis_output("</div>")
```
.quelle[ (vgl. Bortz & Döring 2009:69; Reuber & Pfaffenbach 2005:94)]

---

# Zusammenhänge erkennen

* Je Skalenniveau → **unterschiedliche Maße**
* **2 Möglichkeiten **bivariater Analyse:
	+ Explorativ:
		- Grafisch („Streudiagramm“)
	+ Stat. Kennzahlen:
			- Beleg Zusammenhang (→ Richtung)
			- Stärke Zusammenhang
	+ Hypothesentests:
		- kategoriale Variablen

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Zusammenhänge - aber wie messen?

---

# Ein Überblick auf ausgewählte Maße


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/EH9-1_Zusammenhangsmaße_nominal_S10_4.png")
knitr::asis_output("</div>")
```
.quelle[(Eigene Überarbeitung 2016 von Hager, 2011)]

---

# Ein Überblick auf ausgewählte Zusammenhangsmaße

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/ImageSlide_11.png")
knitr::asis_output("</div>")
```
.quelle[(Eigene Erstellung 2018; adaptiert von: Heger & Prust 2009)]

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Kategoriale Zusammenhänge erkunden

---

# Zusammenhang bei zwei nicht-metrischen Variablen

* Klassiker: **Kreuz- bzw. Kontingenztabellen**
	+ Absolute / relative Häufigkeiten der Kombination von Merkmalsausprägungen → nominale & ordinale Daten
	+ Inhaltliche Auswertung → Randsummen
	+ BSP: 

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/EH9-1_Zusammenhangsmaße_nominal_S12_5.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# Zufall, oder nicht? Der Chi²-Test

* Suche nach „signifikanten“ (α < 0,05) Zusammenhängen

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/EH9-1_Zusammenhangsmaße_nominal_S14_6.png")
knitr::asis_output("</div>")
```
.quelle[(Kuckartz et al. 2010:87ff.)]

---

# Der Chi²-Test: Ein Beispiel


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/EH9-1_Zusammenhangsmaße_nominal_S15_7.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# Der Chi²-Test: Ein Beispiel

**BSP: **Verkehrsmittelwahl

	+ **Einshypothese H****1****: Es besteht ein statistisch signifikanter Zusammenhang zwischen Geschlecht & Verkehrsmittelwahl**
	+ **Nullhypothese H****0****:Es besteht KEIN ein statistisch signifikanter Zusammenhang zwischen Geschlecht & Verkehrsmittelwahl**
	+ ****
* Excel (CHIQU.TEST)bzw. R (chisq.test) → p=0,001203623
	+ **Da p kleiner ist als ****α**** (0,05) → Ablehnung der Nullhypothese H****0******Es besteht ein statistisch signifikanter Zusammenhang zwischen Geschelcht & Verkehrsmittel

---

# Cramers V: Die Stärke des Zusammenhangs zwischen kategorialen Variablen

* _ 𝜒 2 __𝜒__ 𝜒 2 __2__ 𝜒 2 _ bei Unabhängigkeit = 0 (beobachtet = erwartet)
	+ Kann beliebig groß werden**→ Normierung notwendig um Aussage zu Stärke desZusammenhangs [0 bis +1]zu treffen**
	+ ****
* **Cramers V [0; +1]:**

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/EH9-1_Zusammenhangsmaße_nominal_S18_8.png")
knitr::asis_output("</div>")
```
.quelle[(Quatember 2007:66)]

---

# Cramers V: Die Stärke des Zusammenhangs zwischen kategorialen Variablen

* **Cramers V [0; +1]:**
	+ Je **größer** V ist, umso **stärker** der stat. der Zusammenhang

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/EH9-1_Zusammenhangsmaße_nominal_S19_9.png")
knitr::asis_output("</div>")
```
.quelle[(Quatember 2007:66)]

---

# Cramers V in Action


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/EH9-1_Zusammenhangsmaße_nominal_S20_10.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# Cramers V in Action

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-1_nomainale_zusammenhaenge/ImageSlide_21.png")
knitr::asis_output("</div>")
```
.quelle[(Quatember 2007:66)]
