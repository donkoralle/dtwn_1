---
title: "R: Grundkonzepte"
subtitle: "716408 | Sozialwiss. Methoden – How 2 do Things with Numbers"
author: "KMH"
date: "SS 22 | KMH (updated: `r Sys.Date()`)"
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

# .emolarge[🤔]<br> Wozu Statistikpakete?

---

# Ein idealtypischer Ablauf

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh6-1_r_basics/ImageSlide_2.png")
knitr::asis_output("</div>")
```
.quelle[(Höferl, 2021, CC BY)]

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> How to?

---

# Base R, Packages & RStudio … all you need

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh6-1_r_basics/ImageSlide_3.png")
knitr::asis_output("</div>")
```
.quelle[(Höferl, 2020, CC BY)]

---

# Lost in R(?)

Eine Auswahl für Einsteigerinnen und Einsteiger:

* https://link.springer.com/book/10.1007/978-3-8348-9677-3
* https://www.statmethods.net/index.html
* https://www.uni-muenster.de/Stochastik/lehre/SS09/PrakStat/Skript.pdf

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh6-1_r_basics/EH6-1_R_basics_S5_1.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# Der R-Interpreter (aka. „R-Console“)


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh6-1_r_basics/EH6-1_R_basics_S6_2.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# Grundlegende arithmetische Operatoren in R

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh6-1_r_basics/ImageSlide_7.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# Grundlegende logische Operatoren in R


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh6-1_r_basics/EH6-1_R_basics_S8_3.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# Grundlegende statistische Operatoren in R


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh6-1_r_basics/EH6-1_R_basics_S9_4.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Geht`s auch einfacher?

---

# Komfortabler: R Studio

* https://www.rstudio.com/

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh6-1_r_basics/EH6-1_R_basics_S10_5.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# Aufwärmrunde in RStudio

* **Kapitel „Orientierung“** im Kurs „R für Psychos“ von Lukas Burk & Tobias Anton:https://r-intro.tadaa-data.de/book/orientierung.html

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh6-1_r_basics/EH6-1_R_basics_S11_6.png")
knitr::asis_output("</div>")
```
.quelle[(Burk & Anton, 2019, CC BY-NC 4.0)]
