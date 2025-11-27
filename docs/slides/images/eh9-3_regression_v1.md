---
title: "Everything hangs together: Lineare Regression"
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

# .emolarge[🤔]<br> Wozu Regression?

---

# Exkurs: Unterschied Regression & Korrelation

* **Korrelation: **Stärke eines monotonen Zusammenhangs→ Korrelation kann von einem 3. (nicht berücksichtigten)Faktor ausgehen
	+ Prognose i.e.S. nicht möglich
* **Regression: **Beschreibung einer Ursache-Wirkungs-Beziehung
	+ Formalisierung eines Zusammenhangs als (z.B. lineare) Gleichung→ Wie genau wirkt ein Faktor auf einen anderen?
	+ Möglichkeit zur **Prognose**

---

# Lineare Regression im Detail

* **Abhängige Variable **(„Regressand“): zu erklärende Variable
* **Unabhängige Variable **(„Regressor“): erklärende Variable(n)
* Überführung in lineare Gleichung*:
* Parameterschätzung über Methode der kleinsten Fehlerquadrate
r1
r2
r3
r4

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-3_regression/eh9-3_regression_v1_S3_1.png")
knitr::asis_output("</div>")
```

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-3_regression/eh9-3_regression_v1_S3_2.png")
knitr::asis_output("</div>")
```

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-3_regression/eh9-3_regression_v1_S3_3.png")
knitr::asis_output("</div>")
```

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-3_regression/eh9-3_regression_v1_S3_4.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# Lineare Regression im Detail


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-3_regression/eh9-3_regression_v1_S4_5.png")
knitr::asis_output("</div>")
```

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-3_regression/eh9-3_regression_v1_S4_6.png")
knitr::asis_output("</div>")
```

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-3_regression/eh9-3_regression_v1_S4_7.png")
knitr::asis_output("</div>")
```

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-3_regression/eh9-3_regression_v1_S4_8.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# @ Güte einer gefunden Regressionslösung

* **Bestimmtheitsmaß R²:**
	+ Durch Regression erklärte Varianz der abhängigen Variable→ Wertbereich: 0 bis 1 (= vollständige Varianzerklärung)

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-3_regression/eh9-3_regression_v1_S5_9.png")
knitr::asis_output("</div>")
```

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh9-3_regression/eh9-3_regression_v1_S5_10.png")
knitr::asis_output("</div>")
```
.quelle[()]
