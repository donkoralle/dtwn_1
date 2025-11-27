---
title: "Zwei Wege um Daten zu gewinnen"
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

# .emolarge[🤔]<br> Daten sind Daten - oder?

---

# Daten gewinnen – aber wie?

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh4-1_primaer_sekundaer/ImageSlide_2.png")
knitr::asis_output("</div>")
```
.quelle[(Höferl 2022, CC BY)]

---

# Zwei Wege um Daten zu gewinnen

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh4-1_primaer_sekundaer/ImageSlide_3.png")
knitr::asis_output("</div>")
```
.quelle[(Höferl 2022, CC BY)]

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Wie entstehen Primärdaten?

---

# Primärerhebung quantitativer Daten


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh4-1_primaer_sekundaer/EH4-1_Primaer_Sekundaerdaten_v1_S4_1.png")
knitr::asis_output("</div>")
```
.quelle[(Überarbeitung von: Reuber & Pfaffenbach 2005:21)]

---

# Primärerhebung quantitativer Daten

* **3 zentrale Methoden:**
	+ **Beobachtung**systematisches Beobachten & quant. Registrieren „relevanter“ Sachverhalte 
	+ **Befragen**systematisch gesteuerte Kommunikation zwischen Personen; basierend auf standardisierten Fragen- & Antwortenkatalog
	+ **Nonreaktive Verfahren**quant. Erfassen einzelner Merkmale von Interesse in Texten, Bildern & Videos

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Wie gewinnt man sekundärstatistische Daten?

---

# Sekundärstatistische Daten gewinnen

* Einfachster Fall: **Repositorien**
	+ Metadaten sichern Auffindbarkeit
		- https://www.data.gv.at/
		- https://www.statistik.at/web_de/services/statcube
		- https://www.govdata.de/
		- https://www-genesis.destatis.de/genesis/online
* Aufwändiger: **Digitalisierung**
	+ Konversionsfehler → erhöhter Zeitaufwand zur Qualitätssicherung
	+ Erschließen (historischer) Datensets

---

# Und woher kommen Sekundärdaten?

* **Sekundärdaten**
	+ Meist: durch Verarbeitung aus Primärdaten gewonnen
		- Eigene Erstellung
		- Erstellung durch Dritte
* **Verarbeitung:**
	+ Klassifikation: 
		- Zuordnung zu Klasse von Dingen mit gemeinsamen Eigenschaften
	+ Aggregation: 
		- Gruppierung bestehender Klassen zu neuen (→ Definition einer neuen Klasse)
	+ Generalisierung: 
		- Definition Teilmengen-Beziehung zwischen Elementen verschiedener Klassen

---

# @ Klassifikation, Aggregation & Generalisierung

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh4-1_primaer_sekundaer/ImageSlide_8.png")
knitr::asis_output("</div>")
```
.quelle[(Höferl, 2019, CC BY)]

---

# @ Klassifikation, Aggregation & Generalisierung

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh4-1_primaer_sekundaer/ImageSlide_9.png")
knitr::asis_output("</div>")
```
.quelle[(Höferl, 2019, CC BY)]

---

# Woher Sekundärdaten beziehen?

https://statcube.at

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh4-1_primaer_sekundaer/EH4-1_Primaer_Sekundaerdaten_v1_S10_2.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# Woher Sekundärdaten beziehen?

https://www-genesis.destatis.de

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh4-1_primaer_sekundaer/EH4-1_Primaer_Sekundaerdaten_v1_S11_3.png")
knitr::asis_output("</div>")
```
.quelle[()]

---

# Woher Sekundärdaten beziehen

```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh4-1_primaer_sekundaer/ImageSlide_12.png")
knitr::asis_output("</div>")
```
.quelle[(Planemad, Wikimedia, CC0; GitHub, Wikimedia, CC0; GESIS, Wikimedia, CC0)]

---

# Eine Auswahl

* Wikidata: https://www.wikidata.org
* gesis: https://www.gesis.org
* European Data Portal: https://data.europa.eu
* OECD Data: https://data.oecd.org/
* UNdata: https://data.un.org/
* NASA Socioeconomic Data and Application Center: https://sedac.ciesin.columbia.edu/
* Registry of Research Data Repositories: https://www.re3data.org/

---

class: zwischentitel, center, middle

# .emolarge[🤔]<br> Was jetzt ... ?

---

# Was jetzt: Primär- oder Sekundärstatische Daten?


```{r echo=FALSE}
knitr::asis_output('<div class="container">')
knitr::include_graphics ("images/eh4-1_primaer_sekundaer/EH4-1_Primaer_Sekundaerdaten_v1_S14_4.png")
knitr::asis_output("</div>")
```
.quelle[(United Nations Photo, CC BY NC ND)]

---

# Was jetzt: Primär- oder Sekundärstatische Daten?

**Pragmatische Entscheidung:**

* Mit welchem **Aufwand** kann ich mir einen Überblick auf das Angebot an Sekundärdaten verschaffen?
* Decken diese Sekundärdaten meine **Untersuchungsfrage & -objekte **ab?
* Sind diese Sekundärdaten für meinen Zweck **nutzbar**?
* Welche **Ressourcen** stehen mir zur Datengewinnung zur Verfügung?
	+ Können Sekundärdaten angekauft werden?
* …

---

# Egal ob Primär- oder Sekundärdaten:

**Qualitätskriterien nicht vergessen:******

* **Objektivität (Unabhängigkeit):**
	+ Unabhängigkeit eines Messinstruments von Untersucher & Untersuchungssituation
* **Reliabilität (Zuverlässigkeit):**
	+ Reproduzierbarkeit von Messergebnissen
* **Validität (Gültigkeit):**
	+ Genauigkeit, mit dem ein Test das misst, was er messen soll (= Erkenntnisgegenstand)
