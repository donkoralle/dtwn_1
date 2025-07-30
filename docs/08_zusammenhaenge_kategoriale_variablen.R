## ----message=FALSE, warning=FALSE-----------------------------------------------------------------
library(tidyverse)

Trendfragen <- read.csv2("data/Trendfragen_Corona_45-20/ZA7677_v1-0-0.csv", encoding = "UTF-8") %>%
  as_tibble(.) %>%
  mutate(id = row_number())
TrendRedux <- Trendfragen %>%
  select(id, bcor5, s8) %>%
  mutate(bcor5 = as.factor(bcor5), s8 = as.factor(s8)) %>%
  filter(s8 != "-1") %>%
  mutate(s8 = droplevels(s8))  


## -------------------------------------------------------------------------------------------------
n.table <- table(TrendRedux$bcor5, TrendRedux$s8)
prop.table(n.table, 2) %>%
  round(., 2) %>%
  knitr::kable()


## ----echo=FALSE-----------------------------------------------------------------------------------
mySlideshow <- "eh9-1_zusammenhangsmasse_nominal_v1"
mySlideWidth <- 600
mySlideHeight <- 450

knitr::asis_output(paste(
  "<iframe src=\"https://kamihoeferl.at/lehre/vu_sozwiss_1/___slides/",
  mySlideshow,
  ".html#1\" width=\"",
  mySlideWidth,
  "px\" height=\"",
  mySlideHeight,
  "px\" class=\"videoframe\" allowfullscreen>Your browser doesnot support iframes <a href=\"<https://kamihoeferl.at/lehre/vu_sozwiss_1/___slides/",
  mySlideshow,
  ".html#1\">click here to view the page directly.</a></iframe>", sep = ""))
knitr::asis_output(paste(
  "<div><a href=\"https://kamihoeferl.at/lehre/vu_sozwiss_1/___slides/",
  mySlideshow,
  ".pdf\">Die Slides als PDF</a></div><br>",
  sep = ""))


## ----auswerteverfahren, out.width=600,  echo=FALSE, fig.cap="Auswerteverfahren für unabhängige Stichproben (Quelle: Eigene Überarbeitung 2016 von [Hager, 2011](https://www.univie.ac.at/soziologie-statistik/lingu/master/Signifikanztests.pdf))", out.extra = "class = 'videoframe fullscreen-enabled'"----
knitr::include_graphics("images/EH5_1_Gruppenunterschiede-Slide-4_cut.png")


## -------------------------------------------------------------------------------------------------
chi <- chisq.test(n.table)
chi


## -------------------------------------------------------------------------------------------------
n.table %>%
  knitr::kable()


## -------------------------------------------------------------------------------------------------
fish <- fisher.test(TrendRedux$s8, TrendRedux$bcor5, simulate.p.value=TRUE)
fish


## -------------------------------------------------------------------------------------------------
round(chi$residuals, 2)


## ----message=FALSE--------------------------------------------------------------------------------
library(corrplot)
corrplot(chi$residuals, is.corr = FALSE,
         outline = TRUE, tl.col = "black",
         col = RColorBrewer::brewer.pal(11, "RdBu"))



## -------------------------------------------------------------------------------------------------
round(prop.table(chi$residuals^2)*100, 2)


## -------------------------------------------------------------------------------------------------
round(prop.table(chi$residuals^2)*100, 2) %>%
  as.data.frame() %>%
  group_by(Var2) %>%
  summarise(perc = sum(Freq)) %>%
  arrange(-perc)


## -------------------------------------------------------------------------------------------------
contrib <- (chi$residuals^2 / chi$statistic) * 100
corrplot(contrib, is.corr = FALSE,
         outline = TRUE, tl.col = "black",
         col = RColorBrewer::brewer.pal(9, "Reds"))


## -------------------------------------------------------------------------------------------------
library(lsr)
lsr::cramersV(TrendRedux$s8, TrendRedux$bcor5)

