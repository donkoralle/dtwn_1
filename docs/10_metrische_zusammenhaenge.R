## ----message=FALSE, warning=FALSE---------------------------------
library(tidyverse)
library(scales)
library(ggrepel)

agesRohdaten <- read.csv2("data/agesRohdaten_25-05-21.csv", encoding = "UTF-8") %>%
  as_tibble()
covidCases <- agesRohdaten %>%
  mutate(Anzahl_100k = Anzahl/(AnzEinwohner/100000),
         AnzahlTot_100k = AnzahlTot/(AnzEinwohner/100000),
         Anzahl7Tage_100k = AnzahlFaelle7Tage/(AnzEinwohner/100000))
covidCases <- covidCases %>%
  mutate(bula = factor(floor(GKZ/100),
                       levels = c(1:9),
                       labels = c("Burgenland",
                                  "Kärnten",
                                  "Niederösterreich",
                                  "Oberösterreich",
                                  "Salzburg",
                                  "Steiermark",
                                  "Tirol",
                                  "Vorarlberg",
                                  "Wien")
                       )
         )


## ----warning=FALSE------------------------------------------------
ggplot(covidCases, aes(x = Anzahl_100k, y = AnzahlTot_100k)) +
  geom_point(size = 3, shape=21, fill = "red") +
  ggrepel::geom_text_repel(aes(label = Bezirk),
                size = 3,
                color = "gray",
                box.padding = 1) +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", 
                                                  decimal.mark = ",")) +
  theme_bw() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x=element_text(angle = 45, hjust = 1)) +
  labs(title = "Zusammenhang COVID-19 Fälle & Verstorbene",
       x = "\nCOVID-19 Fälle je 100.000 EW",
       y = "an/mit COVID-19\nVerstorbene je 100.000 EW\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at",
       color = "Bundesland")


## ----warning=FALSE------------------------------------------------
ggplot(covidCases, aes(x = Anzahl_100k, y = AnzahlTot_100k)) +
  geom_point(aes(fill = bula), shape=21, size = 3) +
  ggrepel::geom_text_repel(aes(label = Bezirk),
                size = 3,
                color = "gray",
                box.padding = 1) +  
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", 
                                                  decimal.mark = ",")) +
  theme_bw() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  labs(title = "Zusammenhang COVID-19 Fälle & Verstorbene",
       x = "\nCOVID-19 Fälle je 100.000 EW",
       y = "an/mit COVID-19\nVerstorbene je 100.000 EW\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at",
       fill = "Bundesland")


## -----------------------------------------------------------------
ggplot(covidCases, aes(x = Anzahl_100k, y = AnzahlTot_100k)) +
  geom_point(aes(fill = bula), shape=21, size = 2.5) +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", 
                                                  decimal.mark = ",")) +
  theme_bw() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x=element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Zusammenhang COVID-19 Fälle & Verstorbene",
       x = "\nCOVID-19 Fälle je 100.000 EW",
       y = "an/mit COVID-19\nVerstorbene je 100.000 EW\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at",
       fill = "Bundesland") +
  facet_wrap(~bula)


## ----echo=FALSE---------------------------------------------------
mySlideshow <- "eh9-2_zusammenhangsmasse_metrisch_v1"
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


## -----------------------------------------------------------------
library(ggpubr)
q1 <- ggplot(covidCases, aes(sample = Anzahl_100k)) +
geom_qq(color = "red") +
geom_qq_line() +
labs(y = "Anzahl_100k")
q2 <- ggplot(covidCases, aes(sample = AnzahlTot_100k)) +
geom_qq(color = "red") +
geom_qq_line() +
labs(y = "AnzahlTot_100k")
ggarrange(q1, q2,
          # labels = c("AnzahlTot_100k", "AnzahlTot_100k"),
          ncol = 2, nrow = 1)


## -----------------------------------------------------------------
library(rstatix)
covidCases %>%
  summarise(across(c(Anzahl_100k,AnzahlTot_100k),
                   ~ rstatix::shapiro_test(.)$p.value))


## -----------------------------------------------------------------
cor.test(covidCases$Anzahl_100k, covidCases$AnzahlTot_100k, method = "spearman")


## -----------------------------------------------------------------
cor.test(covidCases$Anzahl_100k, covidCases$AnzahlTot_100k)


## -----------------------------------------------------------------
r_values <- covidCases %>%
  filter(GKZ != 900) %>%
  group_by(bula) %>%
  summarise(r = cor(Anzahl_100k, AnzahlTot_100k, method = "spearman"))
p_values <- covidCases %>%
  filter(GKZ != 900) %>%
  group_by(bula) %>%
  summarise(p = cor.test(Anzahl_100k, AnzahlTot_100k, method = "spearman")$p.value)
r_values %>%
  inner_join(p_values, by = c("bula")) %>%
  filter(p <= 0.05) %>%
  arrange(-r) %>%
  knitr::kable(digits = 2)


## ----message=FALSE, warning=FALSE---------------------------------
library(GGally)
covidCases %>%
  filter(GKZ != 900) %>%
  GGally::ggpairs(., columns = c("Anzahl_100k", "AnzahlTot_100k", "Anzahl7Tage_100k"),
          legend = 1,
          mapping = ggplot2::aes(color = bula),
          diag = list(continuous = wrap("densityDiag", alpha=0.5)),
          upper = list(continuous = wrap("cor", method = "spearman", size = 3))
          ) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  labs(title = "Zusammenhang COVID-19 Fälle & Verstorbene\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at",
       fill = "Bundesland")


## -----------------------------------------------------------------
library(ggpubr)
ggscatter(covidCases, x = "Anzahl_100k", y = "AnzahlTot_100k",
          color = "bula", palette = "jco",
          alpha = 0.3,
          size = 2.5) +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".",
                                                  decimal.mark = ",")) +
  facet_wrap(~bula) +
  stat_cor(aes(color = bula),
           method = "spearman",
           p.accuracy = 0.01, 
           r.accuracy = 0.01,
           size = 3,
           hjust = 1,
           label.x = 12500,
           label.y = 20) +
  theme_bw() +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x=element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Zusammenhang COVID-19 Fälle & Verstorbene",
       x = "\nCOVID-19 Fälle je 100.000 EW",
       y = "an/mit COVID-19\nVerstorbene je 100.000 EW\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at",
       color = "Bundesländer")


## ----echo=FALSE---------------------------------------------------
mySlideshow <- "eh9-3_regression_v1"
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


## ----message=FALSE, warning=FALSE---------------------------------
ggplot(covidCases, aes(x = Anzahl_100k, y = AnzahlTot_100k)) +
  geom_point(size = 3, shape=21, fill = "red") +
  geom_smooth(method = "lm", formula= y~x, se = FALSE,
              size = 1.25, color = "black") +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", 
                                                  decimal.mark = ",")) +
  ggrepel::geom_text_repel(aes(label = Bezirk),
              size = 3,
              color = "gray",
              box.padding = 1) +
  theme_bw() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x=element_text(angle = 45, hjust = 1)) +
  labs(title = "Zusammenhang COVID-19 Fälle & Verstorbene",
       x = "\nCOVID-19 Fälle je 100.000 EW",
       y = "an/mit COVID-19\nVerstorbene je 100.000 EW\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at",
       color = "Bundesland")


## ----message=FALSE, warning=FALSE---------------------------------
ggscatter(covidCases, x = "Anzahl_100k", y = "AnzahlTot_100k",
          shape=21, size = 3, fill = "red", color = "black",
          add = "reg.line", add.params = list(color = "blue")) +
  stat_cor(aes(label = paste(..rr.label..,
                             ..p.label..,
                             sep = "~`,`~")),
           color = "blue",
           label.x = 9000,
           label.y = 245) +
  stat_regline_equation(color = "blue",
                        label.x = 9000,
                        label.y = 220) +
  ggrepel::geom_text_repel(aes(label = Bezirk),
            size = 3,
            color = "gray",
            box.padding = 1) +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".", 
                                                  decimal.mark = ",")) +
  theme_bw() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x=element_text(angle = 45, hjust = 1)) +
  labs(title = "Zusammenhang COVID-19 Fälle & Verstorbene\n",
       x = "\nCOVID-19 Fälle je 100.000 EW",
       y = "an/mit COVID-19\nVerstorbene je 100.000 EW\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at")


## ----message=FALSE, warning=FALSE---------------------------------
ggscatter(covidCases, x = "Anzahl_100k", y = "AnzahlTot_100k",
          color = "bula",
          alpha = 0.3,
          size = 2.5,
          add = "reg.line",
          cor.coeff.args = list(method = "pearson",
                                label.sep = "\n")) +
  stat_cor(aes(label = paste(..rr.label..,
                             ..p.label..,
                             sep = "~`,`~"),
               color = bula),
           size = 3,
           label.x = 1500) +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".",
                                                  decimal.mark = ",")) +
  theme_bw() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x=element_text(angle = 45, hjust = 1)) +
  labs(title = "Zusammenhang COVID-19 Fälle & Verstorbene\n",
       x = "\nCOVID-19 Fälle je 100.000 EW",
       y = "an/mit COVID-19\nVerstorbene je 100.000 EW\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at",
       color = "Bundesland")


## ----message=FALSE, warning=FALSE---------------------------------
ggscatter(covidCases, x = "Anzahl_100k", y = "AnzahlTot_100k",
  color = "bula", palette = "jco",
  alpha = 0.3,
  size = 2.5,
  add = "reg.line") +
  scale_x_continuous(labels = scales::label_comma(big.mark = ".",
                                                  decimal.mark = ",")) +
  facet_wrap(~bula) +
  stat_cor(aes(label = paste(..rr.label..,
                             ..p.label..,
                             sep = "~`,`~"),
               color = bula),
           p.accuracy = 0.01,
           size = 3,
           hjust = 1,
           label.x = 12500,
           label.y = 20) +
  theme_bw() +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x=element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Zusammenhang COVID-19 Fälle & Verstorbene",
       x = "\nCOVID-19 Fälle je 100.000 EW",
       y = "an/mit COVID-19\nVerstorbene je 100.000 EW\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at",
       fill = "Bundesland")


## -----------------------------------------------------------------
r_values %>%
  inner_join(p_values, by = c("bula")) %>%
  filter(p <= 0.05) %>%
  arrange(-r) %>%
  knitr::kable(digits = 2)

