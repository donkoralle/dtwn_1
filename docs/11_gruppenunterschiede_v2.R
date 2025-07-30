## ----message=FALSE, warning=FALSE---------------------------------------------------------
library(tidyverse)
library(scales)

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


## -----------------------------------------------------------------------------------------
ggplot(covidCases, aes(x = forcats::fct_reorder(bula, AnzahlTot_100k, median), y = AnzahlTot_100k)) +
  geom_boxplot(fill = "azure3") +
  geom_jitter(shape=16, position=position_jitter(0.1),
              color = "red", alpha = 0.2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x=element_text(angle = 45, hjust = 1)) +
  labs(title = "COVID-19 Todesfälle nach Bundesländern\n",
       x = "\nBundesländer",
       y = "COVID-19 Todesfälle\nje 100.000 EW\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at")


## ----methodchart11, out.width=600, echo=FALSE, fig.cap="Auswerteverfahren für unabhängige Stichproben (Quelle: Eigene Überarbeitung 2016 von [Hager, 2011](https://www.univie.ac.at/soziologie-statistik/lingu/master/Signifikanztests.pdf))", out.extra = "class = 'videoframe fullscreen-enabled'"----
knitr::include_graphics("images/EH5_1_Gruppenunterschiede-Slide-4_cut.png")


## -----------------------------------------------------------------------------------------
ggplot(covidCases, aes(sample = AnzahlTot_100k)) +
  stat_qq(color = "red") +
  stat_qq_line()


## -----------------------------------------------------------------------------------------
ggplot(covidCases, aes(sample = AnzahlTot_100k, color = bula)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ bula)


## -----------------------------------------------------------------------------------------
library(rstatix)

# Wien entfernen, da nur 1 Beobachtung in diesem Bundesland
covidCasesNoVie <- covidCases %>%
  filter(bula != "Wien")

covidCasesNoVie %>%
  # group_by(bula) %>%
  rstatix::shapiro_test(AnzahlTot_100k) %>%
  arrange(p)


## -----------------------------------------------------------------------------------------
covidCasesNoVie %>%
  rstatix::levene_test(AnzahlTot_100k ~ bula) %>%
  knitr::kable()


## ----message=FALSE, warning=FALSE---------------------------------------------------------
covidCasesNoVie %>%
  rstatix::anova_test(AnzahlTot_100k ~ bula) %>%
  knitr::kable()


## -----------------------------------------------------------------------------------------
covidCasesNoVie %>%
  rstatix::tukey_hsd(AnzahlTot_100k ~ bula) %>%
  select(group1, group2, estimate, p.adj, p.adj.signif) %>%
  arrange(p.adj)


## -----------------------------------------------------------------------------------------
covidCasesNoVie %>%
  welch_anova_test(AnzahlTot_100k ~ bula) %>%
  knitr::kable()


## -----------------------------------------------------------------------------------------
covidCasesNoVie %>% 
  games_howell_test(AnzahlTot_100k ~ bula) %>%
  select(group1, group2, estimate, p.adj, p.adj.signif) %>%
  arrange(p.adj)


## -----------------------------------------------------------------------------------------
Trendfragen <- read.csv2("data/Trendfragen_Corona_45-20/ZA7677_v1-0-0.csv", encoding = "UTF-8")
SelTrendfragen <- Trendfragen %>%
  select(s7, bcor1_1) %>%
  mutate(bcor1_1 = factor(bcor1_1, exclude = c("k.A.", "weiß nicht")),
         s7 = factor(s7, exclude = "-1")) %>%
  filter(!is.na(bcor1_1) & !is.na(s7))


## ----testraw, results='asis'--------------------------------------------------------------
table(SelTrendfragen$s7) %>%
  knitr::kable("simple", col.names = c("s7", "Freq"))
table(SelTrendfragen$bcor1_1) %>%
  knitr::kable("simple", col.names = c("bcor1_1", "Freq"))


## -----------------------------------------------------------------------------------------
SelTrendfragen <- SelTrendfragen %>%
  mutate(bcor1_1 = forcats::fct_relevel(bcor1_1,
                                        c("sehr große Sorgen",
                                          "große Sorgen",
                                          "weniger große Sorgen",
                                          "keine Sorgen")
                                        )
         )


## -----------------------------------------------------------------------------------------
# Kontingenztabelle ermitteln & als Dataframe ablegen
konttab <- table(SelTrendfragen$bcor1_1, SelTrendfragen$s7) %>%
  prop.table(., margin = 2) %>%   # 2 ... Spaltenprozent
  round(., 2) %>%
  as.data.frame()

vis.order <- konttab %>%
  filter(Var1 == "keine Sorgen") %>%
  arrange(Freq) %>%
  select(Var2)
vis.order.vector <- as.character(vis.order$Var2)

# Datensatz für Visualisierung vorbereiten
vis.data.1 <- SelTrendfragen
vis.data.1$s7 <- factor(SelTrendfragen$s7, levels = vis.order.vector)

ggplot(vis.data.1, aes(x = s7, fill = bcor1_1)) +
  geom_bar(position = "fill", color = "black", width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(color = "gray")) +
  labs(title = "Sorge um Ansteckung nach Bundestagswahlabsicht\n",
       caption = "\n(Daten: Presse- und Informationsamt der Deutschen Bundesregierung, 2021)",
       x = "Bundestagswahlabsicht\n",
       y = "",
       fill = "Sorge um Ansteckung") +
  guides(fill = guide_legend(reverse = TRUE))



## -----------------------------------------------------------------------------------------
library(rstatix)
SelTrendfragen %>%
  kruskal_test(bcor1_1 ~ s7) 
  # knitr::kable("simple")


## -----------------------------------------------------------------------------------------
SelTrendfragen %>%
  dunn_test(bcor1_1 ~ s7, p.adjust.method = "holm") %>%
  select(group1, group2, p, p.adj, p.adj.signif) %>%
  filter(p.adj <= 0.1) %>%    # Blick auf die knapp nicht signifikanten Unterschiede
  arrange(group1) %>%         # Hervorheben der Unähnlichsten
  knitr::kable(digits = 3, "pipe")

