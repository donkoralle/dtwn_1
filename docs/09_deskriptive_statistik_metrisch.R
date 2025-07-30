## ----message=FALSE--------------------------------------------------------------------------------
library(tidyverse)
library(scales)


## -------------------------------------------------------------------------------------------------
agesRohdaten <- read.csv2("data/agesRohdaten_25-05-21.csv", encoding = "UTF-8") %>%
  as_tibble()
str(agesRohdaten)


## -------------------------------------------------------------------------------------------------
colSums(is.na(agesRohdaten)) %>%
  knitr::kable()


## -------------------------------------------------------------------------------------------------
covidCases <- agesRohdaten %>%
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
summary(covidCases$bula)


## -------------------------------------------------------------------------------------------------
covidCases <- covidCases %>%
  mutate(Anzahl_100k = Anzahl/(AnzEinwohner/100000),
         AnzahlTot_100k = AnzahlTot/(AnzEinwohner/100000),
         Anzahl7Tage_100k = AnzahlFaelle7Tage/(AnzEinwohner/100000))


## -------------------------------------------------------------------------------------------------
summary(covidCases)


## -------------------------------------------------------------------------------------------------
covidCases %>%
  summarise(avg_Faelle7tage = mean(Anzahl7Tage_100k))


## -------------------------------------------------------------------------------------------------
covidCases %>%
  summarise(across(Anzahl_100k:Anzahl7Tage_100k, mean)) %>%
  knitr::kable(digits = 1)


## -------------------------------------------------------------------------------------------------
covidCases %>%
  group_by(bula) %>%
  summarise(across(Anzahl_100k:Anzahl7Tage_100k, mean)) %>%
  knitr::kable(digits = 1)


## -------------------------------------------------------------------------------------------------
covidCases %>%
  group_by(bula) %>%
  summarise(across(Anzahl_100k:Anzahl7Tage_100k, median)) %>%
  knitr::kable(digits = 1)


## -------------------------------------------------------------------------------------------------
covidCases %>%
  group_by(bula) %>%
  summarise(avg_Anzahl_100k = mean(Anzahl_100k),
            avg_Anzahl = mean(Anzahl),
            avg_AnzahlTot_100k = mean(AnzahlTot_100k),
            avg_AnzahlTot = mean(AnzahlTot)) %>%
  knitr::kable(digits = 1)


## ----fig.width=9, fig.height=4.75-----------------------------------------------------------------
library(sf)
library(tmap)
bez <- read_sf("data/bez/bez_aut.shp")
tm_shape(bez) +
  tm_polygons()


## -------------------------------------------------------------------------------------------------
joined_bez <- left_join(bez, covidCases, by = c("id" = "GKZ"))


## ----fig.width=9, fig.height=4.75-----------------------------------------------------------------
tmap::qtm(joined_bez, fill = "Anzahl_100k")


## ----fig.width=9, fig.height=3.75-----------------------------------------------------------------
mymap <- tm_shape(joined_bez) +
  tm_polygons("Anzahl_100k",
              title = "Erkrankte\nje 100.00 EW",
              palette = "YlOrRd",
              legend.hist = TRUE) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_legend(outside = TRUE,
            legend.outside.size = 0.15,
            hist.width = 1,
            outer.margins = 0)
mymap


## ----message=FALSE, warning=FALSE-----------------------------------------------------------------
tmap_save(mymap, filename = "output/covid_faelle_100k_2021.png",
          units = "px", dpi = 300,
          width = 2000)


## -------------------------------------------------------------------------------------------------
ggplot(covidCases, aes(x = Anzahl7Tage_100k)) +
  geom_histogram(bins = 50,
                 col = "black", fill = "red")


## -------------------------------------------------------------------------------------------------
covidCases %>%
  select(Bezirk, Anzahl7Tage_100k) %>%
  filter(Anzahl7Tage_100k >= 90) %>%
  arrange(-Anzahl7Tage_100k)


## ----warning=FALSE--------------------------------------------------------------------------------
ggplot(covidCases, aes(x = Anzahl7Tage_100k)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  # geom_density(size = 1, color = "red") +
  facet_wrap(~bula)


## ----warning=FALSE--------------------------------------------------------------------------------
ggplot(covidCases, aes(x = Anzahl7Tage_100k, y = ..density..)) +
  geom_histogram(bins = 50,
                 col = "black", fill = "red") +
  geom_rug(aes(x = Anzahl7Tage_100k, y = NULL)) +
  geom_density(size = 1.5)


## ----echo = FALSE, fig.cap="Struktur von Boxplots in R (Quelle: [Coleman, 2020](https://www.leansigmacorporation.com/box-plot-with-minitab/))"----
knitr::include_graphics("images/boxplot_explanation.png")


## -------------------------------------------------------------------------------------------------
ggplot(covidCases, aes(x = bula, y = AnzahlTot_100k)) +
  geom_boxplot()


## ----message=FALSE, warning=FALSE-----------------------------------------------------------------
ggplot(covidCases, aes(x = forcats::fct_reorder(bula, AnzahlTot_100k, median), y = AnzahlTot_100k)) +
  geom_boxplot(fill = "azure3") +
  geom_jitter(shape=16, position=position_jitter(0.1),
              color = "red", alpha = 0.2) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x=element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  labs(title = "Verteilung der COVID-19 Todesfälle\n",
       x = "\nBundesländer",
       y = "COVID-19 Todesfälle\nje 100.000 EW\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at")


## ----message=FALSE, warning=FALSE-----------------------------------------------------------------
covidCasesNoVIE <- covidCases %>%
  filter(GKZ != 900)

ggplot(covidCasesNoVIE, aes(x = AnzEinwohner, y = Anzahl, fill = bula)) +
  geom_point(aes(size = AnzahlTot), shape=21) +
  scale_x_continuous(labels = scales::label_comma(big.mark = "."), breaks = scales::breaks_extended(6)) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".")) +
  theme_bw() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x=element_text(angle = 45, hjust = 1)) +
  labs(title = "Polit. Bezirke nach Einwohnern,\nCOVID-19 Fällen und Verstorbenen\n",
       x = "\nEinwohner",
       y = "COVID-19 Fälle\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at",
       size = "Anzahl Verstorbene",
       fill = "Bundesland") +
  guides(fill = guide_legend(override.aes = list(size = 5)))


## ----warning=FALSE--------------------------------------------------------------------------------
ggplot(covidCasesNoVIE, aes(x = AnzEinwohner, y = Anzahl, fill = bula)) +
  geom_point(aes(size = AnzahlTot), shape=21) +
  geom_text(aes(label = Bezirk)) +
  scale_x_continuous(labels = scales::label_comma(big.mark = "."), breaks = scales::breaks_extended(6)) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".")) +
  theme_bw() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x=element_text(angle = 45, hjust = 1)) +
  labs(title = "Polit. Bezirke nach Einwohnern,\nCOVID-19 Fällen und Verstorbenen\n",
       x = "\nEinwohner",
       y = "COVID-19 Fälle\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at",
       size = "Anzahl Verstorbene",
       fill = "Bundesland") +
  guides(fill = guide_legend(override.aes = list(size = 5)))


## ----warning=FALSE--------------------------------------------------------------------------------
library(ggrepel)
ggplot(covidCasesNoVIE, aes(x = AnzEinwohner, y = Anzahl, fill = bula)) +
  geom_point(aes(size = AnzahlTot), shape=21) +
  geom_text_repel(aes(label = Bezirk),
                  size = 3,
                  color = "gray",
                  # point.padding = 5,
                  box.padding = 0.4,
                  min.segment.length = 1) +
  scale_x_continuous(labels = scales::label_comma(big.mark = "."), breaks = scales::breaks_extended(7)) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".")) +
  theme_bw() +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.text.x=element_text(angle = 45, hjust = 1)) +
  labs(title = "Polit. Bezirke nach Einwohnern,\nCOVID-19 Fällen und Verstorbenen\n",
       x = "\nEinwohner",
       y = "COVID-19 Fälle\n",
       caption = "\nDaten: AGES, 2021 - covid19-dashboard@ages.at",
       size = "Anzahl Verstorbene",
       fill = "Bundesland") +
  guides(fill = guide_legend(override.aes = list(size = 5)))

