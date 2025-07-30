## -----------------------------------------------------------------------------------------------------------------
library(tidyverse)


## -----------------------------------------------------------------------------------------------------------------
somedata <- data.frame(zahlen = c(1,2,3,4,5,6,7,8,9,10))

# statt:
mean(somedata$zahlen)

# geht jetzt:
somedata %>%
  summarise(mean(zahlen))
# und das:
somedata %>%
  summarise(Durchschnitt = mean(zahlen),
            Median = median(zahlen))
# und das:
somedata %>%
  summary(.)


## -----------------------------------------------------------------------------------------------------------------
Trendfragen <- as_tibble(read.csv2("data/Trendfragen_Corona_45-20/ZA7677_v1-0-0.csv",
                                   encoding = "UTF-8"))


## -----------------------------------------------------------------------------------------------------------------
Trendfragen <- Trendfragen %>%
  mutate(id = row_number())


## -----------------------------------------------------------------------------------------------------------------
df <- Trendfragen %>%
  select(id, bcor5, s8) %>%
  mutate(bcor5 = as.factor(bcor5), s8 = as.factor(s8))    # oder:
  # mutate(across(c(bcor5, s8), factor))                  # oder:
  # mutate(across(where(is.character), factor))

glimpse(df)


## -----------------------------------------------------------------------------------------------------------------
table(df$bcor5)
# oder:
df %>%
  count(s8)


## -----------------------------------------------------------------------------------------------------------------
colSums(is.na(df))

# alternativ:
df %>%
  summarise(across(everything(), list(n_miss = ~ sum(is.na(.x)))))


## -----------------------------------------------------------------------------------------------------------------
df2 <- df %>%
  filter(s8 != "-1") %>%
  mutate(s8 = droplevels(s8))

df2 %>%
  count(s8) %>%
  arrange(n)
  # arrange(desc(n))    # oder:
  # arrange(-n)


## -----------------------------------------------------------------------------------------------------------------
table(df2$bcor5)
prop.table(table(df2$bcor5))


## -----------------------------------------------------------------------------------------------------------------
df2 %>%
  group_by(bcor5) %>%
  summarise(n = n()) %>%
  mutate(relFreq = n / sum(n)) %>%
  mutate(relFreq = round(relFreq, 2)) %>%
  arrange(-relFreq)


## -----------------------------------------------------------------------------------------------------------------
# cheap trick:
ggplot(df2, aes(x = s8, y = ..count.., group = 1)) +
  geom_bar()


## -----------------------------------------------------------------------------------------------------------------
# more shiny:
ggplot(df2, aes(x = forcats::fct_infreq(s8), y = ..count.., group = 1)) +
  geom_bar(fill = "blue", color = "black") +
  coord_flip() +
  labs(title = "Catchy Titel",
     subtitle = "Was noch gesagt werden sollte",
     x = "Polit. Orientierung\nletzte Bundestagswahl\n",
     y = "\nabs. Anzahl",
     caption = "\n(Daten: Presse- und Informationsamt der Deutschen Bundesregierung, 2021)") +
  theme_bw() +
  theme(text = element_text(size = 11),
        plot.caption = element_text(hjust = 0.5))


## ----message=FALSE------------------------------------------------------------------------------------------------
# anders shiny:
library(scales)   # zum Formatieren der Achsen etc.

ggplot(df2, aes(x = forcats::fct_infreq(s8), y = ..prop.., group = 1)) +
  geom_bar(fill = "red") +
  labs(title = "Wieder ein catchy Titel",
     subtitle = "Was noch immer gesagt werden sollte",
     x = "\n Polit. Orientierung bei letzter Bundestagswahl",
     y = "Anteile [%]\n",
     caption = "\n(Daten: Presse- und Informationsamt der Deutschen Bundesregierung, 2021)") +
  scale_y_continuous(labels = percent) +
  theme_bw() +
  theme(text = element_text(size = 11),
        axis.text.x=element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0.5))


## -----------------------------------------------------------------------------------------------------------------
# ein letztes mal anders shiny:
library(RColorBrewer)   # Sammlung von diskreten& kontinuierlichen Farbpaletten
display.brewer.all()

ggplot(df2, aes(x = "", y = ..count.., fill = forcats::fct_rev(forcats::fct_infreq(s8)))) +
  geom_bar(position = "fill", color = "black") +
  coord_polar("y", start=0, direction = 1) +
  scale_y_continuous(labels = percent) +
  # scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Catchy Titel",
     subtitle = "Was noch gesagt werden sollte",
     caption = "\n(Daten: Presse- und Informationsamt der Deutschen Bundesregierung, 2021)",
     fill = "polit. Ausrichtung")


## -----------------------------------------------------------------------------------------------------------------
table(df2$bcor5, df2$s8)

table(df2$bcor5, df2$s8) %>%
  prop.table(., margin = 2) %>%   # 2 ... Spaltenprozent
  round(., 2)


## -----------------------------------------------------------------------------------------------------------------
df2 %>%
  group_by(bcor5, s8) %>%
  summarise(n = n(), .groups = "drop") %>%
  spread(c(s8), c(n))

df2 %>%
  group_by(bcor5, s8) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(relFreq = n / sum(n)) %>%
  mutate(relFreq = round(relFreq, 2)) %>%
  subset(select = c(bcor5, s8, relFreq)) %>%
  spread(c(s8), c(relFreq))


## -----------------------------------------------------------------------------------------------------------------
library(janitor)
janitor::tabyl(df2, bcor5, s8) %>%
  adorn_totals(c('row')) %>%
  adorn_percentages('col') %>% 
  adorn_pct_formatting(digits = 0) %>%
  # adorn_ns() %>%
  adorn_title('combined') %>%
  knitr::kable()


## -----------------------------------------------------------------------------------------------------------------
ggplot(df2, aes(x = s8, fill = bcor5)) +
  geom_bar()


## -----------------------------------------------------------------------------------------------------------------
ggplot(df2, aes(x = s8, fill = bcor5)) +
  geom_bar(position = "fill", color = "black", width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      panel.grid.major.x = element_line(color = "gray")) +
  labs(title = "Catchy Titel",
   subtitle = "Was noch gesagt werden sollte\n",
   caption = "\n(Daten: Presse- und Informationsamt der Deutschen Bundesregierung, 2021)",
   x = "polit. Aursrichtung\n",
   y = "",
   fill = "Beurteilung\nMaßnahmen") +
  guides(fill = guide_legend(reverse = TRUE))


## -----------------------------------------------------------------------------------------------------------------
# Kontingenztabelle ermitteln & als Dataframe ablegen
konttab <- table(df2$bcor5, df2$s8) %>%
  prop.table(., margin = 2) %>%   # 2 ... Spaltenprozent
  round(., 2) %>%
  as.data.frame()

# gewünschte Reihenfolge herstellen & ablegen: 
# Beurteilung angemessen absteigend nach rel. Häufigkeit
vis.order <- konttab %>%
  filter(Var1 == "angemessen") %>%
  arrange(Freq) %>%
  select(Var2)
vis.order.vector <- as.character(vis.order$Var2)

# Datensatz für Visualisierung vorbereiten
vis.data.1 <- df2
vis.data.1$s8 <- factor(df2$s8, levels = vis.order.vector)

# Plotten
ggplot(vis.data.1, aes(x = s8, fill = forcats::fct_rev(bcor5))) +
  geom_bar(position = "fill", color = "black", width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      panel.grid.major.x = element_line(color = "gray")) +
  labs(title = "Catchy Titel",
   subtitle = "Was noch gesagt werden sollte\n",
   caption = "\n(Daten: Presse- und Informationsamt der Deutschen Bundesregierung, 2021)",
   x = "polit. Aursrichtung\n",
   y = "",
   fill = "Beurteilung\nMaßnahmen") +
  guides(fill = guide_legend(reverse = TRUE))

