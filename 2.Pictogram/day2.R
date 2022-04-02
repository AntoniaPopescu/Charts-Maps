#load libraries
library(readxl)
library(ggplot2)
library(tidyverse)
library(showtext)
library(MetBrewer)
library(sysfonts)
library(emojifont)


#load data
day2data <- read_excel("2.Pictogram/day2data.xlsx")

#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Ubuntu", "Ubuntu")


people = day2data %>%
  rowwise() %>%
  mutate(values = list(1:25)) %>%
  unnest(c(values)) %>%
  group_by(statement) %>%
  mutate(iconColor = if_else(row_number() <= in25, "#ff0000", "grey80"))


ggplot(people) +
  geom_text(
    aes(
      x = values,
      y = ID,
      color = iconColor,
      label = emoji("bust_in_silhouette")
    ),
    family = "EmojiOne",
    size = 14
  ) +
  geom_text(
    aes(0.5, ID, label = statement),
    family =  "Ubuntu",
    size = 5,
    stat = "unique",
    hjust = 0,
    nudge_y = -0.4,
    color = "#c00000"
  ) +
  scale_color_identity() +
  labs(title = "Prejudices and stereotypes about sexual violence in Italy",
       subtitle = "Percent of partecipants agreeing with the following statemnts in a 2018 ISTAT Survey:",
       caption = "Data: Italian National Istitute of Statistics (ISTAT) | Plot: @AntoniaPopes | #30DayChartChallenge") +
  theme_void() +
  theme(
    text = element_text(
      family = "Ubuntu",
      face = 'bold',
      colour = "#004D40"
    ),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(
      hjust = 0.2,
      size = 30,
      margin = margin(15, 0, 0, 0),
      color = "#151515"
    ),
    plot.subtitle = element_text(
      hjust = 0.1,
      size = 16,
      margin = margin(20, 0, 20, 0),
      color = "#c00000"
    ),
    plot.caption = element_text(
      hjust = 1,
      margin = margin(10, 0, 10, 0),
      color = "grey50"
    ),
    plot.margin = margin(1, 1, 1, 1, unit = "cm")
  )


#save
ggsave(p, filename = "day2.png")
