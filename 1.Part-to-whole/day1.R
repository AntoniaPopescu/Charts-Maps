#load libraries
library(readxl)
library(ggplot2)
library(tidyverse)
library(showtext)
library(dplyr)
library(ggalluvial)
library(MetBrewer)

#load data
day1data <- read_excel("1.Part-to-whole/day1data.xlsx")

#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Ubuntu", "Ubuntu")
sysfonts::font_add_google("Varela Round", "Varela Round")

#format data
day1data = day1data %>%
  mutate(total = sum(Population)) %>%
  mutate(freq = Population / total) %>%
  mutate(percent = freq * 100) %>%
  mutate(label = paste(day1data$Country, paste(round(day1data$percent, 2), "%"), sep =
                         "-"))

day1data$label = factor(
  day1data$label,
  levels = c(
    "India-11.94 %",
    "Nigeria-12.69 %",
    "Italy-13.43 %",
    "Republic of Ireland-15.67 %",
    "Poland-46.27 %"
  )
)

colors = met.brewer("Hokusai3", 5)

day1data %>%
  mutate(
    Location = fct_rev(as.factor(Location)),
    Category = fct_rev(as.factor(Category)),
    Country = fct_rev(as.factor(label))
  ) %>%  
  ggplot(aes(
    y = freq,
    axis3 = Location,
    axis2 = Category,
    axis1 = Country
  )) +
  geom_alluvium(
    aes(fill = Country),
    aes.bind = TRUE,
    curve_type = "arctangent",
    show.legend = FALSE,
    width = 0,
    knot.pos = 0,
    reverse = FALSE
  ) +
  geom_stratum(
    width = 1 / 6,
    fill = "#004D40",
    color = "#004D40",
    reverse = FALSE
  ) +
  geom_label(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    color = "#004D40",
    reverse = FALSE
  ) +
  coord_flip() +
  scale_x_discrete(limits = c("Location", "Category", "Country"),
                   expand = c(.06, .05)) +
  scale_fill_manual(values = colors) +
  labs(title = "Who are the non-British \nnationals living in Scotland?",
       subtitle = "Most common non-British nationalities, Scotland, July 2020 to June 2021",
       caption = "Data: Office of National Statistics (ONS) | Plot: @AntoniaPopes | #30DayChartChallenge") +
  theme_void() +
  theme(
    text = element_text(
      family = "Ubuntu",
      face = 'bold',
      colour = "#004D40"
    ),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(
      margin = margin(20, 0, 10, 0),
      size = 36,
      family = "Varela Round"
    ),
    plot.subtitle = element_text(margin = margin(10, 0, 10, 0), size = 14),
    plot.margin = margin(1,1,1,1, unit = "cm")
  )

#save
ggsave(p,filename="day1.png")
