#load libraries
library(readxl)
library(ggplot2)
library(tidyverse)
library(showtext)
library(dplyr)
library(MetBrewer)
library(ggstream)

#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Jost", "Jost")
sysfonts::font_add_google("Secular One", "Secular One")


#load data
PA_data <- read_excel("PA_data.xlsx")

#format data
activity = PA_data %>%
  filter(Gender == "All" &
           Ethnicity == "All" & age_bracket != "16-74") %>%
  select(age_bracket, activity_level, percent) %>%
  group_by(activity_level, age_bracket) %>%
  summarise(avg = mean(as.numeric(percent)))

activity$avg = round(activity$avg, digits = 0)

activity$age = factor(
  activity$age_bracket,
  levels = c(
    '16+',
    '16-24',
    '25-34',
    '35-44',
    '45-54',
    '55-64',
    '65-74',
    '75+'
  )
)

#plot
colors = met.brewer("Greek", 3)

p = ggplot(data = activity,
           mapping = aes(x = activity_level, y = avg, fill = activity_level)) +
  geom_bar(stat = 'identity') +
  geom_point(
    data = activity,
    aes(x = activity_level, y = avg, fill = activity_level),
    shape = 21,
    stroke = 0,
    size = 12,
    color = "white"
  ) +
  geom_text(aes(label = signif(avg)), color = "white") +
  geom_text(
    hjust = 1.1,
    size = 3,
    aes(x = activity_level, y = 0, label = activity_level)
  ) +
  coord_polar(theta = "y") +
  facet_wrap( ~ age, nrow = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = colors) +
  theme_void() +
  labs(title = "Levels of physical activity in England",
       subtitle = "\nPercentages have been rounded to the nearest whole number\n\nData is recorded for 3 groups:\n\n-physically active: people who do 150 minutes or more a week\n\n-fairly active: people who do between 30 and 149 minutes a week\n\n-physically inactive: people who do less than 30 minutes a week\n",
       caption = "Data: Gov.UK | Plot: @AntoniaPopes | #30DayChartChallenge") +
  theme(
    text = element_text(family = "Jost"),
    plot.background = element_rect(fill = "#efedf5", color = NA),
    plot.title = element_text(
      margin = margin(15, 0, 0, 0),
      size = 30,
      family = "Secular One"
    ),
    plot.subtitle = element_text(margin = margin(10, 0, 10, 0), size = 14),
    legend.position = "none",
    plot.caption = element_text(hjust = 0,
                                margin = margin(10, 0, 10, 0)),
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
  )
p

#save plot
ggsave(
  p,
  filename = "day7.png",
  height = 1350,
  width = 1080,
  units = "px"
)
