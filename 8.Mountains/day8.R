#load libraries
library(readxl)
library(ggplot2)
library(tidyverse)
library(showtext)
library(dplyr)
library(MetBrewer)

#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Jost", "Jost")
sysfonts::font_add_google("Secular One", "Secular One")

#load data
dolomites_data <- read_excel("8.Mountains/dolomites_data.xlsx")

#format data

for (i in colnames(dolomites_data)) {
  dolomites_data[[i]] = as.numeric(gsub("cm", "", dolomites_data[[i]]))
  
}


p = ggplot(dolomites_data, aes(x = year)) +
  geom_area(aes(y = total_snowfall),
            fill = "#dfdcdb") +
  geom_area(
    aes(y = avg_summit_depth, label = "Average Summit Snow Depth"),
    fill = "#97989b",
    alpha = 0.7
  ) +
  geom_area(aes(y = avg_base_depth),
            fill = "#565654", alpha = 0.7) +
  geom_segment(
    aes(
      x = year,
      xend = year,
      y = 0,
      yend = total_snowfall + 80
    ),
    size = 1.2,
    linetype = 2,
    color = "#3f2828"
  ) +
  geom_point(
    aes(y = total_snowfall + 80),
    shape = 21,
    fill = "white",
    stroke = 2,
    size = 10
  ) +
  geom_text(aes(y = total_snowfall + 80, label = days), size = 4) +
  geom_label(x = 2018,
             y = 280,
             label = "Total Snowfall",
             fill = "#dfdcdb") +
  geom_label(x = 2019,
             y = 250,
             label = "Average Summit Snow Depth",
             fill = "#97989b") +
  geom_label(x = 2020,
             y = 220,
             label = "Average Base Snow Depth",
             fill = "#565654") +
  theme_minimal() +
  scale_x_continuous(breaks = dolomites_data$year) +
  ylab("Snowfall (cm)") +
  xlab("") +
  labs(title = "How much snow did the Three Peaks Dolomites get this season?",
       subtitle = "\nThe Three Peaks in the nature Park by the same name form \none of the most distinctive mountain chains in the Dolomites\n\n    - Area plot: Yearly snowfall stats\n\n    - Points: Snow days for each year\n",
       caption = "Data: Onthesnow.co.uk | Plot: @AntoniaPopes | #30DayChartChallenge") +
  theme(
    text = element_text(family = "Jost"),
    plot.title = element_text(
      margin = margin(15, 0, 0, 0),
      size = 30,
      family = "Secular One"
    ),
    plot.subtitle = element_text(margin = margin(10, 0, 10, 0), size = 14),
    plot.caption = element_text(hjust = 1,
                                margin = margin(10, 0, 10, 0)),
    plot.background = element_rect(fill = "#accee7ff", color = NA),
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    axis.text = element_text(size = 14),
    panel.grid = element_line(linetype = "dotted", size = 1)
  )

p

#save plot
ggsave(
  p,
  filename = "dolomites.png",
  width = 1650,
  height = 1080,
  units = "px"
)
