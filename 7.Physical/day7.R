#load libraries
library(readxl)
library(ggplot2)
library(tidyverse)
library(showtext)
library(dplyr)
library(MetBrewer)

#load data
day1data <- read_excel("1.Part-to-whole/day1data.xlsx")

install.packages("rlang")
library(installr)
uninstall.packages("tidyverse", "dplyr", "rlang")

install.packages("tidyverse", "dplyr", "rlang")
