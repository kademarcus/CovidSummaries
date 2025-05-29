rm(list=ls())

#installations
#install.packages("tidyverse")
install.packages("shiny")
install.packages("utils")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("tidyr")

#libraries
library(tidyverse)
library(shiny)
library(utils)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

#bring in data
covid <- read.csv("https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv")

View(covid)

#cleaning 
covid <- covid %>%
  filter(!is.na(continent))%>% #going to summarize by continent 
  dplyr::mutate(date = as.Date(date)) %>% #make sure date is correct format
  dplyr::mutate(vaccine_period = if_else(date < as.Date("2021-01-01"), 
                                         "Pre-Vaccine", "Post-Vaccine")) #before and after vaccine introduced

#---------------------Summaries and tables---------------------------------------------
summary_vaccine_period <- covid %>%
  group_by(continent, vaccine_period) %>%
  summarise(
    total_cases = sum(new_cases, na.rm = TRUE),
    total_deaths = sum(new_deaths, na.rm = TRUE),
    avg_new_cases = mean(new_cases, na.rm = TRUE),
    avg_new_deaths = mean(new_deaths, na.rm = TRUE)
    ) %>%
  ungroup()

View(summary_vaccine_period)

#cleaned up table
summary_table <- summary_vaccine_period %>%
  pivot_wider(
    names_from = vaccine_period,
    values_from = c(total_cases, total_deaths, avg_new_cases, avg_new_deaths)
  )

print(summary_table)
#---------------------Graphs and Maps---------------------------------------------

#Will use power BI for some of this, export data here

#---------------------Correlations/Regressions---------------------------------------------
