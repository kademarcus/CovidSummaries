rm(list=ls())

#installations
#install.packages("tidyverse")
install.packages("shiny")
install.packages("utils")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("gt")
install.packages("webshot2")

#libraries
library(tidyverse)
library(shiny)
library(utils)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(gt)
library(webshot2)

#bring in data
covid <- read.csv("https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv")

View(covid)

#-------------------------Cleaning--------------------------------------------------
covid <- covid %>%
  filter(!is.na(continent))%>% #going to summarize by continent 
  mutate(
    continent = case_when(
      iso_code == "OWID_WRL" ~ "World",
      iso_code == "OWID_EUR" ~ "Europe",
      iso_code == "OWID_AFR" ~ "Africa",
      iso_code == "OWID_ASI" ~ "Asia",
      iso_code == "OWID_NAM" ~ "North America",
      iso_code == "OWID_SAM" ~ "South America",
      iso_code == "OWID_OCE" ~ "Oceania",
      TRUE ~ continent  # leave others unchanged
    )
  )%>% #some continent values were blank, but have iso codes
  filter(iso_code != "OWID_EUN",
         iso_code != "OWID_HIC",
         iso_code != "OWID_LIC",
         iso_code != "OWID_LMC", 
         iso_code != "OWID_HMC", 
         iso_code != "OWID_UMC")%>% #only want regional continents
  dplyr::mutate(date = as.Date(date)) %>% #make sure date is correct format
  dplyr::mutate(vaccine_period = if_else(date < as.Date("2021-01-01"), 
                                         "Pre-Vaccine", "Post-Vaccine")) #before and after vaccine introduced


#---------------------Summaries and Tables---------------------------------------------
#view raw summary
summary_table <- covid %>%
  group_by(continent, vaccine_period) %>% #pre and post, and continent variables
  summarise(
    total_cases = sum(new_cases, na.rm = TRUE),
    total_deaths = sum(new_deaths, na.rm = TRUE),
    avg_new_cases = mean(new_cases, na.rm = TRUE), #average new cases over pre and post time
    avg_new_deaths = mean(new_deaths, na.rm = TRUE) #average new deaths over pre and post time
  ) %>%
  dplyr::mutate(vaccine_period = factor(vaccine_period, levels = c("Pre-Vaccine", "Post-Vaccine")))%>%
  ungroup() 


#cleaned up table
names(summary_table) <- gsub("_Pre-Vaccine", " (Pre)", names(summary_table))
names(summary_table) <- gsub("_Post-Vaccine", " (Post)", names(summary_table))

# Create gt table
formatted_covid_table <- gt(summary_table%>%
                              mutate(vaccine_period = factor(vaccine_period, levels = c("Pre-Vaccine", "Post-Vaccine"))) %>%
                              arrange(continent, vaccine_period))
# Bold column labels
formatted_covid_table <- tab_style(
  formatted_covid_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels()
)

# Row coloring by vaccine period
formatted_covid_table <- tab_style(
  formatted_covid_table,
  style = list(cell_fill(color = "lightblue")),
  locations = cells_body(
    rows = vaccine_period == "Pre-Vaccine"
  )
)

formatted_covid_table <- tab_style(
  formatted_covid_table,
  style = list(cell_fill(color = "white")),
  locations = cells_body(
    rows = vaccine_period == "Post-Vaccine"
  )
)

# Header and source
formatted_covid_table <- tab_header(
  formatted_covid_table,
  title = md("**COVID-19 Summary by Continent**"),
  subtitle = md("*Comparing Pre- and Post-Vaccine Periods*")
)

formatted_covid_table <- tab_source_note(
  formatted_covid_table,
  source_note = "Data from Our World in Data. Figures are total or average per day by period."
)

# Display
formatted_covid_table

gtsave(formatted_covid_table, "covid_table.png")
rm(summary_table, formatted_covid_table) 


#---------------------Graphs and Maps---------------------------------------------
#plot new cases over time with ggplot2
#get data for continents
covid_cumulative <- covid %>%
  filter(!is.na(continent)) %>%
  filter(continent != "") %>%
  group_by(continent, date) %>%
  summarise(daily_cases = sum(new_cases, na.rm = TRUE), .groups = "drop") %>%
  arrange(continent, date) %>%
  group_by(continent) %>%
  mutate(cumulative_cases = cumsum(daily_cases)) %>%
  ungroup() %>%
  mutate(date = as.Date(date))  # ensure date is Date class

ggplot(covid_cumulative, aes(x = date, y = cumulative_cases, color = continent)) +
  geom_line(linewidth = 1) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Cumulative COVID-19 Cases by Continent",
    x = "Year",
    y = "Cumulative Reported Cases",
    color = "Continent"
  ) +
  theme_minimal() 

rm(covid_cumulative)
#---------------------Correlations and Tests---------------------------------------------
