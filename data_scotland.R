library(fs)
library(tidyverse)
library(lubridate)

source("utils.R")


# Import Scottish Data

WATTY62PATH <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/"

WATTY62CASES <- "new_daily_cases.csv"
WATTY62REGIONALCASES <- "regional_cases.csv"
WATTY62POP <- "HB_Populations.csv"
WATTY62DEATHS <- "regional_deaths.csv"
WATTY62TESTS <- "scot_tests.csv"

# Health board population data
scot_pop <- read_csv(file = paste0(WATTY62PATH, WATTY62POP))

# Scottish healthboard cases data
scot_data_raw <- read_csv(file = paste0(WATTY62PATH, WATTY62REGIONALCASES)) 

scot_data <- scot_data_raw %>%
  mutate(date = dmy(Date)) %>%
  select(-Date) %>%
  pivot_longer(`Ayrshire and Arran`:`Grand Total`,
               names_to = "health_board",
               values_to = "confirmed_cases") %>% 
group_by(health_board) %>% 
  mutate(new_cases = confirmed_cases - replace_na(lag(confirmed_cases), 0)) %>%
  mutate(doubling_time = calc_doubling(confirmed_cases, n = 7)) %>%
  replace_na(list(new_cases = 0)) %>%
  mutate(country_region = "Scotland")



scot_data_health_board_total <- scot_data_health_board %>% 
  group_by(health_board) %>%
  summarise(CasesSum = max(confirmed_cases, na.rm = T))

# Scottish overall cases
scot_cases_raw <- read_csv(file = paste0(WATTY62PATH, WATTY62CASES))
scot_cases <- scot_cases_raw %>%
  rename("new_cases" = "New cases") %>%
  mutate(date = lubridate::dmy(Date)) %>%
  select(-Date) %>%
  mutate(confirmed_cases = cumsum(new_cases)) %>%
  mutate(doubling_time_week = 7*log(2)/log(confirmed_cases/replace_na(lag(confirmed_cases,7),0))) %>%
  mutate(country_region = "Scotland")

scot_data <- scot_cases

# Scottish death data
scot_deaths <- read_csv(file = WATTY62DEATHS) 
scot_deaths[scot_deaths == "x"] <- NA             

scot_deaths <- scot_deaths %>%
  mutate(Date = case_when(Date == "10-Apr-2020" & lag(.$Date) == "31-Mar-2020" ~ "1-Apr-2020", 
                          TRUE ~ Date)) %>%
  rename(deaths= `Grand Total`) %>%
  mutate(new_deaths = deaths - replace_na(lag(deaths),0)) %>%
  mutate(doubling_time_week = 7*log(2)/log(deaths/replace_na(lag(deaths,7),0))) %>%
  mutate(date = lubridate::dmy(Date)) %>%
  select(-Date) %>%
  replace_na(list(Deaths = 0)) %>%
  mutate(country_region = "Scotland")


scot_data_all <- select(scot_data, country_region, date, new_cases, confirmed_cases) %>%
  full_join(select(scot_deaths, country_region, date, new_deaths, deaths), by = c("country_region", "date"))
