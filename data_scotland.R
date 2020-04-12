library(fs)
library(tidyverse)
library(lubridate)




# doubling times ================================================================== 

calc_doubling <- function(x, n = 7){
  # n is number of points to count over
  dbl <- n * log(2) / log(x / replace_na(lag(x, n), 0))
  # zero doubling shouldn't exist
  ifelse(dbl == 0, NA, dbl)
}


# Import Scottish Data ============================================================

cov_scotdata <- function(){
  
  WATTY62PATH <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/"
  WATTY62POP <- "HB_Populations.csv"
  WATTY62CASES <- "new_daily_cases.csv"
  WATTY62REGIONALCASES <- "regional_cases.csv"
  WATTY62DEATHS <- "regional_deaths.csv"
  WATTY62TESTS <- "scot_tests.csv"
  
  source("utils.R")
  
  # Import Scottish Case Data ----------------------------------------------------
  
  scot_cases_raw <- read_csv(file = paste0(WATTY62PATH, WATTY62REGIONALCASES))
  
  scot_cases <- scot_cases_raw %>%
    mutate(date = dmy(Date)) %>%
    select(-Date) %>%
    pivot_longer(`Ayrshire and Arran`:`Grand Total`,
      names_to = "health_board",
      values_to = "confirmed_cases"
    ) %>%
    group_by(health_board) %>%
    arrange(date) %>% 
    mutate(new_cases = confirmed_cases - replace_na(lag(confirmed_cases), 0)) %>%
    mutate(doubling_time_cases = calc_doubling(confirmed_cases, n = 7)) %>%
    replace_na(list(new_cases = 0)) %>%
    mutate(country_region = "Scotland") %>%
    ungroup()
  
  scot_cases_health_board_total <- scot_cases %>%
    group_by(health_board) %>%
    summarise(CasesSum = max(confirmed_cases, na.rm = T)) %>%
    ungroup()
  
  # Scottish death data ----------------------------------------------------------
  scot_deaths_raw  <- read_csv(file = paste0(WATTY62PATH, WATTY62DEATHS),
                          na = "x")
  
  scot_deaths_raw <- scot_deaths_raw %>%
    mutate(Date = case_when(
      Date == "10-Apr-2020" & lag(.$Date) == "31-Mar-2020" ~ "1-Apr-2020",
      TRUE ~ Date
    ))
  
  scot_deaths <- scot_deaths_raw %>% 
    mutate(date = dmy(Date)) %>%
    select(-Date) %>%
    pivot_longer(`Ayrshire and Arran`:`Grand Total`,
                 names_to = "health_board",
                 values_to = "deaths"
    ) %>%
    group_by(health_board) %>%
    arrange(date) %>% 
    mutate(new_deaths = deaths - replace_na(lag(deaths), 0)) %>%
    mutate(doubling_time_deaths = calc_doubling(deaths, n = 7)) %>%
    replace_na(list(deaths = 0)) %>%
    mutate(country_region = "Scotland")
  
  
  # Join report datasets ---------------------------------------------------------
  
  scot_data <- scot_cases %>%
    full_join(
      scot_deaths,
      by = c("country_region", "health_board", "date")
    ) %>% 
    select(date, country_region, health_board, everything())
    
  # Add Health board population data ---------------------------------------------
  
  scot_pop <- read_csv(file = paste0(WATTY62PATH, WATTY62POP)) %>% 
    janitor::adorn_totals(name = "Grand Total") %>% 
    rename(health_board = Name,
           population = Population)
  
  scot_data %>% 
    left_join(scot_pop, by = "health_board")
  
  }




cov_scotpop <- function(){
  
  
}
