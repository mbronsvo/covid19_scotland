rm(list = ls())
path <- "/Users/smazeri/Documents/GitHub/covid19_scotland/"


# Load packages
library(flexdashboard) ; library(shiny) ; library(readr); library(dplyr); library(tidyr); library(purrr); library(forcats); library(stringr); library(htmlwidgets); library(lubridate); library(sf); library(RcppRoll); library(plotly); library(shinythemes);library(leaflet); library(classInt); library(ggrepel); library(scales); library(leaflet.extras);library(viridis)
library(httr); library(readxl)
#library(tidyverse) ; library(httr) ;  ; library(readxl) ; library(DT) ; #library(rvest); #library(htmltools) ;  library(xml2);
#library(RColorBrewer); 
#library(ggsci);  

# Functions for uk and world data download
source(paste0(path,"data_download.R"))
# Functions for log date trajectories plots
source(paste0(path, "log_time_traj.R"))

# Import UK and world data
dat_world <- cov_globaldata() 
dat_uk <- cov_ukdata() 
dat_uk_total <- dat_uk %>% 
  group_by(country_region) %>% 
  summarise(CumDeaths = max(deaths,na.rm = T))

# Import Scottish Data
WATTY62CASES <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/new_daily_cases.csv"
WATTY62REGIONALCASES <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/regional_cases.csv"
WATTY62POP <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/HB_Populations.csv"
WATTY62DEATHS <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/regional_deaths.csv"
WATTY62TESTS <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/scot_tests.csv"

# Health board population data
scot_pop <- read_csv(file =WATTY62POP )

# Scottish healthboard cases data
scot_data_raw <- read_csv(file = WATTY62REGIONALCASES) 
#write_csv(scot_data_raw, "regional_cases_upto_07042020.csv")

scot_data <- scot_data_raw %>%
  rename(confirmed_cases= `Grand Total`) %>%
  mutate(new_cases = confirmed_cases - replace_na(lag(confirmed_cases),0)) %>%
  mutate(doubling_time_week = 7*log(2)/log(confirmed_cases/replace_na(lag(confirmed_cases,7),0))) %>%
  mutate(date = lubridate::dmy(Date)) %>%
  select(-Date) %>%
  replace_na(list(new_cases = 0)) %>%
  mutate(country_region = "Scotland")

scot_data_health_board <- scot_data %>% 
  select(date, `Ayrshire and Arran`:`Western Isles`) %>%
  pivot_longer(`Ayrshire and Arran`:`Western Isles`,
               names_to = "health_board",
               values_to = "confirmed_cases") %>% 
  group_by(health_board) %>%
  mutate(new_cases = confirmed_cases - replace_na(lag(confirmed_cases), 0)) %>%
  ungroup() %>%
  replace_na(list(new_cases = 0))

scot_data_health_board_total <- scot_data_health_board %>% 
  group_by(health_board) %>%
  summarise(CasesSum = max(confirmed_cases, na.rm = T))

# Scottish overall cases
scot_cases <- read_csv(file = WATTY62CASES) %>%
  rename("new_cases" = "New cases") %>%
  mutate(date = lubridate::dmy(Date)) %>%
  mutate(confirmed_cases = cumsum(new_cases)) %>%
  mutate(doubling_time_week = 7*log(2)/log(confirmed_cases/replace_na(lag(confirmed_cases,7),0))) %>%
  select(-Date) %>%
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

uk_scot_data <- dat_uk %>%
  filter(country_region == "United Kingdom") %>%
  arrange(date) %>%
  mutate(new_deaths = deaths - replace_na(lag(deaths),0)) %>%
  mutate(new_cases = confirmed_cases - replace_na(lag(confirmed_cases), 0)) %>%
  mutate(country_region = recode(country_region, "United Kingdom" = "UK")) %>%
  bind_rows(scot_data_all)


# Scottish tests
scot_tests <- read_csv(file = WATTY62TESTS) 
scot_tests <- scot_tests %>%
  mutate(date = seq(dmy("24-Jan-2020"),dmy("24-Jan-2020")+days(nrow(scot_tests)-1), by = 1)) %>%
  left_join(select(scot_deaths, date,new_deaths, deaths), by = "date") %>%
  mutate(deaths_per_case = deaths/`Total Positive`,
         cases_per_test = `Total Positive`/Total)

scot_tests_long <- scot_tests %>%
  pivot_longer(cols = `Today Positive`:`Today Negative`, names_to = "Result", values_to = "Number") %>%
  mutate(Result = factor(recode(Result, "Today Negative" = "Negative",
                                "Today Positive" = "Positive"), levels = c("Positive", "Negative")))


# Map files
# SCOTLAND MAP
cases_by_area <- sf::st_read(paste0(path,"SG_NHS_HealthBoards_2019c.geojson")) %>%
  st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) %>%
  left_join(scot_data_health_board_total, by = c("HBName" = "health_board")) %>%
  left_join(scot_pop, by = c("HBName" = "Name")) 

cases_by_area <- cbind(cases_by_area, st_coordinates(st_centroid(cases_by_area)))

cases_by_area$cases_popup <- paste(cases_by_area$HBName, cases_by_area$CasesSum, "cases", sep = " ")

cases_by_area$CasesRate <- 10000* cases_by_area$CasesSum/cases_by_area$Population
cases_by_area$cases_popup_pop <- paste(cases_by_area$HBName, round(cases_by_area$CasesRate,2), "cases per 10,000 population", sep = " ")

## Covid daily hospital data
myurl <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/documents/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/govscot%3Adocument/Data%2BTable%2B%252810-04-2020%2529.xlsx?forceDownload=true"
GET(myurl, write_disk(tmp <- tempfile(fileext = ".xlsx")))

data_hosp <- read_excel(tmp, sheet = "Table 1", skip = 3)

data_hosp <- data_hosp %>%
  rename("date" = "...1",
         "ICU_confirmed" = "Confirmed...2",
         "ICU_suspected" ="Suspected...3", 
         "ICU_total" = "Total...4",
         "Hospital_confirmed" = "Confirmed...5",
         "Hospital_suspected" = "Suspected...6",
         "Hospital_total" = "Total...7" ) %>%
  filter(!is.na(ICU_total)) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
  mutate(ICU_confsusp = case_when(is.na(ICU_confirmed) & is.na(ICU_suspected) ~ ICU_total)) %>%
  mutate(Hospital_confsusp = case_when(is.na(Hospital_confirmed) & is.na(Hospital_suspected) ~ Hospital_total)) %>%
  select(date, contains("ICU"), everything())

data_hosp_icu <- data_hosp %>%
  select(date, contains("ICU")) %>%
  pivot_longer(cols = c(ICU_confirmed, ICU_suspected, ICU_confsusp), names_to = "Patients", values_to = "Number") %>%
  mutate(Patients = recode(Patients,  "ICU_confsusp" = "Confirmed/Suspected", 
                           "ICU_confirmed" = "Confirmed", 
                           "ICU_suspected" = "Suspected")) %>%
  mutate(Patients = factor(Patients, levels = c("Confirmed/Suspected", "Confirmed", "Suspected")))

data_hosp_hosp <- data_hosp %>%
  select(date, contains("hospital")) %>%
  pivot_longer(cols = c(Hospital_confirmed, Hospital_suspected, Hospital_confsusp), names_to = "Patients", values_to = "Number") %>%
  mutate(Patients = recode(Patients,  "Hospital_confsusp" = "Confirmed/Suspected", 
                           "Hospital_confirmed" = "Confirmed", 
                           "Hospital_suspected" = "Suspected")) %>%
  mutate(Patients = factor(Patients, levels = c("Confirmed/Suspected", "Confirmed", "Suspected")))

# Covid death demographics
myurl2 <- "https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdeaths-involving-coronavirus-covid-19"
covid_deaths<- read_csv(myurl2)
last_week <- lubridate::ymd(str_remove(covid_deaths$DateCode, "w/c ")) %>% max(na.rm = T)
covid_deaths_2020 <-subset(covid_deaths, DateCode == "2020" & `Cause Of Death` == "COVID-19 related" & Sex != "All" & Age != "All") %>% arrange(Age, Sex)


# Align covid_deaths_2020 and scotland demography
scot_agesex <- cov_demography()

covid_deaths_2020_pop <- covid_deaths_2020 %>% 
  mutate(Age = str_remove(Age, " years$"),
         Age = case_when(Age == 0                   ~ "0-0.999",
                         Age == "85 years and over" ~ "86-90",
                         TRUE                       ~ Age)) %>% 
  separate(Age, into = c("age_from", "age_to"), sep = "-", remove = FALSE) %>%
  mutate_at(c("age_from", "age_to"), parse_double) %>% 
  left_join(scot_agesex, by = c("Sex" = "gender")) %>% 
  filter(age >= age_from & age <= age_to) %>% 
  arrange(Sex, Age, age) %>% 
  group_by(Sex, Age) %>% 
  summarise(population = sum(count, na.rm = TRUE),
            deaths = unique(Value)) %>% 
  ungroup() %>% 
  mutate(Age = if_else(Age == "0-0.999", "0-1", Age))

#save.image(file = "/Users/smazeri/Dropbox/Scot_Covid19/app_all.RData")
save(cases_by_area,cov_globaldata,cov_offset, cov_trajplot, cov_ukdata,
     covid_deaths_2020, dat_uk, dat_uk_total, dat_world, data_hosp_hosp, 
     data_hosp_icu, last_week, 
     ref, 
     scot_cases, scot_data, scot_data_all, scot_data_health_board, scot_data_health_board_total,
     scot_data_raw, scot_deaths, scot_pop,  scot_tests, scot_tests_long, uk_scot_data, 
     scot_agesex, covid_deaths_2020_pop,
     file = "/Users/smazeri/Dropbox/Scot_Covid19/app_all.RData")
#rm(list = ls())
#load("app_all.RData")

#myurl <- "https://www.dropbox.com/s/453v8vfpghe9bre/app_all.RData?raw=1"
#GET(myurl, write_disk(tmp <- tempfile(fileext = ".Rdata")))
#load(tmp)



