rm(list = ls())
path <- "/Users/smazeri/Documents/GitHub/covid19_scotland/"


# Load packages
library(flexdashboard) ; library(shiny) ; library(readr); library(dplyr); library(tidyr); library(purrr); 
library(forcats); library(stringr); library(htmlwidgets); library(lubridate); library(sf); library(RcppRoll); 
library(plotly); library(shinythemes);library(leaflet); library(classInt); library(ggrepel); library(scales);
library(leaflet.extras);library(viridis)
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

# Scot gov daily data
url_scot_country <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/coronavirus-covid-19-trends-in-daily-data/documents/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/govscot%3Adocument/Trends%2Bin%2Bdaily%2BCOVID-19%2Bdata%2B-%2B110520.xlsx?forceDownload=true"
#url_scot_hb <-      "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/documents/covid-19-data-by-nhs-board/covid-19-data-by-nhs-board/govscot%3Adocument/COVID-19%2Bdata%2Bby%2BNHS%2BBoard.xlsx?forceDownload=true"
url_scot_hb <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/coronavirus-covid-19-trends-in-daily-data/documents/covid-19-data-by-nhs-board/covid-19-data-by-nhs-board/govscot%3Adocument/COVID-19%2Bdata%2Bby%2BNHS%2BBoard.xlsx?forceDownload=true"
  
GET(url_scot_country, write_disk(scot_data_gov <- tempfile(fileext = ".xlsx")))
GET(url_scot_hb, write_disk(scot_data_gov_hb <- tempfile(fileext = ".xlsx")))


# Import Scottish Data
WATTY62CASES <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/new_daily_cases.csv"
WATTY62REGIONALCASES <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/regional_cases.csv"
WATTY62POP <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/HB_Populations.csv"
WATTY62DEATHS <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/regional_deaths.csv"
WATTY62TESTS <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/scot_tests.csv"

# Health board population data
scot_pop <- read_csv(file =WATTY62POP )

# Scottish healthboard cases data
data_hb_scot_gov <- read_excel(scot_data_gov_hb, sheet = "Table 1 - Cumulative cases", skip = 2) %>%
                    rename("Grand Total"= Scotland) %>%
                    mutate(date = ymd(Date)) %>%
                    select(-Date) %>%
                    filter(date > dmy("26/04/2020")) %>%
                    mutate_at(vars(-date),
                      ~parse_number(as.character(.)) )
names(data_hb_scot_gov) <- str_remove(names(data_hb_scot_gov), "NHS ") 

data_hb_scot_gov <- data_hb_scot_gov %>%
rename("Ayrshire and Arran" = "Ayrshire & Arran", 
       "Dumfries and Galloway" = "Dumfries & Galloway",
        "Greater Glasgow and Clyde" = "Greater Glasgow & Clyde") 

scot_data_raw <- read_csv(file = WATTY62REGIONALCASES) %>%
  #select(-`Grand Total`) %>%
  mutate(date = lubridate::dmy(Date)) %>%
  filter(date < dmy("27-Apr-2020")) %>%
  select(-Date) %>%
  bind_rows(data_hb_scot_gov) %>%
  mutate(country_region = "Scotland")


scot_data_health_board <- scot_data_raw %>% 
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
scot_cases <- read_excel(scot_data_gov, sheet = "Table 5 - Testing", skip = 3) %>%
           rename("Date" = "...1", 
         "confirmed_cases" = "Positive") %>%
           select(-Negative, -Total) %>%
  mutate(new_cases = c(1, diff(confirmed_cases, 1))) %>%
  mutate(date = lubridate::ymd(Date)) %>%
  mutate(doubling_time_week = 7*log(2)/log(confirmed_cases/replace_na(lag(confirmed_cases,7),0))) %>%
  select(-Date) %>%
  mutate(country_region = "Scotland")

scot_data <- scot_cases

# Scottish death data

scot_deaths <- read_excel(scot_data_gov, sheet = "Table 8 - Deaths", skip =2) %>%
  select(Date, `Number of COVID-19 confirmed deaths registered to date`) %>%
  rename(deaths = `Number of COVID-19 confirmed deaths registered to date`) %>%
  mutate(new_deaths = deaths - replace_na(lag(deaths),0)) %>%
  mutate(doubling_time_week = 7*log(2)/log(deaths/replace_na(lag(deaths,7),0))) %>%
  mutate(date = lubridate::ymd(Date)) %>%
  select(-Date) %>%
  mutate(country_region = "Scotland")


scot_data_all <- select(scot_data, country_region, date, new_cases, confirmed_cases) %>%
  full_join(select(scot_deaths, country_region, date, new_deaths, deaths), by = c("country_region", "date"))

uk_scot_data <- dat_uk %>%
  filter(country_region == "United Kingdom") %>%
  arrange(date) %>%
  mutate(new_deaths = deaths - lag(deaths),0) %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases), 0) %>%
  mutate(country_region = recode(country_region, "United Kingdom" = "UK")) %>%
  bind_rows(scot_data_all)



# Scottish tests
data_testing <- read_excel(scot_data_gov, sheet = "Table 5 - Testing", skip = 3) %>%
  rename("Date" = "...1", 
         "Total Positive" = "Positive", 
         "Total Negative" = "Negative") %>%
  mutate(date = ymd(Date)) %>%
  select(-Date) %>%
  filter(date > dmy("26/04/2020"))

scot_tests <- read_csv(file = WATTY62TESTS) %>%
  mutate(date = dmy(Date)) %>%
  filter(date < dmy("27-Apr-2020")) %>%
  select(-Date) %>%
  bind_rows(data_testing) %>%
  mutate(Conducted = c(2, diff(Total, 1)),
         `Today Positive` = c(0, diff(`Total Positive`, 1)),
         `Today Negative` = c(2, diff(`Total Negative`, 1)))
  
scot_tests <- scot_tests %>%
left_join(select(scot_deaths, date,new_deaths, deaths), by = "date") %>%
mutate(deaths_per_case = deaths/`Total Positive`,
         cases_per_test = `Total Positive`/Total)

scot_tests_long <- scot_tests %>%
  pivot_longer(cols = `Today Positive`:`Today Negative`, names_to = "Result", values_to = "Number") %>%
  mutate(Result = factor(recode(Result, "Today Negative" = "Negative",
                                "Today Positive" = "Positive"), levels = c("Positive", "Negative")))


#Overall deaths 
myurl_deaths <- "https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-21.xlsx"
GET(myurl_deaths, write_disk(tmp <- tempfile(fileext = ".xlsx")))
nrs_deaths_covid_raw <- read_excel(tmp, sheet = "Table 1 - COVID deaths", skip = 3) %>%
  select(-`Year to Date`) %>%
  rename("Details" = "...2",
         "Total" = "...25")
nrs_deaths_covid <- nrs_deaths_covid_raw %>% filter(`Week beginning` == "Deaths involving COVID-194") %>%
  pivot_longer(cols = `43829`:names(nrs_deaths_covid_raw)[ncol(nrs_deaths_covid_raw)-1],
               names_to = "week_beginning", 
               values_to = "weekly_deaths_total") %>%
  mutate(week_beginning = janitor::excel_numeric_to_date(parse_number(week_beginning))) %>%
  mutate(Type = "Covid19 deaths") %>%
  select(week_beginning, weekly_deaths_total, Type)

nrs_week <- max(nrs_deaths_covid$week_beginning)+6

nrs_deaths_all <- read_excel(tmp, sheet = "Table 2 - All deaths", skip = 3) %>% 
  select(-`...24`) %>%
  rename("Details" = "...2",
         "Total" = "...25") %>%
  filter(`Week beginning` %in% c("Total deaths from all causes", "Total deaths: average of corresponding")) %>%
  mutate_if(is.numeric,as.character, is.factor, as.character) %>%
  pivot_longer(cols = `43829`:names(.)[ncol(.)-1],
               names_to = "week_beginning", 
               values_to = "weekly_deaths_total") %>%
  mutate(week_beginning = janitor::excel_numeric_to_date(parse_number(week_beginning))) %>%
  mutate(Type = recode(`Week beginning`, "Total deaths from all causes" = "All causes - 2020",
                       "Total deaths: average of corresponding" = "All causes - 5 year average")) %>%
  select(week_beginning, weekly_deaths_total, Type) %>%
  mutate(weekly_deaths_total = parse_number(weekly_deaths_total))


nrs_deaths_comparison <- bind_rows(nrs_deaths_covid, nrs_deaths_all)

nrs_deaths_healthboards_total <- nrs_deaths_covid_raw %>%
                          filter(Details %in% scot_data_health_board_total$health_board) %>%
  pivot_longer(cols = `43829`:names(nrs_deaths_covid_raw)[ncol(nrs_deaths_covid_raw)-1],
               names_to = "week_beginning", 
               values_to = "weekly_deaths_total") %>% 
  mutate(week_beginning = janitor::excel_numeric_to_date(parse_number(week_beginning))) %>% 
  mutate(Type = "Covid19 deaths") %>%
  arrange(desc(week_beginning)) %>%
  distinct(Details, .keep_all = TRUE) %>%
  rename("Deaths" = Total, 
         "health_board" = Details)

nrs_deaths_healthboards_total <- nrs_deaths_healthboards_total %>% 
                     left_join(scot_pop, by = c("health_board" = "Name")) %>%
                     mutate(deaths_per_10000 = 10000*weekly_deaths_total/Population)

nrs_deaths_covid_week_location <- nrs_deaths_covid_raw %>% 
  filter(`Week beginning` == "Deaths involving COVID-194" | 
           Details %in% c("Care Home", "Home / Non-institution", "Hospital", "Other institution")) %>%
  pivot_longer(cols = `43829`:names(nrs_deaths_covid_raw)[ncol(nrs_deaths_covid_raw)-1],
               names_to = "week_beginning", 
               values_to = "weekly_deaths_total") %>%
  mutate(Details = case_when(`Week beginning` == "Deaths involving COVID-194" ~ "All",
                             TRUE ~ Details)) %>%
  mutate(week_beginning = janitor::excel_numeric_to_date(parse_number(week_beginning))) %>%
  mutate(Type = "Covid19 deaths") %>%
  rename("Location" = Details) %>%
  filter(Location != "Other institution") %>%
  select(Location, week_beginning, weekly_deaths_total, Type) %>%
  mutate(week_ending = week_beginning + 6)

#NRS HB weekly deaths
nrs_deaths_covid_hb <- nrs_deaths_covid_raw %>% slice(33:46) %>%
  #c("Ayrshire and Arran","Borders",  "Dumfries and Galloway", "Fife","Forth Valley", "Grampian", "Greater Glasgow and Clyde" , "Highland" , "Lanarkshire", "Lothian","Orkney", "Shetland" ,"Tayside", "Western Isles")
  pivot_longer(cols = `43829`:names(nrs_deaths_covid_raw)[ncol(nrs_deaths_covid_raw)-1],
               names_to = "week_beginning", 
               values_to = "weekly_deaths_total") %>%
  mutate(week_beginning = janitor::excel_numeric_to_date(parse_number(week_beginning))) %>%
  mutate(Type = "Covid19 deaths") %>%
  rename("health_board" = Details) %>%
  select(health_board, week_beginning, weekly_deaths_total, Type) %>%
  mutate(week_ending = week_beginning + 6)

nrs_deaths_covid_hb <- nrs_deaths_covid_hb %>%
                       left_join(scot_pop, by = c("health_board" = "Name")) %>%
                        mutate(deaths_per_10000 = 10000*weekly_deaths_total/Population)

nrs_deaths_covid_week_location$week_beginning_f <- factor(format(nrs_deaths_covid_week_location$week_beginning, "%d %b"), 
                                                    levels= unique(format(nrs_deaths_covid_week_location$week_beginning, "%d %b") ))
nrs_deaths_covid_week_location$Location <- factor(nrs_deaths_covid_week_location$Location, levels = c("Hospital", "Care Home", "Home / Non-institution"))

# NRS HB location 
nrs_deaths_hb_location <- read_excel(tmp, sheet = "Table 3 - deaths by location", skip = 3) %>% 
                          slice(8:21) %>%
                          rename(health_board = "...1",
                                 `Care home` = "Care\r\nHome...2", 
                                 `Home / Non-institution` = "Home / Non-institution...3", 
                                 Hospital = "Hospital...4", 
                                 `Other institution` = "Other\r\ninstitution3...5", 
                                 Total = "All locations...6") %>%
                          select(health_board:Total) %>%
                          mutate_if(is.numeric,as.character, is.factor, as.character) %>%
                          pivot_longer(cols = `Care home`:`Other institution`, names_to = "Location", values_to = "deaths") %>%
                          mutate(deaths = parse_number(deaths),
                                 Total = parse_number(Total))

# Map files
# SCOTLAND MAP
cases_by_area <- sf::st_read(paste0(path,"SG_NHS_HealthBoards_2019c.geojson")) %>%
  #rmapshaper::ms_simplify(keep = 0.15) %>%
  st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) %>%
  left_join(scot_data_health_board_total, by = c("HBName" = "health_board")) %>%
  left_join(scot_pop, by = c("HBName" = "Name")) %>%
  left_join(select(nrs_deaths_healthboards_total, health_board, Deaths), by = c("HBName" = "health_board"))

#cases_by_area <- cbind(cases_by_area, st_coordinates(st_centroid(cases_by_area)))
cases_by_area$cases_popup <- paste(cases_by_area$HBName, cases_by_area$CasesSum, "cases", sep = " ")
cases_by_area$CasesRate <- 10000* cases_by_area$CasesSum/cases_by_area$Population
cases_by_area$DeathsRate <- 10000* cases_by_area$Deaths/cases_by_area$Population
cases_by_area$cases_popup_pop <- paste(cases_by_area$HBName, round(cases_by_area$CasesRate,2), "cases per 10,000 population", sep = " ")
cases_by_area$deaths_popup <- paste(cases_by_area$HBName, cases_by_area$Deaths, "deaths", sep = " ")
cases_by_area$deaths_popup_pop <- paste(cases_by_area$HBName, round(cases_by_area$DeathsRate,2), "deaths per 10,000 population", sep = " ")

## Covid daily hospital data
#myurl <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/documents/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/govscot%3Adocument/Data%2BTable%2B%252810-04-2020%2529.xlsx?forceDownload=true"
myurl <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/coronavirus-covid-19-trends-in-daily-data/documents/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/govscot%3Adocument/Trends%2Bin%2Bdaily%2BCOVID-19%2Bdata%2B-%2B110520.xlsx?forceDownload=true"
GET(myurl, write_disk(tmp <- tempfile(fileext = ".xlsx")))

#data_hosp <- read_excel(tmp, sheet = "Table 1", skip = 3)
data_hosp <- read_excel(tmp, sheet = "Table 2 - Hospital Care", skip = 3)

data_hosp <- data_hosp %>%
  rename("date" = "...1",
         "ICU_confirmed" = "Confirmed...2",
         "ICU_suspected" ="Suspected...3", 
         "ICU_total" = "Total...4",
         "Hospital_confirmed" = "Confirmed...5",
         "Hospital_suspected" = "Suspected...6",
         "Hospital_total" = "Total...7" ) %>%
  filter(!is.na(ICU_total)) %>%
  #mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
  mutate(date = lubridate::ymd(date)) %>%
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

covid_deaths_2020 <- covid_deaths_2020 %>%
                     mutate(Age = str_remove(Age, " years"), 
                            Age = case_when(Age == "85 and over" ~ ">=85", 
                                            TRUE ~ Age)) %>%
                     mutate(Age = factor(Age, levels = c("0", "1-14", "15-44", "45-64", 
                                                          "65-74", "75-84", ">=85")))

#save.image(file = "/Users/smazeri/Dropbox/Scot_Covid19/app_all.RData")
save(cases_by_area,cov_globaldata,cov_offset, cov_trajplot, cov_ukdata,
     covid_deaths_2020, dat_uk, dat_uk_total, dat_world, data_hosp_hosp, 
     data_hosp_icu, last_week, nrs_week, nrs_deaths_covid_week_location,
     ref, 
     scot_cases, scot_data, scot_data_all, scot_data_health_board, scot_data_health_board_total,
     scot_data_raw, scot_deaths, scot_pop,  scot_tests, scot_tests_long, uk_scot_data, 
     scot_agesex, covid_deaths_2020_pop, nrs_deaths_comparison,
     nrs_deaths_covid_hb, nrs_deaths_healthboards_total,
     nrs_deaths_hb_location,
     file = "/Users/smazeri/Dropbox/Scot_Covid19/app_all.RData")
#rm(list = ls())
#load("app_all.RData")

#myurl <- "https://www.dropbox.com/s/453v8vfpghe9bre/app_all.RData?raw=1"
#GET(myurl, write_disk(tmp <- tempfile(fileext = ".Rdata")))
#load(tmp)



