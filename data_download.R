# READ data from various github places

# GLOBAL data

cov_globaldata <- function() {
  
  
  # World Covid19 data
  # https://github.com/CSSEGISandData/COVID-19
  
  globaldata <- tribble(
    ~"type", ~"file",
    "confirmed_cases", "time_series_covid19_confirmed_global.csv",
    "deaths", "time_series_covid19_deaths_global.csv",
    "recovered", "time_series_covid19_recovered_global.csv"
  ) %>%
    mutate(path = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/")
  
  read_global <- function(type, file, path) {
    read_csv(file = paste0(path, file)) %>%
      pivot_longer(
        cols = -(`Province/State`:Long),
        names_to = "date",
        values_to = "n"
      ) %>%
      janitor::clean_names()
  }
  
  dat <- globaldata %>%
    mutate(data = pmap(., read_global)) %>%
    unnest(data) %>%
    filter(is.na(province_state)) %>%
    select(-file, -path, -province_state, -lat, -long) %>%
    mutate(date = mdy(date)) %>%
    pivot_wider(
      names_from = "type",
      values_from = "n"
    )
  
  #ourworldindata.org
  world_data_china<- read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv") %>%
    filter(location == "China") %>%
    rename( "country_region" = "location",
            "deaths" = "total_deaths",
            "confirmed_cases" = "total_cases")
  
  dat <- bind_rows(dat, world_data_china) %>%
    mutate(country_region = recode(country_region, "United Kingdom" = "UK"))
}

cov_ukdata <- function() {
  # data sources
  wt_SC <- "covid-19-totals-scotland.csv"
  
  wt_NI <- "covid-19-totals-northern-ireland.csv"
  
  wt_WA <- "covid-19-totals-wales.csv"
  
  wt_UK <- "covid-19-totals-uk.csv"
  
  wt_EN <- "covid-19-totals-england.csv"
  
  data_sources <- tribble(
    ~source, ~Region,
    wt_SC, "Scotland",
    wt_NI, "Northern Ireland",
    wt_WA, "Wales",
    wt_UK, "United Kingdom",
    wt_EN, "England"
  ) %>%
    mutate(source = paste0("https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/", source))
  
  # Build main data
  dat <- data_sources %>%
    mutate(data = purrr::map(source, read_csv)) %>%
    unnest(data) %>%
    select(-source, -Tests) %>% 
    rename(country_region = Region,
           date = Date,
           confirmed_cases = ConfirmedCases,
           deaths = Deaths) %>% 
    mutate(recovered = NA)
  
  # impute England
  
   dat %>% 
     pivot_longer(cols = c(confirmed_cases, deaths, recovered),
                  names_to = "type", values_to = "value") %>% 
     pivot_wider(names_from = country_region, values_from = value) %>%  
   mutate(`England` = `United Kingdom` - replace_na(Scotland, 0) - replace_na(`Northern Ireland`, 0) - replace_na(Wales, 0)) %>% 
     pivot_longer(cols = Scotland:England,
                  names_to = "country_region",
                  values_to = "value") %>% 
     pivot_wider(names_from = type, values_from = value)
}