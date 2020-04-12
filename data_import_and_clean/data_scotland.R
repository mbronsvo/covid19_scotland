# Import Scottish Data ============================================================

cov_scotdata <- function(){
  
  WATTY62PATH <- "https://raw.githubusercontent.com/watty62/Scot_covid19/master/data/processed/"
  WATTY62POP <- "HB_Populations.csv"
  WATTY62CASES <- "new_daily_cases.csv"
  WATTY62REGIONALCASES <- "regional_cases.csv"
  WATTY62DEATHS <- "regional_deaths.csv"
  WATTY62TESTS <- "scot_tests.csv"
  
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
  
  # Add testing ------------------------------------------------------------------
  
  scot_tests <- read_csv(file = paste0(WATTY62PATH,WATTY62TESTS)) %>% 
    clean_names() %>%
    mutate(date = dmy(date)) %>% 
    rename(tests_total = total,
           tests_today = conducted) %>% 
    mutate(health_board = "Grand Total")
  
  scot_data <- scot_data %>% 
    left_join(scot_tests, by = c("date", "health_board"))
  
  scot_data
  
}



cov_scothospdata <- function(){
  ## Covid daily hospital data
  myurl <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/04/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/documents/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/govscot%3Adocument/Data%2BTable%2B%252810-04-2020%2529.xlsx?forceDownload=true"
  
  GET(myurl, write_disk(tmp <- tempfile(fileext = ".xlsx")))
  scot_hosp <- read_excel(tmp, sheet = "Table 1", skip = 3)
  file.remove(tmp)
  
  scot_hosp <- scot_hosp %>%
    rename("date" = "...1",
           "icu_confirmed" = "Confirmed...2",
           "icu_suspected" ="Suspected...3", 
           "icu_total" = "Total...4",
           "hosp_confirmed" = "Confirmed...5",
           "hosp_suspected" = "Suspected...6",
           "hosp_total" = "Total...7" ) %>%
    filter(!is.na(hosp_total)) %>%
    mutate(date = excel_numeric_to_date(as.numeric(date))) %>% 
    mutate(icu_confsusp = case_when(is.na(icu_confirmed) & is.na(icu_suspected) ~ icu_total)) %>%
    mutate(hosp_confsusp = case_when(is.na(hosp_confirmed) & is.na(hosp_suspected) ~ hosp_total)) %>% 
    pivot_longer(icu_confirmed:hosp_confsusp, names_to = "key", values_to = "number") %>% 
    separate(key, into = c("location", "class"), sep = "_") %>% 
    mutate(class = fct_recode(class, "Confirmed/Suspected" = "confsusp"),
           class = str_to_title(class))
  
  scot_hosp
}
