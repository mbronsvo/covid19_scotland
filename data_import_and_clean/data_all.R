# get all the data -------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)

source("data_import_and_clean/data_utils.R")
source("data_import_and_clean/data_scotland.R")
source("data_import_and_clean/data_ukglobal.R")

data_scot <- cov_scotdata()
data_uk <- cov_ukdata()
data_global <- cov_globaldata()

