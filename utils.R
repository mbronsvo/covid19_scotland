# doubling times

doubling <- function(x, n = 7){
  # n is number of points to count over
  week <- 7
  dbl <- week * log(2) / log(x / replace_na(lag(x, week), 0))
  # zero doubling shouldn't exist
  ifelse(dbl == 0, NA, dbl)
}
