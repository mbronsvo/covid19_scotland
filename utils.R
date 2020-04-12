# doubling times

calc_doubling <- function(x, n = 7){
  # n is number of points to count over
  dbl <- n * log(2) / log(x / replace_na(lag(x, n), 0))
  # zero doubling shouldn't exist
  ifelse(dbl == 0, NA, dbl)
}
