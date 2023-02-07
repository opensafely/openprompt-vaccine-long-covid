little_sample_size_fn <- function(y, n, is_months = TRUE){
  # convert to X 100,000 person-years as the denominator
  if(is_months){
    denom <- (n/12)/1e5
  }else{
    denom <- n/1e5
  }
  # calculate the rate per 100,000 person-years
  num <- y/denom
  # calculate sigma
  sigma <- sqrt(num/denom)
  # print results
  cat("rate ", num, "\n")
  cat("sigma ", sigma, "\n")
  cat("CI width ", sigma*2, "\n")
}
little_sample_size_fn(y = 80, n = 1.6e6, is_months = T)
little_sample_size_fn(y = 200, n = 20e6, is_months = T)
little_sample_size_fn(y = 4e4, n = 40e6, is_months = F)
