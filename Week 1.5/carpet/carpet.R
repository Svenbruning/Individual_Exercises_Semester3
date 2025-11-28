calculate_growth_rate <- function(years, visitors) {
  
  difference_visitors <- visitors[length(visitors)] - visitors[1]
  difference_years <- years[length(years)] - years[1]
  
  growht_rate <- difference_visitors / difference_years
  
  return(growht_rate)
}

predict_visitors <- function(years, visitors, year) {
  
  growht_rate <- calculate_growth_rate(years, visitors)
  
  last_known_year <- years[length(years)]
  last_known_visitors <- visitors[length(visitors)]
  
  years_difference <- year - last_known_year
  
  prediction <- last_known_visitors + (growht_rate * years_difference)
  prediction <- round(prediction, 2)
  return(prediction)
}

visitors <- read.csv("visitors.csv")
year <- as.integer(readline("Year: "))
predicted_visitors <- predict_visitors(visitors$year, visitors$visitors, year)
cat(paste0(predicted_visitors, " million visitors\n"))

