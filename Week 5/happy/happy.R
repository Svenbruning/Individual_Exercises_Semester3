country <- readline("Country: ")

for (year in 2020:2024) {
  
  filename <- paste0(year, ".csv")
  data <- read.csv(filename)
  
  if (country %in% data$country){
    row <- data[data$country == country, ]
    score <- sum(row[, -1])
    score <- round(score, 2)
    cat(paste0(country, " (", year, "): ", score), "\n")
  } else {
    cat(paste0(country, " (", year, "): data unavailable"), "\n")
  }
}
