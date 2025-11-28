country <- readline("Do you prefer Dutch or German beer? ")
quality <- readline("Do you prefer Good or Bad beer ? ")

if (!(country %in% c("Dutch", "German"))) {
  cat("Enter either 'Dutch' or 'German' for country.")
} else if (!(quality %in% c("Good", "Bad"))) {
  cat("Enter either 'Good' or 'Bad' for beer quality.")
} else {
  if (country == "Dutch" & quality == "Good") {
    cat("You might like Amstel")
  } else if (country == "Dutch" & quality == "Bad") {
    cat("You might like Heineken")
  } else if (country == "German" & quality == "Good") {
    cat("You might like Veltins")
  } else if (country == "German" & quality == "Bad") {
    cat("You might like Becks")
  }
}