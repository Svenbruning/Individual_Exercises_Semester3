flavor <- readline("Flavor: ")
caffeine <- readline("Caffeine: ")

if (!(flavor %in% c("Light", "Bold"))) {
  cat("Enter either 'Light' or 'Bold' for flavor")
} else if (!(caffeine %in% c("Yes", "No"))) {
  cat("Enter either 'No' or 'Yes' for caffeine")
} else {
  if (flavor == "Light" & caffeine == "Yes") {
    cat("You might like green tea!")
  } else if (flavor == "Bold" & caffeine == "Yes") {
    cat("You might like black tea!")
  } else if (flavor == "Light" & caffeine == "No") {
    cat("You might like chamomile!")
  } else if (flavor == "Bold" & caffeine == "No") {
    cat("You might like rooibos!")
  }
}