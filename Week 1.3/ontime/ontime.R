bus_data <- read.csv("bus.csv")
rail_data <- read.csv("rail.csv")

route_input <- readline("Route: ")

if(!(route %in% bus_data$route) && !(route %in% rail_data$route)) {
  print("Please enter a valid route.")
} else {
  bus_route <- bus_data[bus_data$route == route_input, ]
  rail_route <- rail_data[rail_data$route == route_input, ]
  
  route_data <- rbind(bus_route, rail_route)
  
  route_data$reliability <- route_data$numerator / route_data$denominator * 100
  
  peak_mean <- round(mean(route_data$reliability[route_data$peak == "PEAK"]))
  offpeak_mean <- round(mean(route_data$reliability[route_data$peak == "OFF_PEAK"]))
  
  cat("On time", peak_mean, "% of the time during peak hours.\n")
  cat("On time", offpeak_mean, "% of the time during off-peak hours.")
}
  


