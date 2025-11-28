# Ask the user for a filename
filename <- readline("Enter CSV filename: ")

# Read the CSV file
data <- read.csv(filename)

# Get the values 
total_stops <- nrow(data)
shortest_stop <- min(data$time)
longest_stop <- max(data$time)
total_time <- sum(data$time)

# Print the results
print(paste("Total numnber of pitstops:", total_stops))
print(paste("Shortest pit stop:", shortest_stop, "seconds"))
print(paste("Longest pit stop:", longest_stop, "seconds"))
print(paste("Total time spent on pit stops:", total_time, "seconds"))
