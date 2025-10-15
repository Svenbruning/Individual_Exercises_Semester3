# 1.R
library(dplyr)

# Lees het bestand
zelda <- read.csv("zelda.csv")

# Splits de kolom 'release' in twee nieuwe kolommen
zelda <- zelda %>%
  separate(release, into = c("year", "system"), sep = ", ")

# Zet 'year' om naar een getal
zelda$year <- as.integer(zelda$year)

# Maak één rij per game-release
zelda <- zelda %>%
  group_by(title, year, system) %>%
  summarize(
    directors = paste(names[role == "Director"], collapse = ", "),
    producers = paste(names[role == "Producer"], collapse = ", "),
    designers = paste(names[role == "Designer"], collapse = ", "),
    programmers = paste(names[role == "Programmer"], collapse = ", "),
    writers = paste(names[role == "Writer"], collapse = ", "),
    composers = paste(names[role == "Composer"], collapse = ", "),
    artists = paste(names[role == "Artist"], collapse = ", "),
    .groups = "drop"
  )

# Lege velden vervangen door NA
zelda[zelda == ""] <- NA

# Opslaan in een RData-bestand
save(zelda, file = "zelda.RData")
