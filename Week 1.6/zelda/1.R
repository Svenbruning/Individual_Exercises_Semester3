library(tidyverse)

zelda_raw <- read_csv("zelda.csv")

# Split de kolom 'release' in year en system
zelda <- zelda_raw %>%
  separate(release, into = c("year", "system"), sep = " - ") %>%
  mutate(year = as.integer(year))

# Maak van de 'role' kolom aparte kolommen
zelda <- zelda %>%
  pivot_wider(
    names_from = role,
    values_from = names,
    values_fn = ~ paste(unique(.x), collapse = ", ")
  )

# Sla het resultaat op
save(zelda, file = "zelda.RData")
