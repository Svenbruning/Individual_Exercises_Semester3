library(tidyverse)

load("zelda.RData")

# Houd alleen de eerste release(s) per titel
zelda <- zelda %>%
  group_by(title) %>%
  filter(year == min(year)) %>%
  arrange(year, title, system) %>%
  ungroup()

# Sla het resultaat op
save(zelda, file = "3.RData")