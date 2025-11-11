library(tidyverse)

load("zelda.RData")

# Zoek titels met meer dan één producer 
zelda <- zelda %>%
  filter(str_detect(Producers, ", ")) %>%
  group_by(title) %>%
  filter(year == min(year)) %>%
  arrange(year, title, system) %>%
  ungroup()

# Sla het resultaat op
save(zelda, file = "5.RData")