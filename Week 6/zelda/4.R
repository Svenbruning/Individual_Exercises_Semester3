library(tidyverse)

load("zelda.RData")

# Filter titels waar Shigeru Miyamoto producer was
zelda <- zelda %>%
  filter(str_detect(producers, "Shigeru Miyamoto")) %>%
  group_by(title) %>%
  filter(year == min(year)) %>%
  arrange(year, title, system) %>%
  ungroup()

# Sla het resultaat op
save(zelda, file = "4.RData")