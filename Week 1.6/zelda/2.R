library(tidyverse)

load("zelda.RData")

# Tel hoeveel releases per jaar
zelda <- zelda %>%
  group_by(year) %>%
  summarize(releases = n()) %>%
  arrange(desc(releases))

# Sla het resultaat op
save(zelda, file = "2.RData")

