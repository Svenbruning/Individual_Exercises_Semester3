load("air.RData")

air <- air %>%
  mutate(emissions = as.numeric(emissions)) %>%
  arrange(desc(emissions))

save(air, file = "2.RData")
