load("air.RData")

air <- air %>%
  filter(county == "OR - Baker") %>%
  mutate(emissions = as.numeric(emissions)) %>%
  arrange(desc(emissions))

save(air, file = "4.RData")
