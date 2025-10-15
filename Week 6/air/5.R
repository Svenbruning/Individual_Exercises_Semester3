load("air.RData")

air <- air %>%
  mutate(emissions = as.numeric(emissions)) %>%
  group_by(county) %>%
  slice_max(emissions, n = 1) %>%
  ungroup()

save(air, file = "5.RData")
