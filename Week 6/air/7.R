load("air.RData")

air <- air %>% 
  mutate(emissions = as.numeric(emissions)) %>%
  group_by(source = level_1, pollutant) %>%
  summarize(emissions = sum(emissions, na.rm = TRUE)) %>%
  arrange(source, pollutant)

save(air, file = "7.RData")