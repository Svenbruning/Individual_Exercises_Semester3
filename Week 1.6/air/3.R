load("air.RData")

air <- air %>%
  filter(county == "OR - Baker")

save(air, file = "3.RData")
