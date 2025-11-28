library(tidyverse)

voetbal <- read_csv("fifa_players.csv")

# Tel het aantal spelers per land
land_telling <- voetbal %>%
  group_by(nationality) %>%
  summarise(aantal_spelers = n()) %>%
  arrange(desc(aantal_spelers)) %>%
  head(20) # Top 20 landen

# grafiek van Top 20 landen  met de meeste spelers
grafiek <- ggplot(land_telling, aes(x = reorder(nationality, aantal_spelers),
                                    y = aantal_spelers,
                                    fill = nationality)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 20 landen met de meeste spelers",
       x = "Land",
       y = "Aantal spelers") +
  theme_minimal()

print(grafiek)

# Sla de grafiek op
ggsave("visualization.png", grafiek, width = 10, height = 8)
