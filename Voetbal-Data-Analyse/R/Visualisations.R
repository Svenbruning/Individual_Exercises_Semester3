# Visualisations.R

# Laden van benodigde packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(corrplot)
library(gridExtra)

# Laden van opgeschoonde data
load("Data/clean_data.RData")

# Gemiddeld aantal doelpunten per seizoen
season_goals <- matches %>%
  group_by(season) %>%
  summarise(
    avg_total_goals = mean(total_goals, na.rm = TRUE),
    avg_home_goals = mean(home_team_goal, na.rm = TRUE),
    avg_away_goals = mean(away_team_goal, na.rm = TRUE)
  )

ggplot(season_goals, aes(x = season, group = 1)) +
  geom_line(aes(y = avg_total_goals, color = "Totaal"), linewidth = 1) +
  geom_line(aes(y = avg_home_goals, color = "Thuis"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = avg_away_goals, color = "Uit"), linewidth = 1, linetype = "dotted") +
  labs(
    title = "Gemiddeld aantal doelpunten per seizoen",
    x = "Seizoen", y = "Gemiddeld aantal doelpunten", color = "Legenda"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Lijnplots per competitie (facet)
comp_season_goals <- matches %>%
  group_by(season, league_name) %>%
  summarise(avg_total_goals = mean(total_goals, na.rm = TRUE))

ggplot(comp_season_goals, aes(x = season, y = avg_total_goals, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 1.5) +
  facet_wrap(~ league_name, scales = "free_y") +
  labs(
    title = "Gemiddeld aantal doelpunten per seizoen per competitie",
    x = "Seizoen", y = "Gemiddeld aantal doelpunten"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heatmap: Gemiddelde doelpunten per seizoen per competitie
season_league_goals <- matches %>%
  group_by(season, league_name) %>%
  summarise(avg_goals = mean(total_goals, na.rm = TRUE)) %>%
  ungroup()

ggplot(season_league_goals, aes(x = season, y = league_name, fill = avg_goals)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "red", name = "Gem. doelpunten") +
  labs(
    title = "Heatmap van gemiddelde doelpunten per seizoen en competitie",
    x = "Seizoen", y = "Competitie"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8)
  )

# Top 10 teams: Gemiddeld aantal doelpunten
team_goals <- matches %>%
  group_by(home_team_api_id) %>%
  summarise(avg_goals = mean(total_goals, na.rm = TRUE)) %>%
  arrange(desc(avg_goals)) %>%
  slice_head(n = 10)

ggplot(team_goals, aes(x = reorder(as.factor(home_team_api_id), avg_goals), y = avg_goals)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(
    title = "Top 10 teams op gemiddeld aantal doelpunten",
    x = "Team ID", y = "Gemiddeld aantal doelpunten"
  ) +
  theme_minimal()

# Top 10 spelers: Gemiddelde rating
player_ratings <- player_attr %>%
  group_by(player_api_id) %>%
  summarise(avg_rating = mean(overall_rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating)) %>%
  slice_head(n = 10)

ggplot(player_ratings, aes(x = reorder(as.factor(player_api_id), avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(
    title = "Top 10 spelers op gemiddelde rating",
    x = "Speler ID", y = "Gemiddelde rating"
  ) +
  theme_minimal()

# Boxplots: Totaal aantal doelpunten per competitie
ggplot(matches, aes(x = league_name, y = total_goals)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot van totaal aantal doelpunten per competitie",
    x = "Competitie", y = "Totaal aantal doelpunten"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualisaties met Team Attributes
# Bereken gemiddelde team attributes per team
team_attr_summary <- team_attr %>%
  group_by(team_api_id) %>%
  summarise(
    avg_buildUpPlaySpeed = mean(buildUpPlaySpeed, na.rm = TRUE),
    avg_chanceCreationShooting = mean(chanceCreationShooting, na.rm = TRUE),
    avg_defencePressure = mean(defencePressure, na.rm = TRUE)
  )

# Koppel aan thuiswedstrijden
team_perf <- matches %>%
  group_by(home_team_api_id) %>%
  summarise(avg_home_goals = mean(home_team_goal, na.rm = TRUE)) %>%
  left_join(team_attr_summary, by = c("home_team_api_id" = "team_api_id")) %>%
  drop_na()

# Scatterplot: Speelstijl vs. Gemiddelde Thuisdoelpunten
ggplot(team_perf, aes(x = avg_buildUpPlaySpeed, y = avg_home_goals)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  labs(
    title = "Relatie tussen Build-Up Play Speed en Gemiddeld aantal thuisdoelpunten",
    x = "Gemiddelde Build-Up Play Speed",
    y = "Gemiddeld aantal thuisdoelpunten"
  ) +
  theme_minimal()

# Heatmap: Correlaties tussen team attributes
team_attr_corr <- team_attr %>%
  select(buildUpPlaySpeed, buildUpPlayPassing,
         chanceCreationPassing, chanceCreationShooting,
         defencePressure, defenceAggression) %>%
  cor(use = "complete.obs")

corrplot(team_attr_corr, method = "color", type = "upper",
         title = "Correlatie tussen teamattributes",
         tl.col = "black", tl.srt = 45)

# Sammenvattende tabellen
team_summary <- matches %>%
  group_by(home_team_api_id) %>%
  summarise(
    gemiddeld_doelpunten = mean(total_goals, na.rm = TRUE),
    gemiddeld_thuisdoelpunten = mean(home_team_goal, na.rm = TRUE),
    gemiddeld_uitdoelpunten = mean(away_team_goal, na.rm = TRUE)
  ) %>%
  arrange(desc(gemiddeld_doelpunten))
print(head(team_summary, 10))

player_summary <- player_attr %>%
  group_by(player_api_id) %>%
  summarise(
    avg_rating = mean(overall_rating, na.rm = TRUE),
    avg_potential = mean(potential, na.rm = TRUE),
    avg_dribbling = mean(dribbling, na.rm = TRUE),
    avg_finishing = mean(finishing, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_rating))
print(head(player_summary, 10))

