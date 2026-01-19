# Visualisations.R

# ---------------------------------------------------
# 0. Laden van benodigde packages
# ---------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(corrplot)
library(gridExtra)

# ---------------------------------------------------
# 1. Laden van opgeschoonde data
# ---------------------------------------------------
load("Data/clean_data.RData")

# ---------------------------------------------------
# 2. Lijnplot: Gemiddeld aantal thuis- en uitdoelpunten per seizoen
# ---------------------------------------------------
season_goals <- matches %>%
  group_by(season) %>%
  summarise(
    avg_home_goals = mean(home_team_goal, na.rm = TRUE),
    avg_away_goals = mean(away_team_goal, na.rm = TRUE)
  )

ggplot(season_goals, aes(x = season, group = 1)) +
  geom_line(aes(y = avg_home_goals, color = "Thuis"),
            linewidth = 1.3, linetype = "solid") +
  geom_line(aes(y = avg_away_goals, color = "Uit"),
            linewidth = 1.3, linetype = "dashed") +
  geom_point(aes(y = avg_home_goals, color = "Thuis"), size = 2.5) +
  geom_point(aes(y = avg_away_goals, color = "Uit"), size = 2.5) +
  scale_y_continuous(
    breaks = seq(
      floor(min(c(season_goals$avg_home_goals,
                  season_goals$avg_away_goals), na.rm = TRUE) * 10) / 10,
      ceiling(max(c(season_goals$avg_home_goals,
                    season_goals$avg_away_goals), na.rm = TRUE) * 10) / 10,
      by = 0.1
    )
  ) +
  labs(
    title = "Gemiddeld aantal doelpunten per seizoen (Thuis vs Uit)",
    x = "Seizoen",
    y = "Gemiddeld aantal doelpunten",
    color = "Legenda"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "grey85")
  )

# ---------------------------------------------------
# 3. Lijnplots per competitie: Gemiddelde doelpunten per wedstrijd
# ---------------------------------------------------
comp_season_goals <- matches %>%
  group_by(season, league_name) %>%
  summarise(
    avg_goals_per_match = mean(total_goals, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(comp_season_goals, aes(x = season, y = avg_goals_per_match, group = 1)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 1.5) +
  facet_wrap(~ league_name, scales = "fixed") +
  labs(
    title = "Gemiddeld aantal doelpunten per wedstrijd per seizoen en competitie",
    subtitle = "Gemiddelde van thuis- en uitdoelpunten per wedstrijd",
    x = "Seizoen",
    y = "Gemiddeld aantal doelpunten per wedstrijd"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------------------------------------------
# 4. Heatmap: Gemiddelde doelpunten per seizoen per competitie
# ---------------------------------------------------
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

# ---------------------------------------------------
# 5. Barplot: Top 10 teams met het hoogste gemiddelde aantal doelpunten per wedstrijd
# ---------------------------------------------------
team_match_goals <- bind_rows(
  matches_raw %>%
    transmute(team_api_id = home_team_api_id, goals = home_team_goal),
  matches_raw %>%
    transmute(team_api_id = away_team_api_id, goals = away_team_goal)
)

team_avg_goals <- team_match_goals %>%
  group_by(team_api_id) %>%
  summarise(avg_goals = mean(goals, na.rm = TRUE), .groups = "drop")

top_10_teams <- team_avg_goals %>%
  left_join(
    teams_raw %>% select(team_api_id, team_long_name),
    by = "team_api_id"
  ) %>%
  filter(!is.na(team_long_name)) %>%
  arrange(desc(avg_goals)) %>%
  slice_head(n = 10)

ggplot(top_10_teams, aes(x = reorder(team_long_name, avg_goals), y = avg_goals)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(
    title = "Top 10 teams met hoogste gemiddelde aantal doelpunten per wedstrijd",
    x = "Team",
    y = "Gemiddeld aantal doelpunten"
  ) +
  theme_minimal()

# ---------------------------------------------------
# 6. Barplot: Top 10 spelers op gemiddelde rating
# ---------------------------------------------------
player_ratings <- player_attr %>%
  group_by(player_api_id) %>%
  summarise(
    avg_rating = mean(overall_rating, na.rm = TRUE),
    .groups = "drop"
  )

top_10_players <- player_ratings %>%
  left_join(
    players_raw %>% select(player_api_id, player_name),
    by = "player_api_id"
  ) %>%
  filter(!is.na(player_name)) %>%
  arrange(desc(avg_rating)) %>%
  slice_head(n = 10)

ggplot(top_10_players, aes(x = reorder(player_name, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(
    title = "Top 10 spelers op gemiddelde rating",
    x = "Speler",
    y = "Gemiddelde rating"
  ) +
  theme_minimal()

# ---------------------------------------------------
# 7. Boxplot: Totaal aantal doelpunten per wedstrijd per competitie
# ---------------------------------------------------
ggplot(matches, aes(x = league_name, y = total_goals)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot van totaal aantal doelpunten per wedstrijd per competitie",
    x = "Competitie",
    y = "Totaal aantal doelpunten per wedstrijd"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------------------------------------------
# 9. Scatterplot: Relatie tussen gemiddelde thuis- en uitdoelpunten per team
# ---------------------------------------------------
home_goals <- matches_raw %>%
  group_by(team_api_id = home_team_api_id) %>%
  summarise(
    avg_home_goals = mean(home_team_goal, na.rm = TRUE),
    .groups = "drop"
  )

away_goals <- matches_raw %>%
  group_by(team_api_id = away_team_api_id) %>%
  summarise(
    avg_away_goals = mean(away_team_goal, na.rm = TRUE),
    .groups = "drop"
  )

team_perf <- home_goals %>%
  left_join(away_goals, by = "team_api_id") %>%
  left_join(
    teams_raw %>% select(team_api_id, team_long_name),
    by = "team_api_id"
  ) %>%
  filter(!is.na(team_long_name))

ggplot(team_perf, aes(x = avg_home_goals, y = avg_away_goals)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Relatie tussen gemiddelde thuis- en uitdoelpunten per team",
    x = "Gemiddeld aantal thuisdoelpunten per wedstrijd",
    y = "Gemiddeld aantal uitdoelpunten per wedstrijd"
  ) +
  theme_minimal()

# ---------------------------------------------------
# 10. Correlatieplot: Team attributes onderling
# ---------------------------------------------------
team_attr_corr <- team_attr %>%
  select(buildUpPlaySpeed, buildUpPlayPassing,
         chanceCreationPassing, chanceCreationShooting,
         defencePressure, defenceAggression) %>%
  cor(use = "complete.obs")

corrplot(team_attr_corr, method = "color", type = "upper",
         title = "Correlatie tussen teamattributes",
         tl.col = "black", tl.srt = 45,
         mar = c(0, 0, 3, 0))


