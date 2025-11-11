# EDA.R

# Laden van benodigde packages
library(dplyr)
library(tidyr)
library(lubridate)

# Laden van opgeschoonde data
load("Data/clean_data.RData")

# 1. Algemene samenvatting van de dataset
cat("ALGEMENE SAMENVATTING\n")
cat("Aantal wedstrijden:", nrow(matches), "\n")
cat("Aantal unieke seizoenen:", length(unique(matches$season)), "\n")
cat("Aantal competities:", length(unique(matches$league_name)), "\n")
cat("Aantal teams:", length(unique(c(matches$home_team_api_id, matches$away_team_api_id))), "\n\n")

# 2. Basisstatistieken over doelpunten
match_stats <- matches %>%
  summarise(
    gemiddeld_totaal_doelpunten = mean(total_goals, na.rm = TRUE),
    mediaan_totaal_doelpunten = median(total_goals, na.rm = TRUE),
    gemiddeld_thuisdoelpunten = mean(home_team_goal, na.rm = TRUE),
    gemiddeld_uitdoelpunten = mean(away_team_goal, na.rm = TRUE),
    gemiddeld_doelpuntverschil = mean(goal_difference, na.rm = TRUE)
  )

cat("BASISSTATISTIEKEN VAN WEDSTRIJDEN\n")
cat(sprintf("Gemiddeld totaal doelpunten: %.2f\n", match_stats$gemiddeld_totaal_doelpunten))
cat(sprintf("Mediaan totaal doelpunten: %.2f\n", match_stats$mediaan_totaal_doelpunten))
cat(sprintf("Gemiddeld thuisdoelpunten: %.2f\n", match_stats$gemiddeld_thuisdoelpunten))
cat(sprintf("Gemiddeld uitdoelpunten: %.2f\n", match_stats$gemiddeld_uitdoelpunten))
cat(sprintf("Gemiddeld doelpuntverschil: %.2f\n", match_stats$gemiddeld_doelpuntverschil))

# 3. Verdeling van wedstrijdresultaten
result_distribution <- matches %>%
  group_by(match_result) %>%
  summarise(aantal = n()) %>%
  mutate(percentage = round(aantal / sum(aantal) * 100, 2))

cat("\nVERDELING VAN WEDSTRIJDRESULTATEN\n")
print(result_distribution)

#4 Trends per seizoen en competitie
# Gemiddelde doelpunten per seizoen en competitie
trend_season_league <- matches %>%
  group_by(season, league_name) %>%
  summarise(
    gemiddeld_totaal_doelpunten = mean(total_goals, na.rm = TRUE),
    gemiddeld_thuisdoelpunten = mean(home_team_goal, na.rm = TRUE),
    gemiddeld_uitdoelpunten = mean(away_team_goal, na.rm = TRUE),
    aantal_wedstrijden = n(),
    .groups = "drop"
  ) %>%
  arrange(season)

cat("\nGEMIDDELDE DOELPUNTEN PER SEIZOEN EN COMPETITIE\n")
print(head(trend_season_league, 20))

# 5. Correlatie tussen teamstijl en prestaties
team_play_corr <- matches %>%
  select(
    avg_buildUpSpeed, avg_buildUpPassing, avg_chanceCreationPassing,
    avg_chanceCreationShooting, avg_defencePressure, avg_defenceAggression,
    total_goals
  ) %>%
  drop_na() %>%
  cor(use = "complete.obs")

cat("\nCORRELATIE TUSSEN TEAMATTRIBUTES EN DOELPUNTEN\n")
print(round(team_play_corr, 2))

# 6. Thuis- versus uitprestaties
home_vs_away <- matches %>%
  summarise(
    gemiddeld_thuisdoelpunten = mean(home_team_goal, na.rm = TRUE),
    gemiddeld_uitdoelpunten = mean(away_team_goal, na.rm = TRUE),
    thuiswin_percentage = mean(match_result == "Home Win") * 100,
    uitwin_percentage = mean(match_result == "Away Win") * 100,
    gelijk_percentage = mean(match_result == "Draw") * 100
  )

cat("\nTHUIS- VERSUS UITPRESTATIES\n")
print(round(home_vs_away, 2))

# 7. Competitieanalyse
# Analyse per competitie: gemiddelde doelpunten, winstpercentages, enz
league_analysis <- matches %>%
  group_by(league_name) %>%
  summarise(
    gemiddeld_doelpunten = mean(total_goals, na.rm = TRUE),
    thuiswinst = mean(match_result == "Home Win") * 100,
    uitwinst = mean(match_result == "Away Win") * 100,
    gelijkspel = mean(match_result == "Draw") * 100,
    wedstrijden = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(gemiddeld_doelpunten))

cat("\nCOMPETITIEANALYSE (TOP 10 OP GEMIDDELDE DOELPUNTEN\n")
print(head(league_analysis, 10))

# 8. Correlatiematrix tussen doelpuntgerelateerde variabelen
corr_data <- matches %>%
  select(home_team_goal, away_team_goal, total_goals, goal_difference) %>%
  drop_na()

cor_matrix <- cor(corr_data)
print(round(cor_matrix, 2))


