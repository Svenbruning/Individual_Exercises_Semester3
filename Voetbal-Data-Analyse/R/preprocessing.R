# preprocessing.R

# Laden van benodigde packages
library(dplyr)
library(lubridate)

# Laden van ruwe data
load("Data/raw_data.RData")

# 1. Selectie van kernvariabelen
matches <- matches_raw %>%
  select(match_api_id, country_id, league_id, season, stage, date,
         home_team_api_id, away_team_api_id,
         home_team_goal, away_team_goal)

player_attr <- player_attr_raw %>%
  select(player_api_id, player_fifa_api_id, date,
         overall_rating, potential, dribbling, finishing)

team_attr <- team_attr_raw %>%
  select(team_api_id, date,
         buildUpPlaySpeed, buildUpPlayPassing,
         chanceCreationPassing, chanceCreationShooting,
         defencePressure, defenceAggression)

# 2. Veilige conversie naar numeric
safe_numeric <- function(x) {
  x_char <- as.character(x)
  suppressWarnings(as.numeric(x_char))
}

num_cols_matches <- c("home_team_goal", "away_team_goal")
matches[num_cols_matches] <- lapply(matches[num_cols_matches], safe_numeric)

num_cols_player <- c("overall_rating", "potential", "finishing", "dribbling")
player_attr[num_cols_player] <- lapply(player_attr[num_cols_player], safe_numeric)

num_cols_team <- c("buildUpPlaySpeed", "buildUpPlayPassing", "chanceCreationPassing",
                   "chanceCreationShooting", "defencePressure", "defenceAggression")
team_attr[num_cols_team] <- lapply(team_attr[num_cols_team], safe_numeric)

# 3. Opschonen van missende waarden
impute_median <- function(x) {
  if (is.numeric(x)) x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}

matches <- as.data.frame(lapply(matches, impute_median))
player_attr <- as.data.frame(lapply(player_attr, impute_median))
team_attr <- as.data.frame(lapply(team_attr, impute_median))

# 4. Conversie van datatypes
matches$date <- as.Date(matches$date)
player_attr$date <- as.Date(player_attr$date)
team_attr$date <- as.Date(team_attr$date)

# 5. Feature Engineering
matches <- matches %>%
  mutate(
    goal_difference = home_team_goal - away_team_goal,
    match_result = case_when(
      goal_difference > 0 ~ "Home Win",
      goal_difference < 0 ~ "Away Win",
      TRUE ~ "Draw"
    ),
    total_goals = home_team_goal + away_team_goal,
  )

# 6. Competitienamen koppelen
matches <- matches %>%
  left_join(leagues_raw %>% select(id, name), by = c("league_id" = "id")) %>%
  rename(league_name = name)

# 7. Samenvattende statistieken
player_summary <- player_attr %>%
  group_by(player_api_id) %>%
  summarise(
    avg_rating = mean(overall_rating, na.rm = TRUE),
    avg_potential = mean(potential, na.rm = TRUE),
    avg_finishing = mean(finishing, na.rm = TRUE),
    avg_dribbling = mean(dribbling, na.rm = TRUE)
  )

team_summary <- team_attr %>%
  group_by(team_api_id) %>%
  summarise(
    avg_buildUpSpeed = mean(buildUpPlaySpeed, na.rm = TRUE),
    avg_buildUpPassing = mean(buildUpPlayPassing, na.rm = TRUE),
    avg_chanceCreationPassing = mean(chanceCreationPassing, na.rm = TRUE),
    avg_chanceCreationShooting = mean(chanceCreationShooting, na.rm = TRUE),
    avg_defencePressure = mean(defencePressure, na.rm = TRUE),
    avg_defenceAggression = mean(defenceAggression, na.rm = TRUE)
  )

# 8. Teamattributen koppelen aan wedstrijden.
matches <- matches %>% left_join(team_summary, by = c("home_team_api_id" = "team_api_id"))

# 9. Opslaan van schone datasets
save(matches, player_attr, team_attr, player_summary, team_summary,
     countries_raw, leagues_raw, teams_raw, players_raw,
     file = "Data/clean_data.RData")

