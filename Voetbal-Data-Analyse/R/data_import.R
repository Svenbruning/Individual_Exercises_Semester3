# data_import.R
# Verbinden met SQLite database en de tabellen inlezen
library(DBI)
library(RSQLite)

# Verbinding maken met de database
connection <- dbConnect(SQLite(), "Data/database.sqlite")

dbListTables(connection)

# Tabellen inlezen
countries_raw <- dbReadTable(connection, "Country")
leagues_raw <- dbReadTable(connection, "League")
matches_raw <- dbReadTable(connection, "Match")
players_raw <- dbReadTable(connection, "Player")
player_attr_raw <- dbReadTable(connection, "Player_Attributes")
teams_raw <- dbReadTable(connection, "Team")
team_attr_raw <- dbReadTable(connection, "Team_Attributes")

names(matches_raw)
# Data opslaan als RData
save(countries_raw, leagues_raw, matches_raw, players_raw, player_attr_raw, teams_raw, team_attr_raw, file = "Data/raw_data.RData")

