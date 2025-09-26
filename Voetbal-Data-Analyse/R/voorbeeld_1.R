library(DBI)
library(RSQLite)

# Verbinding maken met de database
connection <- dbConnect(SQLite(), "Data/database.sqlite")

# Overzicht van alle tabellen in de database
dbListTables(connection)

# Eerste rijen van de Match tabel bekijken
matches_raw <- dbReadTable(connection, "Match")
head(matches_raw[, c("match_api_id", "date", "season", 
                     "home_team_goal", "away_team_goal")])

