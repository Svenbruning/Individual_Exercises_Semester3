library(tidyverse)

tekst <- read_file("lyrics/beatles.txt")

# Zet alles om naar kleine letters
tekst <- str_to_lower(tekst)

# Haal leestekens weg zoals komma’s, haakjes en puntjes
tekst <- str_replace_all(tekst, "[[:punct:]]", " ")

# Vervang meerdere spaties en enters door één spatie
tekst <- str_replace_all(tekst, "\\s+", " ")

# Splits de tekst op in losse woorden
woorden <- str_split(tekst, " ")[[1]]

# Verwijder lege woorden
woorden <- woorden[woorden != ""]

# Maak een tabel van hoeveel keer elk woord voorkomt
tabel <- table(woorden)

# Zet de tabel om in een data frame
data <- as.data.frame(tabel)

# Hernoem de kolommen
colnames(data) <- c("woord", "aantal")

# Sorteer van hoog naar laag
data <- data[order(-data$aantal), ]

# Maak een grafiek met ALLE woorden (dus niets wordt weggefilterd)
grafiek <- ggplot(data, aes(x = reorder(woord, aantal), y = aantal, fill = woord)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Woordenfrequentie in 'Here Comes The Sun' - The Beatles",
       x = "Woord",
       y = "Aantal keer") +
  theme_minimal()

print(grafiek)

# Sla de grafiek op als afbeelding
ggsave("beatles.png", grafiek, width = 10, height = 8)
