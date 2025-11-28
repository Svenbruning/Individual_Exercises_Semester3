authors <- read.csv("authors.csv")
books <- read.csv("books.csv")

# The Writer: Mia Morgan's only book
subset(books, author == "Mia Morgan")

# The Musician: A book from 1613 with the topic Music
subset(books, topic == "Music" & year == 1613)

# The Traveler: A book from 1775 with Lysandra Silverleaf or Elena Petrova as author
subset(books, (author == "Lysandra Silverleaf" | author == "Elena Petrova") & year == 1775)

#The Painter: A book published in either 1990 or 1992, between the 200 and 300 pages, with the topic Art.
subset(books, (year == 1990 | year == 1992) & pages >= 200 & pages <= 300 & topic == "Art")

#The Scientist: A book with "Quantum Mechanics" in the title
subset(books, grepl("Quantum Mechanics", title))

#The Teacher: A book with the topic of education published in the 1700s, The author hailed from the town of Zenthia
zenthia_authors <- subset(authors, hometown == "Zenthia")
subset(books, author %in% zenthia_authors$author & topic == "Education" & year >= 1700 & year < 1800)
