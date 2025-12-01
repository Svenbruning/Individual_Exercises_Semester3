
count_vowels <- function(text) {
  # Error als de input geen enkele character string is
  if (!is.character(text) || length(text) != 1) {
    stop("Input must be a single string")
  }
  
  # Als de string leeg is return 0
  if (nchar(text) == 0) {
    return(0)
  }
  
  # lijst met klinkers
  vowels <- c("a", "e", "i", "o", "u")
  
  # tekst naar kleine letters voor hoofdletters
  lower_text <- tolower(text)
  
  # splitsen in individuele characters
  chars <- unlist(strsplit(lower_text, ""))
  
  # tellen hoeveel characters in de lijst 'vowels' zitten
  sum(chars %in% vowels)
}
