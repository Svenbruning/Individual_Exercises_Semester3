random_character <- function() {
  sample(letters, 1)
}

print_sequence <- function(length) {
  for (i in 1:20) { 
    cat(random_character())
    Sys.sleep(0.25)
  } 
}

print_sequence()
