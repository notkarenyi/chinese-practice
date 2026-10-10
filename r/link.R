library(readxl)
library(data.table)
library(purrr)
library(dplyr)
library(ggplot2)
library(jsonlite)

location <- ""

clean_vocab <- function(vocab) {
  #' clean vocab data
  #' read data (manually created...)
  #' @example clean_vocab(vocab)

  vocab$name <- seq_len(nrow(vocab))
  names(vocab) <- tolower(names(vocab))

  # get a list of all characters occurring in the set
  vocab$v <- vocab$chinese %>% strsplit("")

  return(vocab)
}

get_chars_list <- function(vocab) {
  #' get a list of all characters occurring in the set
  #' @example get_chars_list(vocab)

  chars <- vocab$chinese %>%
    strsplit("") %>%
    unlist()

  chars <- as.data.table(table(chars))

  # get only those characters occurring more than once in the set
  # (ignore this if calculating statistics)
  if (location != "r/") {
    chars <- chars[N > 1, ]
    chars <- chars[order(N, decreasing = T), ]
  }

  names(chars) <- c("v", "N")

  return(chars)
}

guess_meaning <- function(x) {
  #' I am lazy to do NLP eg stemming, lemming
  table(strsplit(paste0(x, collapse = " "), " "))
}

most_likely <- function(x) {
  #' I will. merely get the most frequently occurring english word among the listed definitions
  r <- names(
    guess_meaning(x)[guess_meaning(x) == max(guess_meaning(x))]
  )
  if (length(r) > 1) {
    return("")
  } else {
    return(r)
  }
}

guess_translations <- function(vocab) {
  #' link vocab with parts of speech
  #' @example guess_translations(vocab)

  pos <- as.data.table(read.csv(paste0(location, "parts-of-speech.csv")))
  names(pos) <- tolower(names(pos))

  # get the corresponding vocab words in the set using each of these characters
  chars <- vocab %>%
    left_join(pos) %>%
    mutate(text = paste0(pinyin, "\n", english, "\n", part)) %>%
    get_chars_list() %>%
    mutate(pos = map(v, ~ grep(., vocab$chinese, fixed = TRUE))) %>%
    # get the corresponding english translations for each "root" word
    mutate(trans = map(pos, ~ vocab$english[.])) %>%
    mutate(most_likely = as.character(map(trans, most_likely)))

  return(chars)
}

clean_stats <- function() {
  #' clean stats data
  #' @example clean_stats(stats)

  stats <- read.csv(paste0(location, "common-chinese.csv")) %>% as.data.table()
  stats <- stats[
    1:5000,
    c("Character", "CHRmillion", "Pinyin", "English", "Known")
  ]
  names(stats) <- c("v", "chrpermil", "pinyin", "most_likely2", "known")

  return(stats)
}

get_translations <- function(chars) {
  #' get the corresponding english translations for each "root" word

  chars <- chars %>%
    left_join(clean_stats())

  # if the most likely translation (stats) is empty, use the second most likely translation (guess)
  chars[most_likely == "", "most_likely"] <- chars[most_likely == "", "most_likely2"]
  chars <- chars[, !c("most_likely2")]

  return(chars)
}

create_network <- function(chars) {
  #' create a network of characters and their corresponding vocab words
  #' make the positions into network connection format
  #' @example create_network(chars)

  # IF YOU DON'T FILTER OUT LISTS OF LENGTH 1 YOU GET BAD RESULTS WITH COMBN()
  # not sure why this happens but we need to do a second check of position length (# of occurrences)
  chars$l <- map(chars$pos, length)
  chars <- chars[l > 1, ]

  # find possible combinations between positions
  x <- map(chars$pos, combn, 2, simplify = F)

  # format by extracting the first and second element of each combination, respectively
  f <- unlist(map(x, map, ~ .[1]))
  t <- unlist(map(x, map, ~ .[2]))

  graph <- as.data.table(data.frame(f, t))
  graph <- distinct(graph)

  return(graph)
}

graph <- as.data.table(read.csv(paste0(location, "vocab.csv"))) %>%
  clean_vocab() %>%
  guess_translations() %>%
  get_translations() %>%
  create_network()
