library(readxl)
library(data.table)
library(purrr)
library(dplyr)
library(ggplot2)
library(jsonlite)

# read data (manually created...)
location <- ""
location <- "r/"

clean_vocab <- function(vocab) {
  #' clean vocab data
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

get_stats <- function(chars, n = 5000, view = FALSE) {
  #' get statistics on vocab learned
  #' @example get_stats(chars)

  stats <- clean_stats() %>%
    select(-known) %>%
    left_join(chars[, c("v", "N", "known")], by = "v") %>%
    mutate(known = ifelse(is.na(known), 0, known))

  stats[stats$known == 0 & !is.na(stats$N), "known"] <- 1
  stats <- stats[1:n, ]

  unknown_n <- stats %>%
    filter(known == 0) %>%
    nrow()
  pct_known <- stats %>%
    filter(known == 1) %>%
    nrow() %>%
    `/`(n) %>%
    `*`(100) %>%
    round()

  cat(
    pct_known,
    "% of top ",
    n,
    " most common characters learned: ",
    unknown_n,
    " characters left to learn.\n"
  )

  if (view) {
    stats %>%
      filter(!is.na(N)) %>%
      filter(known == 0) %>%
      View()
  }

  return(unknown_n)
}

chars <- as.data.table(read.csv(paste0(location, "vocab.csv"))) %>%
  clean_vocab() %>%
  guess_translations() %>%
  get_translations()

# print interesting statistics
# what vocab do I still have left to learn?
if (location == "r/") {
  file_history <- fromJSON("file_history.json")

  all_commits <- data.frame(
    commit = unlist(file_history$commit, use.names = FALSE),
    date = unlist(file_history$date, use.names = FALSE),
    content = unlist(file_history$content, use.names = FALSE),
    stringsAsFactors = FALSE
  ) %>%
    arrange(date)

  commit_history <- read.csv("commit_history.csv")
  if (!nrow(commit_history)) {
    unknown_5000 <- c()
    unknown_3000 <- c()
    dates <- c()
    sha <- c()
  } else {
    unknown_5000 <- commit_history$unknown_5000
    unknown_3000 <- commit_history$unknown_3000
    dates <- commit_history$dates
    sha <- commit_history$commit
  }

  for (sha in all_commits$commit) {
    if (sha %in% commit_history$commit) {
      next
    }
    print(sha)
    commit_stats <- all_commits %>%
      filter(commit == sha)
    commit_table <- read.csv(
      text = as.character(commit_stats$content[[1]]),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ) %>%
      as.data.table() %>%
      select(1:3)
    if ("Name" %in% names(commit_table)) {
      names(commit_table) <- c("Chinese", "Pinyin", "English")
    }
    cat(nrow(commit_table), " words in this commit.\n")
    commit_chars <- commit_table %>%
      clean_vocab() %>%
      guess_translations() %>%
      get_translations()
    cat(nrow(commit_chars), " characters in this commit.\n")

    dates <- c(dates, commit_stats$date)
    unknown_5000 <- c(unknown_5000, get_stats(commit_chars, 5000))
    unknown_3000 <- c(unknown_3000, get_stats(commit_chars, 3000))
    sha <- c(sha, commit_stats$commit)
  }

  df <- data.frame(dates, unknown_5000, unknown_3000)
  write.csv(df, "commit_history.csv", row.names = FALSE)
  g <- df %>%
    mutate(
      learned_5000 = 100 - (unknown_5000 / 5000) * 100,
      learned_3000 = 100 - (unknown_3000 / 3000) * 100,
      dates = as.POSIXct(dates / 1000, origin = "1970-01-01", tz = "UTC")
    ) %>%
    mutate(
      month = format(dates, "%Y-%m")
    ) %>%
    group_by(month) %>%
    summarize(
      percent_5000 = mean(learned_5000),
      percent_3000 = mean(learned_3000)
    ) %>%
    ggplot(aes(x = month)) +
    geom_point(aes(y = percent_5000), color = "blue") +
    geom_point(aes(y = percent_3000), color = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(g)
}

# make the positions into network connection format-----------------------------

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

# write.csv(chars, "r/chars.csv")
