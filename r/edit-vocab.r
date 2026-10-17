#' Edit vocab list
#' Create a graph of the number of characters learned over time
#' Created by Karen Yi on 2026-10-10

library(tidyverse)

if (!grepl("r\\b", getwd())) {
  setwd("r")
}

source("link.R")

get_stats <- function(chars, n = 5000, view = FALSE) {
  #' get statistics on vocab learned
  #' @example get_stats(chars)

  stats <- clean_stats() %>%
    rename(known_stats = known) %>%
    left_join(chars[, c("v", "N", "known")], by = "v") %>%
    mutate(known = known | known_stats) %>%
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
      filter(known_stats == 0, known == 0) %>%
      as.data.frame() %>%
      View()
  }

  return(unknown_n)
}

# deduplicate vocab list
vocab <- read.csv("vocab.csv")
vocab <- vocab[!duplicated(vocab$Chinese), ]
write.csv(vocab, "vocab.csv", row.names = FALSE, quote = FALSE)

# print interesting statistics
# what vocab do I still have left to learn?

get_commits <- function() {
  #' get all commits from git log
  #' @example get_commits()

  file_history <- fromJSON("../file_history.json")

  all_commits <- data.frame(
    commit = unlist(file_history$commit, use.names = FALSE),
    date = unlist(file_history$date, use.names = FALSE),
    content = unlist(file_history$content, use.names = FALSE),
    stringsAsFactors = FALSE
  ) %>%
    arrange(date)

  return(all_commits)
}

get_chars <- function(commit_table) {
  #' get the corresponding english translations for each "root" word
  #' @example get_chars(commit_table)

  commit_chars <- commit_table %>%
    clean_vocab() %>%
    guess_translations() %>%
    get_translations()

  return(commit_chars)
}

process_commits <- function(all_commits) {
  #' process commits and update commit history
  #' @example process_commits()

  if (!file.exists("../commit_history.csv")) {
    cat("No existing commit history found. Creating new commit history.\n")
    unknown_5000 <- c()
    unknown_3000 <- c()
    dates <- c()
    commits <- c()
    commit_history <- data.frame()
  } else {
    commit_history <- read.csv("../commit_history.csv")
    cat("Found existing commit history with", nrow(commit_history), "rows.\n")
    unknown_5000 <- commit_history$unknown_5000
    unknown_3000 <- commit_history$unknown_3000
    dates <- commit_history$dates
    commits <- commit_history$commit
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
    commit_chars <- get_chars(commit_table)
    cat(nrow(commit_chars), " characters in this commit.\n")

    date <- as.character(
      as.POSIXct(
        commit_stats$date,
        format = "%Y-%m-%dT%H:%M:%OSZ",
        tz = "UTC"
      )
    )
    dates <- c(dates, date)
    view <- ifelse(sha == all_commits$commit[nrow(all_commits)], TRUE, FALSE)
    unknown_5000 <- c(unknown_5000, get_stats(commit_chars, 5000, view = view))
    unknown_3000 <- c(unknown_3000, get_stats(commit_chars, 3000, view = view))
    commits <- c(commits, sha)

    commit_history <- data.frame(dates, unknown_5000, unknown_3000, commits)
    write.csv(
      filter(commit_history, commits != "latest"),
      "../commit_history.csv",
      row.names = FALSE
    )
  }

  return(commit_history)
}

get_stats(get_chars(read.csv("vocab.csv")), 3000, view = TRUE)
today_commit <- data.frame(
  commit = c("latest"),
  date = c(format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%OSZ")),
  content = paste(readLines("vocab.csv", warn = FALSE), collapse = "\n"),
  stringsAsFactors = c(FALSE)
)
# today_commit <- data.frame()
all_commits <- bind_rows(
  today_commit,
  get_commits()
)
commit_history <- process_commits(all_commits)

graph_data <- commit_history %>%
  mutate(
    learned_5000 = 100 - (unknown_5000 / 5000) * 100,
    learned_3000 = 100 - (unknown_3000 / 3000) * 100,
    month = as.Date(paste0(format(as.Date(dates), "%Y-%m"), "-01"))
  ) %>%
  group_by(month) %>%
  summarize(
    percent_5000 = mean(learned_5000),
    percent_3000 = mean(learned_3000)
  ) %>%
  arrange(month)

months <- seq(
  min(graph_data$month),
  max(graph_data$month),
  by = "month"
)

g <- graph_data %>%
  ggplot(aes(x = month)) +
  geom_smooth(aes(y = percent_5000, color = "Top 5000", group = 1), se = FALSE) +
  geom_point(aes(y = percent_5000, color = "Top 5000", group = 1)) +
  geom_smooth(aes(y = percent_3000, color = "Top 3000", group = 2), se = FALSE) +
  geom_point(aes(y = percent_3000, color = "Top 3000", group = 2)) +
  theme_minimal() +
  # scale_x_discrete(breaks = months) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Percent of Common Chinese Characters Learned",
    x = "Month",
    y = "Percent Learned"
  )
print(g)
