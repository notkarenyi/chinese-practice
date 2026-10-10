source("link.R")

if (!grepl("r\\b", getwd())) {
    setwd("r")
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

# deduplicate vocab list
vocab <- read.csv("vocab.csv")
vocab <- vocab[!duplicated(vocab$Chinese), ]
write.csv(vocab, "vocab.csv", row.names = FALSE)

# print interesting statistics
# what vocab do I still have left to learn?
file_history <- fromJSON("file_history.json")

all_commits <- data.frame(
    commit = unlist(file_history$commit, use.names = FALSE),
    date = unlist(file_history$date, use.names = FALSE),
    content = unlist(file_history$content, use.names = FALSE),
    stringsAsFactors = FALSE
) %>%
    arrange(date)

all_comits <- all_commits %>%
    bind_rows(
        data.frame(
            commit = "latest",
            date = Sys.time(),
            content = read.csv("vocab.csv"),
            stringsAsFactors = FALSE
        )
    )

commit_history() <- read.csv("commit_history.csv")
if (nrow(commit_history)) {
    unknown_5000 <- commit_history$unknown_5000
    unknown_3000 <- commit_history$unknown_3000
    dates <- commit_history$dates
    commits <- commit_history$commit
} else {
    unknown_5000 <- c()
    unknown_3000 <- c()
    dates <- c()
    commits <- c()
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
    commits <- c(commits, sha)
}

commit_history <- data.frame(dates, unknown_5000, unknown_3000, commits)
write.csv(commit_history, "commit_history.csv", row.names = FALSE)
graph <- commit_history %>%
    mutate(
        learned_5000 = 100 - (unknown_5000 / 5000) * 100,
        learned_3000 = 100 - (unknown_3000 / 3000) * 100,
        dates = as.POSIXct(dates / 1000, origin = "1970-01-01", tz = "UTC")
    ) %>%
    mutate(
        month = as.Date(paste0(format(dates, "%Y-%m"), "-01"))
    ) %>%
    group_by(month) %>%
    summarize(
        percent_5000 = mean(learned_5000),
        percent_3000 = mean(learned_3000)
    ) %>%
    arrange(month)
months <- seq(
    min(graph$month),
    max(graph$month),
    by = "month"
)
g <- graph %>%
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
