if (!grepl("r\\b", getwd())) {
    setwd("r")
}
v <- read.csv("vocab.csv")
v <- v[!duplicated(v$Chinese), ]
write.csv(v, "vocab.csv", row.names = FALSE)
