library(readxl)
library(data.table)
library(purrr)
library(dplyr)

# read data (manually created...)
# vocab <- as.data.table(read_xlsx("r/vocab.xlsx"))
vocab <- as.data.table(read_xlsx("vocab.xlsx"))
names(vocab) <- tolower(names(vocab))
vocab <- vocab[,text:=paste0(pinyin, " / ", english)]

# get a list of all characters occurring in the set
vocab$v <- vocab$chinese %>% strsplit("") 
v <- vocab$chinese %>% strsplit("") %>% unlist()
chars <- as.data.table(table(v))

# get only those characters occurring more than once in the set
chars <- chars[N>1,]
chars <- chars[order(N,decreasing=T),]

# get the corresponding vocab words in the set using each of these characters
chars <- mutate(chars, pos = map(v, ~grep(., vocab$chinese)))

# get the corresponding english translations for each "root" word
chars <- mutate(chars, trans = map(pos, ~vocab$english[.]))

guess_meaning <- function(x) {
  #' I am lazy to do NLP eg stemming, lemming
  table(strsplit(paste0(x,collapse=" "), " "))
}

most_likely <- function(x) {
  #' I will. merely get the most frequently occurring english word among the listed definitions
  r <- names(
    guess_meaning(x)[guess_meaning(x)==max(guess_meaning(x))]
  )
  if (length(r)>1) {
    return("Not yet translated")
  } else {
    return(r)
  }
}

chars <- mutate(chars, most_likely = map(trans, most_likely))

# make the positions into network connection format-----------------------------

# IF YOU DON'T FILTER OUT LISTS OF LENGTH 1 YOU GET BAD RESULTS WITH COMBN()
# not sure why this happens but we need to do a second check of position length (# of occurrences)
chars$l <- map(chars$pos,length)
chars <- chars[l>1,]

# find possible combinations between positions
x <- map(chars$pos,combn,2,simplify=F)

# format by extracting the first and second element of each combination, respectively
f <- unlist(map(x, map, ~.[1]))
t <- unlist(map(x, map, ~.[2]))

graph <- as.data.table(data.frame(f,t))
graph <- distinct(graph)

# what vocab do I still have left to learn?
# stats <- read_xlsx("r/vocab.xlsx",skip=2,sheet="Statistics") %>% as.data.table()
# stats <- stats[,c("Character","CHR/million")]
# names(stats) <- c("v","chrpermil")
# stats <- left_join(stats,chars)
# stats[is.na(N),][300:2000,]

# write.csv(graph, "r/graph.csv")
# write.csv(chars, "r/chars.csv")
