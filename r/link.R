library(readxl)
library(data.table)
library(tidyverse)
library(igraph)

# read data (manually created...)
# vocab <- read_csv("r/vocab.csv")
vocab <- as.data.table(read_xlsx("vocab.xlsx"))
names(vocab) <- tolower(names(vocab))
vocab <- vocab[,text:=paste0(pinyin, " / ", english)]

# get a list of all characters occurring in the set
vocab$v <- vocab$chinese %>% strsplit("") 
v <- vocab$chinese %>% strsplit("") %>% unlist()
dt <- as.data.table(table(v))

# get only those characters occurring more than once in the set
dt <- dt[N>1,]
dt <- dt[order(N,decreasing=T),]

# get the corresponding vocab words in the set using each of these characters
dt <- mutate(dt, pos = map(v, ~grep(., vocab$chinese)))

# get the corresponding english translations for each "root" word
dt <- mutate(dt, trans = map(pos, ~vocab$english[.]))

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
    return(0)
  } else {
    return(r)
  }
}

dt <- mutate(dt, x = map(trans, most_likely))

# make the positions into network connection format-----------------------------

# IF YOU DON'T FILTER OUT LISTS OF LENGTH 1 YOU GET BAD RESULTS WITH COMBN()
# not sure why this happens but we need to do a second check of position length (# of occurrences)
dt$l <- map(dt$pos,length)
dt <- dt[l>1,]

# find possible combinations between positions
x <- map(dt$pos,combn,2,simplify=F)

# format by extracting the first and second element of each combination, respectively
f <- unlist(map(x, map, ~.[1]))
t <- unlist(map(x, map, ~.[2]))

graph <- as.data.table(data.frame(f,t))
graph <- distinct(graph)
