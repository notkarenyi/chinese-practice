library(readxl)
library(data.table)
library(purrr)
library(dplyr)

# read data (manually created...)
location <- ""
# location <- "r/"
pos <- as.data.table(read_xlsx(paste0(location,"vocab.xlsx"),sheet='Common Words'))
vocab <- as.data.table(read_xlsx(paste0(location,"vocab.xlsx")))
vocab <- left_join(vocab,pos)
names(vocab) <- tolower(names(vocab))
vocab <- vocab[,text:=paste0(pinyin, " / ", english, ' / ', part)]

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
    return("")
  } else {
    return(r)
  }
}

chars <- mutate(chars, most_likely = as.character(map(trans, most_likely)))

# what vocab do I still have left to learn?
stats <- read_xlsx(paste0(location,"vocab.xlsx"),sheet="Common Chinese") %>% as.data.table()
# remove the traditional characters towards the bottom of the list
stats <- stats[1:4345,c("Character","CHR/million","Pinyin","English")]
names(stats) <- c("v","chrpermil","pinyin","most_likely2")
chars <- left_join(chars,stats) # get all character translations

# print interesting statistics
# stats <- left_join(stats,chars)
# print(paste0(sum(is.na(stats$N)), " characters left to learn. ", round(sum(!is.na(stats$N))/nrow(stats)*100), "% of top 4345 most common characters learned"))
# stats <- stats[is.na(stats$N)]

chars[most_likely=="","most_likely"] <- chars[most_likely=="","most_likely2"]
chars <- chars[,!c("most_likely2")]

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

# write.csv(chars, "r/chars.csv")
