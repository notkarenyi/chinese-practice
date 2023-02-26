# Use a database of sentences and one of vocabulary to randomly generate a prompt.

library(shiny)
library(tidyverse)
library(readr)
library(topicmodels)
library(NLP)
library(openNLP)
library(data.table)

####### READ DATA
sentences <- read.csv("sentences.csv")
sentences <- sentences[[1]]

vocab <- read_csv("chinese/vocab.csv")
View(vocab)


sentAnnotator <- Maxent_Sent_Token_Annotator(language="en",probs=TRUE,model=NULL)
wordAnnotator <- Maxent_Word_Token_Annotator(language="en",probs=TRUE,model=NULL)
posAnnotator <- Maxent_POS_Tag_Annotator(language="en",probs=TRUE,model=NULL)

sentence <- vocab$english
sentence <- map(sentence, ~paste0(toupper(substr(.,0,1)),
                                  substr(.,2,nchar(.))))

sentence <- paste0(sentence,collapse=". ")

results <- annotate(sentence,
                    posAnnotator,
                    annotate(sentence,
                             wordAnnotator,
                             annotate(sentence,
                                      sentAnnotator)))

words <- results[results$type=="word"]$features %>% unlist()
words <- words[names(words)=="POS"] %>% unname()
words[words=="."] <- ""
words <- data.frame(sentences = 1:length(words),words)

sentences <- results[results$type=="sentence"]$features %>% unlist()
sentences <- data.frame(sentences, name = names(sentences)) %>%
    filter(name!="prob") %>%
    mutate(sep = name=="constituents1") %>%
    mutate(sentences = sentences - 121) %>%
    as.data.table()
sentences <- sentences[,id:=cumsum(sep)]
sentences <- sentences[,!c("sep")]
sentences <- left_join(sentences,words) %>%
    filter(words!="")
sentences <- sentences[,.(list(words)),by="id"]

# words <- words[grep("[A-Z]",words)]

parts <- c("noun", "verb")

####### CLEAN DATA

vocab$english <- map(vocab$english, trimws)
vocab$english <- map(vocab$english, ~gsub("â€™", ., "\'"))

####### APP
ui <- fluidPage(
    h1("Chinese Practice"),
    verbatimTextOutput("prompt"),
    
    textInput("caption","Translate this sentence")
)

server <- function(input, output) {
    output$prompt <- renderText({ 
        s <- sentences[sample(1:length(sentences),1)]
        print(s)
        s <- strsplit(s, " ")[[1]]

        for (j in parts) {
            subset <- filter(vocab, speech==j)
            for (i in grep(j,s))
                s[i] <- sub(j,subset$english[sample(1:nrow(subset),1)],s[i])
        }
        
        #capitalize first letter
        s[0] <- toupper(s[0])
        
        #print
        s
    })
}

shinyApp(ui, server)
