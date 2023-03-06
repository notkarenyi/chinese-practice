# Use a database of sentences and one of vocabulary to randomly generate a prompt.

library(shiny)
library(tidyverse)
library(readr)
library(topicmodels)
library(NLP)
library(openNLP)
library(data.table)
library(SnowballC)

####### READ DATA
sentences <- read.csv("sentences.csv")
independent <- sentences$independent
independent <- independent[independent!=""]
dependent <- sentences$dependent

vocab <- read_csv("vocab.csv")

sentAnnotator <- Maxent_Sent_Token_Annotator(language="en",probs=TRUE,model=NULL)
wordAnnotator <- Maxent_Word_Token_Annotator(language="en",probs=TRUE,model=NULL)
posAnnotator <- Maxent_POS_Tag_Annotator(language="en",probs=TRUE,model=NULL)

pos <- function(sentence) {
    results <- annotate(sentence,
                        posAnnotator,
                        annotate(sentence,
                                 wordAnnotator,
                                 annotate(sentence,
                                          sentAnnotator)))
    
    results <- results$features %>% unlist()
    results <- results[names(results)=="POS"] %>% unname()
    results <- chunk(results)
    return(results)
}

chunk <- function(x) {
    # print(x)
    # print(sum(grep("VBP",x)))
    if (sum(grep("VBP",x))) {
        return("verb")
    } else if (sum(grep("JJ",x))) {
        return("adjective")
    } else {
        return("noun")
    }
}

vocab <- as.data.table(vocab)
vocab <- vocab[,pos:=pos(english),by="chinese"]

parts <- c("noun","verb","adjective")

####### CLEAN DATA

vocab$english <- map(vocab$english, trimws)
# vocab$english <- map(vocab$english, ~gsub("â€™", ., "\'"))
# remove phrases
vocab <- vocab[nchar(english)<20,]
# vocab <- vocab[,english:=gsub("[,;(].*","",english),by="chinese"]

####### APP
ui <- fluidPage(
    h1("Chinese Practice"),
    verbatimTextOutput("prompt"),
    
    textInput("caption","Translate this sentence"),
    
    actionButton("submit", "Submit")
)

server <- function(input, output) {
    
    observeEvent(input$submit, {output$prompt <- renderText({
        # get a sentence template
        s <- independent[sample(1:length(independent),1)]
        if (sample(c(1,2),1)==1) {
            s <- paste0(dependent[sample(1:length(dependent),1)], ", ", s)
        }
        print(s)
        s <- unlist(strsplit(s, " "))
        
        # mad lib the words using the vocab list
        for (part in parts) {
            subset <- filter(vocab, pos==part)
            for (word in grep(part,s))
                s[word] <- sub(part,
                               subset$english[sample(1:nrow(subset),1)],
                               s[word])
        }
        
        # form the sentence
        s <- paste0(paste0(s,collapse = " "), ".")
    
        #capitalize first letter
        s <- paste0(toupper(substr(s,0,1)), substr(s,2,nchar(s)))
        
        #print
        s
    })})
}

shinyApp(ui, server)
