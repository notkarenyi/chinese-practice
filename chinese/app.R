# Use a database of sentences and one of vocabulary to randomly generate a prompt.

library(shiny)
library(tidyverse)

####### READ DATA
sentences <- read.csv("sentences.csv")
sentences <- sentences[[1]]

#???: how to read in chinese characters and pinyin
vocab <- read.csv("vocab.csv")
View(vocab)

vocab <- read.csv("C:/Users/notka/Documents/GitHub/chinese-practice/chinese/vocab.csv")

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
