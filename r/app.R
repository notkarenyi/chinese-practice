# This is a Shiny web application for displaying semantic connections between Chinese vocab words from a list. Run the application by clicking the 'Run App' button above.
#
# link.R produces 3 data frames:
#  graph: position pairs containing all unique combinations of character phrases
#  vocab: vocab phrases to learn
#  chars: characters (roots) associated with the phrases

# setup-------------------------------------------------------------------------

library(shiny)
library(shinybrowser)
library(plotly)
library(ggnetwork)
library(igraph)

source("link.R")
# graph <- read.csv("graph.csv")
text <- readLines("text.txt", encoding="UTF-8")

get_nodes <- function(root,counter,stop,nodes=data.frame()) {
    #' Using a "seed" or "root" word, select nodes recursively, 
    #' or "prune" a mini-tree from the mother vocabulary knowledge graph
    #' 
    #' @param root string A Chinese character from user input
    #' @param counter int Keeps track of which iteration we're on
    #' @param stop int How many iterations to run before stopping
    #' 
    #' @return nodes data.frame All relevant nodes, pruned from the knowledge graph
    
    # get all unique nodes that are in the edges of interest
    new <- vocab[grep(root,vocab$chinese),]
    new$colors <- as.character(counter)
    nodes <- bind_rows(nodes,new) %>% distinct(chinese,.keep_all = TRUE)

    if (counter==stop) {
      
        # if we are over the max number of nodes, randomly select some to display
        # keep all roots, though
        if (nrow(nodes)>70) {
            orig = nodes[nodes$colors==1,]
            nodes <- bind_rows(orig,
                               sample_n(nodes[nodes$colors!=1,],70-nrow(orig)))
        }
        return(nodes)
        
    } else {
        
        # get all individual characters from the nodes of interest 
        # (this includes previous root by definition)
        root <- nodes$v %>% unlist() %>% unique() %>% paste0(collapse='|')
        
        get_nodes(root,counter+1,stop,nodes=nodes)
    }
}

# define UI---------------------------------------------------------------------

ui <- fluidPage(
    shinybrowser::detect(),
  
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "index.css")
    ),

    titlePanel(textOutput("word")),

    sidebarLayout(
        sidebarPanel(
            # inputs------------------------------------------------------------

            selectizeInput("root",
                           "Type or select a word to focus on",
                           choices=chars$v,
                           selected='中'),
            
            p("or if you prefer:"),
            
            actionButton("randomize",
                         "Randomize"),
            
            hr(),
            tags$b("Options"),
            checkboxInput("pinyin",
                          "View all pinyin",
                          value=FALSE,
                          width='100%'),
            
            hr(),
            tags$b("Instructions"),
            p("Hover over or tap a word for the English translation."),
            p("Click a word to center it in the graph (desktop only)."),
            
            # information accordion---------------------------------------------
            hr(),
            
            tags$details(tags$summary(tags$a("About this app (expand)")),
                         tags$br(),
                         tags$p(text[1],
                                tags$a(href=text[2],text[3]),
                                text[4]),
                         tags$p(text[5]),
                         tags$p(text[6],
                                tags$a(href=text[7],text[8]), 
                                text[9]),
                         tags$p(text[10])),
            
            hr(),
            p("Created by Karen Yi"),
            a("View on Github",href="https://github.com/notkarenyi/chinese-practice"),
            width=2
        ),

        # show plot of the chosen subset
        mainPanel(
           plotlyOutput("network")
        )
    )
)

# define server logic-----------------------------------------------------------

server <- function(input, output) {
    
    # set dashboard title to reflect the most likely English meaning of the root word
    output$word <- reactive({
      meaning = chars$most_likely[chars$v==input$root]
      if (is.na(meaning)) {
        # if unavailable, use chinese word directly
        meaning = input$root
      }
      paste0("chinese word net for: ", meaning)
    })
    
    output$network <- renderPlotly({
      
        # get all phrases with the root character
        # input = c()
        # input$root = '不'
        nodes <- get_nodes(input$root, counter=1, stop=2) %>% distinct()
        
        if (input$pinyin) {
          nodes$chinese <- paste0(nodes$chinese,'\n',nodes$pinyin)
        }
        
        # set base color for the root words (relevant later)
        nodes$colors[grep(input$root,nodes$chinese)] <- "1"
        nodes$name <- as.character(nodes$name)
        
        # get a list of all characters occurring in the set
        temp <- data.table(char = unique(unlist(nodes$v)))
        
        # for each character V, get all the IDs of the phrases containing V 
        temp <- mutate(temp, pos = map(temp$char, ~nodes$name[grep(.,nodes$chinese)]))

        # IF YOU DON'T FILTER OUT LISTS OF LENGTH 1 YOU GET BAD RESULTS WITH COMBN()
        # not sure why this happens but we need to do a second check of position length (# of occurrences)
        temp$l <- map(temp$pos,length)
        temp <- temp[l>1,]
        
        # find possible combinations between positions
        x <- map(temp$pos,combn,2,simplify=F)
        
        # format by extracting the first and second element of each combination, respectively
        f <- unlist(map(x, map, ~.[1]))
        t <- unlist(map(x, map, ~.[2]))
        
        edges <- as.data.table(data.frame(f,t)) %>% distinct()

        # format for ggplot
        g <- graph.data.frame(edges)
        net <- ggnetwork(g,layout=layout_with_kk(g))
        
        # add back information for labels etc
        net <- left_join(net,select(nodes,name,chinese,text,colors),by='name')
    
        # create gradient of colors based on number of levels of recursion
        cols <- c()
        for (i in 1:2) {
            cols <- c(cols,rgb(red=(140+100*i/2)/255, 
                               green=(140+100*i/2)/255, 
                               blue=1, 
                               alpha=1))
        }

        # adjust graph size based on number of nodes present
        size = round((800+nrow(nodes)**1.4)/100,0)*100

        p <- ggplot(net, aes(x=x, y=y, xend=xend, yend=yend, 
                             # label the tooltips ?
                             text=text,
                             # identify which point was clicked
                             key=chinese)) +
            geom_edges(color="grey60",
                       size=.1) +
            geom_nodes(aes(color=colors),size=18) +
            geom_nodetext(aes(label=chinese)) +
            scale_color_manual(values=cols, labels=as.character(1:2)) +
            theme_blank() +
            theme(legend.position="none") 
        
        p %>%
            # sets the specific order of tooltip variables (in this case 1)
            ggplotly(tooltip="text",
                     width=size,
                     height=size,
                     # ties plotly_click event to data
                     source="name") %>%
            layout(xaxis = list(fixedrange = T),
                   yaxis = list(fixedrange = T),
                   font = list(family = "sans serif"),
                   dragmode = F) %>%
            config(displayModeBar = F) 
    }) 
    
    # update graph when we change the root word via randomization
    observeEvent(input$randomize,{
        updateSelectizeInput(inputId="root",selected=sample(chars$v,1))
    })
    
    # update graph when we change the pinyin setting, without changing input
    observeEvent(input$pinyin,{
        updateSelectizeInput(inputId="root",selected=input$root)
    })

    # update graph whenever we click a node to explore more
    observeEvent(event_data("plotly_click",source="name"), {
      
        dt <- event_data("plotly_click",source="name")
        
        # RESTRICT FEATURE ON DESKTOP FOR NOW bc touch is for navigation on mobile
        if (!is.null(dt) & get_device()=="Desktop") {
            newWord <- strsplit(dt$key,"") %>% unlist()
            # make the new word NOT equal to the current root and PRESENT in the possible list
            newWord <- newWord[(newWord!=input$root) & (newWord %in% chars$v)]
            # random otherwise
            newWord <- newWord[sample(length(newWord))]
            # print(newWord)
            # make sure we didn't filter out all possible words
            if (length(newWord)) {
                updateSelectizeInput(inputId="root",selected=newWord)
            }
        } 
    })
}

# run app-----------------------------------------------------------------------
shinyApp(ui = ui, server = server)

