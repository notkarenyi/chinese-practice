#
# This is a Shiny web application for displaying semantic connections between Chinese vocab words from a list. You can run the application by clicking
# the 'Run App' button above.
#
# Hooray for these people!!!
#
#   https://kateto.net/network-visualization
#   https://minimaxir.com/notebooks/interactive-network/
#

# setup-------------------------------------------------------------------------

library(shiny)
library(plotly)
library(ggnetwork)

source("link.R")
# graph <- read.csv("graph.csv")
text <- readLines("text.txt", encoding="UTF-8")

get_nodes <- function(root,counter,stop) {
    #' a recursive selection of nodes
    
    if (counter>stop) {
        return(root)
    } else {
        
        # get all edges containing the phrases of interest
        edges <- graph[f %in% root & t %in% root,]
        
        # get all unique nodes that are in the edges of interest
        nodes <- vocab[vocab$name %in% root,]
        
        v <- nodes$chinese %>% strsplit("") %>% unlist() %>% unique()
        
        for (i in v) {
            root = c(root, unlist(unname(vocab_clean[v==i,"pos"])))
        }
        
        root <- unique(root)
        
        get_nodes(root,counter+1,stop)
    }
}

# define UI---------------------------------------------------------------------

ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "index.css")
    ),

    # Application title
    titlePanel("chinese word net"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("root",
                           "Type or select a word to focus on",
                           choices=vocab_clean$v),
            actionButton("randomize",
                         "Randomize"),
            hr(),
            sliderInput("recursions",
                        "Levels of detail",
                        1,5,value=1,
                        ticks=F),
            br(),
            p("Tip: Hover over or tap a word for the English translation."),
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

        # Show a plot of the chosen subset
        mainPanel(
           plotlyOutput("network")
        )
    )
)

# define server logic-----------------------------------------------------------

server <- function(input, output) {
    
    make_network <- function(root) {
        
        output$network <- renderPlotly({
            
            # Generate graph based on selection---------------------------------
            
            # get all phrases with the root character
            root_phrase <- unlist(unname(vocab_clean[v==root,"pos"]))
            root_node <- get_nodes(root_phrase, 1, stop=input$recursions)
            
            # get all edges containing the phrases of interest
            edges <- graph[f %in% root_node & t %in% root_node,]
            
            # get all unique nodes that are in the edges of interest
            nodes <- vocab[vocab$name %in% root_node,]
            
            # color the root words differently
            nodes$col <- ifelse((nodes$name %in% root_phrase),
                                "Root",
                                "Other")
            
            nodes$name <- as.character(nodes$name)
            
            # format for ggplot
            net <- ggnetwork(graph.data.frame(edges))
            
            # add back information for labels etc
            net <- left_join(net,select(nodes,name,chinese,text,col))
            
            # set seed such that the random node placement is the same every time
            # set.seed(123)
            
            p <- ggplot(net, aes(x = x, y = y, 
                                 xend = xend, yend = yend, text=text)) +
                geom_edges(color="grey60",size=.1) +
                geom_nodes(aes(color=col),size=18) +
                geom_nodetext(aes(label=chinese)) +
                scale_color_manual(values=c("white",rgb(red=200/255, 
                                                        green=200/255, 
                                                        blue=255/255, 
                                                        alpha=1)),
                                   labels=c("Other","Root")) +
                ggtitle(paste0("Words related to: ",
                               vocab_clean$most_likely[vocab_clean$v==root])) +
                theme_blank() +
                theme(legend.position="none")
            p %>%
                # sets the specific order of tooltip variables (in this case 1)
                ggplotly(tooltip="text",
                         width=800 + 6*nrow(nodes),
                         height=600 + 12*nrow(nodes)) %>%
                layout(xaxis = list(fixedrange = T), 
                       yaxis = list(fixedrange = T),
                       font = list(family = "sans serif"),
                       dragmode = F) %>%
                config(displayModeBar = F)
        })
    }
    
    observeEvent(input$root,{
        make_network(input$root)
    })
    
    observeEvent(input$randomize,{
        print(sample(vocab_clean$v,1))
        make_network(sample(vocab_clean$v,1))
    })
}

# run app-----------------------------------------------------------------------

shinyApp(ui = ui, server = server)
