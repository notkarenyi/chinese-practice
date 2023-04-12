# This is a Shiny web application for displaying semantic connections between Chinese vocab words from a list. Run the application by clicking the 'Run App' button above.
#
# link.R produces 3 data frames:
#  graph: position pairs containing all unique combinations of character phrases
#  vocab: vocab phrases to learn
#  chars: characters (roots) associated with the phrases
#
# Hooray for these people!!!
#
#   https://kateto.net/network-visualization
#   https://minimaxir.com/notebooks/interactive-network/

# setup-------------------------------------------------------------------------

library(shiny)
library(plotly)
library(ggnetwork)
library(igraph)

source("link.R")
# graph <- read.csv("graph.csv")
text <- readLines("text.txt", encoding="UTF-8")

get_nodes <- function(root,counter,stop,color,colors=c(),nodes=data.frame()) {
    #' Select nodes recursively
    
    # get all edges containing the phrases of interest
    edges <- graph[f %in% root & t %in% root,]
    
    # get all unique nodes that are in the edges of interest
    nodes <- bind_rows(nodes,vocab[vocab$name %in% root,]) %>% distinct()

    colors <- c(colors,rep(as.character(color),nrow(nodes)-length(colors)))

    if (counter==stop) {
        return(c(edges,nodes,data.frame(colors)))
    } else {
        
        # get all individual characters from the nodes of interest
        characters <- nodes$chinese %>% strsplit("") %>% unlist() %>% unique()
        
        # get all positions of phrases containing characters of interest
        for (character in characters) {
            root = c(root, unlist(unname(chars[v==character,"pos"])))
        }
        
        root <- unique(root)
        
        get_nodes(root,counter+1,stop,color+1,colors,nodes=nodes)
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
                           choices=chars$v),
            actionButton("randomize",
                         "Randomize"),
            hr(),
            sliderInput("recursions",
                        "Levels of detail",
                        1,4,value=1,
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
            
            stop=input$recursions + 1
            
            # get all phrases with the root character
            root_phrases <- unlist(unname(chars[v==root,"pos"]))
            graph_results <- get_nodes(root_phrases, counter=1, stop=stop, color=1)
            
            # deal with multiple output function
            edges <- data.frame(graph_results[c('f','t')])
            nodes <- data.frame(graph_results[!names(graph_results) %in% c('f','t','v')])
            nodes$colors[grep(root,nodes$chinese)] <- 1

            nodes$name <- as.character(nodes$name)
            
            # format for ggplot
            net <- ggnetwork(graph.data.frame(edges))
            
            # add back information for labels etc
            net <- left_join(net,select(nodes,name,chinese,text,colors))
            
            # set seed such that the random node placement is the same every time
            # set.seed(123)
            
            cols <- c()
            for (i in 1:stop) {
                cols <- c(cols,rgb(red=(140+100*i/stop)/255, green=(140+100*i/stop)/255, blue=1, alpha=1))
            }
                
            p <- ggplot(net, aes(x = x, y = y, 
                                 xend = xend, yend = yend, text=text)) +
                geom_edges(color="grey60",size=.1) +
                geom_nodes(aes(color=colors),size=18) +
                geom_nodetext(aes(label=chinese)) +
                scale_color_manual(values=cols,labels=as.character(1:stop)) +
                ggtitle(paste0("Words related to: ",
                               chars$most_likely[chars$v==root])) +
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
        print(sample(chars$v,1))
        make_network(sample(chars$v,1))
    })
}

# run app-----------------------------------------------------------------------

shinyApp(ui = ui, server = server)
