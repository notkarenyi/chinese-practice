#
# This is a Shiny web application for displaying semantic connections between Chinese vocab words from a list. You can run the application by clicking
# the 'Run App' button above.
#
# Hooray for this person!!!
#
#   https://kateto.net/network-visualization
#

library(shiny)

# source("r/link.R")
source("link.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Chinese Vocab!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("root",
                           "Select the word to focus on",
                           choices=dt$v)
        ),

        # Show a plot of the chosen subset
        mainPanel(
           plotOutput("network")
        )
    )
)

# Define server logic required 
server <- function(input, output) {

    output$network <- renderPlot({
        
        # generate graph based on selection
        
        # get all phrases with the root character
        root = unlist(unname(dt[v==input$root,"pos"]))
        
        get_nodes <- function(root,counter,stop) {
            #' a recursive selection of nodes
            
            if (counter>stop) {
                return(root)
            } else {
                
                # get all edges containing the phrases of interest
                edges <- graph[f %in% root & t %in% root,]
                
                # get all unique nodes that are in the edges of interest
                nodes <- vocab[vocab$id %in% root,]
                
                # create color
                # why doesn't this work?
                nodes$col <- ifelse((nodes$id %in% root),
                                    "brickred",
                                    "purple")
                
                v <- nodes$chinese %>% strsplit("") %>% unlist() %>% unique()
                
                
                for (i in v) {
                    root = c(root, unlist(unname(dt[v==i,"pos"])))
                }
                
                root <- unique(root)
                
                get_nodes(root,counter+1,stop)
            }
        }
        
        root <- get_nodes(root, 0, stop=0)
        
        # get all edges containing the phrases of interest
        edges <- graph[f %in% root & t %in% root,]
        
        # get all unique nodes that are in the edges of interest
        nodes <- vocab[vocab$id %in% root,]
        
        # create color
        nodes$col <- ifelse((nodes$id %in% root),
                        "brickred",
                        "purple")
        
        net <- graph_from_data_frame(d=edges,
                                     vertices=nodes,
                                     directed=F)
        plot(net,
             edge.arrow.size=.4,
             # vertex.label=NA,
             vertex.size=20,
             vertex.color=V(net)$col,
             # vertex.shape="none",
             bbox=c(1000,1000),
             margin=0,
             layout=layout_with_kk(net),
             # get the vertices (nodes) from the net 
             vertex.label=V(net)$chinese)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
