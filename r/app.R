#
# This is a Shiny web application for displaying semantic connections between Chinese vocab words from a list. You can run the application by clicking
# the 'Run App' button above.
#
# Hooray for this person!!!
#
#   https://kateto.net/network-visualization
#

library(shiny)
library(plotly)
library(ggnetwork)

# source("r/link.R")
source("link.R")

face="chinese"

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Chinese Vocab!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("root",
                           "Select the word to focus on",
                           choices=dt$v),
            sliderInput("recursions",
                        "Levels of detail",
                        1,5,value=1),
            # actionButton("flip","Flip cards"),
            width="10%"
        ),

        # Show a plot of the chosen subset
        mainPanel(
           plotlyOutput("network", width="100%")
        )
    )
)

# Define server logic required 
server <- function(input, output) {
    
    # f <- reactiveValues(face="chinese")

    # observeEvent(input$flip, 
    #               f$face<-ifelse(f$face=="chinese",
    #                      "english",
    #                      "chinese"))

    output$network <- renderPlotly({
        
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
                
                v <- nodes$chinese %>% strsplit("") %>% unlist() %>% unique()
                
                
                for (i in v) {
                    root = c(root, unlist(unname(dt[v==i,"pos"])))
                }
                
                root <- unique(root)
                
                get_nodes(root,counter+1,stop)
            }
        }
        
        root2 <- get_nodes(root, 1, stop=input$recursions)
        
        # get all edges containing the phrases of interest
        edges <- graph[f %in% root2 & t %in% root2,]
        
        # get all unique nodes that are in the edges of interest
        nodes <- vocab[vocab$id %in% root2,]
        
        # create color
        nodes$col <- ifelse((nodes$id %in% root),
                        "Root",
                        "Other")
        
        net <- graph_from_data_frame(d=edges,
                                     vertices=nodes,
                                     directed=F)
        net <- graph.data.frame(edges)
        
        V(net)$chinese <- nodes$chinese
        V(net)$english <- nodes$english
        V(net)$col <- nodes$col
        
        # get the vertices (nodes) from the net 
        set.seed(123)
        p <- ggplot(ggnetwork(net),
                  aes(x = x, y = y, xend = xend, yend = yend)) +
           geom_edges(color="grey60",size=.1) +
           geom_nodes(aes(color=col),size=18) +
           geom_nodetext(aes(label=chinese)) +
           ggtitle("") +
           theme_blank()
        p %>%
            ggplotly(tooltip=V(net)$english) %>%
            layout(autosize=F,
                   width=1500,
                   height=1500)
        

        # if (f$face=="chinese") {
        #     plot(net,
        #          layout=l,
        #          vertex.label=V(net)$chinese
        #     )
        # } else {
        #     plot(net,
        #          layout=l,
        #          vertex.label=V(net)$english
        #     )
        # }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
