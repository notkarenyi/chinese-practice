#
# This is a Shiny web application. You can run the application by clicking
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
        
        # get all edges containing the phrases of interest
        edges <- graph[f %in% root|t %in% root,]
        
        # get all unique nodes that are in the edges of interest
        n <- unique(c(unlist(edges$f),unlist(edges$t)))
        nodes <- vocab[vocab$id %in% n,]
        
        net <- graph_from_data_frame(d=edges,
                                     vertices=nodes)
        plot(net,
             edge.arrow.size=.4,
             # vertex.label=NA,
             vertex.size=200,
             vertex.shape="none",
             vertex.label=V(net)$chinese)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
