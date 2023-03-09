#
# This is a Shiny web application for displaying semantic connections between Chinese vocab words from a list. You can run the application by clicking
# the 'Run App' button above.
#
# Hooray for this person!!!
#
#   https://kateto.net/network-visualization
#

# setup-------------------------------------------------------------------------

library(shiny)
library(plotly)
library(ggnetwork)

source("link.R")

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
            root = c(root, unlist(unname(dt[v==i,"pos"])))
        }
        
        root <- unique(root)
        
        get_nodes(root,counter+1,stop)
    }
}

# define UI---------------------------------------------------------------------

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
                        1,5,value=1,
                        ticks=F),
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

    output$network <- renderPlotly({
        
        # Generate graph based on selection-------------------------------------
        
        # get all phrases with the root character
        root = unlist(unname(dt[v==input$root,"pos"]))
        root2 <- get_nodes(root, 1, stop=input$recursions)
        
        # get all edges containing the phrases of interest
        edges <- graph[f %in% root2 & t %in% root2,]
        
        # get all unique nodes that are in the edges of interest
        nodes <- vocab[vocab$name %in% root2,]
        
        # color the root words differently
        nodes$col <- ifelse((nodes$name %in% root),
                            "Root",
                            "Other")
        
        nodes$name <- as.character(nodes$name)
        
        # format for ggplot
        net <- ggnetwork(graph.data.frame(edges))
        
        # add back information for labels etc
        net <- left_join(net,select(nodes,name,chinese,text,col))

        # set seed such that the random node placement is the same every time
        set.seed(123)
        
        p <- ggplot(net, aes(x = x, y = y, xend = xend, yend = yend, text=text)) +
            geom_edges(color="grey60",size=.1) +
            geom_nodes(aes(color=col),size=18) +
            geom_nodetext(aes(label=chinese)) +
            scale_color_manual(values=c("white","skyblue"),
                               labels=c("Other","Root")) +
            ggtitle("") +
            theme_blank() +
            theme(legend.position="none",
                  text=element_text(family="Comic Sans"))
        p %>%
            # sets the specific order of tooltip variables (in this case 1)
            ggplotly(tooltip="text",
                     width=1200,
                     height=1500)
    })
}

# run app-----------------------------------------------------------------------

shinyApp(ui = ui, server = server)
