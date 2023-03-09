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
    titlePanel("chinese word net"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("root",
                           "Type or select a word to focus on",
                           choices=dt$v),
            sliderInput("recursions",
                        "Levels of detail",
                        1,5,value=1,
                        ticks=F),
            actionButton("randomize",
                         "Randomize (IN DEVELOPMENT)"),
            br(),
            p("Tip: Hover over a word for the English translation."),
            p("___"),
            h4("Background"),
            p("The Chinese language provides interesting opportunities for linguistic analysis. There are two semantic units: within characters, we have 偏旁部首 or 'radicals' that provide clues to the meaning; and the characters themselves are reused in phrases with related meanings. A common question for a native speaker to ask when learning a new word is: '__ 是什么 __?', meaning 'what phrases is this character found in?'"),
            p("For example, the two-character phrase 编程 is composed of 编, a word used in phrases such as 编故事 or 编织 that mean more or less 'to weave', and 程, a word used in phrases such as 工程师 that relate to engineering. 编程 means 'to program/code'."),
            p("This common question recognizes the finding from educational psychology that organizing new knowledge into existing schemas is important for improving retention. In other words, learning vocabulary can be much faster when we make these lingistic connections."),
            p("This app organizes a given list of vocabulary words based on common characters and graphs them into a network using the igraph and plotly packages."),
            p("___"),
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
    
    observeEvent(input$randomize,
                 {root = sample(dt$v,1)})

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
            scale_color_manual(values=c("white","lavender"),
                               labels=c("Other","Root")) +
            ggtitle("") +
            theme_blank() +
            theme(legend.position="none",
                  text=element_text(family="Comic Sans"))
        p %>%
            # sets the specific order of tooltip variables (in this case 1)
            ggplotly(tooltip="text",
                     width=1000 + 20*length(nodes),
                     height=1500 + 50*length(nodes)) %>%
            layout(xaxis = list(fixedrange = TRUE), 
                   yaxis = list(fixedrange = TRUE)) %>%
            config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))
        
    })
}

# run app-----------------------------------------------------------------------

shinyApp(ui = ui, server = server)
