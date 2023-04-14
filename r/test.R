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
      sliderInput("niter",
                  "niter",
                  50,1000,value=10,
                  step=50,
                  ticks=F),
      sliderInput("temp",
                  "temp",
                  .1,2,value=.1,
                  step=.1,
                  ticks=F),
      sliderInput("x",
                  "x",
                  1,4,value=1,
                  ticks=F),
      sliderInput("w",
                  "w",
                  800,3000,value=1600,step=200,
                  ticks=F),
      sliderInput("h",
                  "h",
                  800,3000,value=1600,step=200,
                  ticks=F),
      br(),
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
  
  make_network <- function() {
    
    output$network <- renderPlotly({
      
      # Generate graph based on selection---------------------------------
      net <- ggnetwork(g,layout=layout_with_fr(g,
                                               niter=input$niter,
                                               start.temp=input$temp * vcount(g)))
      
      
      # add back information for labels etc
      net$name <- as.numeric(net$name)
      nodes$name <- as.numeric(nodes$name)
      net <- left_join(net,select(nodes,name,chinese,text,colors))
      
      # set seed such that the random node placement is the same every time
      set.seed(123)
      
      p <- ggplot(net, aes(x = x, y = y, 
                           xend = xend, yend = yend, text=text)) +
        geom_edges(color="grey60",
                   size=.1) +
        geom_nodes(aes(color=colors),size=18) +
        geom_nodetext(aes(label=chinese)) +
        theme_blank() +
        theme(legend.position="none")
      
      p %>%
        # sets the specific order of tooltip variables (in this case 1)
        ggplotly(tooltip="text",
                 width=input$w,
                 height=input$w) %>%
        config(displayModeBar = F)
    })
  }
  
  make_network()
  
}

# run app-----------------------------------------------------------------------

shinyApp(ui = ui, server = server)
