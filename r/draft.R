# load libraries and scripts----------------------------------------------------

library(shiny)
library(plotly)
library(ggnetwork)
library(igraph)

source("link.R")
source("helper_functions.R")
text <- readLines("text.txt", encoding="UTF-8")

tap="no"
newWord=NULL

# ui----------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="index.css")
  ),
  
  titlePanel("chinese word net"),
  
  sidebarLayout(
    sidebarPanel(
      # inputs------------------------------------------------------------------
      selectizeInput("root",
                     "Type or select a word to focus on",
                     choices=chars$v,
                     selected='中'),
      actionButton("randomize",
                   "Randomize"),
      hr(),
      br(),
      p("Tip: Hover over or tap a word for the English translation. Click a word to center it in the graph."),
      hr(),
      # information accordion---------------------------------------------------
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

# server------------------------------------------------------------------------

server <- function(input, output) {
  
  # select the data the first time, on select or randomize, recursively
  # color the nodes appropriately on each recursion
  net <- reactive({
    if (!is.null(newWord)) {
      add_data(newWord)
    } else {
      # get all phrases with the root character
      # input=c(a=2,root='不')
      root_phrases <- unlist(unname(chars[v==input$root,"pos"]))
      graph_results <- get_nodes(root_phrases, counter=1, stop=2)
      
      # deal with multiple-output function
      nodes <- data.frame(graph_results[c('name','chinese','pinyin','english','text','colors')])
      edges <- data.frame(graph_results[c('f','t')])
      
      # format for ggplot
      g <- graph.data.frame(edges)
      net <- ggnetwork(g,layout=layout_with_kk(g))
      
      # add back information for labels etc
      net <- left_join(net,select(nodes,name,chinese,text,colors))
    }
    return(net)
  })
  
  output$network <- renderPlotly({
    net <- net()

    # create gradient of colors based on number of levels of recursion
    # cols <- c(rgb(240/255,240/255,255/255,1))
    cols <- c()
    for (i in 0:2) {
      cols <- c(cols,rgb(red=(235-45*i)/255, green=(235-45*i)/255, blue=1, alpha=1))
    }

    # adjust graph size based on number of nodes present
    size=round((800+length(unique(net$name))**1.4)/100,0)*100
    
    p <- ggplot(net, aes(x=x, y=y, xend=xend, yend=yend, 
                         # label the tooltips ?
                         text=text,
                         # identify which point was clicked
                         key=chinese)) +
      geom_edges(color="grey60",size=.1) +
      geom_nodes(aes(color=colors),size=18) +
      geom_nodetext(aes(label=chinese)) +
      scale_color_manual(values=rev(cols), labels=as.character(1:3)) +
      theme_blank() +
      theme(legend.position="none") 
    
    p %>%
      # sets the specific order of tooltip variables (in this case 1)
      ggplotly(tooltip="text",
               width=size,
               height=size,
               # ties plotly_click event to data
               source="name") %>%
      layout(xaxis=list(fixedrange=T),
             yaxis=list(fixedrange=T),
             font=list(family="sans serif"),
             dragmode=F) %>%
      config(displayModeBar=F)
  })
  
  # update graph when we change the root word via randomization
  observeEvent(input$randomize,{
    updateSelectizeInput(inputId="root",selected=sample(chars$v,1))
  })
  
  # MOBILE COMPATIBILITY
  # YOUVE BEEN TAPPED
  # YOUVE BEEN TAPPED AGAIN
  # update graph whenever we click a node to explore more
  observeEvent(event_data("plotly_click",source="name"), {
    if (tap=="yes") {
      tap="no"
      dt <- event_data("plotly_click",source="name")
      if (!is.null(dt)) {
        newWord <- split_characters(dt$key)
        # make the new word NOT equal to the current root and PRESENT in the possible list
        newWord <- newWord[(newWord!=input$root) & (newWord %in% chars$v)]
        # random otherwise
        newWord <- newWord[sample(length(newWord))]
        # print(newWord)
        # make sure we didn't filter out all possible words
        if (length(newWord)) {
          add_data(newWord)
        }
        newWord=NULL
      } 
    } else {
      tap="yes"
    }
  })
}

shinyApp(ui=ui, server=server)
