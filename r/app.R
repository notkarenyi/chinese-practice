# This is a Shiny web application for displaying semantic connections between Chinese vocab words from a list. Run the application by clicking the 'Run App' button above.
#
# link.R produces 3 data frames:
#  graph: position pairs containing all unique combinations of character phrases
#  vocab: vocab phrases to learn
#  chars: characters (roots) associated with the phrases

# setup-------------------------------------------------------------------------

library(shiny)
library(plotly)
library(ggnetwork)
library(igraph)

source("link.R")
# graph <- read.csv("graph.csv")
text <- readLines("text.txt", encoding = "UTF-8")
maximum <- 50

get_nodes <- function(root, counter, stop, color, orig_root, colors = c(), nodes = data.frame()) {
  #' Select nodes recursively
  #' @example get_nodes(unlist(unname(chars[v=='中',"pos"])), counter=1, stop=2, color=1)

  # get all edges containing the phrases of interest
  edges <- graph[f %in% root & t %in% root, ]

  # get all unique nodes that are in the edges of interest
  nodes <- bind_rows(nodes, vocab[vocab$name %in% root, ]) %>% distinct()

  # ensure we stay a manageable number of nodes
  if (counter == stop) {
    # make sure we don't delete any of the first layer root nodes
    removeable <- nodes$name[!(nodes$name %in% orig_root)]

    remove <- sample(removeable, max(0, length(removeable) - maximum))

    nodes <- nodes[!nodes$name %in% remove, ]
    edges <- edges[!(f %in% remove | t %in% remove)]
  }

  colors <- c(colors, rep(as.character(color), nrow(nodes) - length(colors)))

  if ((counter == stop) | (nrow(nodes) == maximum)) {
    # recursion base case
    return(c(edges, nodes, data.frame(colors)))
  } else {
    # get all individual characters from the nodes of interest
    characters <- nodes$chinese %>%
      strsplit("") %>%
      unlist() %>%
      unique()

    # get all positions of phrases containing characters of interest
    for (character in characters) {
      root <- c(root, unlist(unname(chars[v == character, "pos"])))
    }

    root <- unique(root)

    get_nodes(root, counter + 1, stop, color + 1, orig_root, colors, nodes = nodes)
  }
}

# define UI---------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "index.css"),
    # cite: ChatGPT
    tags$script(HTML("
          $(document).on('shiny:connected', function() {
            var mobile = /Android|iPhone|iPad/i.test(navigator.userAgent);
            Shiny.onInputChange('isMobile', mobile);
          });
        "))
  ),
  titlePanel("chinese word net"),
  sidebarLayout(
    sidebarPanel(
      # inputs------------------------------------------------------------
      selectizeInput("root",
        "Type or select a word to focus on",
        choices = chars$v,
        selected = "中"
      ),
      actionButton(
        "randomize",
        "Randomize"
      ),
      hr(),

      # information accordion---------------------------------------------
      br(),
      p("Tip: Hover over or tap a word for the English translation. Click a word to center it in the graph."),
      hr(),
      tags$details(
        tags$summary(tags$a("About this app (expand)")),
        tags$br(),
        tags$p(
          text[1],
          tags$a(href = text[2], text[3]),
          text[4]
        ),
        tags$p(text[5]),
        tags$p(
          text[6],
          tags$a(href = text[7], text[8]),
          text[9]
        ),
        tags$p(text[10])
      ),
      hr(),
      p("Created by Karen Yi"),
      a("View on Github", href = "https://github.com/notkarenyi/chinese-practice"),
      width = 2
    ),

    # show plot of the chosen subset
    mainPanel(
      plotlyOutput("network")
    )
  )
)

# define server logic-----------------------------------------------------------

server <- function(input, output) {
  output$network <- renderPlotly({
    # set seed such that the random node placement is the same every time
    # set.seed(123)

    # get all phrases with the root character
    # input = c()
    # input$root = '不'
    stop <- 2
    root_phrases <- unlist(unname(chars[v == input$root, "pos"]))
    graph_results <- get_nodes(root_phrases, counter = 1, stop = stop, color = 1, orig_root = root_phrases)

    # deal with multiple-output function
    nodes <- data.frame(graph_results[c("name", "chinese", "pinyin", "english", "text", "colors")])
    edges <- data.frame(graph_results[c("f", "t")])

    nodes$colors[grep(input$root, nodes$chinese)] <- 1
    nodes$name <- as.character(nodes$name)

    # format for ggplot
    weights <- ifelse(edges$f == 103, 2, 1)
    g <- graph.data.frame(edges)
    # net <- ggnetwork(g,layout=layout_with_fr(g))
    net <- ggnetwork(g, layout = layout_with_kk(g, weights = weights))

    # add back information for labels etc
    net <- left_join(net, select(nodes, name, chinese, text, colors))

    # create gradient of colors based on number of levels of recursion
    cols <- c()
    for (i in 1:stop) {
      cols <- c(cols, rgb(red = (140 + 100 * i / stop) / 255, green = (140 + 100 * i / stop) / 255, blue = 1, alpha = 1))
    }

    # adjust graph size based on number of nodes present
    size <- round((800 + length(graph_results$name)**1.4) / 100, 0) * 100

    p <- ggplot(net, aes(
      x = x, y = y, xend = xend, yend = yend,
      # label the tooltips ?
      text = text,
      # identify which point was clicked
      key = chinese
    )) +
      geom_edges(
        color = "grey60",
        linewidth = .1
      ) +
      geom_nodes(aes(color = colors), size = 18) +
      # wrap if longer than 4 chars
      geom_nodetext(aes(label = gsub("(.{4})(.+)", "\\1\n\\2", chinese))) +
      scale_color_manual(values = cols, labels = as.character(1:stop)) +
      labs(title = paste0(
        "Words related to: ",
        chars$most_likely[chars$v == input$root]
      )) +
      theme_blank() +
      theme(
        legend.position = "none",
        text = element_text(family = "sans-serif")
      )

    p %>%
      # sets the specific order of tooltip variables (in this case 1)
      ggplotly(
        tooltip = "text",
        width = size,
        height = size,
        # ties plotly_click event to data
        source = "name"
      ) %>%
      layout(
        xaxis = list(fixedrange = T),
        yaxis = list(fixedrange = T),
        font = list(family = "sans serif"),
        dragmode = F
      ) %>%
      config(displayModeBar = F)
  })

  # update graph when we change the root word via randomization
  observeEvent(input$randomize, {
    updateSelectizeInput(inputId = "root", selected = sample(chars$v, 1))
  })

  # update graph whenever we click a node to explore more
  observeEvent(event_data("plotly_click", source = "name"), {
    dt <- event_data("plotly_click", source = "name")
    if (!is.null(dt) & !input$isMobile) {
      newWord <- strsplit(dt$key, "") %>% unlist()
      # make the new word NOT equal to the current root and PRESENT in the possible list
      newWord <- newWord[(newWord != input$root) & (newWord %in% chars$v)]
      # random otherwise
      newWord <- newWord[sample(length(newWord))]
      # print(newWord)
      # make sure we didn't filter out all possible words
      if (length(newWord)) {
        updateSelectizeInput(inputId = "root", selected = newWord)
      }
    }
  })
}

# run app-----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
