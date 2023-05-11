split_characters <- function(x) {
  x <- x %>% strsplit("") %>% unlist() %>% unique()
  return(x)
}

get_nodes <- function(root,counter,stop,nodes=data.frame()) {
  #' Select nodes recursively
  
  # get all edges containing the phrases of interest
  edges <- graph[f %in% root & t %in% root,]
  
  # get all unique nodes that are in the edges of interest
  nodes_new <- vocab[vocab$name %in% root,]
  nodes_new$colors <- as.character(counter)
  nodes <- bind_rows(nodes,nodes_new) %>% distinct(chinese,.keep_all=T)
  
  if (counter==stop) {
    return(c(edges,nodes))
  } else {
    # eg, get all phrases containing xiang, chi, fan
    for (character in split_characters(nodes$chinese)) {
      root = c(root, unlist(unname(chars[v==character,"pos"])))
    }
    
    get_nodes(unique(root),counter+1,stop,nodes=nodes)
  }
}

add_data <- function(newWord) {
  # function to add and subtract nodes as needed, on click
  # add nodes for 1 recursion around new select
  # compare: delete all from 2nd recursion of old table, if not in 1st recursion of new
  
  root_phrases <- unlist(unname(chars[v==newWord,"pos"]))
  graph_results <- get_nodes(root_phrases, counter=3, stop=3)
  
  # deal with multiple-output function
  new_nodes <- data.frame(graph_results[c('name','chinese','pinyin','english','text','colors')])
  new_edges <- data.frame(graph_results[c('f','t')])
  
  return(net)
}