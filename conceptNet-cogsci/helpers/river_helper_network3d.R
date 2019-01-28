cluster_river <- function(subgraphs_by_age) {
  
  # river node data
  nodes = tibble(name = character(0))
  
  for (i in 1:length(subgraphs_by_age)) {
    subgraphs <- subgraphs_by_age[[i]]
    for (j in 1:length(subgraphs)) {
      name <- V(g)[which.max(eigen_centrality(g)$vector)]$name 
      nodes[nrow(nodes) + 1,] <- list(name)
    }
  }
  
  # edge data
  edges = tibble(source = integer(0),
                 target = integer(0),
                 value = double(0))
  
  for (i in 1:(length(subgraphs_by_age) - 1)) {
    curr_subgraphs <- subgraphs_by_age[[i]]
    next_subgraphs <- subgraphs_by_age[[i + 1]]
    
    for(from in 1:length(curr_subgraphs)) {
      for(to in 1:length(next_subgraphs)) {
        from_id <- str_glue("{i}_{from}")
        to_id <- str_glue("{i + 1}_{to}")
        flow <- length(intersect(V(curr_subgraphs[[from]])$name,
                                 V(next_subgraphs[[to]])$name))
        if (flow > 0) {
          edges[nrow(edges) + 1,] <- list(from_id, to_id, flow)
        }
      }
    }
  }
  
  # enumerate
  edges <- edges %>%
    mutate(source = map(edges$source, ~ which(nodes$id == .)[1] - 1),
           target = map(edges$target, ~ which(nodes$id == .)[1] - 1))
  nodes <- nodes %>%
    mutate(id = seq(0,length(nodes$id) - 1))
  
  #return(list(nodes = nodes, edges = edges))
  
  
  
  nodes <- tibble(name = c("One", "Two", "Three", "Four", "Five", "Six"))
  links <- tibble(source = c(0,0,1,1,2,2,3,3),
                  target = c(2,3,2,3,4,5,4,5),
                  value = c(1.0,2,3,4,1,2,3,4))
  return(list(nodes = nodes, links = links))
}

