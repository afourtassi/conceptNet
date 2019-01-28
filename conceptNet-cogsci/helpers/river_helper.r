cluster_river <- function(subgraphs_by_age) {
  
  # river node data
  nodes = data.frame(ID = character(0),
                     x = integer(0),
                     labels = character(0),
                     stringsAsFactors = FALSE)
  
  for (i in 1:length(subgraphs_by_age)) {
    subgraphs <- subgraphs_by_age[[i]]
    for (j in 1:length(subgraphs)) {
      id <- str_glue("{i}_{j}")
      x <- i
      g <- subgraphs[[j]]
      label <- "" # V(g)[which.max(eigen_centrality(g)$vector)]$name 
      nodes[nrow(nodes) + 1,] <- list(id, x, label)
    }
  }
  
  # edge data
  edges = data.frame(N1 = character(0),
                     N2 = character(0),
                     Value = integer(0),
                     stringsAsFactors = FALSE)
  
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
  
  return(makeRiver(nodes = nodes, edges = edges))
}

