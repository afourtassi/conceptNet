clique.community <- function(graph, k) {
  require(igraph)
  clq <- cliques(graph, min=k, max=k) %>% lapply(as.vector)
  
  
  #find edges
  edges <- c()
  for (i in seq_along(clq)) {
    for (j in seq_along(clq)) {
      if ( length(unique(c(clq[[i]], clq[[j]]))) == k+1 ) {
        edges[[length(edges)+1]] <- c(i,j)
      }
    }
  }
  
  #Create an empty graph and then adding edges
  clq.graph <- make_empty_graph(n = length(clq)) %>% add_edges(unlist(edges))
  clq.graph <- simplify(clq.graph)
  V(clq.graph)$name <- seq_len(vcount(clq.graph))
  
  
  comps <- decompose.graph(clq.graph)
  
  lapply(comps, function(x) {
    unique(unlist(clq[ V(x)$name ]))
  })
}

######################################################################################################################

clique.community.opt <- function(graph, k){
  require(igraph)
  
  ###################################
  ### STEP #1: Clique discovery
  ###################################
  
  clq <- cliques(graph, min=k, max=k) %>% lapply(as.vector)
  
  
  ###################################
  ### STEP #2: Clique-graph creation
  ###################################
  
  #find edges between cliques
  edges <- c()
  for (i in 1:(length(clq)-1)) {
    for (j in (i+1):length(clq)) {
      if ( length(unique(c(clq[[i]], clq[[j]]))) == k+1 ) {
        edges[[length(edges)+1]] <- c(i,j)
      }
    }
  }
  
  #Create an empty graph and then adding edges
  clq.graph <- make_empty_graph(n = length(clq)) %>% add_edges(unlist(edges))
  clq.graph <- simplify(clq.graph)
  V(clq.graph)$name <- seq_len(vcount(clq.graph))
  
  
  comps <- decompose.graph(clq.graph)
  
  lapply(comps, function(x) {
    unique(unlist(clq[ V(x)$name ]))
  })
}

######################################################################################################################

clique.community.opt.par <- function(graph, k){
  require(igraph)
  if(!require(foreach)){
    install.packages("foreach")
    library(foreach)
  }
  
  ###################################
  ### STEP #1: Clique discovery
  ###################################
  
  clq <- cliques(graph, min=k, max=k) %>% lapply(as.vector)
  
  
  ###################################
  ### STEP #2: Clique-graph creation
  ###################################
  
  #find edges between cliques
  edges <- c()
  edges <- foreach (i=1:(length(clq)-1), .combine=c) %dopar% 
  {
    tmp_edg <- list()
    for (j in (i+1):length(clq)) {
      if ( length(unique(c(clq[[i]], clq[[j]]))) == k+1 ) {
        tmp_edg[[length(tmp_edg)+1]] <- c(i,j)
        #tmp_edg <- c(tmp_edg, c(i,j))
      }
    }
    return(tmp_edg)
  }
  
  #Create an empty graph and then adding edges
  clq.graph <- make_empty_graph(n = length(clq)) %>% add_edges(unlist(edges))
  clq.graph <- simplify(clq.graph)
  V(clq.graph)$name <- seq_len(vcount(clq.graph))
  
  
  comps <- decompose.graph(clq.graph)
  
  lapply(comps, function(x) {
    unique(unlist(clq[ V(x)$name ]))
  })
}

