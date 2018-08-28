# returns jacard distance between two igraphs / name vectors
jacard_distance <- function(g1, g2) {
  if (is_igraph(g1)) {
    e1 <- V(g1)$name # elements of graph
  } else {
    e1 <- g1
  }
  if (is_igraph(g2)) {
    e2 <- V(g2)$name # elements of graph
  } else {
    e2 <- g2
  }
  return(length(intersect(e1,e2)) / length(union(e1,e2)))
}

# returns list containing a list of subgraph clusters for each age
get_subgraphs_by_age <- function(network, cluster_function) {
  ages <- seq(min(V(network)$age), max(V(network)$age))
  result <- list()
  for (age_limit in ages) {
    net <- network %N>% filter(age <= age_limit)
    clusters <- cluster_function(net)
    
    subgraphs <- list()
    for (i in 1:length(clusters)) {
      subgraphs[[length(subgraphs) + 1]] <-
        induced.subgraph(net,
                         which(clusters$membership == i)) %>%
        as_tbl_graph() %>%
        set_graph_attr("age", age_limit)
    }
    ordered_subgraphs <- subgraphs[order(map_dbl(subgraphs, vcount), decreasing = T)]
    
    for (i in seq_along(ordered_subgraphs)) {
      ordered_subgraphs[[i]] <- ordered_subgraphs[[i]] %>%
        set_graph_attr("index", i)
    }
    
    result[[length(result) + 1]] <- ordered_subgraphs
  }
  return(result)
}

# returns list of indices for predecessors at previous time step
phi <- function(subgraphs_by_age, time_step, index, delta) {
  previous_subgraphs <- subgraphs_by_age[[time_step - 1]]
  
  best_indices = integer(0)
  best_jacard = 0
  for (indices in powerset(seq_along(previous_subgraphs))) {
    subgraphs <- previous_subgraphs[indices]
    elements <- subgraphs %>% map(~ V(.)$name) %>% reduce(union)
    jacard <- jacard_distance(elements, subgraphs_by_age[[time_step]][[index]])
    if (jacard > best_jacard) {
      best_indices <- indices
      best_jacard <- jacard
    }
  }
  
  if (best_jacard < delta) {
    best_indices <- integer(0)
  }
  
  return(best_indices)
}

precursors <- function(subgraphs_by_age, delta) {
  prec <- list()
  for (time_step in 2:length(subgraphs_by_age)) {
    prec[[time_step]] <- list()
    subgraphs <- subgraphs_by_age[[time_step]]
    for (index in seq_along(subgraphs)) {
      prec[[time_step]][[index]] <- phi(subgraphs_by_age, time_step, index, delta)
    }
  }
  return(prec)
}

precursors_graph <- function(subgraphs_by_age, delta) {
  # rows
  all_vertex_rows <- list()
  for (time_step in seq_along(subgraphs_by_age)) {
    for (index in seq_along(subgraphs_by_age[[time_step]])) {
      all_vertex_rows[[length(all_vertex_rows) + 1]] <-
        tibble(name = str_glue("{time_step}_{index}"),
               time_step = time_step,
               index = index,
               x = time_step,
               y = index)
    }
  }
  vertices <- dplyr::bind_rows(all_vertex_rows)
  
  # edges
  precs_by_time <- precursors(subgraphs_by_age = subgraphs_by_age, delta = delta)
  all_rows <- list()
  for (time_step in 2:length(precs_by_time)) {
    precs_by_index <- precs_by_time[[time_step]]
    for (index in seq_along(precs_by_index)) {
      precs <- precs_by_index[[index]]
      parents <- suppressWarnings(str_glue("{time_step - 1}_{precs}"))
      child <- str_glue("{time_step}_{index}")
      rows <- tibble(parent = parents, child = child, weight=1)
      all_rows[[length(all_rows) + 1]] <- rows
    }
  }
  edges <- dplyr::bind_rows(all_rows)
  
  # generate graph
  graph <- graph_from_data_frame(edges, vertices = vertices) %>% as_tbl_graph()
  
  return(graph)
}