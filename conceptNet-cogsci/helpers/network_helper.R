# a vocabulary with aoa data -> a word2vec network
# vocab_to_w2v_network <- function(vocab) {
#   vocab <- filter_to_w2v(vocab)
#   w2v_links <- make_w2v_links(vocab = vocab)
#   network <- graph_from_data_frame(w2v_links, vertices = vocab, directed = FALSE) %>%
#     as_tbl_graph() %E>%
#     mutate(weight = w2v_similarity) # layout and clustering algorithms automatically use the weight attribute
# }

# a vocabulary with aoa data -> a word2vec network
vocab_to_w2v_network <- function(vocab, filter_to = "both") {
  if (filter_to == "both") {  # filter vocab to just words for which we have both w2v and features
    vocab <- vocab %>%
      filter_to_w2v() %>%
      filter_to_features()
  } else if (filter_to == "w2v") {
    vocab <- filter_to_w2v(vocab)
  } else if (filter_to == "features") {
    vocab <- filter_to_features(vocab)
  } else {
    # don't filter vocab
  }
 
  w2v_links <- make_w2v_links(vocab = vocab)
  
  return(
    graph_from_data_frame(w2v_links, vertices = vocab, directed = FALSE) %>%
      as_tbl_graph() %E>%
      mutate(weight = w2v_similarity)
  ) # layout and clustering algorithms automatically use the weight attribute
  
}

vocab_to_feature_network <- function(vocab, feature_types, filter_to = "both") {
  if (filter_to == "both") {  # filter vocab to just words for which we have both w2v and features
    vocab <- vocab %>%
      filter_to_w2v() %>%
      filter_to_features()
  } else if (filter_to == "w2v") {
    vocab <- filter_to_w2v(vocab)
  } else if (filter_to == "features") {
    vocab <- filter_to_features(vocab)
  } else {
    # don't filter vocab
  }
  
  feature_links <- make_feature_links(vocab = vocab, feature_types)
  
  return(
    graph_from_data_frame(feature_links, vertices = vocab, directed = FALSE) %>%
      as_tbl_graph() %E>%
      mutate(weight = shared)
  ) # layout and clustering algorithms automatically use the weight attribute
  
}


# igraph network -> communities object
w2v_walktrap <- function(w2v_network) {
  communities_obj <- w2v_network %E>%
    filter(w2v_similarity > 0) %>%
    cluster_walktrap(weights = E(.)$w2v_similarity)
  return(communities_obj)
}

# igraph network -> communities object
feature_walktrap <- function(feature_network) {
  communities_obj <- feature_network %E>%
    filter(shared > 0) %>%
    cluster_walktrap(weights = E(.)$shared)
  return(communities_obj)
}


build_clusters <- function(vocab, membership_vec) {
  vocab <- as_ids(vocab)
  clusters <- list()
  if (length(membership_vec) == 0) {
    membership_vec = 1
  }
  for (i in 1:max(membership_vec)) {
    clusters[[i]] <- vocab[membership_vec == i]
  }
  return(clusters)
}