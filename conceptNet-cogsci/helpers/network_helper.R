# a vocabulary with aoa data -> a word2vec network
vocab_to_w2v_network <- function(vocab) {
  vocab <- filter_to_w2v(vocab)
  w2v_links <- make_w2v_links(vocab = vocab)
  network <- graph_from_data_frame(w2v_links, vertices = vocab, directed = FALSE) %>%
    as_tbl_graph() %E>%
    mutate(weight = w2v_similarity) # layout and clustering algorithms automatically use the weight attribute
}

# igraph network -> communities object
w2v_waltrap <- function(w2v_network) {
  communities_obj <- w2v_network %E>%
    filter(w2v_similarity > 0) %>%
    cluster_walktrap(weights = E(.)$w2v_similarity)
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