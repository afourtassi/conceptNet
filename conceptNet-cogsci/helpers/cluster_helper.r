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