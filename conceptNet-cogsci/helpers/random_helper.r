# random_clusters_of_shape <- function(shape_clusters, source_clusters) {
#   source_vocab <- unlist(source_clusters)
#   random_vocab <- sample(source_vocab, length(unlist(shape_clusters)))
#   
#   random_clusters <- list()
#   for (cl in 1:length(shape_clusters)) {
#     start_idx <- length(unlist(shape_clusters[1:cl - 1])) + 1
#     end_idx <- length(unlist(shape_clusters[1:cl]))
#     random_clusters[[cl]] <- random_vocab[start_idx: end_idx]
#   }
#   return(random_clusters)
# }

# randomize_clusters_evo <- function(clusters_evo) {
#   random_clusters_evo <- list()
#   random_clusters_evo[[length(clusters_evo)]] <- clusters_evo[[length(clusters_evo)]]
#   for (age in (length(clusters_evo) - 1):1) {
#     random_clusters_evo[[age]] <- random_clusters_of_shape(clusters_evo[[age]], random_clusters_evo[[age + 1]])
#   }
#   return(random_clusters_evo)
# }

# randomize_network_evo <- function(net_evo) {
#   net_sizes <- map(net_evo, ~ length(V(.)) )
#   random_net_evo <- list()
#   random_net_evo[[length(net_evo)]] <- net_evo[[length(net_evo)]]
#   for (age in (length(net_evo) - 1):1) {
#     random_net_evo[[age]] <-random_net_evo[[age + 1]] %N>% sample_n(net_sizes[[age]])
#   }
#   return(random_net_evo)
# }

randomize_aoa <- function(vocab) {
  shuffled <- vocab[sample(nrow(vocab)),]
  return(tibble(
    uni_lemma = shuffled$uni_lemma,
    definition = shuffled$definition,
    age = vocab$age,
    category = shuffled$category
  ))
}

randomize_aoa_to_nearest <- function(vocab) {
  vocab <- randomize_aoa(vocab) # picks random first word and randomizes ties in similarity comparisons
  w2v <- load_w2v() %>%
    filter(from %in% vocab$uni_lemma) %>%
    filter(to %in% vocab$uni_lemma)
  for(i in 2:(nrow(vocab) - 1)) {
    learned <- vocab$uni_lemma[1:(i-1)]
    unlearned <- vocab$uni_lemma[i:nrow(vocab)]
    sim_to_learned <- tidyr::crossing(tibble(from = learned),
                                      tibble(to = unlearned)) %>%
      left_join(w2v, by = c("from", "to")) %>%
      mutate(w2v_similarity = replace_na(w2v_similarity, 0)) %>%
      group_by(to) %>%
      summarise(mean_sim = mean(w2v_similarity)) # note mean does not work since 0 similarities are sometimes ommitted
    closest_word <- sim_to_learned[which.max(sim_to_learned$mean_sim),]$to
    # swap ith row of vocab with the word closest to the 1:i-1 rows
    idx_closest_word <- which(vocab$uni_lemma == closest_word)
    order <- 1:nrow(vocab)
    order[[i]] <- idx_closest_word
    order[[idx_closest_word]] <- i
    vocab <- vocab %>%
      slice(order)
  }
  return(vocab)
}

randomize_within_last_clustering <- function(vocab, n_clusters) {
  vocab_network <- vocab_to_w2v_network(vocab) # drops words w/o w2v data, NOTE memoizes w2v data
  
  clusters <- vocab_network %>%
    w2v_waltrap() %>%
    cut_at(., no = n_clusters) %>%
    build_clusters(V(vocab_network), .)
  
  shuffled <- clusters[sample(length(clusters))] %>% # reorder clusters
    map(~ .[sample(length(.))]) %>%
    unlist()
  
  shuffled <- tibble(uni_lemma = shuffled)
  shuffled <- shuffled %>%
    left_join(vocab, by = c("uni_lemma"))
  
  return(tibble(
    uni_lemma = shuffled$uni_lemma,
    definition = shuffled$definition,
    age = shuffled$age %>%
      sort(),
    category = shuffled$category
  ))
}

randomize_clustering <- function(clustering) {
  lengths <- map(clustering, length)
  words <- sample(unlist(clustering))
  
  new_clustering <- list()
  for (l in lengths) {
    new_clustering[[length(new_clustering) + 1]] <- words[1:l]
    words <- words[l+1:length(words)]
  }
  return(new_clustering)
}