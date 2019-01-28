### Basic cluster measures
min_max_size_ratio <- function(clusters) {
  sizes <- clusters %>%
    map_int(length)
  
  return(min(sizes) / max(sizes))
}



### Pairwise statistics

check_same_cluster <- function(elem1, elem2, clusters) {
  for (cluster in clusters) {
    if (elem1 %in% cluster & elem2 %in% cluster) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# pairwise measurements between clusters
# NOTE these functions only correspond to the definition for partitions
pair_true_positives <- function(clusters, true_clusters) {
  total <- 0
  for (cluster in clusters) {
    if (length(cluster) > 1) {
      for (pair in combn(cluster, 2, simplify = FALSE)) {
        if (check_same_cluster(pair[[1]], pair[[2]], true_clusters)) {
          total = total + 1
        }
      }
    }
  }
  return(total)
}

pair_false_positives <- function(clusters, true_clusters) {
  total <- 0
  for (cluster in clusters) {
    if (length(cluster) > 1) {
      for (pair in combn(cluster, 2, simplify = FALSE)) {
        if (!check_same_cluster(pair[[1]], pair[[2]], true_clusters)) {
          total = total + 1
        }
      }
    }
  }
  return(total)
}

pair_false_negatives <- function(clusters, true_clusters) {
  available_words <- unlist(clusters)
  true_clusters <- true_clusters %>%
    map(~ .[. %in% available_words])
  return(pair_false_positives(true_clusters, clusters))
}

pair_precision <- function(clusters, true_clusters) {
  tp <- pair_true_positives(clusters, true_clusters)
  fp <- pair_false_positives(clusters, true_clusters)
  return(tp / (tp + fp))
}

pair_recall <- function(clusters, true_clusters) {
  tp <- pair_true_positives(clusters, true_clusters)
  fn <- pair_false_negatives(clusters, true_clusters)
  return(tp / (tp + fn))
}

pair_f1 <- function(clusters, true_clusters) {
  precision <- pair_precision(clusters, true_clusters)
  recall <- pair_recall(clusters, true_clusters)
  return(2 * (precision * recall) / (precision + recall))
}

# measures of clustering evolutions

pair_measures_to_last <- function(clusters_evo, measure) {
  measures <- double()
  for (i in 1:length(clusters_evo)) {
    measures[[i]] <- measure(clusters_evo[[i]], clusters_evo[[length(clusters_evo)]])
  }
  return(measures)
}

pair_measures_to_gold <- function(clusters_evo, gold_clusters, measure) {
  measures <- double()
  for (i in 1:length(clusters_evo)) {
    measures[[i]] <- measure(clusters_evo[[i]], gold_clusters)
  }
  return(measures)
}

