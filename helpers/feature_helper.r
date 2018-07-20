make_feature_pairs <- function(lemma_list, features) {
  wb <- lemma_list
  
  # Here I still should deal with words in parenthesis (homophony/polysemy), e.g., chiken (animal) vs. chiken (food), etc...
  
  item_feat <- wb %>%
    left_join(features) %>%
    rename(item.definition = uni_lemma,
           item_feat = Feature) %>%
    filter(!is.na(item_feat)) %>%
    select(item, item.definition, item_feat)
  
  
  item_feat_list <- (wb %>%
                       filter(item %in% item_feat$item) %>%
                       select(item))$item
  
  # List these words pair-wise and compute the number of shared feature for each pair
  item_pair <- expand.grid(item = item_feat_list,
                           pair = item_feat_list)
  
  pair_feat <- item_feat %>%
    rename(pair = item,
           pair_feat = item_feat,
           pair.definition = item.definition)
  
  item_feat_pair <- left_join(item_feat, item_pair)
  
  item_pair_feat <- item_feat_pair %>%
    left_join(pair_feat)
  
  item_pair_shared <- item_pair_feat %>%
    group_by(item, item.definition, pair, pair.definition) %>%
    summarise(shared = sum(pair_feat == item_feat)) %>%
    filter(item > pair) %>% # removes item = pair and duplicates
    ungroup()
  
  return(item_pair_shared)
}

load_features <- function() {
  features <- read_delim("data/MacRae.csv", delim = ",") %>%
    select(Concept, Feature, WB_Label, BR_Label) %>%
    rename(uni_lemma=Concept)
  
  return(features)
}

feature_network <- function(vocab, features, feature_types, shared_threshold) {
  feature_links <- make_feature_pairs(lemma_list = vocab,
                                      features = filter(features, BR_Label %in% feature_types)) %>%
    filter(shared >= shared_threshold) %>%
    select(item, pair, shared)
  
  graph <- graph_from_data_frame(feature_links, directed=FALSE,
                                 vertices = vocab) %>%
    set.graph.attribute("name", "Feature")
  
  return(as_tbl_graph(graph))
}