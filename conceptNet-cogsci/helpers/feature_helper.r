load_features <- function() {
  features <- read_delim("data/MacRae.csv", delim = ",") %>%
    select(Concept, Feature, WB_Label, BR_Label)# %>%
  #rename(uni_lemma=Concept)
  
  return(features)
}

filter_to_features <- function(vocab) {
  features <- load_features()
  
  return (vocab %>%
    filter(uni_lemma %in% features$Concept))
}

make_feature_links <- function(vocab, feature_types) {
  features <- load_features() %>%                  
    filter(Concept %in% vocab$uni_lemma) %>% # remove unneccessary features
    filter(BR_Label %in% feature_types) %>%  # filter to feature type
    select(Concept, Feature)
  
  pairs <- inner_join(features, features, by="Feature") %>%
    filter(Concept.x < Concept.y) %>% # remove duplicates
    count(Concept.x, Concept.y) %>%
    rename(from = Concept.x, to = Concept.y, shared = n)
  
  return(pairs)
}