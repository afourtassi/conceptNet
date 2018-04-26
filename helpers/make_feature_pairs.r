make_feature_pairs <- function(lemma_list) {
  #Get the McRae features
  features <-
    hi <- read_delim("data/MacRae.csv", delim = ",") %>%
    select(Concept, Feature, WB_Label, BR_Label) %>%
    filter(BR_Label=="visual-form_and_surface") %>% #Here it depends what kind of features we would like to keep (perceptual? conceptual? both?)
    rename(uni_lemma=Concept)
  #Intersection of Wordbank with McRae concepts
  #(Note that we work with unilemmas and not with definitions becuase we would like to use cross-linguistic data)
  
  #List of word types in WordBank
  wb <- lemma_list
  # List of wordbank with feature
  
  #Here I still should deal with words in parenthesis (homophony/polysemy), e.g., chiken (animal) vs. chiken (food), etc...
  
  item_feat <- wb %>%
    left_join(features) %>%
    rename(item.definition = uni_lemma,
           item_feat = Feature) %>%
    filter(!is.na(item_feat)) %>%
    select(item, item.definition, item_feat)
  
  
  item_feat_list <- (wb %>%
                       filter(item %in% item_feat$item) %>%
                       select(item))$item
  
  # List these words pair-wise and compate the number of shared feature for each pair
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
    filter(item!=pair)
  
  return(item_pair_shared)
}