make_assoc_pairs <- function(lemma_list) {
  
  cue_target<- read.csv("data/association_cue_target.csv", as.is = T)
  # filter until words in lemma_list remain
  lemma_list<- lemma_list %>% filter((uni_lemma %in% cue_target$cue) | (uni_lemma %in% cue_target$target))
  lemma<- lemma_list$uni_lemma
  cue_target<- cue_target %>% 
    filter(cue %in% lemma, 
           target %in% lemma, 
           normed=="YES") %>% 
    select(cue, target) %>% 
    mutate(link=1)
  
  assoc_table<- expand.grid(cue= lemma, target= lemma) %>% 
    left_join(cue_target) %>% 
    mutate(link=if_else(is.na(link),0,link))
  
  #make a association network dataframe with item number
  #rename stuffs so it could conform to the format  needs
  #item corresponds to target ;  pair corresponds to cue
  assoc_link <- assoc_table %>%
    rename(pair.definition = cue) %>%
    left_join(lemma_list, c("pair.definition" = "uni_lemma")) %>%
    rename(pair = item, item.definition = target) %>%
    left_join(lemma_list, c("item.definition" = "uni_lemma")) %>%
    select(item, item.definition, pair, pair.definition, link) %>%
    arrange(item, pair) %>%
    filter(item > pair) # filters item = pair and duplicates
  
  return(assoc_link)
}

assoc_network <- function(vocab, max_age) {
  vocab_up_to_age = filter(vocab, age <= max_age)
  assoc_links <- make_assoc_pairs(lemma_list = vocab_up_to_age) %>%
    filter(link==1) %>%
    select(item, pair)
  graph <- graph_from_data_frame(assoc_links, directed=FALSE, vertices=vocab_up_to_age) %>%
    set.graph.attribute("name", "Assoc") %>%
    set.graph.attribute("age", max_age)
  return(graph)
}
