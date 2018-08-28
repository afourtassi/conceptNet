load_w2v <- function() {
  w2v <- read_delim("data/w2v_pair.csv", delim = ",") %>%
    rename(from = item.definition, to = pair.definition, w2v_similarity = sim) %>%
    # filter(w2v_similarity > 0.000001) %>%
    filter(from < to) %>% # remove duplicates
    trim_w2v()

  return(w2v)
}

trim_w2v <- function(w2v) {
  w2v <- w2v %>%
    mutate(from = gsub(" \\s*\\([^\\)]+\\)","", from)) %>% # remove parenthesized
    mutate(to = gsub(" \\s*\\([^\\)]+\\)","", to)) %>%
    group_by(from, to) %>%
    summarise(w2v_similarity = max(w2v_similarity)) %>%
    filter(from < to) # remove duplicates created during trimming
  
  return(w2v)
}

filter_to_w2v <- function(vocab) {
  w2v <- load_w2v()
  
  return (vocab %>%
    filter(uni_lemma %in% w2v$from | uni_lemma %in% w2v$to)) # word must occur in word2vec data
}

make_w2v_links <- function(vocab) {
  w2v <- load_w2v() %>%
    filter(from %in% vocab$uni_lemma &
             to %in% vocab$uni_lemma)
  
  return(w2v)
}