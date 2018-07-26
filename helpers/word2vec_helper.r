load_w2v <- function() {
  w2v <- read_delim("data/w2v_pair.csv", delim = ",") %>%
    rename(from = item.definition, to = pair.definition, w2v_similarity = sim) %>%
    #filter(w2v_similarity > 0.0) %>%
    filter(from < to) # remove duplicates

  return(w2v)
}

make_w2v_links <- function(vocab) {
  w2v <- load_w2v() %>%
    filter(from %in% vocab$uni_lemma &
             to %in% vocab$uni_lemma)
  
  return(w2v)
}