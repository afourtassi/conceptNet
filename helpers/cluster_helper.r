apply_color_box <- function(s, color) {
  return(str_glue("<span style=\"padding: 0 4px; border-radius: 4px; background-color: {color}\">{s}</span>"))
} # display: block; 

clusters_to_html_vec <- function(clusters, colors) {
  word_freqs <- clusters %>%
    unlist() %>%
    table() %>% 
    sort(decreasing = T)
  color_word <- function(w) {
    color <- colors[[word_freqs[[w]]]]
    return(apply_color_box(w, color))
  }
  formatted_clusters <- clusters %>%
    map(~ names(sort(word_freqs[.], decreasing = T))) %>% # sort
    map(~ map_chr(.,color_word)) %>%                      # color words
    map_chr(~ paste0(., collapse = " ")) %>%              # concatenate
    map_chr(~ paste0("<span style='line-height: 1.7'>", ., "</span>"))
  return(formatted_clusters)
}

kable_clusters <- function(clusters, title, ...) {
  if (!is.null(clusters$membership)) { # igraph communities object
    count <- length(clusters)
    clusters <- unname(clusters[1:count])
  }
  colors <- viridis(4, alpha = 0.4, direction = -1)
  max_count <- clusters %>%
    unlist() %>%
    table() %>% 
    sort(decreasing = T) %>%
    .[[1]]
  color_display <- 1:max_count %>%
    map_chr(~ apply_color_box(as.character(.), colors[[.]])) %>%
    paste(collapse = " ")
  tibble(clusters = clusters_to_html_vec(clusters, colors)) %>%
    kable(format="html",
          escape = FALSE,
          col.names = c(str_glue("{title}  ({length(clusters)} Clusters)")),
          align = c("l")) %>%
    kable_styling(...) %>%
    footnote(general = str_glue("Color indicates number of clusters in which a word occurs: {color_display}"), escape = F)
}