cluster_cfinder <- function(g, debug = FALSE, cfinder_program) {
  temp_dir <- paste(tempdir(), as.integer(Sys.time()), sep = "/")
  while (dir.exists(temp_dir)) {temp_dir <- paste(temp_dir,"_",sep = "")}
  dir.create(temp_dir)
  if (debug) {print(str_glue("Working in directory: {temp_dir}"))}
  write_graph(g, str_glue("{temp_dir}/graph"), format = "ncol")
  system(str_glue("{cfinder_program} -i {temp_dir}/graph"),
         ignore.stdout = !debug,
         ignore.stderr = !debug)
  
  ## load calculated clusterings
  k <- 3
  clusterings <- list()
  file <- str_glue("{temp_dir}/graph_files/k={k}/communities")
  while (file.exists(file)) {
    clusterings[[k]] <- load_cfinder_communities(file)
    k <- k + 1
    file <- str_glue("{temp_dir}/graph_files/k={k}/communities")
  }
 
  # optimal <- FALSE
  # k <- 3
  # while (!optimal) {
  #   file <- str_glue("{temp_dir}/graph_files/k={k}/communities")
  #   if (file.exists(file)) {
  #     clusters <- load_cfinder_communities(file)
  #   } else {
  #     k <- NULL
  #     break
  #   }
  #   sizes <- sort(map_int(clusters, length), decreasing = T)
  #   if (length(clusters) >= 2 && sizes[[2]] * 2 > sizes[[1]]) {
  #     optimal <- TRUE
  #   } else {
  #     k <- k + 1
  #   }
  # }
  # 
  # if (is.null(k)) {
  #   clusters <- list()
  # } else {
  #   clusters <- load_cfinder_communities(str_glue("{temp_dir}/graph_files/k={k}/communities"))
  # }
  # return(list(clusters = clusters, k = k))
  
  return(clusterings)
}

load_cfinder_communities <- function(file) {
  x <- system(str_glue("grep -v -E '(^#|^[[:space:]]*$)' {file} | sed 's/^[^:]*://g'"), intern = TRUE)
  string_to_vec <- function(s) {unlist(str_split(str_trim(s), " "))}
  return(map(x, string_to_vec))
}