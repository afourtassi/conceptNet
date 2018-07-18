plot_network <- function(g, name=NA, remove_isolated=FALSE, frame=FALSE, labels=FALSE, clusters=NULL) {
  if (remove_isolated) {
    g <- delete.vertices(g, degree(g)==0)
  }
  l <- layout_with_fr(g) #layout.forceatlas2(g, iterations=2000, plotstep=5000) #layout_with_fr(g)
  
  if (!is.null(clusters)) {
    ## Paint them to different colors
    colbar <- rainbow(length(clusters) + 1)
    for (i in seq(along=clusters)) {
      V(g)[ clusters[[i]] ]$color <- colbar[i+1]
    }
    
    ## Paint the vertices in multiple communities to red
    V(g)[ unlist(clusters)[ duplicated(unlist(clusters)) ] ]$color <- "red"
  }
  
  if (labels) {
    plot(g, layout=l, vertex.label=V(g)$uni_lemma, edge.width=0.5, vertex.size=4, frame=frame)
  } else {
    plot(g, layout=l, vertex.label=NA, edge.width=0.5, vertex.size=4, frame=frame)
  }
}

