plot_network <- function(g, name=NA, remove_isolated=FALSE, frame=FALSE, labels=FALSE, clusters=NULL) {
  if (remove_isolated) {
    g <- delete.vertices(g, degree(g)==0)
  }
  l <- layout_with_fr(g, niter = 500)
  # l <- layout.forceatlas2(g,
  #                         iterations=1500,
  #                         plotstep=500,
  #                         directed = FALSE,
  #                         delta = 1,
  #                         gravity = 100,
  #                         k = 10000)
  
  # rescale
  x_size <- max(l[,1]) - min(l[,1])
  y_size <- max(l[,2]) - min(l[,2])
  max_size <- max(x_size, y_size)
  
  l <- l / max_size
  
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
    plot(g,
         layout=l,
         frame=frame,
         rescale=FALSE,
         asp=0,
         xlim=c(min(l[,1]), max(l[,1])),
         ylim=c(min(l[,2]), max(l[,2])),
         
         # === vertex
         vertex.color = rgb(0.0,0.0,0.7,0.5),                        # Node color
         vertex.frame.color = NA,                 # Node border color
         vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
         vertex.size=2,                               # Size of the node (default is 15)
         vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
         
         # === vertex label
         vertex.label=V(g)$uni_lemma,                   # Character vector used to label the nodes
         vertex.label.color="black",
         vertex.label.family="Helvetica",                  # Font family of the label (e.g.“Times”, “Helvetica”)
         vertex.label.font=1,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
         vertex.label.cex=0.5,                           # Font size (multiplication factor, device-dependent)
         vertex.label.dist=0.5,                          # Distance between the label and the vertex
         vertex.label.degree=pi/2 ,                       # The position of the label in relation to the vertex (use pi)
         
         # === Edge
         edge.color="grey",                           # Edge color
         edge.width=0.5,                                 # Edge width, defaults to 1
         edge.arrow.size=1,                            # Arrow size, defaults to 1
         edge.arrow.width=1,                           # Arrow width, defaults to 1
         edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
         edge.curved=0                               # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
    )
    
  } else {
    plot(g, layout=l, vertex.label=NA, edge.width=0.5, vertex.size=4, frame=frame)
  }
}

