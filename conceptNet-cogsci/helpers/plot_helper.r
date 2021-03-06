layout_network <- function(g, iterations = 10000, delta = 1, gravity = 5, k = 5000) {
  l <- layout.forceatlas2(g %E>% select(weight),
                          iterations=iterations,
                          plotstep=floor(iterations / 10),
                          directed = FALSE,
                          delta = delta,
                          gravity = gravity,
                          k = k)
  
  # move disconnected vertices to 0, 0
  l[degree(g) == 0,] <- 0
  
  # move them to bottom
  y_size <- max(l[,2]) - min(l[,2])
  l[degree(g) == 0,2] <- min(l[,2]) - (y_size / 6)
  
  # spread them out
  num_disconnected <- length(l[degree(g) == 0,1])
  x_size <- max(l[,1]) - min(l[,1])
  l[degree(g) == 0,1] <- (0:(num_disconnected - 1) / (num_disconnected - 1)) * x_size + min(l[,1])
  
  return(l)
}

plot_vocab_network <- function(g, layout = NULL, name=NA, frame=FALSE, clusters=NULL, plot_title = NULL, edge_color = "grey", labels=V(g)$uni_lemma) {
  
  # layout 
  if (!is.null(layout)) {
    # do nothing
  } else if (is.null(layout) && !is.null(V(g)$x)) {
    x <- V(g)$x
    y <- V(g)$y
    layout <- cbind(x,y)
  } else if (is.null(layout) && is.null(V(g)$x)) {
    layout <- layout_with_fr(g)
  }
  
  # rescale layout
  x_size <- max(layout[,1]) - min(layout[,1])
  y_size <- max(layout[,2]) - min(layout[,2])
  max_size <- max(x_size, y_size)
  
  layout <- layout / max_size
  
  # color clusters
  if (!is.null(clusters)) {
    V(g)$color <- clusters+1
    
    ## Paint the vertices in multiple communities to red
    #V(g)[ unlist(clusters)[ duplicated(unlist(clusters)) ] ]$color <- "red"
  }
  
  
  plot(g,
       layout=layout,
       frame=frame,
       rescale=FALSE,
       asp=0, #y_size / x_size,
       xlim=c(min(layout[,1]), max(layout[,1])),
       ylim=c(min(layout[,2]), max(layout[,2])),
       
       # === vertex
       # vertex.color = rgb(0.0,0.0,0.7,0.5),                        # Node color
       # vertex.frame.color = NA,                 # Node border color
       vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
       vertex.size=1.5,                               # Size of the node (default is 15)
       vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
       
       # === vertex label
       vertex.label = labels,                   # Character vector used to label the nodes
       vertex.label.color="black",
       vertex.label.family="Helvetica",                  # Font family of the label (e.g.“Times”, “Helvetica”)
       vertex.label.font=1,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
       vertex.label.cex=0.7,                           # Font size (multiplication factor, device-dependent)
       vertex.label.dist=0.5,                          # Distance between the label and the vertex
       vertex.label.degree=pi/2 ,                       # The position of the label in relation to the vertex (use pi)
       
       # === Edge
       edge.color = edge_color,                           # Edge color
       edge.width=0.5,                                 # Edge width, defaults to 1
       edge.arrow.size=0.1,                            # Arrow size, defaults to 1
       edge.arrow.width=1,                           # Arrow width, defaults to 1
       edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
       edge.curved=0                               # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
  )
  if (!is.null(plot_title)) {
    plot_title
  }
  
}

plot_river_network <- function(g, layout = NULL, name=NA, frame=FALSE, clusters=NULL, title = NULL) {
  
  # layout 
  if (is.null(layout)) {
    x <- V(g)$x
    y <- V(g)$y
    layout <- cbind(x,y)
  }
  
  # rescale layout
  x_size <- max(layout[,1]) - min(layout[,1])
  y_size <- max(layout[,2]) - min(layout[,2])
  max_size <- max(x_size, y_size)
  
  layout <- layout / max_size
  
  # color clusters
  if (!is.null(clusters)) {
    V(g)$color <- clusters+1
    
    ## Paint the vertices in multiple communities to red
    #V(g)[ unlist(clusters)[ duplicated(unlist(clusters)) ] ]$color <- "red"
  }
  
  
  plot(g,
       layout=layout,
       frame=frame,
       main=title,
       rescale=FALSE,
       asp=0, #y_size / x_size,
       xlim=c(min(layout[,1]), max(layout[,1])),
       ylim=c(min(layout[,2]), max(layout[,2])),
       
       # === vertex
       # vertex.color = rgb(0.0,0.0,0.7,0.5),                        # Node color
       # vertex.frame.color = NA,                 # Node border color
       vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
       vertex.size=1.5,                               # Size of the node (default is 15)
       vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
       
       # === vertex label
       vertex.label=NA, #V(g)$uni_lemma,                   # Character vector used to label the nodes
       vertex.label.color="black",
       vertex.label.family="Helvetica",                  # Font family of the label (e.g.“Times”, “Helvetica”)
       vertex.label.font=1,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
       vertex.label.cex=0.7,                           # Font size (multiplication factor, device-dependent)
       vertex.label.dist=0.5,                          # Distance between the label and the vertex
       vertex.label.degree=pi/2 ,                       # The position of the label in relation to the vertex (use pi)
       
       # === Edge
       edge.color= rgb(.9 - E(g)$weight / 2,
                       .9 - E(g)$weight / 2,
                       .9 - E(g)$weight / 2),                           # Edge color
       edge.width=0.5,                                 # Edge width, defaults to 1
       edge.arrow.size=0.1,                            # Arrow size, defaults to 1
       edge.arrow.width=1,                           # Arrow width, defaults to 1
       edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
       edge.curved=0                               # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
  )
  
}



