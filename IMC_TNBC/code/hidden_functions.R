#Hidden functions

# buildEdgeList - Helpers
.createEdgeList <- function(list, list_length){

out <- lapply(list, function(x){
  list_options <- list[length(x)+1 == list_length]
  
  if(length(list_options) != 0){
    list_select <- list_options[sapply(list_options,function(y){length(intersect(y,x)) == length(x)})]  
    
    if(length(list_select) != 0){ 
      out <- data.frame("from" = paste(x, collapse = "_"),
                        "to" = sapply(list_select, paste, collapse = "_"), 
                        row.names = NULL)
    }else{
      NULL
    }
  }else{
    NULL
  }
})

edges <- do.call(rbind, out)

return(edges)
}

# plotSpatialContext - Helpers 

.generatePlot <- function(graph,
                          node_color_by,
                          node_size_by,
                          node_color_fix,
                          node_size_fix,
                          node_label_repel,
                          node_label_color_by, 
                          node_label_color_fix,  
                          draw_edges,
                          edge_color_fix){
  
  node_color_by <- if(is.null(node_color_by)) NULL else node_color_by
  node_size_by <- if(is.null(node_size_by)) NULL else node_size_by
  node_label_color_by <- if(is.null(node_label_color_by)) NULL else node_label_color_by
  
  edge_color_fix <- if(is.null(edge_color_fix)) "black" else edge_color_fix #defaults
  node_color_fix <- if(is.null(node_color_fix)) "cornflowerblue" else node_color_fix #defaults
  node_size_fix <- if(is.null(node_size_fix)) "3" else node_size_fix #defaults
  node_label_color_fix <- if(is.null(node_label_color_fix)) "navy" else node_label_color_fix #defaults
  
  ## edge geom  
  if(draw_edges){cur_geom_edge <- geom_edge_link(color = edge_color_fix)
  }else{cur_geom_edge <- NULL}
  
  ## node geom
  if(!is.null(node_color_by)){color = as.character(vertex_attr(graph, node_color_by)) #node color
  }else{color = as.character(node_color_fix)}
  if(!is.null(node_size_by)){size = vertex_attr(graph, node_size_by) #node size
  }else{size = as.character(node_size_fix)}
  
  cur_geom_node <- geom_node_point(aes_(color = color, size = size))
  
  #specify vertical layout with sugiyama
  LO <- layout.sugiyama(graph,vertex_attr(graph,"length"))
  
  #plot
  p <- ggraph(graph, layout = LO$layout)+
    cur_geom_edge+
    cur_geom_node+
    guides(color = "none", size = guide_legend(as.character(node_size_by)))+
    theme_blank()
  
  #node geom post-processing
  if (is.null(node_color_by)) {
    names(node_color_fix) <- as.character(node_color_fix)
    p <- p + scale_color_manual(values = node_color_fix)
  }
  if (is.null(node_size_by)) {
    p <- p + scale_size_manual(values = as.numeric(node_size_fix), 
                               guide = "none")}
  
  ## node geom label
  if(!is.null(node_label_color_by)){color_label = vertex_attr(graph, node_label_color_by) #node geom label
  }else{color_label = as.character(node_label_color_fix)}
  
  if(node_label_repel){
    if(!is.null(node_label_color_by)){
      cur_geom_node_label <- geom_node_label(aes_(color = color_label, label = vertex_attr(graph, "name")), repel = TRUE)
      p <- p + cur_geom_node_label
    }else{
      cur_geom_node_label <- geom_node_label(aes_(label = vertex_attr(graph, "name")), color = color_label, repel = TRUE)
      p <- p + cur_geom_node_label 
    }
  }  
  return(p)
}
