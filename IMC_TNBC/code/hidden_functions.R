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
                          #nodes
                          node_color_by = c("name","freq","n_samples"),
                          node_size_by = c("freq","n_samples"),
                          node_color_fix = NULL,
                          node_size_fix = NULL,
                          #node labels
                          node_label_repel = TRUE,
                          node_label_color_by = c("name","freq","n_samples"),
                          node_label_color_fix = NULL,  
                          #plot graph - edges
                          draw_edges = TRUE,
                          edge_color_fix = NULL){
  
  node_color_by <- if(is.null(node_color_by)) NULL else node_color_by
  node_size_by <- if(is.null(node_size_by)) NULL else node_size_by
  node_label_color_by <- if(is.null(node_label_color_by)) NULL else node_label_color_by
  edge_color_fix <- if(is.null(edge_color_fix)) "black" else edge_color_fix
  
  if (!is.null(node_color_fix)){node_color_by <- as.character(node_color_fix)
  } else { node_color_by <- node_color_by }
  if (!is.null(node_size_fix)){ node_size_by <- as.character(node_size_fix)
  } else { node_size_by <- node_size_by }
  if (!is.null(node_label_color_fix)){node_label_color_by <- as.character(node_label_color_fix)
  } else { node_label_color_by <- node_label_color_by }
  
  #edge geom  
  if(draw_edges){
    cur_geom_edge <- geom_edge_link(color = edge_color_fix)
  }else{
    cur_geom_edge <- NULL}
  
  #node geom
  cur_geom_node <- geom_node_point(aes_(color = vertex_attr(g, node_color_by),
                                        size = vertex_attr(g, node_size_by)))
  
  #node_label geom
  if(node_label_repel){
    cur_geom_node_label <- geom_node_label(aes_(color = vertex_attr(g,node_label_color_by), label = vertex_attr(g, "name")), repel = TRUE)
  }else{
    cur_geom_node_label <- NULL
  }
  
  p <- ggraph(graph, layout = "sugiyama")+
    cur_geom_edge+
    cur_geom_node+
    cur_geom_node_label+
    guides(color = "none")
  p
}

.postProcessPlot <- function(){} #see Nils - use for fix colors or sizes





