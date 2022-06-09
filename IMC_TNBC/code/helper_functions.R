# 1. Spatial context detection functions

# 1.1 detectSpatialContext - downstream of buildSpatialGraph and aggregateNeighbors
detectSpatialContext <- function(object, 
                                 entry = NULL, 
                                 threshold = 0.9,
                                 name = NULL){

  entry <- ifelse(is.null(entry), "aggregatedNeighbors", entry) #default
  name <- ifelse(is.null(name), "spatial_context", name) #default
  
  .valid.detectSpatialContext.input(object, entry, threshold, name) #validity check
  
  cur_dat <- colData(sce)[,entry]
  
  out_dat <- apply(cur_dat, 1, function(x){
    
    out <- cumsum(sort(x, decreasing = TRUE))
    
    return(paste(sort(as.numeric(names(out[seq_len(sum(out < threshold) + 1)]))), collapse = "_"))
  })
  
  colData(object)[[name]] <- out_dat
  return(object)
}

# 1.2 buildSpatialContextGraph 
buildEdgeList <- function(object, 
                          entry = "spatial_context",
                          img_id = "sample_id",
                          combined = NULL){
  
  combined <- ifelse(is.null(combined), TRUE, combined) #default
  
  .valid.buildEdgeList.input(object, entry, img_id, combined) #validity check
  
  #data  
  data <- colData(object)[,colnames(colData(sce)) %in% c(entry,img_id)] %>% table() %>% as.data.frame
  data_wide <- data %>% pivot_wider(values_from = Freq, names_from = entry) %>% column_to_rownames(img_id)
  
  edges <- if(combined == TRUE){ #Option 1: For all images combined  
    list <- str_split(unique(data$spatial_context), "_")
    list_length <- sapply(list, length)
    edges <- .createEdgeList(list, list_length) #hidden function
    return(edges)
    
  }else{ #Option 2: For each img_id separately
    cur_dat <- apply(data_wide, 1, function(x){
      cur <- x
      names(cur) <- colnames(data_wide)
      dat <- names(cur[cur != 0])
      return(dat)
    })
    
    edges <- lapply(cur_dat, function(z){ 
      list <- str_split(z, "_")
      list_length <- sapply(list, length)
      edges <- .createEdgeList(list, list_length) #hidden function
      return(edges)
    })
    
    edges <- do.call(rbind,edges)
    edges$sample_id <- paste0(str_split(rownames(edges),"\\.", simplify = TRUE)[,1],".",str_split(rownames(edges),"\\.", simplify = TRUE)[,2])
    rownames(edges) <- NULL
    return(edges)
  }
}

#1.3 plotSpatialContextGraph
plotSpatialContext <- function(edges, 
                                    object,
                                    entry = "spatial_context",
                                    img_id = "sample_id",
                                    combined = NULL, 
                                    #build graph
                                    directed = TRUE,
                                    #nodes
                                    node_color_by = NULL, #c("name","freq","n_samples"),
                                    node_size_by = NULL, #c("freq","n_samples"),
                                    node_color_fix = NULL,
                                    node_size_fix = NULL,
                                    #node labels
                                    node_label_repel = TRUE,
                                    node_label_color_by = NULL, #c("name","freq","n_samples"),
                                    node_label_color_fix = NULL,  
                                    #plot graph - edges
                                    draw_edges = TRUE,
                                    edge_color_fix = NULL){
  
  .valid.plotSpatialContext.input(edges, object, entry, img_id, combined, directed, node_color_by, node_size_by, node_color_fix, node_size_fix, node_label_repel, node_label_color_by, node_label_color_fix, draw_edges, edge_color_fix)
  
  #data
  data <- colData(object)[,colnames(colData(sce)) %in% c(entry,img_id)] %>% table() %>% as.data.frame
  
  if(combined == TRUE){ # For combined samples
    anno <- data.frame(spatial_context = unique(data[,entry]), 
                       length = listLen(str_split(unique(data[,entry]),"_")),
                       freq = data %>% group_by_at(entry) %>% summarise(sum = sum(Freq)) %>% pull(sum), 
                       n_samples = data %>% group_by_at(entry) %>% filter(Freq != 0) %>% count() %>% pull(n)
    )
    
    g <- graph_from_data_frame(edges, directed = directed,vertices = anno)
    
    #Plot using ggraph
    .generatePlot(graph = g, node_color_by, node_size_by, node_color_fix, node_size_fix, node_label_repel, node_label_color_by, node_label_color_fix, draw_edges, edge_color_fix) #hidden function
    
  }else{ # For multiple SC graphs
    anno <- split(data %>% select(-as.name(img_id)), f = data[,img_id])

    anno <- lapply(anno, function(x){
      cur_anno <- x %>% filter(Freq !=0)
      cur_anno$length <- listLen(str_split(cur_anno[,entry],"_"))
      return(cur_anno)
    })
    
    #edges
    edges_list <- split(edges %>% select(-as.name(img_id)), f = edges[,img_id])
    
    #generate graph
    g <- mapply(function(x,y){
      g <- graph_from_data_frame(x, directed = TRUE, vertices = y)
      #specify vertical layout using sugiyama
      return(g)}, edges_list, anno)
    
    #generate plots
    all_plots <- lapply(g, function(x){
      p <- .generatePlot(graph = x, node_color_by, node_size_by, node_color_fix, node_size_fix, node_label_repel, node_label_color_by, node_label_color_fix, draw_edges, edge_color_fix) #hidden function
      return(p)
      })
      
    plot_grid(plotlist = all_plots) 
  }
}
