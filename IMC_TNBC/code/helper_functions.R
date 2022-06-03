# 1. Spatial context detection functions

# 1.1 downstream of spatialgraph and aggregate neighbors
detectSpatialContext <- function(object, 
                                 entry = NULL, 
                                 threshold = 0.9,
                                 name = NULL){
  
  entry <- ifelse(is.null(entry), "aggregatedNeighbors", entry)
  name <- ifelse(is.null(name), "spatial_context", name)
  
  cur_dat <- colData(sce)[,entry]
  
  out_dat <- apply(cur_dat, 1, function(x){
    
    out <- cumsum(sort(x, decreasing = TRUE))
    
    return(paste(sort(as.numeric(names(out[seq_len(sum(out < threshold) + 1)]))), collapse = "_"))
  })
  
  colData(object)[[name]] <- out_dat
  return(object)
}

# 1.2 Including spatialgraph and aggregate neighbors

# not sure whether this is relevant? 


# 2. buildSpatialContextGraph 

buildEdgeList <- function(object, 
                          entry = "spatial_context",
                          img_id = "sample_id",
                          combined = NULL
                          ){
#defaults
#combined <- ifelse(isTRUE(combined), TRUE, FALSE) #decide in which way to generate edgelist

registerDoMC(cores=detectCores()-2) #perform foreach loop on multiple cores with %dopar%

#data  
data <- colData(object)[,colnames(colData(sce)) %in% c(entry,img_id)] %>% table() %>% as.data.frame

data_wide <- data %>% pivot_wider(values_from = Freq, names_from = entry) %>% column_to_rownames(img_id)

edges <- if(combined == TRUE){
  
  #Option 1: For all samples combined  
  list <- str_split(unique(data$spatial_context), "_")
  list_length <- sapply(list, length)
  
  edges <- foreach(i = seq_along(list), .combine = "rbind")%dopar%{
    
    list_options <- list[length(list[[i]])+1 == list_length]
    
    if(length(list_options) != 0){
      
      list_select <- list_options[sapply(list_options, function(x){length(intersect(x,list[[i]])) == length(list[[i]])})]
      
      if(length(list_select) != 0){ 
        data.frame("from" = sapply(list[i], paste, collapse = "_"),
                   "to" = sapply(list_select, paste, collapse = "_")
        )
      } else {
        NULL
      } 
    } else {
      NULL
    }
  } 
  
}else{
  #Option 2: For each sample_id separately
  
  cur_dat <- apply(data_wide, 1, function(x){
    
    cur <- x
    
    names(cur) <- colnames(data_wide)
    
    dat <- names(cur[cur != 0])
    
    return(dat)
  })
  
  #Create list with edge_list for each sample_id
  cur_dat_final <- lapply(cur_dat, function(x){
    list <- str_split(x, "_")
    list_length <- sapply(list, length)
    
    edges <- foreach(i = seq_along(list), .combine = "rbind")%dopar%{
      
      list_options <- list[length(list[[i]])+1 == list_length]
      
      if(length(list_options) != 0){
        
        list_select <- list_options[sapply(list_options, function(x){length(intersect(x,list[[i]])) == length(list[[i]])})]
        
        if(length(list_select) != 0){ 
          data.frame("from" = sapply(list[i], paste, collapse = "_"),
                     "to" = sapply(list_select, paste, collapse = "_")
          )
        } else {
          NULL
        } 
      } else {
        NULL
      }
    }
    
    return(edges)
  }) 
  cur_dat_final <- do.call(rbind,cur_dat_final)
  cur_dat_final$sample_id <- paste0(str_split(rownames(cur_dat_final),"\\.", simplify = TRUE)[,1],".",str_split(rownames(cur_dat_final),"\\.", simplify = TRUE)[,2])
  rownames(cur_dat_final) <- NULL
  return(cur_dat_final)
}

}

#3. plotSpatialContextGraph
plotSpatialContextGraph <- function(edges, 
                                    object,
                                    combined,
                                    entry = "spatial_context",
                                    img_id = "sample_id",
                                    node_size_by = c("Freq","n_samples")){
  #data
  data <- colData(object)[,colnames(colData(sce)) %in% c(entry,img_id)] %>% table() %>% as.data.frame
  
  #node colors
  col <- list(name = colorRampPalette(brewer.pal(9, "Set1"))(length(sort(unique(unfactor(data[,entry]))))))
  col_node <- col[[1]]
  names(col_node) = sort(unique(unfactor(data[,entry])))
  col_node <- list(col_node)
  names(col_node) <- "name"
  
  if(combined == TRUE){ #For combined samples
    anno <- data.frame(spatial_context = unique(data[,entry]), 
                       length = listLen(str_split(unique(data[,entry]),"_")),
                       Freq = data %>% group_by_at(entry) %>% summarise(sum = sum(Freq)) %>% pull(sum), 
                       n_samples = data %>% group_by_at(entry) %>% filter(Freq != 0) %>% count() %>% pull(n)
    )
    
    g <- graph_from_data_frame(edges, directed = TRUE,vertices = anno)
    
    #Plot using ggraph
    ggraph(g, layout = "sugiyama")+
      geom_edge_link()+
      geom_node_point(aes_(color = V(g)$name, size = as.name(node_size_by)))+
      guides(color="none")+
      geom_node_label(aes(color = name,label = name), repel = TRUE)+
      theme_blank()+
      scale_color_manual(values = col_node$name)
    
  }else{ ### For multiple SC graphs
    
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
      p <- ggraph(x, layout = "sugiyama")+
        geom_edge_link()+
        geom_node_point(aes(color = name, size = Freq))+
        guides(color="none")+
        geom_node_label(aes(color = name,label = name), repel = TRUE)+
        scale_color_manual(values = col_node$name)+
        ggtitle(names(g))+
        theme_blank()
      return(p)}
    )
    
    plot_grid(plotlist = all_plots) 
    
  }}