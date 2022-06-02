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
                          combined = TRUE
                          ){
#defaults
combined <- ifelse(isTRUE(combined), TRUE, FALSE) #decide in which way to generate edgelist

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

# 3. plotSpatialContextGraph

plotSpatialContextGraph <- function(object, #edge-lists
                                    anno = NULL, #annotation dataframe - if annotation dataframe should be constructed by the function? 
                                    combined = TRUE, #whether or not separate graphs per sample_id should be constructed
                                    #node attributes
                                    node_color_by = NULL, 
                                    node_shape_by = NULL, 
                                    node_size_by = NULL, 
                                    node_color_fix = NULL,
                                    node_shape_fix = NULL,
                                    node_size_fix = NULL,
                                    #node label attributes 
                                    node_label = TRUE,
                                    node_label_color_by = NULL,
                                    node_label_size_by = NULL,
                                    node_label_color_fix = NULL,
                                    node_label_size_fix = NULL,
                                    node_label_repel = TRUE, 
                                    #graph layout
                                    layout = sugiyama, #not sure
                                    #general plot
                                    color_guide = FALSE, #not sure - should be TRUE when node_label = FALSE
                                    theme = blank, #not sure
                                    ){}

g <- graph_from_data_frame(edges, directed = FALSE,vertices = anno)

layout.
#specify vertical layout using sugiyama
LO <- layout.sugiyama(g, V(g)$length)

#Plot using ggraph
library(ggraph)

ggraph(g, layout = LO$layout)+
  geom_edge_link()+
  geom_node_point(aes(color = name, size = sum))+
  guides(color=FALSE)+
  geom_node_label(aes(color = name,label = name, size = length), repel = TRUE)+
  theme_blank()

