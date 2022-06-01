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

# relevant? 


# 2. buildSpatialContextGraph 

buildSpatialContextGraph  <- function(object, 
                                 entry = NULL, 
                                 threshold = 0.9,
                                 name = NULL){}











# 3. plotSpatialContextGraph

plotSpatialContextGraph  <- function(object, 
                                      entry = NULL, 
                                      threshold = 0.9,
                                      name = NULL){}
