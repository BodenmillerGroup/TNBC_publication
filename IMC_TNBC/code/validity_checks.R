#Validity checks

.valid.detectSpatialContext.input <- function(object,
                                              entry,
                                              threshold,
                                              name){
  if (!is(object, "SingleCellExperiment")) {
    stop("'object' needs to be a SingleCellExperiment object.")
  }
  if (!entry %in% names(colData(object))) {
    stop("'entry' not in 'colData(object)'.")
  }
  if (!(is.numeric(threshold) & (0 <= threshold && threshold <= 1)) 
      ) {
    stop("'threshold' needs to be a single numeric between 0-1.")
  }
  if (!is.character(name)) {
    stop("'name' has to be a character'.")
  }
}

.valid.buildEdgeList.input <- function(object,
                                       entry,
                                       img_id,
                                       combined){
  
  if (!is(object, "SingleCellExperiment")) {
    stop("'object' needs to be a SingleCellExperiment object.")
  }
  if (!entry %in% names(colData(object))) {
    stop("'entry' not in 'colData(object)'.")
  }
  if (!img_id %in% names(colData(object))) { 
    stop("'img_id' not in 'colData(object)'.")
  }
  if (!is.logical(combined)) {
    stop("'combined' has to be logical'.")
  }
}

#1.3 plotSpatialContextGraph
.valid.plotSpatialContext.input <- function(edges, 
                                            object,
                                            entry,
                                            img_id,
                                            combined, 
                                            directed,
                                            node_color_by, 
                                            node_size_by,
                                            node_color_fix,
                                            node_size_fix,
                                            node_label_repel,
                                            node_label_color_by,
                                            node_label_color_fix,  
                                            draw_edges,
                                            edge_color_fix){

if (!is.data.frame(edges)){
  stop("'edges' has to be a data.frame.")
}

if (!is(object, "SingleCellExperiment")) {
  stop("'object' needs to be a SingleCellExperiment object.")
}

if (!entry %in% names(colData(object))) {
  stop("'entry' not in 'colData(object)'.")
}
if (!img_id %in% names(colData(object))) { 
  stop("'img_id' not in 'colData(object)'.")
}

if (!is.logical(combined)) {
  stop("'combined' has to be logical'.")
}

if(combined == TRUE){
  if(!ncol(edges) == 2){
    stop("When 'combined' is set to TRUE, ncol('edges') has to be 2.")
  }}else{
    if(!ncol(edges) == 3){
      stop("When 'combined' is set to FALSE, ncol('edges') has to be 3.")
    }}

if (!is.logical(directed)) {
  stop("'directed' has to be logical'.")
}

if (!is.null(node_color_by) &&
    (!node_color_by %in% c("name","Freq","n_samples"))){
  stop("'node_color_by' has to be one off 'name','Freq','n_samples'.")
}

if (!is.null(node_size_by) &&
    (!node_size_by %in% c("Freq","n_samples"))){
  stop("'node_size_by' has to be 'Freq' or 'n_samples'.")
}

if (!is.null(node_label_color_by) && 
    (!node_label_color_by %in% c("name","Freq","n_samples"))){
  stop("'node_label_color_by' has to be one off 'name','Freq','n_samples'.")
}

if (!is.logical(node_label_repel)) {
  stop("'node_label_repel' has to be logical'.")
}

if (!is.logical(draw_edges)) {
  stop("'draw_edges' has to be logical'.")
}

if (!is.null(node_color_fix) && 
    (!is.character(node_color_fix))){
  stop("'node_color_fix' has to be a character'.")
}

if (!is.null(node_size_fix) &&
    (!is.character(node_size_fix))){
  stop("'node_size_fix' has to be a character'.")
}

if (!is.null(node_label_color_fix) &&
    (!is.character(node_label_color_fix))){
  stop("'node_label_color_fix' has to be a character'.")
}

if (!is.null(edge_color_fix) &&
    (!is.character(edge_color_fix))){
  stop("'edge_color_fix' has to be a character'.")
}
  
if(!is.null(node_color_by) &&
   (!is.null(node_color_fix))){
  stop("'node_color_by' and 'node_color_fix' can not be defined at the same time.")
}

if(!is.null(node_label_color_by) &&
  (!is.null(node_label_color_fix))){
stop("'node_label_color_by' and 'node_label_color_fix' can not be defined at the same time.")
  }  
  
if(!is.null(node_size_by) &&
  (!is.null(node_size_fix))){
stop("'node_size_by' and 'node_size_fix' can not be defined at the same time.")
  }      

if(combined == FALSE && 
   (node_size_by == "n_samples")){
  stop("When 'combined' is set to FALSE, 'node_size_by' can not be defined as 'n_samples'")
}
}

