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


.valid.plotSpatialContext.input <- function(){}




