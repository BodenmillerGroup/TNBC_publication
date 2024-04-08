setImageType('FLUORESCENCE')
setChannelNames("DAPI","CK5","CK19","SOX9")
selectAnnotations()
runPlugin('qupath.imagej.detect.cells.WatershedCellDetection', '{"detectionImage":"DAPI","requestedPixelSizeMicrons":1.0,"backgroundRadiusMicrons":8.0,"backgroundByReconstruction":false,"medianRadiusMicrons":0.0,"sigmaMicrons":1.5,"minAreaMicrons":20.0,"maxAreaMicrons":200.0,"threshold":400.0,"watershedPostProcess":true,"cellExpansionMicrons":5.0,"includeNuclei":true,"smoothBoundaries":true,"makeMeasurements":true}')
runObjectClassifier("CK5_pos", "CK19_pos", "SOX9_pos")