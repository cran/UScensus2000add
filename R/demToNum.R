demToNum<-function(sp.object=NULL){
	### Check to make sure the object is sp-class
	if(class(sp.object)[1]!="SpatialPolygonsDataFrame")
		cat("Incompatible class!")
	
	b<-which(names(sp.object)=="pop2000") ## Grab first index
	e<-ncol(sp.object@data) ## Grab last index
	temp<-apply(sp.object@data[,b:e],2,as.numeric) ## Convert
	sp.object@data[,b:e]<-temp	## Replace
	sp.object ## Return
	}