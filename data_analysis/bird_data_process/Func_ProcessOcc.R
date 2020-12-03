"
Aim: Helping unctions for organizing bird observation data
"

CreateShape = function(data) {
  
  library(sp)
  
  data$X_obs=data$x
  data$Y_obs=data$y
  
  shp=data
  coordinates(shp)=~X_obs+Y_obs
  proj4string(shp)<- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
  
  return(shp)
  
}

ConvertPolytoDf = function(kmsquares_poly) {
  
  library(plyr)
  library(dplyr)
  library(ggplot2)
  
  kmsquares_poly@data$id = rownames(kmsquares_poly@data)
  kmsquares_poly.points = fortify(kmsquares_poly, region="id")
  kmsquares_poly.df = join(kmsquares_poly.points, kmsquares_poly@data, by="id")
  
  as.numeric(kmsquares_poly.df$KMHOK)
  colnames(kmsquares_poly.df)[colnames(kmsquares_poly.df)=="KMHOK"] <- "kmsquare"
  kmsquares_poly.df=ddply(kmsquares_poly.df,~kmsquare+X+Y,summarise,sum=length(kmsquare))
  
  return(kmsquares_poly.df)
}

Gen_absence = function(GrW_atl_abs,spname,outname,nofsamp) {
  
  GrW_atl_abs_lgn8 <- subset(GrW_atl_abs, landcover_lgn8 %in% c(16,17,30,322,332,41,42))
  
  GrW_atl_abs_lgn8=GrW_atl_abs_lgn8[GrW_atl_abs_lgn8$Jaar!=2019,]
  
  GrW_atl_abs_b=st_buffer(st_as_sf(GrW_atl_abs_lgn8), 500)
  GrW_atl_abs_b_sp <- sf:::as_Spatial(GrW_atl_abs_b)
  
  GrW_atl_abs_b_sp_union <- unionSpatialPolygons(GrW_atl_abs_b_sp,rep(1, length(GrW_atl_abs_b_sp)))
  
  GrW_genabs=spsample(GrW_atl_abs_b_sp_union,n=nofsamp,"random")
  GrW_genabs.df=as.data.frame(GrW_genabs)
  
  GrW_genabs.df$species <- spname
  GrW_genabs.df$occurrence <- 0
  
  GrW_genabs.df_shp=CreateShape(GrW_genabs.df)
  raster::shapefile(GrW_genabs.df_shp, paste(outname,".shp",sep=""),overwrite=TRUE)
  
  GrW_genabs_20 <- spatialEco:::subsample.distance(GrW_genabs.df_shp,size=nofsamp-1,d=20,replacement=FALSE) 
  raster::shapefile(GrW_genabs_20, paste(outname,"_20.shp",sep=""),overwrite=TRUE)
}

thin.max <- function(x, cols, npoints){
  #Create empty vector for output
  inds <- vector(mode="numeric")
  
  #Create distance matrix
  this.dist <- as.matrix(dist(x[,cols], upper=TRUE))
  
  #Draw first index at random
  inds <- c(inds, as.integer(runif(1, 1, length(this.dist[,1]))))
  
  #Get second index from maximally distant point from first one
  #Necessary because apply needs at least two columns or it'll barf
  #in the next bit
  inds <- c(inds, which.max(this.dist[,inds]))
  
  while(length(inds) < npoints){
    #For each point, find its distance to the closest point that's already been selected
    min.dists <- apply(this.dist[,inds], 1, min)
    
    #Select the point that is furthest from everything we've already selected
    this.ind <- which.max(min.dists)
    
    #Get rid of ties, if they exist
    if(length(this.ind) > 1){
      print("Breaking tie...")
      this.ind <- this.ind[1]
    }
    inds <- c(inds, this.ind)
  }
  
  return(x[inds,])
}

Gen_absence2 = function(GrW_atl_abs,spname,outname,nofsamp) {
  
  GrW_atl_abs_b=st_buffer(st_as_sf(GrW_atl_abs), 500)
  GrW_atl_abs_b_sp <- sf:::as_Spatial(GrW_atl_abs_b)
  
  GrW_atl_abs_b_sp_union <- unionSpatialPolygons(GrW_atl_abs_b_sp,rep(1, length(GrW_atl_abs_b_sp)))
  
  GrW_genabs=spsample(GrW_atl_abs_b_sp_union,n=nofsamp,"random")
  GrW_genabs.df=as.data.frame(GrW_genabs)
  
  GrW_genabs.df$species <- spname
  GrW_genabs.df$occurrence <- 0
  
  GrW_genabs.df_shp=CreateShape(GrW_genabs.df)
  raster::shapefile(GrW_genabs.df_shp, paste(outname,".shp",sep=""),overwrite=TRUE)
  
  GrW_genabs_20 <- spatialEco:::subsample.distance(GrW_genabs.df_shp,size=nofsamp-1,d=20,replacement=FALSE) 
  raster::shapefile(GrW_genabs_20, paste(outname,"_20.shp",sep=""),overwrite=TRUE)
}