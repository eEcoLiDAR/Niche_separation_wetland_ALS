"
@author: Zsofia Koma, UvA
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