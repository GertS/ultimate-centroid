#' find the centroid
#' 
#' Takes in a spatial polygon
#' @param pol One or more polygons to find
#' @param ultimate optional Boolean, TRUE = find polygon furthest away from centroid. False = ordinary centroid

library(rgeos)

centroid <- function(pol,ultimate=TRUE,iterations=5){
  if (ultimate){
    new_pol_list <- list()
    # For every polygon do this:
    for (i in 1:length(pol)){
      width <- -10
      area <- gArea(pol[i,])
      centr <- pol[i,]
      wasNull <- FALSE
      for (j in 1:iterations){
        if (!wasNull){ # stop when buffer polygon was alread too small
          centr_new <- gBuffer(centr,width=width)
          # if the buffer has a negative size:
          substract_width <- width/4 
          while (is.null(centr_new)){
            width <- width-substract_width
            centr_new <- gBuffer(centr,width=width)
            wasNull <- TRUE
          }
          new_area <- gArea(centr_new)
          #linear regression:
          slope <- (new_area-area)/width
          #aiming at quarter of the area for the new polygon
          width <- (area/4-area)/slope
          #preparing for next step:
          area <- new_area
          centr<- centr_new
        }
      }
      #take the biggest polygon:
      d <- disaggregate(centr)
      if (length(d)>1){
        biggest_area <- gARea(d[1,])
        which_pol <- 1                             
        for (k in 2:length(d)){
          if (gARea(d[k,]) > biggest_area){
            biggest_area <- gARea(d[k,])
            which_pol <- k
          }
        } 
        centr <- d[which_pol,]
      }
      #add to class polygons:
      print (class(centr))
      new_pol_list[i] <- centr
    }
    new_pol <- rbind(new_pol_list)
    centroids <- gCentroid(pol,byid=TRUE)
  }else{
    centroids <- gCentroid(pol,byid=TRUE)  
  }  
  return(centroids)
}