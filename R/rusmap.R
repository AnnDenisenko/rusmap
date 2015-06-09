#' It's plot single region by it's ID
#'
#' Plotting region by ID
#'
#' Build part of map by ID
#' @param n - region's ID number,
#' layer - SpatialPolygonsDataFrame file  
#' @return Image of region
#' @export
#' @examples map <- readOGR(dsn = "data", layer = "map_1")
#' plotRegbyID(n=1, layer = map)

plotRegbyID <- function(n,layer){
  return(plot(map[map$ID_1 == n,]))
}

#' Find regyons by ID
#'
#' This function looking for regyons, defined by the ID, at the map. 
#'
#' Built several regyons by there IDs.
#' @param vec - vector with IDs of regions,
#' layer - SpatialPolygonsDataFrame file,
#' colour - colour of region  
#' @return Image of map with colourful selected regions
#' @export
#' @examples v <- c(1,2,3)
#' map <- readOGR(dsn = "data", layer = "map_1")
#' findRegbyID(vector = v, layer = map, colour = "green")

findRegbyID <- function(vector, layer, colour = "red"){
  plot(layer)
  for(i in vector){
    plot(layer[layer$ID_1 ==i,], col = colour, add=TRUE)
  }
}

