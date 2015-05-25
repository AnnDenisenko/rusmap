#' It's plot single region by it's ID
#'
#' Plotting region by ID
#'
#' Build part of map by ID
#' @param n - region's ID number,
#' layer - SpatialPolygonsDataFrame file  
#' @return Image of region
#' @export

plotRegbyID <- function(n,layer){
  return(plot(map[map$ID_1 == n,]))
}
