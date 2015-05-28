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

findRegbyID <- function(vector, layer){
  plot(layer)
  for(i in vector){
    plot(layer[layer$ID_1 ==i,], col = "red", add=TRUE)
  }
}

#' It's colour map by some parameter
#'
#' Colouring by parameter
#'
#' Map with graduate colour
#' @param par - chosen parameter,
#' layer_f - data frame file  
#' @return Image of map with 
#' @export

colby <- function(par,layer){
  ggplot(layer_f, aes(long, lat, group = group, fill = par)) +
    geom_polygon() +
    coord_equal() +
    # For white lines
    ggtitle("MAP by par") + geom_polygon(data = layer_f, aes(long,lat), 
                                    fill=NA, 
                                    color = "darkblue",
                                    size=0.1)+ 
    
    # If we don't want text
    theme(axis.line=element_blank(),   
          axis.text.x=element_blank(), 
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank())
   + scale_fill_gradient(low = "lightblue", high = "blue")
}