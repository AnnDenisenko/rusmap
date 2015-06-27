#' It's plot moskovskaya oblast' 
#'
#' Combination of two or more layers of information on one map.
#'
#' Map of Moskovskaya oblast' with railway lines. 
#' @param layer - the initial SpatialPolygonsDataFrame file.
#' 
#' add - vector of additional SpatialPolygonsDataFrame file. The size of each image should 
#' be comparable with size of the initial image. 
#' 
#' n - number of additional layers
#'
#' clr - vector with colours of all additional images.   
#' @return Image of mosobl with addings. 
#' @export
#' @examples  mosobl <- readOGR("mosobl", "boundary-polygon")
#' railway_mo <- readOGR("mosobl", "railway-line")
#' waterline_mo <- readOGR("mosobl", "water-line")
#' additional_files <- c(railway_mo, waterline_mo)
#' colour = c("red", "blue")
#' combine(layer = mosobl, add = additional_files, n = 2, clr = colour)


combine <- function(layer, add, n, clr){
  layer_f = fortify(layer)
  layer$id = as.character(1:nrow(layer@data))
  layer_f = left_join(layer_f, layer@data)
  
  first <- ggplot(layer_f, aes(long, lat, group = group, fill=NAME)) +  
    
    geom_polygon(data = layer_f, aes(long,lat), 
                 fill = NA, 
                 color = "black",
                 size=0.1)+ 
    theme(axis.line=element_blank(),   
          axis.text.x=element_blank(), 
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank()) 
  numb = nrow(add)
  add_f=NULL
  for(i in 1:n){
    add_f[[i]]= fortify(add[[i]])
    add[[i]]$id = as.character(1:nrow(add[[i]]@data))
    add_f[[i]] = left_join(add_f[[i]], add[[i]]@data)
    first = first + geom_polygon(data = add_f[[i]], aes(long,lat), 
                                 fill=NA, 
                                 color = clr[i],
                                 size=0.1)
  } 
  return(first)
}

