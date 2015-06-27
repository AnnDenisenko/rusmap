#' It's colour map by some parameter
#'
#' Colouring by parameter
#'
#' Map with graduate colouring. 
#' @param 
#' layer - SpatialPolygonsDataFrame file 
#' 
#' data  - csv file with data.
#' 
#' col  - colour of map.
#' 
#' par - chosen parameter, as in data file. 
#' @return Image of map, shaded by choosen parameter.  
#' @export
#' @examples map <- readOGR(dsn = "data", layer = "map_1")
#' pop <- read.csv("csv/population.csv", sep = ';', encoding = "UTF-16")
#' plotcolby(layer = map, data = pop, col = "gold", par = "POP_2002")

plotcolby <- function(layer, data, col = "gold", par = "POP_2002"){
  layer@data = full_join(data, layer@data)
  
  q = cut(layer@data[,par], breaks= c(quantile(layer@data[,par],
                                                 probs = seq(from = 0,to = 1,by = 0.25))), include.lowest=T)
  golds = paste0(col, seq(from = 1, to = 4, by = 1))
  clr_gold = as.character(factor(q, labels = golds))
  
  map = plot(layer, col=clr_gold)
  return(map)
}

#' It's colour map by some parameter
#'
#' Colouring by parameter
#'
#' Map with graduate colour
#' @param par - chosen parameter,
#' layer - SpatialPolygonsDataFrame file 
#' @return Image of map with 
#' @export
#' @examples map <- readOGR(dsn = "data", layer = "map_1")
#' pop <- read.csv("csv/population.csv", sep = ';', encoding = "UTF-16")
#' ggplotcolby(layer = map, data = pop, par = "POP_2002")

ggplotcolby <- function(layer, data, par = "POP_2002"){
  layer@data = full_join(data, layer@data)
  layer_f = fortify(layer)
  layer$id = as.character(1:nrow(layer@data))
  layer_f = left_join(layer_f, layer@data)
  map = ggplot(layer_f, aes(long, lat, group = group, fill = layer_f[,par])) + 
      geom_polygon() +
      coord_equal() +
      geom_polygon(data = layer_f, aes(long,lat), 
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
          panel.background=element_blank()) + 
    scale_fill_gradient(low = "lightblue", high = "blue")
  return(map)
}

