#' Several chosen regions
#'
#' Computes the chosen regions in JPEG file 
#'
#' Each region will be imaged in single cell of matrix of a given size. 
#' @param layer - SpatialPolygonsDataFrame file.
#' regions - a character vector naming one or more regions, as in layer.
#' outname - name of JPEG file.
#' q - quality of image.  
#' x - number of images in the vertical.
#' y - number of images in the horizontal.
#' t - size of text.
#' @return JPEG file with chosen regions. 
#' @export
#' @examples 
#' map <- readOGR(dsn = "data", layer = "map_1")
#' reg.matrix(layer = map, regions = c("Tuva", "Kemerovo"), outname = "First.jpeg")
#' reg.matrix(layer = map, regions = c("Tuva", "Kemerovo"), outname = "Second.jpeg", q = 50)
#' reg.matrix(layer = map, regions = c("Tuva", "Kemerovo"), outname = "Third.jpeg", q = 200, x = 10)

reg.matrix <- function(layer, regions, 
                           outname = "matrix_with_regions.jpeg", q = 1000,
                           x = 3, y = 3, t=5){
  jpeg(filename = outname, width = 1200, height = 600, quality = q)  
  pushViewport(viewport(layout = grid.layout(x, y)))
  
  j=0
  
  map_f = fortify(layer)
  layer$id = as.character( c(1:nrow(layer@data)))
  map_f = left_join(map_f, layer@data) 
  
  for (i in regions)
  {
    j=j+1
    msk = subset(map_f, NAME_1==i)
    cnames = aggregate(cbind(long, lat, group) ~ NAME_1, data=msk, 
                        FUN=function(x)mean(range(x)))
    
    p = ggplot(msk, aes(long, lat, group = group)) +
      geom_polygon(fill="gold", alpha = 0.6, linetype=0) +
      coord_equal() +
      geom_polygon(data = msk, aes(long,lat), 
                   fill=NA, 
                   color = "darkblue",
                   size=0.1)+     
      geom_text(data=cnames, aes(long, lat, group = group, label = NAME_1), size=t) + 
      theme(axis.line=element_blank(),   
            axis.text.x=element_blank(), 
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank())
      
    #Место на листе каждой картинки
    print(p, vp = vplayout(floor((j-1)/y)+1, (j)%%y+((j)%%y==0)*y))
  }
  #Выгрузим картинку. 
  dev.off()
}

#' Several chosen regions with smaller administrarive division
#'
#' Computes the chosen regions in JPEG file 
#'
#' Each region will be imaged in single cell of matrix of a given size. 
#' @param layer_2 - SpatialPolygonsDataFrame file with smaller administrative division.
#' regions - a character vector naming one or more regions, as in layer_2.
#' outname - name of JPEG file.
#' q - quality of image.  
#' x - number of images in the vertical.
#' y - number of images in the horizontal.
#' t - size of text.
#' @return JPEG file with chosen regions. 
#' @export
#' @examples 
#' map_2 <- readOGR(dsn = "data", layer = "map_2")
#' reg.matrix.small(layer_2 = map_2, regions = c("Tuva", "Tomsk"), outname = "First.jpeg")
#' reg.matrix.small(layer_2 = map_2, regions = c("Tuva", "Tomsk"), outname = "Second.jpeg", q = 50)
#' reg.matrix.small(layer_2 = map_2, regions = c("Tuva", "Tomsk"), outname = "Third.jpeg", q = 200, x = 10)

reg.matrix.small <- function(layer_2, regions, 
                           outname = "matrix_with_regions.jpeg", q = 60000,
                           x = 3, y = 3, t = 2){
  jpeg(filename = outname, width = 1200, height = 600, quality = q)  
  pushViewport(viewport(layout = grid.layout(x, y)))
  
  j=0
  
  map_f = fortify(layer_2)
  layer_2$id = as.character( c(1:nrow(layer_2@data)))
  map_f = left_join(map_f, layer_2@data) 
  
  for (i in regions)
  {
    j=j+1
    msk = subset(map_f, NAME_1==i)
    cnames = aggregate(cbind(long, lat, group) ~ NAME_2, data=msk, 
                       FUN=function(x)mean(range(x)))
    
    p = ggplot(msk, aes(long, lat, group = group)) +
      geom_polygon(fill="gold", alpha = 0.6, linetype=0) +
      coord_equal() +
      geom_polygon(data = msk, aes(long,lat), 
                   fill=NA, 
                   color = "darkblue",
                   size=0.1)+     
      geom_text(data=cnames, aes(long, lat, group = group, label = NAME_2), size=t) + 
      theme(axis.line=element_blank(),   
            axis.text.x=element_blank(), 
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank())
    #Место на листе каждой картинки
    print(p, vp = vplayout(floor((j-1)/y)+1, (j)%%y+((j)%%y==0)*y))
  }
  #Выгрузим картинку. 
  dev.off()
}

