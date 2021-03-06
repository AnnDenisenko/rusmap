% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/rusmapwithmatrix.R
\name{reg.matrix}
\alias{reg.matrix}
\title{Several chosen regions}
\usage{
reg.matrix(layer, regions, outname = "matrix_with_regions.jpeg", q = 1000,
  x = 3, y = 3, t = 5)
}
\arguments{
\item{layer}{- SpatialPolygonsDataFrame file.
regions - a character vector naming one or more regions, as in layer.
outname - name of JPEG file.
q - quality of image.
x - number of images in the vertical.
y - number of images in the horizontal.
t - size of text.}
}
\value{
JPEG file with chosen regions.
}
\description{
Computes the chosen regions in JPEG file
}
\details{
Each region will be imaged in single cell of matrix of a given size.
}
\examples{
map <- readOGR(dsn = "data", layer = "map_1")
reg.matrix(layer = map, regions = c("Tuva", "Kemerovo"), outname = "First.jpeg")
reg.matrix(layer = map, regions = c("Tuva", "Kemerovo"), outname = "Second.jpeg", q = 50)
reg.matrix(layer = map, regions = c("Tuva", "Kemerovo"), outname = "Third.jpeg", q = 200, x = 10)
}

