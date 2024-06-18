# Nearest neighbor search for k-means app
library(dplyr)
library(FNN)

#' Title
#'
#' @param data Data frame to search for nearest neighbors
#' @param points Data frame of the initial points to find nearest neighbors for
#'
#' @return A data frame of points that can be used as the initial center
#' @export
findNeighbors <- function(data, points){
  restrictData <- data[, 1:ncol(points)]

  nearest <- get.knnx(
    data = restrictData,
    query = points,
    k = 10
  )

  # Check for duplicates
  neighborIndices <- nearest$nn.index[, 1]
  count <- 1
  while (anyDuplicated(neighborIndices) > 0) {
    dups <- duplicated(neighborIndices)
    neighborIndices[which(dups)] <- nearest$nn.index[which(dups), 1 + count]
    count <- count  + 1
  }

  newCenters <- data[neighborIndices, ]

  return(newCenters)

}
#'
#' @examples
#' findNeighbors(data = clusteredData(), points = userCentroids())