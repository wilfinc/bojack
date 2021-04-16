
# function to use the lookup table to pull grid cell name for easting northing

#' en2grid
#'
#' Returns OSGB grid cell for OSGB easting/northing coordinates.
#'
#' @param data Your data. This should be a dataframe with columns 'easting' and 'northing'. Additional columns are ignored.
#' @param grid.size The desired output grid size, in km. Currently, only 1 and 10 km grid cells are accepted.
#'
#' @return Grid cells exported as vector
#' @export
#'
#' @examples test_data <- data.frame(easting = c(455328, 631490, 427622), northing = c(199154, 275050, 164165), grid10 = c('SU59', 'TM37', 'SU26'), grid1 = c('SU5599', 'TM3175', 'SU2764'))
#'
#' grid2en(test_data$grid10, grid.size = 10)
#'
#' en2grid(test_data, grid.size = 10)


en2grid <- function(data, grid.size = 10){

  if(is.null(data)){stop('No data')}
  if(is.null(data$easting)){stop('Invalid easting value')}
  if(is.null(data$northing)){stop('Invalid northing value')}
  if(!(grid.size %in% c(1, 10))){stop('Invalid \'grid.size\' value. Currently only 1 and 10km grids are supported')}

  out <- rep(NA, nrow(data))

  if(grid.size == 1){
    for(i in 1:nrow(data)){
      te <- floor(data$easting[i]/(grid.size*1000))*(grid.size*1000)
      tn <- floor(data$northing[i]/(grid.size*1000))*(grid.size*1000)
      out[i] <- grid_1$sq1[which(grid_1$east == te & grid_1$north == tn)]
    }
  }

  if(grid.size == 10){
    for(i in 1:nrow(data)){
      te <- floor(data$easting[i]/(grid.size*10000))*(grid.size*10000)
      tn <- floor(data$northing[i]/(grid.size*10000))*(grid.size*10000)
      out[i] <- grid_10$sq1[which(grid_10$east == te & grid_10$north == tn)]
    }
  }

  return(out)

}
