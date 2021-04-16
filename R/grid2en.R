
#' grid2en
#'
#' Returns OSGB easting/northing coordinates from OSGB grid cells.
#'
#' @param grids OSGB grid cells
#' @param grid.size Known size of the grid cells. Only valid options are 1 or 10 km grid cells.
#'
#' @return Gives a dataframe of easting and northing coordinates.
#' @export
#'
#' @examples test_data <- data.frame(easting = c(455328, 631490, 427622), northing = c(199154, 275050, 164165), grid10 = c('SU59', 'TM37', 'SU26'), grid1 = c('SU5599', 'TM3175', 'SU2764'))
#'
#' grid2en(test_data$grid10, grid.size = 10)
#'
#' en2grid(test_data, grid.size = 10)


grid2en <- function(grids, grid.size = 10){

  if(is.null(grids)){stop('No grids to convert')}
  if(!(grid.size %in% c(1, 10))){stop('Invalid \'grid.size\' value. Currently only 1 and 10km grids are supported')}

  out <- matrix(NA, nrow = length(grids), ncol = 2)

  if(grid.size == 1){
    for(i in 1:length(grids)){
      out[i,1] <- unname(grid_1$easting[which(grid_1$sq1 == grids[i])])
      out[i,2] <- unname(grid_1$northing[which(grid_1$sq1 == grids[i])])
    }
  }

  if(grid.size == 10){
    for(i in 1:length(grids)){
      out[i,1] <- unname(grid_10$easting[which(grid_10$sq10 == grids[i])])
      out[i,2] <- unname(grid_10$northing[which(grid_10$sq10 == grids[i])])
    }
  }

  return(as.data.frame(out, col.names = c('easting', 'northing')))

}
