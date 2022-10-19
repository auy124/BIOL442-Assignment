#'@title Latitude Longitude Conversion
#'
#'@description Converts degrees decimels minutes (DDM) coordinates to decimal degrees coordinates using given inputs in tabular format. 
#' @param input_latitude_column the column data of the latitude coordinates from the given data.frame
#' @param input_longitude_column the column data of the longitude coordinates from the given data.frame
#' @param flip_column If true, will provide Longitude, Latitude output. If false, will provide Latitude, Longitude output.
#' @keywords latitude longitude coordinates
#' @export
#' @examples 
#' lat_long_conv 

lat_long_conv <- function(input_latitude_column, input_longitude_column, flip_columns=TRUE) {
  
  input_longitude_column <- ((as.numeric(gsub("°.*", "", input_longitude_column))) + 
                   (as.numeric(gsub(".*°|\\'.*", "", input_longitude_column))/60)) * -1 ##Converts Longtitude

  input_latitude_column <- ((as.numeric(gsub("°.*", "", input_latitude_column))) + 
                  (as.numeric(gsub(".*°|\\'.*", "", input_latitude_column))/60)) ##Converts Latitude

  if (!flip_columns) { 
    
    ll_output <- data.frame(input_longitude_column, input_latitude_column)
    colnames(ll_output) <- c("Longitude", "Latitude")
    
    return(ll_output)
    
    }  
  
  if (flip_columns) {
    
    ll_output <- data.frame(input_latitude_column, input_longitude_column)
    colnames(ll_output) <- c("Latitude", "Longitude")
    
    return(ll_output)
    
    }
  
}
