#'@title Latitude Longitude Conversion
#'
#'@description Converts degrees decimels minutes (DDM) coordinates to decimal degrees coordinates using given inputs in tabular format. 
#' @param input_latitude_column the column data of the latitude coordinates from the given data.frame
#' @param input_longitude_column the column data of the longitude coordinates from the given data.frame
#' @param flip_columns If true, will provide Longitude, Latitude output. If false, will provide Latitude, Longitude output.
#' @keywords latitude longitude coordinates
#' @export
#' @examples 
#' lat_long_conv 

lat_long_conv <- function(input_latitude_column, input_longitude_column, flip_columns=TRUE) {
  
  #Generates dataframe of lat and long 
  copy <- data.frame(input_latitude_column, input_longitude_column)
  colnames(copy)[1] ="Latitude"
  colnames(copy)[2] ="Longitude"
  
  #Generates second copy
  copy2 <- data.frame(input_latitude_column, input_longitude_column)
  colnames(copy2)[1] ="Latitude"
  colnames(copy2)[2] ="Longitude"
  
  #Assigns -1 to West Longitudes and +1 to East Longitudes
  copy$Longitude <- gsub(".*\\'", "", copy$Longitude)
  copy$Longitude <- as.numeric(gsub("W", "-1", copy$Longitude))
  copy$Longitude <- as.numeric(gsub("E", "1", copy$Longitude))
  
  #Assigns -1 to South Latitudes and +1 to North Latitudes
  copy$Latitude <- gsub(".*\\'", "", copy$Latitude)
  copy$Latitude <- as.numeric(gsub("N", "1", copy$Latitude))
  copy$Latitude <- as.numeric(gsub("S", "-1", copy$Latitude))
  
  #Converts Longtitude to numerical
  copy2$Longitude <- ((as.numeric(gsub("°.*", "", copy2$Longitude))) + 
                   (as.numeric(gsub(".*°|\\'.*", "", copy2$Longitude))/60)) 

  #Converts Latitude to numerical
  copy2$Latitude <- ((as.numeric(gsub("°.*", "", copy2$Latitude))) + 
                  (as.numeric(gsub(".*°|\\'.*", "", copy2$Latitude))/60))
  
  #Multiplies the dataframes "copy" and "copy2" such that final represents converted coordinates
  final <- data.frame(Map(function(x,y) 
    if (all(is.numeric(x),is.numeric(y))) x * y 
    else x, copy, copy2))
  
  # Flips columns if flip_columns == TRUE
  if (flip_columns) { 
    
    ll_output <- data.frame(final$Longitude, final$Latitude)
    colnames(ll_output) <- c("Longitude", "Latitude")
    
    return(ll_output)
    
    } else {
      
      ll_output <- data.frame(final$Latitude, final$Longitude)
      colnames(ll_output) <- c("Latitude", "Longitude")
      
      return(ll_output)
      
      }
    
}
