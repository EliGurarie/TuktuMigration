#' Prep data for Hierarchical Migration analysis
#' 
#' Simplifies a simple feature with an ID and time column into a data frame suitable for 
#' hierarchical migration analysis
#' 
#' @param data movement data to analyze, a simple feature in a metric coordinate system 
#' (i.e. not longitude-latitude), or a data frame with X and Y columns in meters
#' @param id.col name of column with indiviudal id's
#' @param {x.col,y.col} if a data frame, column names for X and Y coordinates
#' @param time.col column name of time column.  Must be POSIX
#' @param yday.col if time.col is NULL, the column that contains the day of year. 
#' Eventually, this can be fractional day of year
#' 
#' @return outputs a list with two elements: \code{raw_data} is just the input data in its 
#' original form, \code{stan_data} is the data processed and reasy to be analyzed in the 
#' hierarchical migration analysis. 
#' 
#' @export

prepData_migration <- function(data, id.col = "ID", x.col = NULL, y.col = NULL,
                               time.col = "Time", yday.col = NULL){
  
  is_proj_in_meters <- function(obj) {
    !is.na(st_crs(obj)) &
      !st_is_longlat(obj) &
      is.null(st_crs(obj)$to_meter)
  }
  
  if(inherits(data, "sf")){
    if(!is_proj_in_meters(data))
      stop("The units of your projection system should really be in meters.") 
    
    df <- as.data.frame(data) 
    df$geometry <- NULL
    df$x <-  st_coordinates(data)[,1]/1e3
    df$y <-  st_coordinates(data)[,2]/1e3
  } else {
    df$x <- data[,x.col]/1e3
    df$y <- data[,y.col]/1e3
  }
  
  if(!is.null(yday.col))
    df$yday <- df[,yday] else
      df$yday <- lubridate::yday(df[,time.col])
    df$id <- as.integer(factor(as.character(df[,id.col])))
    df$dday <- tapply(df$yday, df$id, function(x) c(0,diff(x)))
    
    stan_data <- with(df, list(n = length(x), 
                               k = length(unique(id)),
                               x = x, y = y, yday = yday, 
                               id = id, squish = 2))
    list(stan_data = stan_data, raw_data = data)
  }
