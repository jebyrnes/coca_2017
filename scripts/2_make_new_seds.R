library(randomForest)
library(rgdal)
library(raster)
library(tidyverse)
library(broom)


#Load the fit model
load("../derived_data/hab_rf_complex.Rdata")

make_new_pred_seds <- function(depth,
                               velocity,
                              # rast_out = "../derived_data_models/inner_predicted.grd",
                               add_depth = 0){
  
  #Load new data sources
  # depth <- raster(depth_name)+add_depth
   names(depth) <- "Depth_m"
  # 
  # velocity <- raster(velocity_name)
   names(velocity) <- "Velocity"
  
  
  mouthDist <- distanceFromPoints(depth, c(255000, 900000))
  names(mouthDist) <- "dist_from_mouth"
  
  #####
  #calculate distance from shore
  ######
  #distance of "shore" cells to nearest water cell
  dist_rast <- distance(depth)
  #make the water NA
  values(dist_rast)[which(values(dist_rast==0))]<-NA
  
  #distance of each water NA cell to nearest shoreline cell
  dist_to_shore <- distance(dist_rast)
  values(dist_to_shore)[which(values(dist_to_shore==0))]<-NA
  
  #rename
  names(dist_to_shore) <- "Distance_to_Shore_cells"
  
  #### Stack 'em
  boston_envt <- stack(velocity, depth, mouthDist, dist_to_shore)
  
  boston_envt_df <- as.data.frame(boston_envt, xy=TRUE) %>%
    na.omit()
  
  ########
  # Predictions
  ########
  pred <- predict(hab_rf_complex, newdata=boston_envt_df)
  boston_envt_df$predicted <- pred
  
  #  qplot(x,y,color=pred, data=boston_envt_df)
  
  # create spatial points data frame
  spg <- boston_envt_df %>% dplyr::select(x,y,predicted)
  coordinates(spg) <- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(spg) <- TRUE
  # coerce to raster
  predicted_sediments_raster <- raster(spg)
  crs(predicted_sediments_raster) <- crs(boston_envt)
  #writeRaster(predicted_sediments_raster, rast_out, dataType="INT2U", options="COMPRESS=LZW", overwrite=TRUE)
  
  return(list(df = boston_envt_df,
              rast = predicted_sediments_raster))
}
