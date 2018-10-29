#'-------------------------------
#'
#' Code to transform spatial information into
#' viable habitats for different biogenic habitat types
#' in east boston
#' 
#' @author Jarrett Byrnes
#'
#'
#'-------------------------------

#Libraries ####
library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)
library(readxl)
library(rlang)

library(ggplot2)
library(ggmap)
library(broom)
library(viridis)

theme_set(theme_void(base_size = 14))

#------------------------
#Load and prep data ####

#shapefiles for plotting
coastline <- readOGR("../raw_data/Massachusetts Municipal Boundaries Lines/GISDATA_TOWNSSURVEY_ARC.shp", layer="GISDATA_TOWNSSURVEY_ARC") 

areas <- readOGR("../geospatial_data/Polygon_areas/Polygon_areas.shp")

#loads in geo_tibble
geo_tibble <- readRDS("../derived_data/geo_tibble.rds")

#a function for later
get_df_for_layer <- function(adf, a_layer){
  a_layer <- enquo(a_layer)
  adf %>%
    mutate(a_layer = map(!!a_layer, ~as.data.frame(.x, xy=TRUE)))  %>%
    dplyr::select(Location, When, Adaptation, a_layer) %>%
    unnest(a_layer) 
}


#load drivers
drivers <- read_csv("../derived_data/drivers_minmax.csv")


slr_idtab <- levels(geo_tibble$sediments[[1]])[[1]] %>%
  mutate(levels = as.character(levels))

#load substrate requirements
substrates <- read_csv("../derived_data/substrates.csv") %>%
  left_join(slr_idtab, by = c("Substrate Type" = "levels")) %>%
  filter(!is.na(ID))

#---------------
# Prep data for plotting environmental layers####

flow_ggplots <- map((geo_tibble%>% split(geo_tibble$Location))[1:2], ~
                      ggmap(.x$ggmap[[1]]) +
                      geom_raster(data =  .x %>% unnest(current_velocity_wgs84_df),
                                  mapping = aes(x = x, y = y, fill = layer), alpha = 0.7) +
                      facet_grid(Adaptation ~  When) +
                      scale_fill_viridis(na.value = NA, guide=guide_colorbar(title="Peak Flow\nm/s")) +
                      # scale_fill_continuous(low="blue", high="red", na.value = NA,
                      #                       guide=guide_colorbar(title="Peak Flow\nm/s")) +
                      coord_cartesian() +
                      ggtitle(.x$Location[[1]]))

depth_ggplots <- map((geo_tibble %>% split(geo_tibble$Location))[1:2], ~
                       ggmap(.x$ggmap[[1]]) +
                       geom_raster(data =  .x %>% unnest(depths_wgs84_df),
                                   mapping = aes(x = x, y = y, fill = layer)) +
                       facet_grid(Adaptation ~  When) +
                       scale_fill_viridis(na.value = NA,
                                          guide=guide_colorbar(title="Depth (ft.)")) +
                       # scale_fill_continuous(low="blue", high="red", na.value = NA,
                       #                       guide=guide_colorbar(title="Depth (ft.)")) +
                       coord_cartesian() +
                       ggtitle(.x$Location[[1]]))


sediments_ggplots <- map((geo_tibble%>% split(geo_tibble$Location))[1:2], ~
                           ggmap(.x$ggmap[[1]]) +
                           geom_raster(data =  .x %>% unnest(sediments_wgs84_df) %>% filter(!is.na(predicted_levels)),
                                       mapping = aes(x = x, y = y, fill = predicted_levels), alpha = 0.7) +
                           facet_grid(Adaptation ~  When) +
                           scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(16),
                                             guide = guide_legend("Sediment Type"))+
                           coord_cartesian() +
                           ggtitle(.x$Location[[1]]))

#Save for later plotting
saveRDS(flow_ggplots, file = "../plots_data_for_report/flow_ggplots.Rds")
saveRDS(depth_ggplots, file = "../plots_data_for_report/depth_ggplots.Rds")
saveRDS(sediments_ggplots, file = "../plots_data_for_report/sediments_ggplots.Rds")


#------------------------
# Determine viable habitats by sediment type ####

#A function to turn sediments into viable habitats
# make_habitat_by_substrate <- function(adf, seds = slr_sediments_predict){
#   hab <- seds %in% adf$ID 
#   values(hab)[values(hab==0)] <- NA
#   hab
# }

subs_list <- substrates %>% split(.$`Biogenic Habitat/Species`)
make_habitat_by_species<- function(a_sp, seds){
  adf <- subs_list[[a_sp]]
  
  hab <- seds %in% adf$ID 
  
  values(hab)[values(hab==0)] <- NA
  
  hab
}

# #get rasters of which areas have the right habitat
# hab_rasts <- cross(list(subs = substrates %>% split(.$`Biogenic Habitat/Species`), 
#                         seds = geo_tibble$sediments_wgs84)) %>%
#   map(~make_habitat_by_substrate(.x$subs, .x$seds))
# 
# 
# hab_df <- crossing(Substrate = unique(substrates$`Biogenic Habitat/Species`), 
#                    geo_tibble %>% dplyr::select(Location, When, Adaptation)) %>%
#   mutate(hab_rasts = hab_rasts) %>%
#   spread(Substrate, hab_rasts, sep = ".")


hab_df <- crossing(Substrate = unique(substrates$`Biogenic Habitat/Species`), 
                   geo_tibble %>% dplyr::select(Location, When, Adaptation, sediments_wgs84)) %>%
  mutate(hab_rasts = map2(Substrate, sediments_wgs84, make_habitat_by_species)) %>%
  dplyr::select(-sediments_wgs84) %>%
  spread(Substrate, hab_rasts, sep = ".")

#----------------------------------------
#Determine viable habitats by drivers

#less than or equal to and greater than or equal to function
le <- function(a,b) a <= b
ge <- function(a,b) a >= b

#compare a raster to a value and return T/F values
tf_driver_rast <- function(arast, comp_fun, adf, mult = 1){
  arast <- comp_fun(arast, adf$Value[1])
  values(arast)[values(arast==0)] <- NA
  arast
}

#function to compare drivers to a relevant raster 
#and return viable habitats
make_habitat_by_driver <- function(adf, add_depth=0){
  
  #which raster set are we dealing with?
  if(adf$`Limiting Driver`[1]=="Flow Velocity"){
    rasts <- geo_tibble$current_velocity_wgs84
  }else{
    rasts <- geo_tibble$depths_wgs84 
    rasts <- map(rasts, ~.x* 0.3048) #because raster is in feet, but driver is in meters
  }
  
  #what function should be used for comparison?
  comp_fun <- ifelse(adf$`Max, Min, or Mean`[1]=="Max", le, ge)
  
  ret <- map(rasts, tf_driver_rast, comp_fun = comp_fun, adf = adf)
  
  
  ret
}

#make maps of T/F for each driver we have
driver_maps <- drivers %>%
  filter(`Limiting Driver` != "Temperature") %>%
  split(list(.$`Biogenic Habitat/Species`, 
             .$`Limiting Driver`, 
             .$`Max, Min, or Mean`), drop=TRUE) %>%
  map(make_habitat_by_driver)

#-----------------------------------
# Create combined summed viable habitat raster maps

#now, take this list and reshape it into a data frame we can 
#merge with the habitat maps and geo_tibble
geo_tibble <- left_join(geo_tibble,  bind_cols(hab_df, as_tibble(driver_maps)))

# sum up multiple output maps for each species/hab type
make_summed_rast <- function(a_sp){
  hab <- dplyr::select(geo_tibble, contains(a_sp))
  
  #make sure we have this species
  if(ncol(hab)==0) return(rep(NA, nrow(geo_tibble)))
  
  all_stack <- pmap(hab, ~raster::stack(.x)) %>%
    map(~calc(.x, sum))
  
  all_stack
  
}


#make a summed raster for viable habitats
all_tolerance <- map(unique(drivers$`Biogenic Habitat/Species`), make_summed_rast)
names(all_tolerance) <- unique(drivers$`Biogenic Habitat/Species`)

#add to our geotibble, and make a new df for plotting
geo_tibble_full <- bind_cols(geo_tibble, as.tibble(all_tolerance))

map_tibble <- bind_cols(geo_tibble_full %>% 
                          dplyr::select(Location, When, Adaptation, ggmap),
                        as.tibble(all_tolerance) %>% 
                          dplyr::select(`Clams`, `Mussel Bed`, `Oysters`, `Seagrass`)) %>%
  gather(Species, map_rast, `Clams`:`Seagrass`) %>%
  filter(Location != "Suffolk Downs") %>%
  mutate(map_rast_df = map(map_rast, ~as.data.frame(.x, xy=TRUE)))

#make pre-prepared ggplots
map_ggplots <- map(map_tibble %>% split(list(.$Species, .$Location)), ~
                     ggmap(.x$ggmap[[1]]) +
                     geom_raster(data =  .x %>% unnest(map_rast_df),
                                 mapping = aes(x = x, y = y, fill = factor(layer)), alpha = 0.7) +
                     scale_fill_manual(values = "green", , na.value = NA, guide = "none") +
                     facet_grid(Adaptation ~  When) +
                     coord_cartesian() +
                     ggtitle(.x$Species[[1]], .x$Location[[1]]))

#------------
# Generate area and some additional calculations
map_tibble <- map_tibble %>%
  mutate(hab_area = map_dbl(map_rast, ~sum(.x[]==1, na.rm=T) * area(.x)[1])) #area scaled by cell size

change_tibble <- map_tibble %>%
  dplyr::select(Location, When, Adaptation, Species, hab_area) %>%
  spread(Adaptation, hab_area)

#-----------------------------------
# Write out objects to be used in report.
saveRDS(geo_tibble_full, "../plots_data_for_report/geo_tibble_fill.Rds")
saveRDS(change_tibble, "../plots_data_for_report/change_tibble.Rds")
saveRDS(map_ggplots, "../plots_data_for_report/map_ggplots.Rds")
