library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)
library(readxl)
library(rlang)
library(ggmap) #from devtools::install_github("dkahle/ggmap", ref = "tidyup")
source("./2_make_new_seds.R")



#bring in a coastline
coastline <- readOGR("../raw_data/Massachusetts Municipal Boundaries Lines/GISDATA_TOWNSSURVEY_ARC.shp", layer="GISDATA_TOWNSSURVEY_ARC") 

areas <- readOGR("../geospatial_data/Polygon_areas/Polygon_areas.shp")%>%
  spTransform(crs(coastline))
interventions_wgs <- readOGR("../geospatial_data/Polygon_areas/Interventions_v2.shp") 

interventions <- interventions_wgs %>%
  spTransform(crs(coastline))


#make a new raster
eb <- raster(nrows=180, ncols=10,
             crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
             ext = extent(-71.072987, -70.971441, 42.339312, 42.403786))
eb_extent <- projectExtent(eb, crs = crs(coastline))

east_bos <- raster::crop(coastline,  extent(eb_extent))


sediments <- raster("../derived_data/sediments.grd") %>%
  crop(extent(eb_extent))

slr_sediments_predict <- raster("../derived_data/slr_current_predicted.grd") %>%
  crop(extent(eb_extent))

slr_idtab <- levels(slr_sediments_predict)[[1]] %>%
  mutate(levels = as.character(levels))

current_velocity <- raster("../derived_data/SWAN/current_velocity_crop_ag.grd") %>%
  crop(extent(eb_extent))

depths <- raster("../derived_data/SWAN/depth_rast_crop_ag.grd") %>%
  crop(extent(eb_extent))

#deal with values < -20 (land)
values(depths)[values(depths) < -20] <- NA


#load drivers
drivers <- read_csv("../derived_data/drivers_minmax.csv")

#load substrate requirements
substrates <- read_csv("../derived_data/substrates.csv") %>%
  left_join(slr_idtab, by = c("Substrate Type" = "levels")) %>%
  filter(!is.na(ID))


#make list of areas
int_list <- map(interventions$Name,
                ~ subset(interventions, interventions$Name==as.character(.x)) %>%
                  buffer(500)
)

names(int_list) <- as.character(interventions$Name)



#check
# par(mfrow=c(2,2))
# lapply(int_list, function(x) {
#   plot(x, axes=TRUE); plot(current_velocity, add=TRUE)
#   })
# par(mfrow=c(1,1))

#make a tibble of elements for each site that can be used later
#to evaluate substrates, etc.

geo_tibble <- list(Location = names(int_list),
                   polys = map(int_list, ~.x),
                   current_velocity = map(int_list, ~crop(current_velocity, extent(.x))),
                   depths = map(int_list, ~crop(depths, extent(.x)))) %>%
  as.tibble()

#now, add new information regarding  
#changes from SLR and adaptations

#SLR
geo_tibble <- geo_tibble %>%
  crossing(When = c("Present", "2030", "2070"),
           Adaptation = c("Existing", "Green", "Grey"))


vel_change <- read_excel("../geospatial_data/Inundated Areas.xlsx", sheet = "vel_change")
depth_change <- read_excel("../geospatial_data/Inundated Areas.xlsx", sheet = "depth_change")
wave_height <- read_excel("../geospatial_data/Inundated Areas.xlsx", sheet = "wave_height")

#Put everything together
geo_tibble <- left_join(geo_tibble, tibble(When = c("Present", "2030", "2070"), SLR_ft = c(0, 0.9, 3.4))) %>%
  left_join(vel_change) %>%
  left_join(depth_change) %>%
  left_join(wave_height) %>%
  replace_na(list(`Velocity Change (m/s)` = 0, 
                  `Depth Increase (ft)` = 0, 
                  `Wave Height (ft)` = 0 ))

#now create mutated velocities and depths
geo_tibble <- geo_tibble %>%
  mutate(current_velocity = map2(current_velocity, `Velocity Change (m/s)`, sum),
         depths = pmap(list(depths, SLR_ft, `Depth Increase (ft)`), sum))


#now make sediments!
#takes a few minutes - get a cuppa!
geo_tibble <- geo_tibble %>%
  mutate(sediments = map2(depths, current_velocity, make_new_pred_seds),
         sediments_df = map(sediments, ~.x$df),
         sediments = map(sediments, ~.x$rast))

#mutate the factors in order
geo_tibble<- geo_tibble %>%
  mutate(When = factor(When, levels = c("Present", "2030", "2070")),
         Adaptation = factor(Adaptation, levels = c("Existing", "Grey", "Green"))
  )

#reproject layers to WGS84 for easier integration with ggmaps
geo_tibble <- geo_tibble %>%
  mutate(current_velocity_wgs84 = map(current_velocity, ~ projectRaster(.x, crs = crs(interventions_wgs))),
         sediments_wgs84 = map(sediments, ~ projectRaster(.x, crs = crs(interventions_wgs), method = "ngb")),
         depths_wgs84 = map(depths, ~ projectRaster(.x, crs = crs(interventions_wgs)))
  )


#fix sediments back to factors

rast_fact <- function(x){
  x <- ratify(x)
  levels(x) <- levels(geo_tibble$sediments[[1]])
  x
}

geo_tibble <- geo_tibble %>%
  mutate(sediments_wgs84 = map(sediments_wgs84, rast_fact))


#make DFs for ggplot

get_df_for_layer <- function(adf, a_layer){
  a_layer <- enquo(a_layer)
  adf %>%
    mutate(a_layer = map(!!a_layer, ~as.data.frame(.x, xy=TRUE)))  %>%
    dplyr::select(Location, When, Adaptation, a_layer) %>%
    unnest(a_layer) 
}

geo_tibble <- geo_tibble %>%
  mutate(current_velocity_wgs84_df =map(current_velocity_wgs84, ~as.data.frame(.x, xy=TRUE)),
         sediments_wgs84_df =map(sediments_wgs84, ~as.data.frame(.x, xy=TRUE)),
         depths_wgs84_df =map(depths_wgs84, ~as.data.frame(.x, xy=TRUE)),
  )


#amd get ggmapss
#first, the initial list of maps
wgs_list <- map(int_list, ~spTransform(.x, crs(interventions_wgs)))

#a <- wgs_list[[1]]
get_box <- function(obj){
  e <- as.vector(extent(obj))
  c(e[1], e[3], e[2], e[4])

}

#get the map, and add to the geotibble
ggmap_list <- map(wgs_list, ~get_stamenmap(get_box(.x),  zoom = 16))
ggmap_df <- tibble(Location = names(ggmap_list), ggmap = ggmap_list)

geo_tibble <- left_join(geo_tibble, ggmap_df)

saveRDS(geo_tibble, "../derived_data/geo_tibble.rds")






