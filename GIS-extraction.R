# GIS-extraction: A list of useful fucntions for GIS Operations in R
# Used to extract and collate GIS variables for camera trap analysis
# Started by M Fennell
# mitchfen@mail.ubc.ca

### 0. Setup ####
#Load Packages

list.of.packages <- c("raster",           # If you are working with Rasters this is essential
                      "osmdata",          # Access the OpenStreet map interface -> it is constantly updated
                      "sf",               # The go to package for spatial operations
                      "bcmaps",           # The BC specific resources
                      "spex",             # ?
                      "dplyr",            # Tidyverse package for data manipulation
                      "ggplot2",          # Plotting tools
                      "rgeos",            # Spatial operation backend package
                      "spatialEco",       # Handy tools for spatial ecology
                      "MODISTools",       # Tool for extracting NDVI
                      "ggmap",            # Add background  
                      "ggsn",             # north arrow 
                      "ggspatial",        # scale bar
                      "cowplot",          # inset map
                      "leaflet")          # Tool for interactive maps!

# Check which ones you dont have
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#Install the ones you dont have
if(length(new.packages)) install.packages(new.packages)
# Load the packages
lapply(list.of.packages, require, character.only = TRUE)

###############################################################
### Load in your camera locations #############################

sta <- read.csv("Example locations/Example_Camera_Locations.csv", header=T)

# Plot them
plot(sta$Longitude, sta$Latitude, asp=T, las=1,
     xlab="Longitude", ylab="Latitude")

# The following code determines the best UTM for your data

mlong <- mean(sta$Longitude); mlat <- mean(sta$Latitude)

# UTM finder function
lonlat2UTM <-  function(lonlat) {
  utm <-  (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}

crs.utm <- lonlat2UTM(c(mlong,mlat))

## Convert your camera points into a shapefile using simple features (sf)
sta.wgs <- st_as_sf(sta, coords = c("Longitude", "Latitude"), crs=4326) # Note the CRS tells R that the projection is WGS1984

## Convert the lat long to UTM - st_transform
sta.utm <- st_transform(sta.wgs,crs=crs.utm) 

### Buffer these points by 50km to create an area of interest(AOI) 
tmp <- st_buffer(sta.utm, 50000)

# Get the bounding box coordinates
aoi.utm <- st_as_sfc(st_bbox(tmp))
aoi.wgs <- st_bbox(st_transform(aoi.utm, 4326))
#remove(tmp)

# Check your aoi is sensible
plot(st_geometry(aoi.utm))  # Plot the box
plot(st_geometry(sta.utm), add=T, col="red", pch=19) # Add your camera stations

st_write(aoi.utm, paste0("Exported_data/", "aoi_utm.shp"), append=F)






###########################################################
### 2. Extract OSM data ###################################

# ESSENTIAL - SEE OSM CATAEGORIES FOR DETAIL:

# https://wiki.openstreetmap.org/wiki/Map_features

# Create your bounding box
bb_wgs <- st_bbox(aoi.wgs)


# Highways represent all linear features
highway <- opq(bb_wgs) %>%
           add_osm_feature("highway") %>%
           osmdata_sf()

# Convert the extracted highways to the chosen UTM
lines.shp <- st_transform(highway$osm_lines, crs=crs(sta.utm))

# Plot in Base R
plot(st_geometry(lines.shp), col="grey2")
# Add camera stations
plot(st_geometry(sta.utm), add=T, col="red", pch=19) # Add your camera stations

# Plot using ggplot (commented out becasue this takes ages for me)
# ggplot(lines.shp, aes(color = osm_id)) +
#   geom_sf() +
#   theme(legend.position = "none")

########
# Filter OSM objects

# Look good? Now filter into desired categories
trails.shp = lines.shp %>%
  filter(highway %in% c("path","bridleway","footway"))

ggplot(lines.shp, aes(color = osm_id)) +
  geom_sf() +
  theme(legend.position = "none")

roads.shp = lines.shp %>%
  filter(highway %in% c("track","unclassified","service"))

ggplot(roads.shp, aes(color = osm_id)) + 
  geom_sf() +
  theme(legend.position = "none")

##############################
# Save shapefiles ############ 
dir.create("Exported_data")

# Save as shapefile [NOT WORKING]
#st_write(trails.shp, paste0("Exported_data/", "New_trails.shp"), append=F)
#st_write(roads.shp,  paste0("Exported_data/", "New_roads.shp"), append=F)
#st_write(lines.shp,  paste0("Exported_data/", "New_roads_trails.shp"), append=F)

colnames(trails.shp)
########################################
### 3. Extract elevation for points ####
# Currently using 25m 1:250000 BC Gov CDED DEM 
# (https://www2.gov.bc.ca/gov/content/data/geographic-data-services/topographic-data/elevation/digital-elevation-model)


# Download DEM for AOI
#extent_CATH_sf <- spex(extent_CATH, crs = CATH_crs)
DEM_raster<- (aoi = aoi.utm)
plot(DEM_raster)
# Check the stations are there!
plot(st_geometry(sta.wgs), add=T)

# Extract value from raster at each point and add it to your dataframe
sta$Elevation <- extract(DEM_raster, sta.wgs)

#alternative option if the area is not in BC (cded_raster only work for BC)



######################################################
# Extract distance to other polygons or lines


### 4. Extract distance to trails and roads (and both) ####
#trails
plot(trails.shp)

# Find the nearest object
nearest  <- st_nearest_feature(sta.utm, trails.shp)
# For each station, measure the distance to the nearest object and add it to the sta datafile
sta$d.TRL <-  st_distance(sta.utm, trails.shp[nearest,], by_element=TRUE)

#Repeat for roads
# Find the nearest object
nearest  <- st_nearest_feature(sta.utm, trails.shp)
# For each station, measure the distance to the nearest object and add it to the sta datafile
sta$d.ROAD <-  st_distance(sta.utm, roads.shp[nearest,], by_element=TRUE)

# Repeat for linear features
# Find the nearest object
nearest  <- st_nearest_feature(sta.utm, lines.shp)
# For each station, measure the distance to the nearest object and add it to the sta datafile
sta$d.LIN <-  st_distance(sta.utm, lines.shp[nearest,], by_element=TRUE)


##################################################################
## 5. linear density in buffer-----------------------------------------------------------

#here using the trails.shp but can be done with any linear feature

#create buffer for each camera traps
sta.utm_buffer <- st_buffer(sta.utm, dist = 500) #pick distance in meters
sta.utm_buffer$area <- st_area(sta.utm_buffer) #calculate area of the buffer

trail_length <- list() #create output

for (i in 1:nrow(sta.utm_buffer)) { #for each buffer we caclulate de density 
  split_lines <- trails.shp %>%
    st_cast(., "MULTILINESTRING", group_or_split = FALSE) %>%
    st_intersection(., sta.utm_buffer[i, ]) %>%
    mutate(lineid = row_number())
  trail_length[[i]] <- split_lines %>%
    mutate(length = sum(st_length(.)))
}

#group by camera trap
trail_length <-  do.call(rbind, trail_length)%>%
  dplyr::group_by(Deployment.Location.ID) %>% #put the name of the camera traps column
  summarize(length = sum(length))

trail_length$length_n <- as.numeric(trail_length$length) 
sta.utm_buffer$area_n <- as.numeric(sta.utm_buffer$area) 

#final math of density calculus 
sta.utm_buffer <- sta.utm_buffer %>%
  st_join(., trail_length, join = st_intersects) %>%
  mutate(density = length_n/area_n)

sta.utm_buffer$density[is.na(sta.utm_buffer$density)] <- 0

#link back to camera trap shapefile
sta <- sta.utm_buffer %>%
  select(Deployment.Location.ID.x, density) %>%
  #  st_drop_geometry(.) %>%
  rename(Deployment.Location.ID = Deployment.Location.ID.x,
         Density_trail=density) %>%
  left_join(sta, by="Deployment.Location.ID") %>%
  st_as_sf(crs=4326) #put the right crs!

######################################################
### 6. Calculate terrain ruggedness ##################
#VRM (Sappington et al 2007)

vrm3 <- vrm(DEM_raster, s=3) #Likely use this one
vrm5 <- vrm(DEM_raster, s=5)

# Extract value from raster at each point
sta$VRM <- extract(vrm3, sta.wgs)

### Save and export master ####

write.csv(sta, "Exported_data/Station_Spatial_covariates.csv", row.names = F)

######################################################
### 7. Make nice maps in R ##################

#check the coordinate
aoi.wgs

#maually copy coordinate here 
myLocation <- c(-120.96290, 48.56223, -119.31807, 49.59065) 

#add background
mad_map <- get_map(location=myLocation, maptype = "roadmap")

#check it load
plot(mad_map)

#you can pick different background 
??get_map # check mmaptype, some are not working 

#using ggmap to create background map 
(g1 <- ggmap(mad_map)+
   geom_sf(data=sta.wgs, inherit.aes = F)+
   ggtitle("My camera traps location"))

#You can change the coordinates above to "zoom"

#add some fake detection rate and species 
sta.wgs$detection <- c(1,5,6,2,3,4,8,9,10)
sta.wgs$species <- c("deer", "elephant", "deer",
                     "lion","dinosaur", "dinosaur",
                     "elephan", "lion","fish")

#group by species and summarise detection
sta.wgs_grp<- sta.wgs %>%
  group_by(species)%>%
  summarise(detection=sum(detection))%>%
  st_as_sf()

#add size and color
g1 <- ggmap(mad_map)+
 geom_sf(data=sta.wgs_grp, inherit.aes = F, aes(size=detection, color=detection))+
  scale_color_gradient(low="white", high="blue")+ #color bar
  scale_size_continuous(guide="none")+ #remove useless legend
  annotation_scale(location="bl", width_hint=0.3)+ # scale bar
  ggtitle("My detection")

#Now add a north arrow and plot the map.
north2(g1, x=.22, y=.3, symbol=9)
  
#Inset map
#select area we want in inset map
mapCan <- getData("GADM", country = "Canada",level = 1)
mapCan <- fortify(mapCan)

#create the inset map with a red rectangle of aoi (need to change coordinate)
g2 <- ggplotGrob(
  ggplot() +
  geom_polygon(data = mapCan, aes(x = long, y = lat, group = group),fill = "#b2b2b2", color = "black", size = 0.4) +
    coord_map() +
    theme_map() +
    theme(panel.background = element_rect(fill = NULL)) +
    geom_rect(
      aes(xmin = -120.96290, xmax = -119.31807, ymin = 48.56223, ymax = 49.59065), fill = NA, 
      col = "red", size = 1
    )
)
  
plot(g2)

#need to play with the coordinate of the map to make it nice, here it is just an example
g1<- g1+
  annotation_custom(grob = g2, xmin = -119.8, xmax = -119.4,
                    ymin = 48.6, ymax = 48.8)
north2(g1, x=.22, y=.3, symbol=9)

### Inset map - alternate method
# The above method can take quite a while to render, 
# here is an alternate, quick and dirty method:

# get BC outline from bcmaps package
mapBC <- bc_bound()

# plot rectangle of aoi on it
inset <- ggplot(mapBC) +
  geom_sf(fill = "white", colour = "black") +
  geom_sf(data = aoi.utm,
          color = "red", fill = "white") +
  theme_void()
inset

# plot together with main map (g1 as example here) 
# You'll need to play around with plot window size, x,y coords
# I also moved by legend in my g1 plot to make a space for the inset map in the bottom right
g1_inset <- ggdraw() +
  draw_plot(g1, x = -0.15, y = 0) +
  draw_plot(inset, x = 0.63, y = 0.1, width = 0.4, height = 0.4)
g1_inset


#quickly make a map for each species 
#I want a closer zoom so I change the ggmap location (you dont have to)

myLocation <- c(-120.27, 49.03, -120.03, 49.12) 

#add background
mad_map <- get_map(location=myLocation, maptype = "roadmap")

#plot map for each species and save map
unique<-unique(sta.wgs_grp$species)

#first create a folder "plot" in your working directory
for (i in unique){
  temp_map<-ggmap(mad_map)+
    geom_sf(data=subset(sta.wgs_grp, sta.wgs_grp$species==i), inherit.aes = F, aes(size=detection, color=detection))+
    ggtitle(i) 
  ggsave(paste0(i, ".png"), path="./plot/", height = 20, width = 20, units="cm")
}


#####################################
# Below not yet implemented



# ####################################################
# ### 4. Extract distance to water ###################
# # Using NRCAN CANVEC (https://maps.canada.ca/czs/index-en.html)
# 
# CATH_watercourse <- shapefile("D:/Mitch/Cathedral/3.Data/3.2 GIS/CANVEC_Water/watercourse_1.shp")
# CATH_waterbody <- shapefile("D:/Mitch/Cathedral/3.Data/3.2 GIS/CANVEC_Water/waterbody_2.shp")
# CATH_waterbody_line <- as(CATH_waterbody, "SpatialLinesDataFrame")
# 
# # Join waterbodies and watercourses (lakes and rivers/streams)
# CATH_water_combi <- raster::union(CATH_watercourse, CATH_waterbody_line)
# CATH_water_combi
# 
# # Re-project to UTM
# CATH_water_combi_proj <- spTransform(CATH_water_combi, CATH_crs)
# CATH_water_combi_proj
# 
# # Fix up points to match
# proj4string(CATH_points_sp) <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
# CATH_points_sp_proj <- spTransform(CATH_points_sp, CATH_crs)
# CATH_points_sp_proj
# 
# # Extract distance to water
# CATH_dist_h2o <- cbind.data.frame(CATH_points$Site, CATH_points$Long, CATH_points$Lat)
# colnames(CATH_dist_h2o) <- c("Site", "Long", "Lat")
# 
# for (i in 1:nrow(CATH_points)){
#   tmp <- gDistance(CATH_points_sp_proj[i], CATH_water_combi_proj)
#   CATH_dist_h2o$d.H2O[i] <- tmp
# }
# 
# CATH_points_master <- left_join(CATH_points_master, CATH_dist_h2o)

