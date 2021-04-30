# Creating function for bringing in a base map
# Code from J. Nowak and adapated by C. Chitwood
# 7/23/19

# Relevant mapping package
library(leaflet)

leaf_base <- function(x = NULL){
  
  out <- leaflet(x) %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Aerial") %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo") %>%
    addLayersControl(
      baseGroups = c("Topo", "Aerial", "NatGeo"),
      options = layersControlOptions(collapsed = TRUE)
    )
  
  return(out)
}

###################################################################################
###################################################################################

# How would I use it?  Make fake data, then call it...
df <- tibble::tibble(
  x = rnorm(10, -115, 2),
  y = rnorm(10, 45, 1)
)

# A basic one time call
leaf_base() %>%
  addMarkers(lng = df$x, lat = df$y)

# Or a little cleaner, same thing, but passing the data from the function
#  and out the other side reduces typing a little
leaf_base(df) %>%
  addMarkers(lng = ~x, lat = ~y)

###################################################################################
###################################################################################

# Applying the function to Missouri elk data

# Load clean database as R Object
dat_raw <- read.csv("Cams_Fall17.csv", stringsAsFactors = F)

dat_proc <- dat_raw %>% 
  rename(Lat = True.Lat, Lon = True.Long) %>% 
  # Remove extra rows inexplicably saved into the CSV file
  slice(1:72)

# View it
head(dat_proc)

# Look at camera locations by year
#dat_proc <- dat_raw %>%
  #filter(Year == 2012) %>% 
  #distinct(Lat, Lon, DeployID)

# Call funtion to bring in map (will automatically recongnize Lat and Lon correctly)
leaf_base(dat_proc) %>%
  addMarkers(
  # Add camera labels that appear when you mouse over point
  label = ~paste("Camera", CamID)
  #  Use next line of code to bring in points as clusters to speed up map loading time
  #  Clusters will separate into indivdual points as you zoom in on map
  #  clusterOptions = markerClusterOptions()
  )
  



###MCP
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("ks", "lattice", "plotrix", "adehabitatHR", "maptools", "foreign", "rgdal", "sp", "raster", "ggplot2", 
              "colorRamps", "rgeos", "sf", "tidyverse", "fasterize", "cowplot", "amt")

ipak(packages)



#utm
cord.dec = SpatialPoints(cbind(dat_raw$True.Long, -dat_raw$True.Lat), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:6512"))


cams_cp <- mcp(cord.UTM, percent=100)
plot(cams_cp)


plot(cord.UTM, axes=T)
plot(cord.dec, axes=T)


