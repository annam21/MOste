# Formatting and running Missouri elk data through STE
# C. Chitwood and A. Moeller, E.Pero

# # Download package
# devtools::install_github("annam21/spaceNtime", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = T, force = T)
# 
# # Load packages
# install.packages("dplyr")
# install.packages("haven")
# install.packages("tidyverse")
# install.packages("usethis")
# install.packages("devtools")
# install.packages("lubridate")

#libraries
# library(usethis)
library(tidyverse)
library(lubridate)
library(spaceNtime)
# library(dplyr)
# library(devtools)

###vignette, had to force by adding 'build_vignettes = T' above)
# browseVignettes("spaceNtime")

# Load clean database as R Object
dat_raw <- readRDS("data/clean/CleanDatabaseF17.RDS")

# Or
#dat_raw <- readRDS("photoDataFor1000mCams.RDS")

# View it to see column names and dimensions
# dat_raw
# dim(dat_raw)

# Format data to match requirements of Anna's package
dat_proc <- dat_raw %>%
  # Make column names correct
  rename(cam = CamID, count = ElkCount) 

# View it to see column names and dimensions
# dat_proc
# pos <- dat_proc %>% filter(count > 0)
# dim(dat_proc)

# Create "deploy" database
# First bring in Missouri deployment data
deploy_raw <- read.csv("data/deploy/Cams_Fall17.csv", stringsAsFactors = F) #%>%
  # Remove extra rows inexplicably saved into the CSV file
 # slice(1:72)


# View it to see column names and dimensions
# deploy_raw
# dim(deploy_raw)

##############################################################################################
# Or, to create "deploy" database...
# Bring in reduced deployment file based on raster grid cells
# I.e.,
# Load camera database already reduced by raster grid
#deploy_edited <- readRDS("1000m.RDS")
# Load overall deployment database
#deploy_full <- read.csv("Cams_Fall17.csv", stringsAsFactors = F) %>%
  # Remove extra rows inexplicably saved into the CSV file
 # slice(1:72)
# Join them
#deploy_raw <- deploy_full %>%
 # inner_join(
  #  deploy_edited %>% dplyr::select(-Lat, -Lon), 
   # by = c("CamID")
#  )
##############################################################################################

# Format "deploy" database
deploy <- deploy_raw %>%
  # Fix start and end times to POSIX
  mutate(start = as.POSIXct(Deployment, format = "%m/%d/%Y", tz = "GMT"),
         end = as.POSIXct(Recovery, format = "%m/%d/%Y", tz = "GMT")) %>%
  # Make column names correct
  rename(cam = CamID) %>%
  # Add area sampled per camera
  # 106 for Reconyx
  # 102 for 60 ft AVERAGE sensing distance as listed in correct Bushnell model manual
  # 73 for 45-ft detection--This is actually listed as average detection range in manual
  # 8 for 15-ft detection and assuming 45 deg detection angle
  # 228 for 80-ft detection
  add_column(area = 8)

# Format "deploy" database, including imputing different detection areas for
#   different cameras
deploy <- deploy_raw %>%
  # Fix start and end times to POSIX
  mutate(start = as.POSIXct(Deployment, format = "%m/%d/%Y", tz = "GMT"),
         end = as.POSIXct(Recovery, format = "%m/%d/%Y", tz = "GMT")) %>%
  # Make column names correct
  rename(cam = CamID) %>%
  # Add area sampled per camera
  # 106 for Reconyx
  # 102 for 60 ft AVERAGE sensing distance as listed in correct Bushnell model manual
  # 73 for 45-ft detection--This is actually listed as average detection range in manual
  # 8 for 15-ft detection and assuming 45 deg detection angle
  # 228 for 80-ft detection
  # Solve spacing issue with the word Glade
  mutate(Cover = replace(Cover, Cover == "Glade ", "Glade")) %>% 
  # Change detection area for different types of cameras, which creates new column for area
  mutate(
    area = case_when(
      Cover == "Plot" ~ 73,
      Cover == "Glade" ~ 20,
      Cover == "Woods" ~ 8
    )
  ) 
# View it
head(deploy)
  
# Specify study dates and sampling occasions
study_dates <- as.POSIXct(c("2017-9-01 00:00:00", "2017-11-30 23:59:59"), tz = "GMT")

occ <- build_occ(samp_freq = 900,
                 samp_length = 10,
                 study_start = study_dates[1],
                 study_end = study_dates[2])

# Create STE encounter history
ste_eh <- ste_build_eh(dat_proc, deploy, occ)
#ste_eh

# Estimate abundance, given study area size
# (896000000 is from Smith et al.)
#  (96159784 is for Peck Ranch) or 96161402
#  (40000000 is from estimated MCP of cameras in the 2017 data set--see leaflet basemap)
#  (45772166 is 100% MCP of cams from 2017 FALL data set)
#  (1e6 = square km)
ste_estN_fn(ste_eh, study_area = 1e6) 

#### Determine number of detections used to derive estimate
ste_eh %>%
filter(!is.na(STE)) %>% 
  count()

# View the detections
ste_eh %>%
  filter(!is.na(STE))
