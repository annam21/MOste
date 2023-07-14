# Run all models for presentation 
# Anna Moeller 
# 2/18/2021

# Download package
# devtools::install_github("annam21/spaceNtime",
#                          build_opts = c("--no-resave-data", "--no-manual"),
#                          build_vignettes = F,
#                          force = T)

library(tidyverse)
library(spaceNtime)
library(lubridate)

# Load data 
elkfiles <- list.files("data/clean", 
                       pattern = "CleanDatabaseF1[[:digit:]]\\.RDS",
                       full.names = TRUE)
deerfiles <- list.files("data/clean", 
                        pattern = "CleanDatabaseDeerF1[[:digit:]]\\.RDS",
                        full.names = TRUE)

# Rejoin them for ease
elk <- map_dfr(elkfiles, readRDS) %>% 
  # Make column names correct
  rename(cam = CamID, count = ElkCount)
# Deer
deer <- map_dfr(deerfiles, readRDS) %>% 
  # Make column names correct
  rename(cam = CamID, count = DeerCount) %>% 
  # We found problems in 2018
  filter(
    !(cam %in% c("9_3", "9_4", "9_6", "9_16", "9_30") & 
      lubridate::year(datetime) == 2018)
  )

# Deploy - including night photos
## Deploy database
depfiles <- list.files("data/deploy",
                       pattern = "Fall",
                       full.names = TRUE)

## Join deploy together
deploy <- map_dfr(depfiles, read_csv) %>%
  # Make start and end times in POSIX
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
  # Change detection area for different types of cameras, which creates new column for area
  mutate(
    area = case_when(
      Cover == "Plot" ~ 73,
      Cover == "Glade" ~ 32,
      Cover == "Woods" ~ 8
    )
  ) %>%
  select(cam, start, end, area, Cover)

############################################################
# # ALTERNATIVELY
# # Deploy only day photos: 0830 to 1615
# deploy <- readRDS("data/deploy/deploy_fall_0830_1615.rds") %>%
#   mutate(
#     area = case_when(
#       Cover == "Plot" ~ 73,
#       Cover == "Glade" ~ 32,
#       Cover == "Woods" ~ 8
#     )
#   )
# # Will need to filter deer and elk to only day photos too b/c deploy doesn't want photos when cam not working
# deer <- deer %>%
#   mutate(Time = hms::as_hms(Time)) %>%
#   filter(Time >= hms::hms(hours = 8, min = 30),
#          Time <= hms::hms(hours = 16, min = 15))
# elk <- elk %>%
#   mutate(Time = hms::as_hms(Time)) %>%
#   filter(Time >= hms::hms(hours = 8, min = 30),
#          Time <= hms::hms(hours = 16, min = 15))
###########################################################

# Our function to run the estimates together
pres_est <- function(study_dates, elk, deer, deploy){
  # Build occasions
  occ <- build_occ(samp_freq = 600,
                   samp_length = 10,
                   study_start = study_dates[1],
                   study_end = study_dates[2])

  # Create STE encounter history
  ste_eh_elk <- ste_build_eh(elk, deploy, occ)
  ste_eh_deer <- ste_build_eh(deer, deploy, occ)
  
  # Estimate abundance, and don't let it break if one has no detections
  if(all(is.na(ste_eh_elk$STE))){
    est_elk <- tibble::tibble(N = NA, SE = NA, LCI = NA, UCI = NA)
  } else {
    est_elk <- ste_estN_fn(ste_eh_elk, study_area = 1e6) %>% 
      mutate(species = "elk",
             season = paste0("fall", lubridate::year(study_dates[[1]]))) %>% 
      rename(Dkm2 = N,
             SE_km2 = SE)
  }
  if(all(is.na(ste_eh_deer$STE))){
    est_deer <- tibble::tibble(N = NA, SE = NA, LCI = NA, UCI = NA)
  } else {
    est_deer <- ste_estN_fn(ste_eh_deer, study_area = 1e6) %>%
      mutate(species = "deer",
             season = paste0("fall", lubridate::year(study_dates[[1]]))) %>% 
      rename(Dkm2 = N,
             SE_km2 = SE)
  }
  
  # Output
  list(
    est_elk,
    est_deer
  )
}

# Specify study dates and sampling occasions
study_dates <- list(
  as.POSIXct(c("2017-9-01 00:00:00",
               "2017-11-30 23:59:59"), 
             tz = "GMT"),
  as.POSIXct(c("2018-9-01 00:00:00",
               "2018-11-30 23:59:59"), 
             tz = "GMT"),
  as.POSIXct(c("2019-9-01 00:00:00",
               "2019-11-30 23:59:59"), 
             tz = "GMT")
)

# Run estimates
# pres_est(study_dates, elk, deer, deploy)
our_est <- map_dfr(study_dates, pres_est, elk, deer, deploy)
toplot <- our_est %>%
    mutate(N_PR = Dkm2 / 1e6 * 96161402,
           SE_PR = SE_km2 / 1e6 * 96161402,
           N_ERZ = Dkm2 / 1e6 * 896000000,
           SE_ERZ = SE_km2 / 1e6 * 896000000,
           D_mi2 = Dkm2 / 0.386,
           SE_mi2 = Dkm2 / 0.386,
           year = as.factor(substr(season, 5, 8)),
           season = substr(season, 1, 4)
           ) 
# save
# saveRDS(toplot, "modelrun_dayonly.rds")
# saveRDS(toplot, "modelrun_dayandnight.rds")

# Look at the numbers 
toplot %>% 
  arrange(species, season) %>%
  select(species, season, Dkm2,D_mi2,  N_PR, N_ERZ)

# Plot
ggplot(toplot) + 
  geom_point(aes(x = year, 
                 y = D_mi2, 
                 color = species),
             position = position_dodge(0.2)) + 
  geom_errorbar(aes(x = year, 
                    ymin = D_mi2 - 1.96*SE_mi2, 
                    ymax = D_mi2 + 1.96*SE_mi2,
                    color = species),
                position = position_dodge(0.2),
                width = 0) + 
  theme_classic()
  
# Debugging
## Find problem cameras 
# unique(elk$cam)[!(unique(elk$cam) %in% deploys$cam)]

