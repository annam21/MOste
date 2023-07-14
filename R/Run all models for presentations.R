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
df <- readRDS("data/clean/CleanElkandDeer.rds")

# elk
elk <- df %>% 
  select(-DeerCount) %>% 
  rename(count = ElkCount)

# Deer
deer <- df %>%
  select(-ElkCount) %>% 
  rename(count = DeerCount)

# Deploy - including night photos
## Deploy database
depfiles <- list.files("data/deploy",
                       pattern = "Cams",
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
  select(cam, start, end, area, Cover) %>% 
  distinct()

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
             # season = paste0("fall", lubridate::year(study_dates[[1]])) # hard code
             ) %>% 
      rename(Dkm2 = N,
             SE_km2 = SE)
  }
  if(all(is.na(ste_eh_deer$STE))){
    est_deer <- tibble::tibble(N = NA, SE = NA, LCI = NA, UCI = NA)
  } else {
    est_deer <- ste_estN_fn(ste_eh_deer, study_area = 1e6) %>%
      mutate(species = "deer",
             # season = paste0("fall", lubridate::year(study_dates[[1]]))
             ) %>% 
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
  # fall
  fall17 = as.POSIXct(c("2017-9-01 00:00:00",
               "2017-11-30 23:59:59"),
             tz = "GMT"),
  fall18 = as.POSIXct(c("2018-9-01 00:00:00",
               "2018-11-30 23:59:59"),
             tz = "GMT"),
  fall19 = as.POSIXct(c("2019-9-01 00:00:00",
               "2019-11-30 23:59:59"),
             tz = "GMT"),
  # winter
  winter1718 = as.POSIXct(c("2017-12-01 00:00:00",
               "2018-02-28 23:59:59"),
             tz = "GMT"),
  winter1819 = as.POSIXct(c("2018-12-01 00:00:00",
               "2019-02-28 23:59:59"),
             tz = "GMT")
)

# Run estimates
# pres_est(study_dates, elk, deer, deploy)
our_est <- map_dfr(study_dates, pres_est, elk, deer, deploy) %>% 
  mutate(season = rep(names(study_dates), each = 2))
toplot <- our_est %>%
    mutate(N_PR = Dkm2 / 1e6 * 96161402,
           SE_PR = SE_km2 / 1e6 * 96161402,
           N_ERZ = Dkm2 / 1e6 * 896000000,
           SE_ERZ = SE_km2 / 1e6 * 896000000,
           D_mi2 = Dkm2 / 0.386,
           SE_mi2 = Dkm2 / 0.386,
           season = factor(season, 
                           levels = c('fall17', 'winter1718', 'fall18', 'winter1819', 'fall19'))
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
  geom_point(aes(x = season, 
                 y = D_mi2, 
                 color = species),
             position = position_dodge(0.2)) + 
  geom_errorbar(aes(x = season, 
                    ymin = D_mi2 - 1.96*SE_mi2, 
                    ymax = D_mi2 + 1.96*SE_mi2,
                    color = species),
                position = position_dodge(0.2),
                width = 0) + 
  theme_classic()
  
