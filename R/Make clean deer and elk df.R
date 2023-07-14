# Make clean deer and elk data
# Anna Moeller 
# 7-14-23

library(tidyverse)

# data
camfiles <- list.files("data/pictures by year", 
                       full.names = TRUE)
camraw_1 <- map_dfr(camfiles[c(1:3,5)], read_csv) %>% 
  mutate(Folder = as.character(Folder))
camraw_2 <- read_csv(camfiles[4])
camraw <- bind_rows(camraw_1, camraw_2)

# Clean up data types and dates (including assigning GMT time so that times
# won't change for daylight savings)
cam <- camraw %>%
  mutate(datetime_chr = paste(Date, Time),
         datetime = as.POSIXct(datetime_chr, format = "%d-%b-%y %H:%M:%S", tz = "GMT")) %>% 
  # Get total count of elk per photo
  mutate(ElkCount = ElkMatBull + ElkSpike + ElkCow + ElkCalf + ElkUnkn,
         DeerCount = WTDbuck + WTDdoe + WTDfawn + WTDunkn) %>% 
  rename(cam = CamID) %>% 
  select(cam, File, Folder, datetime, ElkCount, DeerCount)  %>% 
  # We found problems in 2018 with the deploy. Taking them all out
  filter(
    !(cam %in% c("9_3", "9_4", "9_6", "9_16", "9_30") & 
        lubridate::year(datetime) == 2018)
  )

saveRDS(cam, "data/clean/CleanElkandDeer.rds")




