# Make deploys with only night photos 
# Anna Moeller 
# 4/30/2021  

# Rule of thumb for night, based on 2017 days
# 8:30 am to 4:15 pm

library(tidyverse)
library(lubridate)

# Deploy database
depfiles <- list.files("data/deploy", 
                       pattern = "Fall",
                       full.names = TRUE)

# Join deploy together 
deploy <- map_dfr(depfiles, read_csv) %>%
  # Make start and end times in POSIX
  mutate(start = as.POSIXct(Deployment, format = "%m/%d/%Y", tz = "GMT"),
         end = as.POSIXct(Recovery, format = "%m/%d/%Y", tz = "GMT")) %>%
  # Make column names correct
  rename(cam = CamID) %>%
  select(cam, start, end, Cover)  # Get rid of this later

# Ideas 
# make a sequence of days between start and end
# Will I need to make this long first? Hard to say

# Expand deploy with every day in the period, then only keep the 
#  ones between start and end 
dayphotos <- function(study_dates, deploy){
  out <- expand_grid(
    cam = unique(deploy$cam),
    day = seq(study_dates[1], study_dates[2], by = "day")
  ) %>% 
    left_join(., deploy, by = "cam") %>% 
    filter(day >= start,
           day <= end) %>% 
    # 8:30 am to 4:15 pm
    mutate(start = day + hours(8) + minutes(30),
           end = day + hours(16) + minutes(15)) %>% 
    select(-day)
  return(out)
}

# # Run it over all the study dates
# study_dates <- list(
#   deploy_day_2017 = as.POSIXct(c("2017-9-01 00:00:00",
#                "2017-11-30 23:59:59"), 
#              tz = "GMT"),
#   deploy_day_2018 = as.POSIXct(c("2018-9-01 00:00:00",
#                "2018-11-30 23:59:59"), 
#              tz = "GMT"),
#   deploy_day_2019 = as.POSIXct(c("2019-9-01 00:00:00",
#                "2019-11-30 23:59:59"), 
#              tz = "GMT")
# )
# 
# deployday <- map(study_dates, dayphotos, deploy = deploy)
# dest <- file.path("data/deploy/deploy day photos", 
#                   paste0(names(deployday), ".csv"))
# walk2(deployday, dest, ~write_csv(.x, path = .y))
## This works just fine for the specified dates.


# If I want deploy for every day each camera was out
stdts <- c(min(deploy$start), max(deploy$end))
deployday2 <- dayphotos(stdts, deploy)
saveRDS(deployday2, "data/deploy/deploy_fall_0830_1615.rds")
# And I'd rather save this as an R object to reduce confusion. 
