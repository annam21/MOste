# Percentage of locations on Peck
# Anna Moeller 
# 5/26/2021

library(tidyverse)
library(lubridate)

# The data
set.seed(123)
dat <- tibble::tibble(
  ID = rep(1:2, each = 6),
  datetime = rep(seq.POSIXt(ymd_hms("2017-11-15T14:02:00"), 
                            ymd_hms("2017-11-20T14:02:00"), 
                            by = "day" ), 
                 2),
  onPeck = rbinom(12, 1, 0.5)
)

# Working with the data
# By week, percentage of that animal's points on Peck
pct <- dat %>% 
  mutate(week = week(datetime)) %>% 
  group_by(ID, week) %>% 
  summarize(onPeck = mean(onPeck)) 
pct

# From here we could...
# - set a cutoff and count the animal as on Peck if >__% of its locations are on 
#   Peck 
# - summarize the total proportion of points per week that were on Peck
pct %>% 
  group_by(week) %>% 
  summarize(onPeck = mean(onPeck))
