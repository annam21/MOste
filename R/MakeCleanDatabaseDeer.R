### Formatting MO deer data for STE
### Data from E. Pero
### Processing by C. Chitwood and A. Moeller
### 7/22/19

library(tidyverse)
# Load data file
dat_raw <- read.csv("Pics_Fall19.csv", stringsAsFactors = F)

# View it to see column names and dimensions
# dat_raw
# dim(dat_raw)

# Remove unnecessary columns to focus on elk
deerOnly <- dat_raw %>%
  select(CamID, File, Folder, Date, Time, WTDbuck, WTDdoe, WTDfawn, WTDunkn)

# View it to see column names and dimensions
# deerOnly
# dim(deerOnly)

# Clean up data types and dates (including assigning GMT time so that times
# won't change for daylight savings)
deerBetter <- deerOnly %>%
  mutate(datetime_chr = paste(Date, Time),
         datetime = as.POSIXct(datetime_chr, format = "%d-%b-%y %H:%M:%S", tz = "GMT")) %>% 
  # Get total count of deer per photo
  mutate(DeerCount = WTDbuck + WTDdoe + WTDfawn + WTDunkn)

# View it to see column names and dimensions and make sure datetime and deerCount looks right
# deerBetter
# dim(deerBetter)
# head(deerBetter$datetime)
# head(deerBetter$DeerCount)
# range(deerBetter$DeerCount)

# Save as R Object
saveRDS(deerBetter, "CleanDatabaseDeerF19.RDS")

# To import clean database as R Object
# db <- readRDS("CleanDatabase.RDS")

# Export clean database to csv for sharing, etc.
write.csv(deerBetter, "CleanDatabaseDeerF19.csv")

