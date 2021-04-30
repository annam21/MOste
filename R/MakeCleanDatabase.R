### Formatting MO elk data for STE
### Data from E. Pero
### Processing by C. Chitwood and A. Moeller
### 7/22/19


# Load data file
dat_raw <- read.csv("Pics_Win1819.csv", stringsAsFactors = F)

# View it to see column names and dimensions
# dat_raw
# dim(dat_raw)

# Remove unnecessary columns to focus on elk
elkOnly <- dat_raw %>%
  select(CamID, File, Folder, Date, Time, ElkMatBull, ElkSpike, ElkCow,
         ElkCow, ElkCalf, ElkUnkn)

# View it to see column names and dimensions
# elkOnly
# dim(elkOnly)

# Clean up data types and dates (including assigning GMT time so that times
# won't change for daylight savings)
elkBetter <- elkOnly %>%
  mutate(datetime_chr = paste(Date, Time),
         datetime = as.POSIXct(datetime_chr, format = "%d-%b-%y %H:%M:%S", tz = "GMT")) %>% 
  # Get total count of elk per photo
  mutate(ElkCount = ElkMatBull + ElkSpike + ElkCow + ElkCalf + ElkUnkn)

# View it to see column names and dimensions and make sure datetime and elkCount looks right
# elkBetter
# dim(elkBetter)
# head(elkBetter$datetime)
# head(elkBetter$ElkCount)
# range(elkBetter$ElkCount)

# Save as R Object
saveRDS(elkBetter, "CleanDatabaseW1819.RDS")

# To import clean database as R Object
# db <- readRDS("CleanDatabase.RDS")

# Export clean database to csv for sharing, etc.
write.csv(elkBetter, "CleanDatabaseW1819.csv")

