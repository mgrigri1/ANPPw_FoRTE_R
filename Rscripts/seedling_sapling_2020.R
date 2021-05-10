# let's create a new script for th 2019 and 2020 seedling and sapling data starting 
# from the fortedata formatted data 

# load libraries
library(dplyr)
library(googledrive)

# first I want to download the data off google drive 
# locate the files
drive_find(pattern = "seedling_sapling_2019_fd", n_max = 30)
drive_find(pattern = "seedling_sapling_2020_fd", n_max = 30)

# download the files to my data folder. "overwrite" overwrites the harddrive copy of
# the data. This way, this script it ALWAYS pulling the data from the shared google
# drive. You could theoretically just do this once, and then use the harddrive copy...
drive_download(
  "seedling_sapling_2019_fd.csv",
  path = "data/seedling_sapling_2019_download.csv",
  overwrite = TRUE
)

# download 2020 data
drive_download(
  "seedling_sapling_2020_fd.csv",
  path = "data/seedling_sapling_2020_download.csv",
  overwrite = TRUE
)

# bring the file into my environment
ss_2019 <- read.csv("data/seedling_sapling_2019_download.csv", na.strings = c("","NA"))
ss_2020 <- read.csv("data/seedling_sapling_2020_download.csv", na.strings = c("","NA"))

# adjust vector class of height_total_cm in the 2019 to join 2019 and 2020
ss_2019$height_total_cm <- as.numeric(ss_2019$height_total_cm)

ss_alltime <- bind_rows(ss_2019, ss_2020)

# filter out NA's which were veg plots without any seedlings
# present (every subplot has at least some seedling/saplings present)