# This script reorganizes the seedling sapling data to adhere to fortedata 
# standard format 
library(dplyr)

# bring in the data 
seed_sap_2020 <- read.csv("data/seedling_sapling_2020.csv", na.strings = c("","NA")) %>% 
  select(subplot, veg_plot, species, base_D, height_2019, height_2020, date)

# create df without 2020 height
year1 <- seed_sap_2020 %>% 
  select(subplot, veg_plot, species, base_D, height_2019, collection_date)

# create df without 2019 height
year2_H_only <- seed_sap_2020 %>% 
  select(height_2020)

# create df with only collection dat
coll_date <- seed_sap_2020 %>% 
  select(collection_date)

#create df with only labels
labs_only <- seed_sap_2020 %>% 
  select(subplot, veg_plot, species, base_D)

# bind the tree df's above to form year2 df (2020)
year2 <- bind_cols(labs_only, year2_H_only, coll_date)

# add a year column to both tables and rename height column to height_cm
year1  <- year1 %>% 
  mutate(year = 2019) %>% 
  rename(height_cm = height_2019)

year2 <- year2 %>% 
  mutate(year = 2020) %>% 
  rename(height_cm = height_2020)

# bind rows from 2019 and 2020 into one df for fortedata upload 
seed_sap_2020 <- bind_rows(year1, year2)


