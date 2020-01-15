#Changed the neighbourhoods to city party (east, west, north, south-east, south, centrum, new-west)
#Officially new-west and westerpoort are two seperate neighbourhoods except they are used interchangable so I combined them
library(tidyverse)
subset = read_csv('2019/subset.csv')
colnames(subset)

subset$host_response_time <- as_factor(subset$host_response_time)
subset$cancellation_policy <- as_factor(subset$cancellation_policy)
subset$room_type <- as_factor(subset$room_type)
subset$bed_type <- as_factor(subset$bed_type)
subset$property_type <- as_factor(subset$property_type)
subset$neighbourhood_cleansed <- as_factor(subset$neighbourhood_cleansed)
subset$security_deposit <- as.numeric(gsub('\\$', '', subset$security_deposit))
subset$cleaning_fee <- as.numeric(gsub('\\$', '', subset$cleaning_fee))
subset$bathrooms <- as_factor(subset$bathrooms)
subset$accommodates <- as_factor(subset$accommodates)
subset$guests_included <- as_factor(subset$guests_included)

summary(subset)

levels(subset$neighbourhood_cleansed)
nrow(subset)

subset$neighbourhood_cleansed %>% 
  fct_count()

subset$neighbourhood_cleansed <- subset$neighbourhood_cleansed %>% 
  fct_collapse(
              Oost = c('Oostelijk Havengebied - Indische Buurt' , 'Watergraafsmeer' ,
           'Oud-Oost' , 'IJburg - Zeeburgereiland'),
              West = c('Bos en Lommer', 'De Baarsjes - Oud-West', 'Osdorp'),
              Zuid = c('Zuid', 'De Pijp - Rivierenbuurt', 'Buitenveldert - Zuidas'),
              Centrum = c('Centrum-Oost', 'Centrum-West'),
              Zuid_Oost = c('Bijlmer-Oost', 'Bijlmer-Centrum', 'Gaasperdam - Driemond'),
              Nieuw_West = c('Geuzenveld - Slotermeer', 'De Aker - Nieuw Sloten', 'Slotervaart', 'Westerpark'),
              Noord = c('Noord-West', 'Noord-Oost', 'Oud-Noord' )
           )

write_csv(subset, '2019/subset.csv')
