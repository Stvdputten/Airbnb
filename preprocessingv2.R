####----Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(car)
library(leaps)

subset = read.csv('2019/subset.csv') %>% 
  select(-X)


#removing groups with n<10 from property_type 
subset <- subset[subset$property_type != 'Tent',]
subset <- subset[subset$property_type != 'Bungalow',]
subset <- subset[subset$property_type != 'Barn',]
subset <- subset[subset$property_type != 'Island',]
subset <- subset[subset$property_type != 'Earth house',]
subset <- subset[subset$property_type != 'Cottage',]
subset <- subset[subset$property_type != 'Dome house',]
subset <- subset[subset$property_type != 'Camper/RV',]
subset <- subset[subset$property_type != 'Chalet',]
subset <- subset[subset$property_type != 'Casa particular (Cuba)',]
table(subset$property_type)

#removing datapoint with price = 0
subset <- subset[subset$price != 0,]



table(subset$host_response_time) #smallest group has 269 obs.
table(subset$host_has_profile_pic) #false has 7 observations. remove
subset <- subset[,-2]
table(subset$host_identity_verified) #smallest group >1000
table(subset$neighbourhood_cleansed) #smallest group 90
table(subset$room_type) #smallest group  43
table(subset$accommodates) #all groups >8 put into 1 group.
subset[as.numeric(subset$accommodates)>= 9, 6] = 'big'
table(subset$bathrooms) #all groups >3.5 into 1 group
subset[as.numeric(subset$bathrooms)>=3.5, 7] = 'big'
table(subset$bedrooms) # all groups >5 into 1 group
subset[as.numeric(subset$bedrooms)>=6, 8] = 'big'
table(subset$beds) #all groups >8 into 1 group
subset[as.numeric(subset$beds) >= 9, 9] = 'big'
table(subset$bed_type) # couch = 4. remove
subset <- subset[subset$bed_type != 'Couch',]
table(subset$security_deposit) #numerical.
table(subset$cleaning_fee) #numerical
table(subset$guests_included) # >6 into one group
subset[as.numeric(subset$guests_included) >= 7, 13] = 'big'
table(subset$number_of_reviews) #numerical
table(subset$review_scores_rating) #percentage
table(subset$cancellation_policy) #super_strict_30 = 17, super_strict_60 = 25
table(subset$reviews_per_month) #continuous

