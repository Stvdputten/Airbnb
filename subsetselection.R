library(tidyverse)
subset <- read_csv('2019/subset.csv', ) %>% 
  select(-'X1')

lmo <- lm(price ~ .,subset)

library(leaps)
subset$host_response_time <- as_factor(subset$host_response_time)
subset$cancellation_policy <- as_factor(subset$cancellation_policy)
subset$room_type <- as_factor(subset$room_type)
subset$bed_type <- as_factor(subset$bed_type)
subset$beds <- as_factor(subset$beds)
subset$property_type <- as_factor(subset$property_type)
subset$neighbourhood_cleansed <- as_factor(subset$neighbourhood_cleansed)
subset$security_deposit <- as.numeric(gsub('\\$', '', subset$security_deposit))
subset$cleaning_fee <- as.numeric(gsub('\\$', '', subset$cleaning_fee))
subset$bathrooms <- as_factor(subset$bathrooms)
subset$accommodates <- as_factor(subset$accommodates)
subset$guests_included <- as_factor(subset$guests_included)
#bathrooms, bedrooms, acommodates, guest_included
subset
predictors <- subset %>% 
  select(-price)
subset
leaps(y =head(subset$price), x= head(predictors )

cwd