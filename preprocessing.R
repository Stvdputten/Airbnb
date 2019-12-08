library(tidyverse)
subset <- read_csv('2019/subset.csv')


# Preprocessing (typecasting) ---------------------------------------------


subset$host_response_time <- as_factor(subset$host_response_time)
subset$cancellation_policy <- as_factor(subset$cancellation_policy)
subset$host_neighbourhood <- as_factor(subset$host_neighbourhood)
subset$room_type <- as_factor(subset$room_type)
subset$bed_type <- as_factor(subset$bed_type)
subset$property_type <- as_factor(subset$property_type)
subset$neighbourhood_cleansed <- as_factor(subset$neighbourhood_cleansed)
subset$security_deposit <- as.numeric(gsub('\\$', '', subset$security_deposit))
subset$cleaning_fee <- as.numeric(gsub('\\$', '', subset$cleaning_fee))
View(subset)

listings <- read_csv('2019/listings.csv')
price <- as.numeric(gsub('\\$', '', listings$price))
subset$price <- price
head(subset$price)
colnames(subset)
# change to since
# change to since
# 
subset$host_since
subset$neighbourhood_cleansed
subset$country_code
as.factor(subset$bed_type)

# Analysing variables -----------------------------------------------------

levels(as.factor(subset$host_response_time))
# levels(as.factor(subset$market))
# 
# subset$market[subset$market == 'Barcelona']
# subset %>% 
#   filter(subset$market != 'Amsterdam') %>% 
#   select(market)
levels(as.factor(subset$host_neighbourhood))
subset$host_neighbourhood
# Removed (because of: ) --------------------------------------------------

# Market remove, because of unbalacned and weirdly parsed data (mostly amsterdam)
View(subset)
colnames(subset)
subset <- subset %>% 
  select(-'state')

head(subset)
subset
# Saving variable ---------------------------------------------------------


write_csv(subset, '2019/subset.csv')
