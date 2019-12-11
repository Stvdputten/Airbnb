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

# Market remove, because of unbalanced and weirdly parsed data (mostly amsterdam)
View(subset)
colnames(subset)
subset <- subset %>% 
  select(-'state')

#street remove, [,6] because of mostly the same data, 
#which was not the street (Amsterdam, Noord-Holland, Netherlands)
subset = as.data.frame(subset[c(1:5, 7:29)])


head(subset)
subset
# Saving variable ---------------------------------------------------------

write_csv(subset, '2019/subset.csv')

lmo <- lm(price ~ host_since + host_response_time + host_neighbourhood + host_has_profile_pic +
            host_identity_verified + neighbourhood_cleansed + property_type + 
            room_type + accommodates + bathrooms + beds + bed_type + security_deposit + 
            cleaning_fee + guests_included + number_of_reviews + number_of_reviews_ltm +
            review_scores_rating + review_scores_accuracy + review_scores_cleanliness + 
            review_scores_checkin + review_scores_communication + review_scores_location + 
            review_scores_value + cancellation_policy + reviews_per_month, data=subset)

plot(lmo)
summary(lmo)

