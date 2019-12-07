####----Libraries
library(dplyr)
#########---------- Linear Models AIR BNB Analysis -------------#########


bnb = read.csv(file="listings.csv", header=TRUE, sep=",")

# data inlcudes 106 variables, this is obvisouly too many as such variable 
# selection based on literature/similar case studies will be performed and
# variables that do not contain any predictive information about listing prices
# will be removed 

# Removeable based on non-predictive information or full empty colunmn: 
# id[,1], listing_url[,2], scape_id[,3], last_scrape[,4]
# experiences_offred[,9], thumbnail_url[,16], medium_url[,17], picture_url[,18]
# xl_picture_url[,19], host_id[,20], host_url[,21], host_name[,22]
# # square_feet[,60], requieres_liscence[,95]



# Removed to reduce dimensionality & unlikeliness of adding predictive power:
# latiude[,49] ,longtiude[,50]
# is_location_exact[,51]
# extra_people[,67]
# minimum_nights[,68], maximum_nights[,69], minimum_minimum_nights[,70]
# maximum_maximum_nights[,71], maximum_minimum_nights_avg_ntm[,72]
# calender updated[,76]
# has_availability[,77], availability_30[,78], availability_60[,79],
# availability_90[,80], availability_365[,81]
# calender_last_scaped[,82]
# first_review[,86]
# requieres_liscence[,94]
# instant_bookable[,97], is_buisness_travel_ready[,98]
# requiere_guest_profile_picture[,100]
# requiere_guest_phone_verification[,101]
# calculated_host_listing_count[,102] 
# calculated_host_listing_count_entire_homes[,103]
# calculated_host_listing_count_private_rooms[,104]
# calculated_host_listing_count_shared_rooms[,105]


# Removed because its text information describing the listing:
# name[,5], summary[,6], space[,7], description[,8], neighborhood_overview[,10]
# notes[,11], transit [,12], acess[,13], interaction[14], house_rules[,15]
# host_about[,25], host_acceptance_rate[,28], host_thumbnail_url[,30]
# host_picture_url[,31], neighbourhood_group_cleansed[,41]
# juristiction_names[,96]



# Included variables and or recoded variablesincluded in the analysis 
# host_since[,23] recode from data to number of days
# host_location[,24] recode categories into numbers
# host_response_time[,26] recode reponse time from categories into number
# host_neighbourhood[,32] categries need to be recoded into numbers
# host_has_profile_pic[,36] true or false coded 
# host_identitiy_verified[,37] coded true or false
# street[,38], neighbourhood [,39] OR neighbourhood_cleansed[,40]
# city[,42], state[,43]
# zipcode[,44] recode into numeric to create multi-categoriy variable
# smart_location[,46]
# country_code[,47], country [,48]
# property_type[,52]
# room_type[,53]
# accomodates[,54]
# bathrooms[,55], bedrooms[,56], beds[,57]
# bed_type[,58] recode into numeric category variable
# security_deposit[,64]
# cleaning_fee[,65]
# guests_included[,66]
# number_of_reviews[,83], (number_of_reviews_ltm[,84])
# review_scores_rating[,87], (review_scores_accuracy[,88]), 
# (review_scores_cleanliness[,89]), (review_scores_checkin[,90]),
# (review_scores_communication[,91]), (review_scores_location[,92])
# (review_score_value[,93])
# cancellation_policy[,99]
# reviews_per_month[,106]

# Pecuiliar variables that need discussion in the group
# host_reponse_rate[,27] variables is in percent & incldues NA(Na can be removed)
# host_is_superhost[,29] coded true or false can be recoded 0,1

# host_listing_count [,33] # potential need to recode, cannot sum
# host_total_listings_count[,34] # potential need to recode, cannot sum
# host_verifications [,35]
# amenities[,59]


# NA variables 
sum(bnb$host_acceptance_rate == "N/A") # 20239
sum(bnb$host_picture_url == "N/A") # 20239
sum(bnb$host_total_listings_count == 0 )
sum(is.na(bnb$square_feet)) # 19873 # high missing values


# Outcome variable 
# price[,61](weeky_price[,62], monthly_price[,63])


#########---------- Data Preperation & Examination -------------#########

#----------create select subset of data & prepare for analysis 

subs = as.data.frame(bnb[c(23:24,26,32,36:40,42:48,52:58,64:66,83:84,87:93,
                           99,106,61)])

# examine type of each column

i = 1
for (i in i:38){
print(typeof(subs[,i]))
i = i + 1

}
# recode variables for analysis 
subs[,38] = as.numeric(subs[,38])

