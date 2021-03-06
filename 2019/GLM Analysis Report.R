####----Libraries
# library(dplyr)
# library(ggplot2)
# library(car)
# library(leaps)
# library(tidyverse)
# library(xtable)
#########---------- Linear Models AIR BNB Analysis -------------#########


# bnb = read.csv(file="listings.csv", header=TRUE, sep=",")

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
# # host_total_listings_count[,34] # potential need to recode, cannot sum
# # host_verifications [,35]
# # amenities[,59]
# 
# 
# # NA variables 
# sum(bnb$host_acceptance_rate == "N/A") # 20239
# sum(bnb$host_picture_url == "N/A") # 20239
# sum(bnb$host_total_listings_count == 0 )
# sum(is.na(bnb$square_feet)) # 19873 # high missing values
# 
# 
# # Outcome variable 
# # price[,61](weeky_price[,62], monthly_price[,63])
# 
# #########---------- Data Preperation & Examination -------------#########
# 
# #----------create select subset of data & prepare for analysis 
# 
# subs = as.data.frame(bnb[c(23:24,26,32,36:40,42:48,52:58,64:66,83:84,87:93,
#                            99,106,61)])
# 
# # some preprocessing done in Precessing R file
# subset = read.csv('2019/subset.csv')
# 
# colnames(subset) # variables review scores can be removed except review score rating
# # and number of review scores review score 
# colnames(subset[,19]) # review scorerating
# colnames(subset[,17]) # number_of_reviews
# 
# # remove additonal variables
# subset = subset[-c(1,3,18,20:25)]
# 
# # some NA can be recoded into 0 e.g. security deposite & cancelation fee
# subset$security_deposit[is.na(subset$security_deposit)] = 0 
# subset$cleaning_fee[is.na(subset$cleaning_fee)] = 0 
# 
# # remove NA rows 
# subset = na.omit(subset)
# # save subset as csv
# write.csv(subset, '2019/subset.csv')
# 

#########--- START SCRIPT FROM HERE ---####################
library(dplyr)
library(ggplot2)
library(car)
library(leaps)
library(tidyverse)
library(xtable)

subset = read_csv("2019/subset.csv")
colnames(subset)

# change class of variables for analysis 
# subset$host_response_time <- as_factor(subset$host_response_time)
# subset$cancellation_policy <- as_factor(subset$cancellation_policy)
# subset$room_type <- as_factor(subset$room_type)
# subset$bed_type <- as_factor(subset$bed_type)
# subset$beds <- as_factor(subset$beds)
# subset$property_type <- as_factor(subset$property_type)
# subset$neighbourhood_cleansed <- as_factor(subset$neighbourhood_cleansed)
# subset$security_deposit <- as.numeric(gsub('\\$', '', subset$security_deposit))
# subset$cleaning_fee <- as.numeric(gsub('\\$', '', subset$cleaning_fee))
# subset$bathrooms <- as_factor(subset$bathrooms)
# subset$accommodates <- as_factor(subset$accommodates)
# subset$guests_included <- as_factor(subset$guests_included)


# number of variables in dataset
ncol(subset)
colnames(subset)


#########---------- Regression Assumptions -------------#########
#-------Collinearity: Independence --------#
# variance Inflation Factor
var_inf = vif(lm(price ~ ., subset)) # number of vifs exceed treshold value, 
# as such there is collinearity present between predictor sets
# predictors can be removed and analysis should be re-run determine if still present
summary(var_inf)
var_inf
xtable(var_inf)
# remove variable with highest VIF = beds
subset = subset[-9]
#rerun vif
var_inf2 = vif(lm(price ~ ., subset))
var_inf2 # no VIF above 10, now its ok collinearity contained
xtable(var_inf2)
#-------- Linear Model ---------#
x = model.matrix(price ~ ., subset)
l_m = lm(price ~ x, subset)
l_m_2 = lm(log10(price) ~ x, subset)
l_m_f = l_m$fitted.values
l_m_f_2 = l_m_2$fitted.values

#----------------- Nomal Errors ------------------#
l_m_res = as.data.frame(l_m$residuals)
l_m_res_2 = as.data.frame(l_m_2$residuals)

l_m_res = as.data.frame(l_m$residuals)

ggplot(l_m_res, aes(sample = l_m_res$`l_m$residuals`)) + stat_qq() +
  ggtitle("Quantile comparison plot Errors") + theme_light() +
  labs(x= "Theoretical Quantiles", y= "Error Quantiles") + stat_qq_line()


ggplot(l_m_res_2, aes(sample = l_m_res_2$`l_m_2$residuals`)) + stat_qq() +
  ggtitle("Quantile comparison plot Errors") + theme_light() +
  labs(x= "Theoretical Quantiles", y= "Error Quantiles") + stat_qq_line()

plot(density(l_m_res$`l_m$residuals`))
plot(density(l_m_res_2$`l_m_2$residuals`))


# examine normality of individual variables

# plot(density(subset[,12])) # deposit
# plot(density(subset[,13])) # cleaning fee
# plot(density(subset[,15])) # number of reviews
# plot(density(subset[,16])) # review score rating
# plot(density(subset[,18])) # review per month
# plot(density(subset[,19])) # price 
# 
# # trial and error of transformations
# plot(density(log10(subset[,12]))) # deposit log transformation # no transformation normalizes
# plot(density(log10(subset[,13]))) # cleaning fee no transformation normalizes
# 
# plot(density(log10(subset[,15]))) # log number of reviews  somewhat better 
# plot(density(subset[,16]^40)) # review score rating # no transformation normalized
# plot(density(log10(subset[,18]))) # review per month 
# plot(density(log10(subset[,19]))) # price # somehow better


#----------------- Constant Errors ------------------#

plot(l_m_f,l_m_res$`l_m$residuals`, main="Residual plot")

plot(l_m_f_2,l_m_res_2$`l_m_2$residuals`, main="Residual plot")

ggplot(subset,aes(l_m_f,
                  l_m_res$`l_m$residuals`)) +
  geom_point() + theme_light() + labs(x= "Price", y= "Residuals") +
  geom_smooth(method= "loess", se=FALSE) + ggtitle("Residual plot")

ggplot(subset,aes(l_m_f_2,
                  l_m_res_2$`l_m_2$residuals`)) +
  geom_point() + theme_light() + labs(x= "Price", y= "Residuals") +
  geom_smooth(method= "loess", se=FALSE) + ggtitle("Residual plot")

# Rule of Thumb for non-constant error variance
# if ratio of smallest vs biggest residual is bigger than 10 or 4 its a problem
abs(max(l_m$residuals)) / abs(min(l_m$residuals)) < 10
abs(max(l_m$residuals)) / abs(min(l_m$residuals)) < 4


#----------------- Linearity ------------------#
lin_c = as.data.frame(subset[c(1:17)])
colnames(lin_c)

# Save variables sepeartely for Component + Residual plot
host_response_time = lin_c$host_response_time
host_has_profile_pic = lin_c$host_has_profile_pic
host_identity_verified = lin_c$host_identity_verified
neighbourhood_cleansed = lin_c$neighbourhood_cleansed
property_type = lin_c$property_type
room_type = lin_c$room_type
accommodates = lin_c$accommodates
bathrooms = lin_c$bathrooms
bedrooms = lin_c$bedrooms
bed_type = lin_c$bed_type
security_deposit = lin_c$security_deposit
cleaning_fee = lin_c$cleaning_fee
guests_included = lin_c$guests_included
number_of_reviews = lin_c$number_of_reviews
review_scores_rating = lin_c$review_scores_rating
cancellation_policy = lin_c$cancellation_policy
reviews_per_month = as.integer(lin_c$reviews_per_month)

# same lm model as before, but lets make sure its here
l_c = lm(price ~ ., lin_c)

# Component + Residual plot to examine the relationship between Y and each X 
# individually. CoPlots show the  partial relationship of Y and X 
# controlling for other Xs.
crPlot(l_c, host_response_time)
crPlot(l_c, host_identity_verified)
crPlot(l_c, neighbourhood_cleansed)
crPlot(l_c, property_type)
crPlot(l_c, room_type)
crPlot(l_c, accommodates)
crPlot(l_c, bathrooms)
crPlot(l_c, beds)
crPlot(l_c, bed_type)
crPlot(l_c, security_deposit)
crPlot(l_c, cleaning_fee)
crPlot(l_c, guests_included)
crPlot(l_c, number_of_reviews)
crPlot(l_c, cleaning_fee)
crPlot(l_c, review_scores_rating)
crPlot(l_c, cancellation_policy)
crPlot(l_c, reviews_per_month)


lin_c2 = lin_c
lin_c2$cleaning_fee = sqrt(lin_c2$cleaning_fee)
lin_c2$reviews_per_month = sqrt(lin_c2$reviews_per_month)

l_c2 = lm(price ~ ., lin_c2)
crPlot(l_c2, reviews_per_month)
crPlot(l_c2, cleaning_fee)


#----------------- Influential Cases ------------------#

#---------- Hat-Values 
hat_v = hatvalues(l_m)
hat_v2 = hatvalues(l_m_2)
#---------- studentized Residuals
s_resid = rstudent(l_m)
s_resid2 = rstudent(l_m_2)
#---------- Cook's Residuals --------#
cd = as.vector(cooks.distance(l_m))
cd2 = as.vector(cooks.distance(l_m_2))
xtable(var_inf)
# bubble plot of laverage vs discrepancy 
ggplot(l_m, aes(x = hat_v, y = s_resid, size = cd)) +
  geom_point(aes(color = )) + theme_light() +
  ggtitle("Leverage vs. Studentized residuals No Trasnformation") +
  labs(x = "Leverage", y = "Studentized Residuals", size = "Cook's Distance")

ggplot(l_m_2, aes(x = hat_v2, y = s_resid2, size = cd2)) +
  geom_point(aes(color =  )) + theme_light() +
  ggtitle("Leverage vs. Studentized residuals Trasnformed") +
  labs(x = "Leverage", y = "Studentized Residuals", 
       size = "Cook's Distance")
