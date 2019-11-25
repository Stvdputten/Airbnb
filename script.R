library(readr)
library(MASS)
library(car)
library(stringr)
library(corrplot)
library(randomcoloR)
library(BioGeoBEARS)
listings <- read_csv("airbnb/listings.csv")
colnames(listings)

henk <- as.numeric(gsub('[$,]', '', listings$price));

listings$price <- as.numeric(gsub('[$,]', '', listings$price));
listings$weekly_price <- as.numeric(gsub('[$,]', '', listings$weekly_price));
listings$monthly_price <- as.numeric(gsub('[$,]', '', listings$monthly_price));
listings$num_amenities <- str_count(listings$amenities, ',')+1;
listings$description_length <- nchar(listings$description);

listingsPlot <- listings[listings$price<500 & listings$price>0 & listings$number_of_reviews>=5 
                 & listings$review_scores_rating>=60 & is.not.na(listings$review_scores_rating),];

truehist(listings$price[listings$price<500], xlim = c(0,500),
     xlab = "Price in Dollar", main = "Price Distribution", ylab = "Frequency")
truehist(listings$review_scores_rating[listings$review_scores_rating>=50 & listings$number_of_reviews>=10],
         main = "Review Score Distribution", xlab = "Review Score", ylab = "Frequency");
plot(price ~ review_scores_rating, 
     data = listings[listings$price<500 & listings$number_of_reviews>=10 
                     & listings$review_scores_rating>=60,],
     main="Review score versus price", ylab = "Price", xlab = "Review Score")
abline(lm(price ~ review_scores_rating, 
          data = listings[listings$price<500 & listings$number_of_reviews>=10 
                          & listings$review_scores_rating>=60,]), col="red")

scatterplotMatrix(subset(listingsPlot, select=c("price", "review_scores_rating", "number_of_reviews")))

plot(price ~ square_feet, data=listings[listings$price<500 & listings$square_feet>0,])
abline(lm(price ~ square_feet, data=listings[listings$price<500 & listings$square_feet>0,]))
qqPlot(listings[listings$price<500 & listings$square_feet>0,]$square_feet)
qqPlot((listingsPlot$price)^0.01)

op <- par(no.readonly = TRUE)
par(mar=c(16, 4, 2, 2))
plot(price ~ as.factor(neighbourhood_cleansed), data = listingsPlot, ylab = "Price", xlab= "", main="Price per neighbourhood", las = 2, cex.axis = 1)
par(op)

geo_colors <- distinctColorPalette(k = 500)[match(listings$neighbourhood_cleansed,listings$neighbourhood_cleansed)];
plot(latitude ~ longitude, data=listings, col=geo_colors)

colfunc <- colorRampPalette(c("green", "red"))
price_colors <- colfunc(23)[ceiling(sqrt(listingsPlot$price))]
plot(latitude ~ longitude, data=listings, col=price_colors)

plot(number_of_reviews ~ price, data = listingsPlot)
abline(lm(number_of_reviews ~ price, data = listingsPlot))

lmo <- lm(price^0.1 ~ neighbourhood_cleansed+accommodates+guests_included+
            num_amenities+description_length+availability_365+reviews_per_month+
            number_of_reviews+bathrooms+bedrooms

, data = listingsPlot)
qqPlot(lmo$residuals)
plot(lmo$residuals, main= "Residuals", ylab = "", xlab="")
summary(lmo)
summary(listings)
 
plot(lmo)

plot(price ~ square_feet, data = listings[listings$price<500,], ylim=c(0, 500),xlim=c(0, 2500))
plot(price ~ reviews_per_month, data = listings[listings$price<500,], ylim=c(0, 500))


listingsCorr <- subset(listingsPlot[listingsPlot$availability_365>0 & listingsPlot$price>0,], select=c('price', 'availability_365'));
listingsCorr$price <- as.numeric(listingsCorr$price);
listingsCorr$availability_365 <- as.numeric(listingsCorr$availability_365);
res <- cor(listingsCorr)

corrplot(res)

library(ggmap)
newmap <- qmap(source = "osm", location = c(lon = -95.3632715, lat = 29.7632836), zoom = 14, force=TRUE)
plot(newmap, xlim = c(4.7, 5.1), ylim = c(52.2, 52.5), asp = 1)
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)

points(listings$longitude, listings$latitude, col = "red", cex = .6)

max(listings$longitude)
min(listings$longitude)


henk = as.data.frame(spTransform(c(listings$longitude, listings$latitude),osm()))
