library(tidyverse)
listings <- read_csv("listings.csv", col_names = TRUE)
colnames(listings)
listings %>%
  select(price, neighbourhood) %>%
  filter(neighbourhood== 'De Pijp') %>%
  mutate(neighbourhood=as.factor(neighbourhood)) %>%
  ggplot(aes(neighbourhood)) + geom_boxplot()
summary(listings)
listings$host_neighbourhood
