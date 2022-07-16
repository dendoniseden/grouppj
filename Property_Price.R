library(tidyverse)
rm(list = ls())

sub <- read.csv("Subscription_General.csv")
property <- read.csv("PropertyPrice_LatLong_Final.csv") %>%
  select("Account.ID","Price.per.square.feet") %>%
  na.omit()

#Merge Property Price table with Subscription General  
property_sub <- merge(x=sub, y=property, by="Account.ID", all.x=FALSE, all.y=FALSE, na.rm=TRUE)
property_sub <- property_sub %>% 
  group_by(Account.ID) %>%
  mutate(Monetary=sum(Subscription.Amount)) %>%
  distinct()

#Standardize Monetary Values and Remove Outliers
property_sub$ZMonetary <- (property_sub$Monetary - mean(property_sub$Monetary)) / sd(property_sub$Monetary)
property_sub$ZPrice.per.square.feet = (property_sub$Price.per.square.feet - mean(property_sub$Price.per.square.feet)) / sd(property_sub$Price.per.square.feet)

property_sub <- property_sub %>%
  filter(ZMonetary<10) %>%
  filter(ZPrice.per.square.feet <10)

#Model relationship between Property Price and Monetary
model_property_price <- lm(Monetary ~ Price.per.square.feet, data=property_sub)
summary(model_property_price)

plot(x=property_sub$ZPrice.per.square.feet, y=property_sub$ZMonetary)
