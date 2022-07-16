library(tidyverse)
rm(list = ls())

sub <- read.csv("Subscription_General.csv")

#Set variable "Season" as numeric value of "Season"
sub$Season <- ifelse(sub$Season=="2013-2014",2013,sub$Season)
sub$Season <- ifelse(sub$Season=="2014-2015",2014,sub$Season)
sub$Season <- ifelse(sub$Season=="2015-2016",2015,sub$Season)
sub$Season <- ifelse(sub$Season=="2016-2017",2016,sub$Season)
sub$Season <- ifelse(sub$Season=="2017-2018",2017,sub$Season)
sub$Season <- ifelse(sub$Season=="2018-2019",2018,sub$Season)
sub$Season <- ifelse(sub$Season=="2019-2020",2019,sub$Season)
sub$Season <- ifelse(sub$Season=="2020-2021",2020,sub$Season)
sub$Season <- ifelse(sub$Season=="2021-2022",2021,sub$Season)
sub$Season <- as.numeric(sub$Season)

#Set variables for subscriptions amounts for each year
sub$Amount2013 <- ifelse(sub$Season==2013,sub$Subscription.Amount,0)
sub$Amount2014 <- ifelse(sub$Season==2014,sub$Subscription.Amount,0)
sub$Amount2015 <- ifelse(sub$Season==2015,sub$Subscription.Amount,0)
sub$Amount2016 <- ifelse(sub$Season==2016,sub$Subscription.Amount,0)
sub$Amount2017 <- ifelse(sub$Season==2017,sub$Subscription.Amount,0)
sub$Amount2018 <- ifelse(sub$Season==2018,sub$Subscription.Amount,0)
sub$Amount2019 <- ifelse(sub$Season==2019,sub$Subscription.Amount,0)
sub$Amount2020 <- ifelse(sub$Season==2020,sub$Subscription.Amount,0)
sub$Amount2021 <- ifelse(sub$Season==2021,sub$Subscription.Amount,0)

#Create Account.ID level sales per year
sub <- sub %>%
  group_by(Account.ID) %>%
  mutate(Amount2013.2=sum(Amount2013)) %>%
  mutate(Amount2014.2=sum(Amount2014)) %>%
  mutate(Amount2015.2=sum(Amount2015)) %>%
  mutate(Amount2016.2=sum(Amount2016)) %>%
  mutate(Amount2017.2=sum(Amount2017)) %>%
  mutate(Amount2018.2=sum(Amount2018)) %>%
  mutate(Amount2019.2=sum(Amount2019)) %>%
  mutate(Amount2020.2=sum(Amount2020)) %>%
  mutate(Amount2021.2=sum(Amount2021))

#Calculate RECENCY score for each entry (Independent Variable #1)
sub$Recency2014 <- ifelse(2014-sub$Season>0,2014-sub$Season,0)
sub$Recency2015 <- ifelse(2015-sub$Season>0,2015-sub$Season,0)
sub$Recency2016 <- ifelse(2016-sub$Season>0,2016-sub$Season,0)
sub$Recency2017 <- ifelse(2017-sub$Season>0,2017-sub$Season,0)
sub$Recency2018 <- ifelse(2018-sub$Season>0,2018-sub$Season,0)
sub$Recency2019 <- ifelse(2019-sub$Season>0,2019-sub$Season,0)
sub$Recency2020 <- ifelse(2020-sub$Season>0,2020-sub$Season,0)
sub$Recency2021 <- ifelse(2021-sub$Season>0,2021-sub$Season,0)
sub$Recency2022 <- ifelse(2022-sub$Season>0,2022-sub$Season,0)
summary(sub)

#Create and model 2016 RFM data
sub2016 <- sub %>%
  filter(Season < 2016) %>%
  group_by(Account.ID) %>%
  mutate(Frequency=n_distinct(Season)) %>%
  mutate(Monetary=sum(Subscription.Amount)) %>%
  select("Account.ID","Amount2017.2","Recency2016","Frequency","Monetary") %>%
  distinct() %>%
  distinct(Monetary,.keep_all=TRUE)

colnames(sub2016) <- c("Account.ID","Next.Year.Amount", "Recency","Frequency","Monetary")
summary(sub2016)

#Standardize Monetary Values and Remove Outliers
sub2016$ZMonetary <- (sub2016$Monetary - mean(sub2016$Monetary)) / sd(sub2016$Monetary)
sub2016$ZNext.Year.Amount <- (sub2016$Next.Year.Amount - mean(sub2016$Next.Year.Amount)) / sd(sub2016$Next.Year.Amount)
sub2016 <- sub2016 %>%
  filter(ZNext.Year.Amount<10)

#Regress 2017 Sales against 2016 RFM Data
model2016 <- lm(ZNext.Year.Amount ~ Recency + Frequency + ZMonetary, data = sub2016)
summary(model2016)

#Same steps for 2017
sub2017 <- sub %>%
  filter(Season < 2017) %>%
  group_by(Account.ID) %>%
  mutate(Frequency=n_distinct(Season)) %>%
  mutate(Monetary=sum(Subscription.Amount)) %>%
  select("Account.ID","Amount2018.2","Recency2017","Frequency","Monetary") %>%
  distinct() %>%
  distinct(Monetary,.keep_all=TRUE)

colnames(sub2017) <- c("Account.ID","Next.Year.Amount", "Recency","Frequency","Monetary")

sub2017$ZMonetary <- (sub2017$Monetary - mean(sub2017$Monetary)) / sd(sub2017$Monetary)
sub2017$ZNext.Year.Amount <- (sub2017$Next.Year.Amount - mean(sub2017$Next.Year.Amount)) / sd(sub2017$Next.Year.Amount)
sub2017 <- sub2017 %>%
  filter(ZNext.Year.Amount<10)%>%
  filter(ZMonetary<10)

model2017 <- lm(ZNext.Year.Amount ~ Recency + Frequency + ZMonetary, data = sub2017)
summary(model2017)

#Same steps for 2018
sub2018 <- sub %>%
  filter(Season < 2018) %>%
  group_by(Account.ID) %>%
  mutate(Frequency=n_distinct(Season)) %>%
  mutate(Monetary=sum(Subscription.Amount)) %>%
  select("Account.ID","Amount2019.2","Recency2018","Frequency","Monetary") %>%
  distinct() %>%
  distinct(Monetary,.keep_all=TRUE)

colnames(sub2018) <- c("Account.ID","Next.Year.Amount", "Recency","Frequency","Monetary")

sub2018$ZMonetary <- (sub2018$Monetary - mean(sub2018$Monetary)) / sd(sub2018$Monetary)
sub2018$ZNext.Year.Amount <- (sub2018$Next.Year.Amount - mean(sub2018$Next.Year.Amount)) / sd(sub2018$Next.Year.Amount)
sub2018 <- sub2018 %>%
  filter(ZNext.Year.Amount<10)%>%
  filter(ZMonetary<10)

model2018 <- lm(ZNext.Year.Amount ~ Recency + Frequency + ZMonetary, data = sub2018)
summary(model2018)

#Same steps for 2019
sub2019 <- sub %>%
  filter(Season < 2019) %>%
  group_by(Account.ID) %>%
  mutate(Frequency=n_distinct(Season)) %>%
  mutate(Monetary=sum(Subscription.Amount)) %>%
  select("Account.ID","Amount2020.2","Recency2019","Frequency","Monetary") %>%
  distinct() %>%
  distinct(Monetary,.keep_all=TRUE)

colnames(sub2019) <- c("Account.ID","Next.Year.Amount", "Recency","Frequency","Monetary")

sub2019$ZMonetary <- (sub2019$Monetary - mean(sub2019$Monetary)) / sd(sub2019$Monetary)
sub2019$ZNext.Year.Amount <- (sub2019$Next.Year.Amount - mean(sub2019$Next.Year.Amount)) / sd(sub2019$Next.Year.Amount)
sub2019 <- sub2019 %>%
  filter(ZNext.Year.Amount<10) %>%
  filter(ZMonetary<10)

model2019 <- lm(ZNext.Year.Amount ~ Recency + Frequency + ZMonetary, data = sub2019)
summary(model2019)

#Combine data for 2016-2019 and modeling
CombineData <- bind_rows(sub2016,sub2017,sub2018,sub2019)
CombineModel <- lm(ZNext.Year.Amount ~ Recency + Frequency + ZMonetary, data = CombineData)

summary(CombineModel)

plot(x = CombineData$Recency, y = CombineData$ZNext.Year.Amount)
plot(x = CombineData$Frequency, y = CombineData$ZNext.Year.Amount)
plot(x = CombineData$ZMonetary, y = CombineData$ZNext.Year.Amount)