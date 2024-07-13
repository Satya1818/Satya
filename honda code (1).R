library(tidyverse)
df <- read_csv("C:/Users/chari/Downloads/honda_sell_data (1).csv")
glimpse(df)
df$price<- parse_number(df$Price)
df <- df %>% subset(Year %in% 2021:2023)
glimpse(df)
sapply(df,function(x)sum(is.na(x))) # the first 7 columns is complete
df$priceCut <- cut(df$price,breaks=c(0,15000,25000,35000,45000,55000),labels=c('<=15k','15k~25k','25k~35k','35k~45k','45k~55k'))
df %>% group_by(priceCut) %>% summarize(n=n()) %>% filter(!is.na(priceCut)) %>%
  ggplot(aes(priceCut,n))+geom_col() # most Honda car pricing falls on USD25K-45k)
df1 <- df %>% drop_na 
df1 %>% dim() # remove all na ,then named f1
table(df$Year)
table(df1$Year) # we can see the missing values largely from 2023 through comparison
table(df$Condition)
table(df1$Condition)
df1 %>% group_by(Condition) %>% summarize(n=n(),'avgPrice'=mean(price),'avgScore'=mean(Consumer_Rating),'avgComfort'=mean(Comfort_Rating))
glimpse(df1)
df1 %>% group_by(Fuel_Type) %>% summarize(n=n(),'pct'=round(n/nrow(df1),2)) 
# Honda cars still use gasoline as main fuel,accounted for 89% of the total,and 11% for the hybrid fuel.<br>
df1$Drivetrain[df1$Drivetrain=='FWD'|df1$Drivetrain=='?'] <- 'Front-wheel Drive'
df1 %>% group_by(Drivetrain) %>% summarize(n=n(),'pct'=round(n/nrow(df1),2)) # Honda car prefers all-wheel drive and front-wheel drive with 56% and 43% respectively
df1$mpgMin <- str_split(df1$MPG,'?') %>% sapply('[[',1)
df1$mpgMax <- str_split(df1$MPG,'?') %>% sapply('[[',2)
df1 <- df1 %>% mutate('avgMpg'=(as.numeric(mpgMin)+as.numeric(mpgMax))/2)
head(df1)
df1 %>% group_by(priceCut) %>% summarize('avgMPG'=mean(avgMpg)) # The more expensive,the more fuel efficient<br>
