
install.packages("psych")
library(psych)



library(plyr)   
library(rpart.plot)  
library(caret) 
library(gridExtra)  
library(tidyverse)  
library(rsample) 
library(e1071)  
library(GGally) 
library(data.table) 
library(DT) 


library(readr) 
library(ggplot2) 
library(dplyr) 
library(tidyr) 
library(corrplot) 

library(generalhoslem) 

setwd("C:/Users/yam27/OneDrive/Documents/Applied Stats") 
getwd() 
churn <- read.csv("customer_churn_data.csv") 
glimpse(churn) 
summary(churn) 

sapply(churn, function(x) sum(is.na(x))) 

churn[is.na(churn$TotalCharges),] 

sum(is.na(churn$TotalCharges))/nrow(churn) 

nrow(churn) 

remove_clean <- na.omit(churn) 
churn_clean <- na.omit(churn) 
sapply(churn_clean, function(x) sum(is.na(x))) 

# Summary for tenure 
summary(churn_clean$tenure) 

# Summary for monthly charges 
summary(churn_clean$MonthlyCharges) 

# Summary for total charges 
summary(churn_clean$TotalCharges) 



churn_clean$InternetService <- 
  as.numeric(mapvalues(churn_clean$InternetService,from=c("No", "DSL","Fiber 
optic"),to=c("0", "1", "2"))) 
churn_clean$InternetService 

glimpse(churn_clean) 

#describe(churn_clean$Churn) 
describe(churn_clean)
describe(churn_clean$tenure) 
glimpse(churn_clean) 
str(churn_clean) 
head(churn_clean) 

Churn <- as.factor(churn_clean$Churn) 
