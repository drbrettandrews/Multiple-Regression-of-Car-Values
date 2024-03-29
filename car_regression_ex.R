#install packages
install.packages ("tidyverse")

#load libraries
library(tidyverse)

#set working directory (adjust this for your own computer)
setwd("/Users/####")

#read dataset into R
cardf <- read.csv("ToyotaCorolla560.csv")
View(cardf)

#recode FuelType variable with 0 for Diesel and 1 for Petrol
cardf$FuelType<-ifelse(cardf$FuelType=="Petrol",1,0)

#Convert categorical variables to factors with levels and labels
cardf$FuelType<-factor(cardf$FuelType,levels = c(0,1),labels = c("Diesel","Petrol"))
cardf$MetColor<-factor(cardf$MetColor,levels = c(0,1),labels = c("No","Yes"))
cardf$Automatic<-factor(cardf$Automatic,levels = c(0,1),labels = c("No","Yes"))

#check for missing data
sum(is.na(cardf))

#generate summary statistics for all variables in dataframe
summary(cardf)

#create a scatterplot showing the relationship between Age and Price and add
#a regression line to the scatterplot
ggplot(data = cardf, mapping = aes(x = Age, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Scatterplot for Car Age and Car Price", x = "Age", y = "Price")

#Calculate a correlation coefficient for the relationship between Age and Price
cor(cardf$Age, cardf$Price)

#Perform a simple linear regression with Car Age and Car Price
toyota_SLR <- lm(Price ~ Age, data = cardf)

#View the simple linear regression output
summary(toyota_SLR)

#create a correlation matrix with all quantitative variables in the dataframe
cor(cardf[c(1, 2, 3, 5)])

#turn off scientific notation for all variables
options(scipen=999) 

#Perform a multiple regression with Car Age, KM, and Horsepower and Car Price
toyota_MR <- lm(Price ~ Age + KM + Horsepower, data = cardf)

#View the multiple regression output
summary(toyota_MR)

#install lm.beta package to extract standardized regression coefficients
install.packages ("lm.beta")

#load lm.beta
library(lm.beta)

#Extract standardized regression coefficients
lm.beta(toyota_MR)

#View the multiple regression output
summary(toyota_MR)

#Steps to create a scatterplot of residuals vs. predicted values of the 
#dependent variable

#Create a vector of predicted values generated from the multiple 
#regression above
toyota_pred = predict(toyota_MR)

#Create a vector of residuals generated from the multiple regression above
toyota_res = resid(toyota_MR)

#Create a data frame of the predicted values and the residuals
pred_res_df <- data.frame(toyota_pred, toyota_res)

#create a scatterplot of the residuals versus the predicted values
ggplot(data = pred_res_df, mapping = aes(x = toyota_pred, y = toyota_res)) +
  geom_point() +
  labs(title = "Plot of residuals vs. predicted values", x = "Predicted values",
       y = "Residuals")

#Steps to create a Normal Probability Plot 

#create a vector of standardized residuals generated from the multiple
#regression above
toyota_std.res = rstandard(toyota_MR)

#produce normal scores for the standardized residuals and create
#normal probability plot
qqnorm(toyota_std.res, ylab = "Standardized residuals", xlab = "Normal scores")

#install packages
install.packages ("car")

#load libraries
library(car)

#create a correlation matrix with all quantitative variables in the dataframe
cor(cardf[c(1, 2, 3, 5)])

#calculate Variance Inflation Factor for each variable to assess 
#multicollinearity
vif(toyota_MR)

#Perform a multiple regression with Car Age, KM, Horsepower, FuelType, 
#MetColor and Automatic as predictor variables and Car Price as the outcome
#variable
toyota_MR_Cat <- lm(Price ~ Age + KM + Horsepower + FuelType + MetColor + Automatic,
                data = cardf)

#View multiple regression output
summary(toyota_MR_Cat)

#Steps to create a new scatterplot of residuals vs. predicted values of the 
#dependent variable with the new categorical variables added

#Create a vector of predicted values generated from the multiple 
#regression above
toyota_pred2 = predict(toyota_MR_Cat)

#Create a vector of residuals generated from the multiple regression above
toyota_res2 = resid(toyota_MR_Cat)

#Create a data frame of the predicted values and the residuals
pred_res_df2 <- data.frame(toyota_pred2, toyota_res2)

#create a scatterplot of the residuals versus the predicted values
ggplot(data = pred_res_df2, mapping = aes(x = toyota_pred2, y = toyota_res2)) +
  geom_point() +
  labs(title = "Plot of residuals vs. predicted values", x = "Predicted values",
       y = "Residuals")
  

#Steps to create a Normal Probability Plot 

#create a vector of standardized residuals generated from the multiple
#regression above
toyota_std.res = rstandard(toyota_MR)

#produce normal scores for the standardized residuals and create
#normal probability plot
qqnorm(toyota_std.res, ylab = "Standardized residuals", xlab = "Normal scores")


#partition the data into a training set and a validation set
#set seed so the random sample is reproducible
set.seed(42)
sample <- sample(c(TRUE, FALSE), nrow(cardf), replace=TRUE, prob=c(0.7,0.3))
traincar  <- cardf[sample, ]
validatecar <- cardf[!sample, ]

#Install package needed for best subsets procedure
install.packages("olsrr")

#Load olsrr library
library(olsrr)

#run a multiple regression model using the "train" dataframe and all 
#available independent variables
toyota_MR_Alltrain <- lm(Price ~ Age + KM + Horsepower + FuelType + MetColor + 
                           Automatic, data = traincar)

summary(toyota_MR_Alltrain)

#run best subsets procedure with multiple regression output "toyota_MR_Alltrain"
bestsubset <- ols_step_all_possible(toyota_MR_Alltrain)

View(bestsubset)

#run a final multiple regression model using the "validate" dataframe 
#and the following predictors: Age, KM, Horsepower, FuelType, and Automatic
toyota_MR_Val <- lm(Price ~ Age + KM + Horsepower + FuelType + Automatic, 
                    data = validatecar)

summary(toyota_MR_Val)

#read inventory dataset into R
inventorydf <- read.csv("toyota_corolla_inventory.csv")
View(inventorydf)

#Convert categorical variables to factors with levels and labels
inventorydf$FuelType<-factor(inventorydf$FuelType,levels = c(0,1),labels = c("Diesel","Petrol"))
inventorydf$Automatic<-factor(inventorydf$Automatic,levels = c(0,1),labels = c("No","Yes"))

#estimate predicted y values and prediction intervals for five additional 
#Toyota Corollas in inventory
predict(toyota_MR_Val, inventorydf, interval = "prediction", level = 0.95)



