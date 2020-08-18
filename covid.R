library(ggplot2)
library(dplyr)
library(readr)
library(forecast)
library(fpp2)
library(TTR)
library(caret)
library(corrplot)
library(e1071)
library(party)
##COVID19 project: 
#1. Perform any needed data cleaning/preparation.
#2. Display aggregate information on your data, perhaps stratified by age, gender, etc..
#3. Create visualizations of univariate distributions of key variables.
#4. Create visualizations of bivariate or trivariate relationships that tell the story of your data (ex:
#scatter plot of regional mortality vs time-to shelter-in-place order, aligned according to date of 20th case, 
#colored by a measure of wealth.
#5. Optional: Fit model(s) of your choosing (classification/regression). Be sure to evaluate your
#models using appropriate visualizations (ex: Residuals vs Fitted plots, ROC curves) and statistics
#(ex: sensitivity/recall, specificity, precision, confusion matrix, AUC).

################################################### EDA ##########################################################
##Johns Hopkins Dataset
##data cleaning 
#read in time series confirmed global data 
setwd('C:/USFMSHI/HS630/hw/assign6/COVID-19/csse_covid_19_data/csse_covid_19_time_series')
time_series <- read.csv("time_series_covid19_confirmed_global.csv", header = T)

#extract out china data
time_china <- time_series[which(time_series$Country.Region == "China"), ]
#take out country, lat and long
time_china <- time_china[, c(-2, -3, -4)]  
time_china <- time_china[,-1]
#first day: 1/22
days <- c(1:97)

colnames(time_china) <- days
China_confirmed <- c()
for(i in colnames(time_china)){
  #print(sum(time_china[,i])) 
  China_confirmed[i] <- sum(time_china[,i])
}

China <- as.data.frame(China_confirmed)
colnames(China) <- "Confirmed"

#read in time series death global data 
time_series2 <- read.csv("time_series_covid19_deaths_global.csv", header = T)
time_series2

#extract out china data
time_china2 <- time_series2[which(time_series2$Country.Region == "China"), ]
#take out country, lat and long
time_china2 <- time_china2[, c(-2, -3, -4)]  
time_china2 <- time_china2[,-1]

colnames(time_china2) <- days
China_deaths <- c()
for(i in colnames(time_china2)){
  #print(sum(time_china[,i])) 
  China_deaths[i] <- sum(time_china2[,i])
}

China_deaths
China$Deaths <- China_deaths

#read in time series recovered global data 
time_series3 <- read.csv("time_series_covid19_recovered_global.csv", header = T)
time_series3

#extract out china data
time_china3 <- time_series3[which(time_series2$Country.Region == "China"), ]
#take out country, lat and long
time_china3 <- time_china3[, c(-2, -3, -4)]  
time_china3 <- time_china3[,-1]

colnames(time_china3) <- days
China_recovered <- c()
for(i in colnames(time_china3)){
  China_recovered[i] <- sum(time_china3[,i])
}

China_recovered
China$Recovered <- China_recovered

#create columns that hold confirmed, death and recovered cases for each day instead of accumulation
#confirmed
day_confirmed <- c()
for(i in (2:length(China$Confirmed))){
  
  day_confirmed[i] <- China$Confirmed[i]- China$Confirmed[i-1]
}
day_confirmed[1] <- China$Confirmed[1]
China$day_confirmed <- day_confirmed

#death
day_death <- c()
for(i in (2:length(China$Deaths))){
  
  day_death[i] <- China$Deaths[i]- China$Deaths[i-1]
}
day_death[1] <- China$Deaths[1]
China$day_death <- day_death

#recovered
day_rec <- c()
for(i in (2:length(China$Recovered))){
  
  day_rec[i] <- China$Recovered[i]- China$Recovered[i-1]
}
day_rec[1] <- China$Recovered[1]
China$day_recovered <- day_rec

#add day column
startDate <- as.Date("2020-01-22")
endDate <- as.Date("2020-04-27")
date <- seq(startDate, endDate, "1 day")  
China$day <- date

##ACAPS Dataset
setwd('C:/USFMSHI/HS630/hw/assign6/other_datasets')
safety <- read.csv("acaps.csv", header = T, stringsAsFactors = FALSE)

#keep variables that are necessary 
safety <- data.frame(safety$COUNTRY, safety$CATEGORY, safety$MEASURE, safety$TARGETED_POP_GROUP, safety$DATE_IMPLEMENTED, safety$SOURCE_TYPE)
safety
#make a dataframe that only has data of china
safety_china <- safety[which(safety$safety.COUNTRY == "China"), ]
safety_china
names <- c("Country", "Category", "Measure", "Targeted_by_pop", "Date_implemented", "Source_type")
colnames(safety_china) <- names
safety_china <- safety_china[,-1]

##data cleaning 
summary(safety_china)

#things to be cleaned:Category, targeted_pop_group, date_implemented, source_type
#Category: get rid of the blank level
lev_c <- c("Governance and socio-economic measures", "Humanitarian exemption", "Lockdown", "Movement restrictions", 
           "Public health measures", "Social distancing")
safety_china$Categories <- factor(safety_china$Category, ordered = F, levels = lev_c)
summary(safety_china$Categories)
safety_china <- safety_china[,-1]

#targeted_pop_group
lev <- c("No", "Yes")
#class(safety_china$Targeted_by_pop)
safety_china$target_pop <- factor(safety_china$Targeted_by_pop, ordered = F, levels = lev)
safety_china <- safety_china[,-2]

#source type
lev_s <- c("Government", "Media", "Other", "Other organisations", "Social media", "UN")
safety_china$Sources <- factor(safety_china$Source_type, ordered = F, levels = lev_s)
summary(safety_china$Sources)     #we have 3 NAs for this column
safety_china <- safety_china[,-3]

#date implemented
summary(safety_china$Date_implemented)
#change date to date variable
safety_china$Date_implemented <- as.Date(safety_china$Date_implemented, format = "%d/%m/%Y")
safety_china$Date_implemented
safety_china <- safety_china[order(safety_china$Date_implemented),]
safety_china$Date_implemented

#create a dataframe that only contains the dates that we want (match China dataset)
safety_china2 <- with(safety_china, safety_china[(Date_implemented >= "2020-01-22" & Date_implemented <= "2020-04-27"), ])

#remove NAs
safety_china2 <- na.omit(safety_china2)

#goal is to clean up the columns in order to combine with China dataframe. 
#issue now is: some dates have multiple entries of safety measures, and some dates have nothing at all

#measures are too specific
safety_china2[which(safety_china2$Date_implemented == '2020-03-16'), ]
#take out measure col
safety_china2 <- safety_china2[,-1]
#renumbering rows
row.names(safety_china2) <- 1:nrow(safety_china2)
safety_china2$date <- c()

#start the process of adding safety categoy and target by pop columns to China dataframe
#added a never level for the category- no action taken for dates with no entry 
cat_lev <- c("No action taken", "Social distancing", "Movement restrictions", "Public health measures", 
             "Governance and socio-economic measures", "Lockdown")

China$categories <- factor(rep(NA, nrow(China)), order = F, levels = cat_lev)
China$target <- factor(rep(NA, nrow(China)), order = F, levels = lev)

startD = as.Date("2020-01-22")
endD = as.Date("2020-04-27")
theDate <- startD

while(theDate <= endD){
  #if the date is not in safety_china2, aka no safety action was implemented that day
  if(!(theDate%in%safety_china2$Date_implemented)){
    China$categories[China$day == theDate] <- "No action taken"
    China$target[China$day == theDate] <- "No"
  }
  #if the date is in date_implemented
  else if(theDate%in%safety_china2$Date_implemented){
    #only one entry 
    if(nrow(safety_china2[safety_china2$Date_implemented == theDate, ]) == 1){
      China$categories[China$day == theDate] <- safety_china2$Categories[safety_china2$Date_implemented == theDate]
      China$target[China$day == theDate] <- safety_china2$target_pop[safety_china2$Date_implemented == theDate]
    }
    #dates with multiple safety categories
    else{
      #maybe implement this if case later
      China$categories[China$day == theDate] <- safety_china2$Categories[safety_china2$Date_implemented == theDate]
      China$target[China$day == theDate] <- safety_china2$target_pop[safety_china2$Date_implemented == theDate]
    }
  }
  theDate = theDate + 1 
}
##Dataset is done!##

############################################## Data Visualizations ###############################################
g1 <- ggplot(China, aes(day, Confirmed))
g1 + geom_point(aes(col = day)) + 
  labs(title = "China confirmed cases over days")

g2 <- ggplot(China, aes(day, Deaths))
g2 + geom_col(fill="#69b3a2") + 
  labs(title = "China death cases over days")

g3 <- ggplot(China, aes(day,Recovered))
g3 + geom_line(col = "purple", size = 2) + 
  labs(title = "China recovered cases over days")

g4 <-ggplot(China, aes(day, day_confirmed)) 
g4 + geom_line(col = "pink", size = 2) +
  labs(title = "Daily confirmed cases in China")

g5 <- ggplot(China, aes(day, day_death))
g5 + geom_col(col = "orange", size = 0.5) +
  labs(title = "Daily death cases in China")

g6 <- ggplot(China, aes(day, day_recovered))
g6 + geom_line(col = "blue", size = 1) + 
  labs(title = "Daily recovered cases in China")   ##there's a negative recovered day 

#trace back see how that happened
China[which(China$day_recovered < 0), ]
#switch these rows to NAs because the recovered cases spiked up in these days, then got adjusted 
#back down 
China[84:86, c(3,6)] <- NA
#re-adjust daily recovered cases for day 87
China[87, 6] <- China[87, 3] - China[83, 3]
China[83:87,]

g7 <- ggplot(China, aes(day_confirmed, day_recovered))
g7 + geom_smooth(col = "darkblue")

#plot some double plots
g8 <- ggplot() +
  
  geom_col(data = China, aes(x = day, y = Confirmed), fill = "orange") + 
  geom_line(data = China, aes(x = day, y = Deaths),
            colour = "red", size = 2) +
  geom_point(data = China, aes(x = day, y=Recovered)) +
  labs(title = "China overall confirmed, death and recovered cases")

plot(g8)

#category
g9 <- ggplot(data = China, aes(categories))
g9 + geom_bar(aes(fill=target), width = 0.5) + 
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) + 
  labs(title="Histogram on safety categories in China") 

#targeted_pop_group
g10 <- ggplot(China, aes(target))
g10 + geom_bar(aes(fill=categories), width = 0.75) + 
  theme(axis.text.x = element_text(angle=75, vjust=0.6)) + 
  labs(title="Histogram on targeted by population in China") 

g11 <- ggplot(China, aes(categories,day)) 
g11 + geom_boxplot(aes(fill = target))+ 
  theme(axis.text.x = element_text(angle=75, vjust=0.6))+
  labs(title = "Box plot for safety cateogies and days")

plot(China)

china_numeric <- China[, -c(7, 8, 9)]
china_numeric$categories <- as.numeric(China$categories)
china_numeric$target <- as.numeric(China$target)

c <- cor(china_numeric, use = "pairwise.complete.obs", method = "spearman")
c
corrplot(c)

#same dataset, but instead of having dates, use tagged days
China2 <- China
China2$days <- days
China2 <- China2[ , -7]
################################################### Modeling ####################################################
#trying 3 dataframes
#1. China
china_na <- na.omit(China)
is.na(china_na)

set.seed(3033)
intrain <- createDataPartition(y = china_na$Confirmed, p= 0.7, list = FALSE)
training <- china_na[intrain,]
testing <- china_na[-intrain,]
dim(intrain); dim(training); dim(testing)

#linear regression
fit_all <- lm(Confirmed~ ., data = training)
summary(fit_all)
fit_null <- lm(Confirmed ~ 1, data = training)

fit_step <- step(fit_null, scope=list(lower=fit_null, upper=fit_all),direction="both")
summary(fit_step)
#adjusted R-squred of 0.89, really high 

plot(fit_step, 1) #very bad, seems to show a trend

p <- predict(fit_step, testing, type = "response")
p
plot(p)

#try different models to find the best one 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
grid <- expand.grid(C = seq(0, 2, length =20))

# RPART
set.seed(3233)
fit.cart <- train(Confirmed~., data = training, method = "rpart", 
                  preProcess = c("center", "scale"),
                  trControl = trctrl)
fit.cart
plot(fit.cart)

# kNN
set.seed(3233)
fit.knn <- train(Confirmed~., data = training, method = "knn", 
                 preProcess = c("center", "scale"),
                 trControl = trctrl, 
                 tuneLength = 10)
fit.knn
plot(fit.knn)
#performance slightly better than rpart, but not that much better 

# SVMRadial
trctrl_rbf <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
svmGrid <- expand.grid(sigma= 2^c(-15,-10, -5, 0), C= 2^c(0:5))

set.seed(3233)

svm_Radial <- train(Confirmed ~., data = training, method = "svmRadial",
                    trControl=trctrl_rbf,
                    preProcess = c("center", "scale"),
                    tuneGrid = svmGrid,
                    tuneLength = 10)

svm_Radial   #extrememly high RMSE and MAE
plot(svm_Radial)

test_pred <- predict(svm_Radial, newdata = testing)
test_pred
plot(test_pred)

# Random Forest
#using random search 
trctrl_rf <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(3233)
mtry <- sqrt(ncol(training))
tunegrid <- expand.grid(.mtry=mtry)
rf_gridsearch <- train(Confirmed ~ ., data = training, method="rf", 
                       trControl=trctrl_rf,
                       preProcess = c("center", "scale"), 
                       tuneLength = 15)
print(rf_gridsearch) #best result so far  1518.527  0.9979277   873.7642
plot(rf_gridsearch)

# standardizes test data the same way as the training data 
test_pred <- predict(rf_gridsearch, newdata = testing)
test_pred
plot(test_pred)

#For China dataset, random forest using random search produced the best result. 
#Use this method to predict the entire dataset:
china_pred <- predict(rf_gridsearch, newdata = China)
china_pred
plot(china_pred, main = "Predicted Confirmed Cases in China Using Random Forest", xlab = "Days", 
     ylab = "Predicted Cases", col = "red")


#2. China with Confirmed, Deaths, Recovered, dates, Category and Target
China_2 <- China[, -c(4, 5, 6)]
China_2
China_2 <- na.omit(China_2)

set.seed(3033)
intrain <- createDataPartition(y = China_2$Confirmed, p= 0.7, list = FALSE)
training <- China_2[intrain,]
testing <- China_2[-intrain,]
dim(intrain); dim(training); dim(testing)

#linear regression
fit_all <- lm(Confirmed~ ., data = training)
summary(fit_all)

fit_null <- lm(Confirmed ~ 1, data = training)

fit_step <- step(fit_null, scope=list(lower=fit_null, upper=fit_all),direction="both")
summary(fit_step)
plot(fit_step, 1) #horrible trend but high R-squared

p <- predict(fit_step, testing, type = "response")
p
plot(p)

#try different models to find the best one 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
grid <- expand.grid(C = seq(0, 2, length =20))

# RPART
set.seed(3233)
fit.cart <- train(Confirmed ~., data = training, method = "rpart", 
                  preProcess = c("center", "scale"),
                  trControl = trctrl)
fit.cart
plot(fit.cart)

# kNN
set.seed(3233)
fit.knn <- train(Confirmed~., data = training, method = "knn", 
                 preProcess = c("center", "scale"),
                 trControl = trctrl, 
                 tuneLength = 10)
fit.knn
plot(fit.knn)
# k   RMSE       Rsquared   MAE     
#5   2829.361  0.9906999  1618.563

# SVMRadial
trctrl_rbf <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
svmGrid <- expand.grid(sigma= 2^c(-15,-10, -5, 0), C= 2^c(0:5))
set.seed(3233)
svm_Radial <- train(Confirmed ~., data = training, method = "svmRadial",
                    trControl=trctrl_rbf,
                    preProcess = c("center", "scale"),
                    tuneGrid = svmGrid,
                    tuneLength = 10)

svm_Radial   #rbf kernel is not the method for this model
plot(svm_Radial)

test_pred <- predict(svm_Radial, newdata = testing)
test_pred

# Random Forest
#using random search 
trctrl_rf <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(3233)
mtry <- sqrt(ncol(training))
tunegrid <- expand.grid(.mtry=mtry)
rf_gridsearch <- train(Confirmed ~ ., data = training, method="rf", 
                       trControl=trctrl_rf,
                       preProcess = c("center", "scale"), 
                       tuneLength = 15)
print(rf_gridsearch) #best result so far  1361.602  0.9980964    780.5278 (better than using entire China)
plot(rf_gridsearch)

# standardizes test data the same way as the training data 
test_pred <- predict(rf_gridsearch, newdata = testing)
test_pred

test <- predict(rf_gridsearch, newdata = China)
test
plot(test, main = "Predicted Confirmed Cases in China Using Random Forest #2", xlab = "Days", 
     ylab = "Predicted Cases", col = "orange")


china_predict <- data.frame(test)
#in order to overlay the predicted plot with the actual confirmed plot, need to fix the length (3 NA rows were omitted)
temp1 <- china_predict[1:83, ]
temp2 <- china_predict[84: nrow(china_predict), ]
temp3 <- rep(NA, 3)

china_pred <- data.frame(c(temp1, temp3, temp2))

colnames(china_pred) <- "Predicted_confirmed"
china_pred$date <- date
china_pred

f <- ggplot()+
  geom_line(data = China, mapping =aes(x = date, y = Confirmed), col = "orange", size = 1) + 
  geom_point(china_pred, mapping =aes(date, Predicted_confirmed), col = "blue")+
  labs(title = "Predicted (blue dots) and Actual (orange line) Confirmed Cases in China")

plot(f)


#3. China with confirmed converted to log to see if prediction can be further improved
#china log 
China_log <- China 
China_log <- na.omit(China_log)
China_log$Confirmed <- log(China_log$Confirmed + 1)

set.seed(3323)
intrain <- createDataPartition(y = China_log$Confirmed, p= 0.7, list = FALSE)
training <- China_log[intrain,]
testing <- China_log[-intrain,]
dim(intrain); dim(training); dim(testing)

#rf
set.seed(3233)
tc <- trainControl("oob") ##out of bag
Grid <- expand.grid(mtry = c(1:8))
fit.rf <- train(Confirmed ~., data = training , method = 'rf', trControl = tc, tuneGrid = Grid)
fit.rf  #best result: RMSE  0.24

test_pred <- predict(fit.rf, testing)
test_pred <- exp(test_pred)-1
test_pred
plot(test_pred)

test <- predict(fit.rf, China)
test <- exp(test)-1
test
plot(test, main = "Predicted Confirmed Cases in China #3", xlab = "days", ylab = "Confirmed Cases", col = "blue")

china_predict2 <- data.frame(test)

#in order to overlay the predicted plot with the actual confirmed plot, need to fix the length (3 NA rows were omitted)
temp1 <- china_predict2[1:83, ]
temp2 <- china_predict2[84: nrow(china_predict2), ]
temp3 <- rep(NA, 3)

china_pred2 <- data.frame(c(temp1, temp3, temp2))

colnames(china_pred2) <- "Predicted_confirmed"
china_pred2$date <- date
china_pred2

f2 <- ggplot()+
  geom_line(data = China, mapping =aes(x = date, y = Confirmed), col = "red", size = 1) + 
  geom_point(china_pred2, mapping =aes(date, Predicted_confirmed), col = "blue")+
  labs(title = "Predicted (blue dots) and Actual (orange line) Confirmed Cases in China #2")

plot(f2)

f3 <- ggplot()+
  geom_line(data = China, mapping =aes(x = date, y = Confirmed), col = "red", size = 1) + 
  geom_point(china_pred, mapping = aes(x = date, y = Predicted_confirmed), col = "green" ) + 
  geom_point(china_pred2, mapping =aes(date, Predicted_confirmed), col = "blue")+
  labs(title = "Predicted (green and blue dots) and Actual (orange line) Confirmed Cases in China ")
plot(f3)