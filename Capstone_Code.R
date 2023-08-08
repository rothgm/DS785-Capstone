###Loading libraries and traffic accident data
#loading and installing libraries
require(dplyr)
require(tidyr)
require(lubridate)
require(mice)
require(randomForest)
library(dplyr)
library(tidyr)
library(lubridate)
library(mice)
library(randomForest)

#loading traffic accident data
raw_data <- read.csv("C:/Users/grrot/Dropbox/Grad School/DS785 Summer 2023/Data/Crash_Vehicle_Data_(SOR).csv")

##Cleaning data
##turn latitude and longitude into rural or urban
#create urban locations
city = c("Des Moines", "Cedar Rapids", "Davenport", "Sioux City", "Iowa City", "West Des Moines", "Ankeny", "Waterloo", "Ames", "Council Bluffs", "Dubuque")
lat = c(41.57, 42.01, 41.52, 42.50, 41.66, 41.55, 41.73, 42.49, 42.03, 41.26, 42.40)
long = c(-93.61, -91.64, -90.58, -96.39,-91.53, -93.78, -93.61, -92.34, -93.62, -95.86, -90.66)
urban_cities = data.frame(city, lat, long)

#add urban_check variable
raw_data <- raw_data %>%
              mutate(urban_check = 0)

#label accident locations as rural or urban
rural_urban_func <- function(df, urban){
  #check to see if accident occured in urban city
  for (i in 1:nrow(df)){
    for (j in 1:nrow(urban)){
      if ((round(df[i,]$Y,2)==urban[j,]$lat) & ((round(df[i,]$X,2)==urban[j,]$long))){
        df[i,]$urban_check = 1 
      }
    }
  }
  return(df)
} 

#calculate urban_check variable
raw_data <- rural_urban_func(raw_data, urban_cities)
raw_data$urban_check <- as.factor(raw_data$urban_check)

##Remove latitude, longitude, and ID columns
raw_data <- raw_data[c("DRIVERAGE", "DRIVERGEN", "DL_STATE", "CHARGED", "ALCRESULT", "DRUGTEST", "DRIVERCOND", "VISIONOBS", "DCONTCIRC1", "DCONTCIRC2", "VCONFIG", "VLP_STATE", "OCCUPANTS", "VACTION", "SEQEVENTS1", "SEQEVENTS2", "SEQEVENTS3", "SEQEVENTS4", "SPEEDLIMIT", "TRAFCONT", "CSURFCOND", "ROADTYPE", "WZ_RELATED", "CRASH_DATETIME_UTC","urban_check")]

##Converting variables to factors
#Convert DRIVERAGE to factor
min_age_bucket <- c(15, 18, 25, 40, 65)
age_buckets <- c("15-17", "18-24", "25-39", "40-64")
raw_data$age_bucket <- cut(raw_data$DRIVERAGE, breaks = min_age_bucket, labels = age_buckets, include.lowest =  TRUE)
raw_data$DRIVERAGE <- NULL

#Convert CHARGED to factor
raw_data$charged_factor <- ifelse(raw_data$CHARGED == 77, "No", "Yes")
raw_data$CHARGED <- NULL

#Convert ALCResult to factor
raw_data$alcresult_factor <- ifelse(raw_data$ALCRESULT > 0, "Yes", "No")
raw_data$ALCRESULT <- NULL

#Convert DRUGTEST to factor
raw_data$drugtest_factor <- ifelse(raw_data$DRUGTEST == 77, "No", "Yes")
raw_data$DRUGTEST <- NULL

#Convert VISIONOBS to factor
raw_data$VISIONOBS_factor <- ifelse(raw_data$VISIONOBS == 77, "No", "Yes")
raw_data$VISIONOBS <- NULL

#convert OCCUPANTS to factor
min_occ_bucket <- c(0,1,2)
occ_buckets <- c("0","1")
raw_data$occupant_bucket <- cut(raw_data$OCCUPANTS, breaks = min_occ_bucket, labels = occ_buckets, include.lowest = TRUE)
raw_data$OCCUPANTS <- NULL

#convert VACTION to factor
raw_data$vacation_factor <- ifelse(raw_data$VACTION == 77, "No", "Yes")
raw_data$VACTION <- NULL

#Convert WZ_RELATED to factor
raw_data$WZ_factor <- ifelse(raw_data$WZ_RELATED == 1, "Yes", "No")
raw_data$WZ_factor <- ifelse(is.na(raw_data$WZ_factor), "No", "Yes")
raw_data$WZ_RELATED <- NULL

#Creating weekday and Time of Day variables
raw_data$date <- as.Date(raw_data$CRASH_DATETIME_UTC)
raw_data$DOW <- weekdays(raw_data$date)
raw_data$date <- NULL

time_intervals <- c(0, 6, 9, 16, 18, 24)
time_of_day <- c("Night", "Morning Commute", "Day", "Afternoon Commute", "Evening")
raw_data$wo_UTC <- gsub("+00","", raw_data$CRASH_DATETIME_UTC)
raw_data$crash_hour <- hour(as.POSIXct(raw_data$wo_UTC, format = "%Y/%m/%d %H:%M:%S"))

raw_data$TOD_bucket <- cut(raw_data$crash_hour, breaks = time_intervals, labels = time_of_day, include.lowest =  TRUE)
raw_data$wo_UTC <- NULL
raw_data$crash_hour <- NULL
raw_data$CRASH_DATETIME_UTC <- NULL

#Convert remaining variables to factor
raw_data$DRIVERCOND <- as.factor(raw_data$DRIVERCOND)
raw_data$DCONTCIRC1 <- as.factor(raw_data$DCONTCIRC1)
raw_data$DCONTCIRC2 <- as.factor(raw_data$DCONTCIRC2)
raw_data$VCONFIG <- as.factor(raw_data$VCONFIG)
raw_data$SEQEVENTS1 <- as.factor(raw_data$SEQEVENTS1)
raw_data$SEQEVENTS2 <- as.factor(raw_data$SEQEVENTS2)
raw_data$SEQEVENTS3 <- as.factor(raw_data$SEQEVENTS3)
raw_data$SEQEVENTS4 <- as.factor(raw_data$SEQEVENTS4)
raw_data$SPEEDLIMIT <- as.factor(raw_data$SPEEDLIMIT)
raw_data$TRAFCONT <- as.factor(raw_data$TRAFCONT)
raw_data$CSURFCOND <- as.factor(raw_data$CSURFCOND)
raw_data$ROADTYPE <- as.factor(raw_data$ROADTYPE)

full_data <- raw_data


#replace NA with imputed values
imputed_data <- mice(raw_data, m=26, method = "rf")
full_data <- complete(imputed_data)

cleaned_data <- full_data

###Variable selection
#forward stepwise
forward_stepwise <- function(data) {
  response <- colnames(data)[1]
  predictors <- colnames(data)[-1]
  selected <- c()
  remaining <- predictors
  
  while(length(remaining) > 0) {
    best_pvalue <- Inf
    best_predictor <- NULL
    
    for(predictor in remaining) {
      formula <- as.formula(paste(response, "~", paste(selected, collapse = "+"), "+", predictor))
      model <- lm(formula, data = data)
      
      pvalue <- summary(model)$coefficients[rownames(summary(model)$coefficients) == predictor, "Pr(>|t|)"]
      
      if(pvalue < best_pvalue){
        best_pvalue <- pvalue
        best_predictor <- predictor
      }
    }
    
    if(best_pvalue < 0.05) {
      selected <- c(selected, best_predictor)
      remaining <- setdiff(remaining, best_predictor)
    } else {
      break
    }
  }
  
  return(selected)
}

selected_predictors <- forward_stepwise(full_data)
print(selected_predictors)


###Logistic Regression
accident <- c("yes","no")
cleaned_data$accident <- sample(accident, nrow(cleaned_data), replace = TRUE)
#identify factors with only one level
single_level_factors <- sapply(cleaned_data, function(column) length(unique(column)) == 1)
data <- cleaned_data[,-single_level_factors]
cleaned_data <- data
#split data
set.seed(123)
sample_indices <- sample(nrow(cleaned_data),nrow(cleaned_data)*.8)
train_data <- cleaned_data[sample_indices,]
test_data <- cleaned_data[-sample_indices,]

train_slf <- sapply(train_data, function(column) length(unique(column)) == 1)
train_data <- train_data[,-train_slf]

#build logistic model
#build logistic regression model
log_model <- glm(train_data$accident~.,data = train_data, family = "binomial")
#make predictions on test data
predict <- predict(log_model, newdata = test_data, type="response")
#convert predicted probabilities to class label
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
#calculate accuracy
accuracy <- mean(predicted_classes == test_data$accident)
cat("Accuracy:",accuracy,"\n")