# AirBnb Market Analysis


 # load libraries
library(caret)
library(car)
library(pROC)
library(dplyr)
library(Hmisc)
library(plyr)
library(psych)
library(rpart)
library(rpart.plot)

# import data
setwd("/Users/my_MAC/Graduate Studies/MS/Business Analytics/MBAD_6211/Data_AirBnb/Data")

df <- read.csv('listings.csv', na.strings=c('NA', ''))

# irrelevant columns to drop
df <- df[,!(names(df) %in% c("listing_url", "scrape_id", "last_scraped",
                             "name","description", "neighborhood_overview",
                             "picture_url", "host_url", "host_name",
                             "host_about", "host_thumbnail_url", "host_picture_url",
                             "neighbourhood_group_cleansed",
                             "latitude", "longitude", "amenities", "calendar_updated",
                             "calendar_last_scraped", "license" ))]

df <- df[,!(names(df) %in% c("neighbourhood", "host_location", "bathrooms",
                             "host_neighbourhood"))]

# output to .csv
write.csv(df,"project_data\\listings_initial_subset.csv", row.names = FALSE)

# summary and structure of data
summary(df)
str(df)


# load libraries
library(caret)
library(car)
library(pROC)
library(dplyr)
library(Hmisc)
library(plyr)
library(psych)
library(rpart)
library(rpart.plot)

# import data
df <- read.csv('listings.csv', na.strings=c('NA', ''))

#check structure and summary of df
str(df)
summary(df)

#---------------------------------------------
#further drop columns that may have high correlation
df <- df[,!(names(df) %in% c("host_listings_count", "bedrooms",
                             "minimum_minimum_nights","maximum_minimum_nights",
                             "minimum_maximum_nights", "maximum_maximum_nights",
                             "minimum_nights_avg_ntm","maximum_nights_avg_ntm",
                             "availability_30", "availability_60",
                             "availability_90", "availability_365",
                             "number_of_reviews_ltm", "number_of_reviews_l30d",
                             "calculated_host_listings_count_entire_homes",
                             "calculated_host_listings_count_private_rooms",
                             "calculated_host_listings_count_shared_rooms"))]

#dropping the columns above allowed the vim() to provide values,
#   but dropping the following columns resulted in VIF scores below 10.
df <- df[,!(names(df) %in% c("host_verifications", "property_type",
                             "bathrooms_text", "review_scores_accuracy", 
                             "review_scores_cleanliness","review_scores_checkin",
                             "review_scores_communication",
                             "review_scores_location", "review_scores_value",
                             "neighbourhood_cleansed","host_response_time"))]
#---------------------------------------------

# reformat columns in data
df$host_response_rate = as.numeric(sub("%", "", df$host_response_rate))/100
df$host_acceptance_rate = as.numeric(sub("%", "", df$host_acceptance_rate))/100
df$host_is_superhost=ifelse(df$host_is_superhost =='t',1,0)
df$host_has_profile_pic=ifelse(df$host_has_profile_pic == 't', 1, 0)
df$host_identity_verified=ifelse(df$host_identity_verified == 't', 1, 0)
df$price=as.numeric(gsub('[$]', '',df$price))
df$has_availability=ifelse(df$has_availability == 't', 1, 0)
df$instant_bookable=ifelse(df$instant_bookable == 't', 1, 0)

df$host_is_superhost[is.na(df$host_is_superhost)] <- 0            #only 1 NA, cat. imp.
df$host_has_profile_pic[is.na(df$host_has_profile_pic)] <- 0      #only 1 NA, cat. imp.
df$host_identity_verified[is.na(df$host_identity_verified)] <- 0  #only 1 NA, cat. imp.

df$host_is_superhost=as.factor(df$host_is_superhost)
df$host_has_profile_pic=as.factor(df$host_has_profile_pic)
df$host_identity_verified=as.factor(df$host_identity_verified)
df$has_availability=as.factor(df$has_availability)
df$instant_bookable=as.factor(df$instant_bookable)

#imputation (numerical)
df$host_response_rate <- with(df,impute(host_response_rate,mean))
df$host_acceptance_rate <- with(df,impute(host_acceptance_rate,mean))
df$host_total_listings_count <- with(df,impute(host_total_listings_count,mean))
df$beds <- with(df,impute(beds,mean))
df$price <- with(df,impute(price,mean))
df$review_scores_rating <- with(df,impute(review_scores_rating,mean))
df$reviews_per_month <- with(df,impute(reviews_per_month,mean))
df$host_age <- with(df,impute(host_age,mean))
df$first_review_age <- with(df,impute(first_review_age,mean))
df$last_review_age <- with(df,impute(last_review_age,mean))

# Sample codes to draw histogram of all numeric columns
multi.hist(df[,sapply(df, is.numeric)], global=FALSE)

#right-skewed variables can be log-transformed
df$accommodates <- log10(df$accommodates+1)  #log10 of 0 is invalid, so add 1

df$price <- log10(df$price+1)  #log10 of 0 is invalid, so add 1
df$number_of_reviews <- log10(df$number_of_reviews+1)  #log10 of 0 is invalid, so add 1

df$reviews_per_month <- log10(df$reviews_per_month+1)  #log10 of 0 is invalid, so add 1

#check structure and summary of df
str(df)
summary(df)

# Sample codes to draw histogram of all numeric columns
multi.hist(df[,sapply(df, is.numeric)], global=FALSE)


# output to .csv
write.csv(df,"project_data\\listings_subset_with_id_cols.csv", row.names = FALSE)


# 10 is a decent VIF threshold for the sake of checking for multicollinearity
df <- df[,!(names(df) %in% c("id", "host_id"))]
vif(lm(formula=price~., data=df))

###################
##  MODELING  ##
###################

library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(pROC)
library(dplyr)
library(ggplot2)
library(Hmisc) #
library(plyr)
library(psych)
library(car)
#install.packages(olsrr)
library(olsrr)
library(neuralnet)

df <- read.csv('final_data_v2.csv',na.strings = c('NA',''))
colSums(is.na(df)) # Saw no nulls

str(df)

df <- subset(df, select = -c(listing_id,host_id,comments,first_review_age,last_review_age,comp_score,host_has_profile_pic))

cat_cols <- c('room_type','has_availability','instant_bookable', 'calculated_host_listings_count','host_is_superhost',
              'host_identity_verified')
df[cat_cols] <- lapply(df[cat_cols], factor)

table(df$room_type) # Highly skewed, convert to two categories
table(df$has_availability) # highly skewed, can be dropped. 
table(df$instant_bookable) # good
table(df$calculated_host_listings_count) #skewed but good
table(df$host_is_superhost) # skewed but good
table(df$host_identity_verified) # skewed but good

df <- subset(df, select = -c(has_availability))

multi.hist(df[,sapply(df,is.numeric)],freq=TRUE,global=FALSE)

table(df$host_response_rate)
table(df$minimum_nights)
table(df$maximum_nights)
# Can be converted into categorical as 1 or below 1
combine.AB <- function(x){
  ifelse(x>=1, "100%", "Below 100%")
}
df$host_response_rate <- apply(df[c('host_response_rate')],2,combine.AB)
table(df$host_response_rate)

combine.AB2 <- function(x){
  ifelse(x>2,'GT 2',ifelse(x>1,'2','1'))
}
df$minimum_nights <- apply(df[c('minimum_nights')],2,combine.AB2)
table(df$minimum_nights)
str(df)

vif(lm(formula=price ~ . ,data = df))
df <- subset(df, select = -c(neu_score,pos_score))

set.seed(101)
# Split into train-test data
trainIndex <- createDataPartition(df$price, p=0.7,
                                  list= FALSE,
                                  times=1)
df.train <- df[trainIndex,]
df.valid <- df[-trainIndex,]
dim(df.train)

# Linear Regression Model
model=lm(price ~ ., data=df.train)
summary(model)
paste0('MSE for Train data=',sum((model$residuals)^2)/(dim(df.train)[1]-dim(df.train)[2]-1)) #(n-p-1)

#Predict on the test data
predict(model,df.valid)

#Square of residual
#df.valid$sqerr <- (df.valid$price - predict(model, df.valid))^2
sqerr_lm <- (df.valid$price - predict(model, df.valid))^2
paste0('MSE for test data = ',mean(sqerr_lm))

#Decision Tree Model
treeKB.model <- train(price~.,
                      data = df.train,
                      method='rpart',
                      na.action = na.pass)
treeKB.model

#Visualize the tree model
prp(treeKB.model$finalModel,type=3)

#Predict on the test data
predict(treeKB.model,df.valid)

sqerr_dt <- (df.valid$price - predict(treeKB.model, df.valid))^2
paste0('MSE for test data = ',mean(sqerr_dt))

#Random Forest Model
rf_default <- train(price~.,
                    data=df.train,
                    method='rf',
                    importance=TRUE,
                    metric="RMSE",
                    ntree=5)
print(rf_default)

varImp(rf_default)

plot(varImp(rf_default))

#Square of residual
sqerr_rf <- (df.valid$price - predict(rf_default, df.valid))^2

paste0('MSE for test data = ',mean(sqerr_rf))

# Neural Network Model
# apply range standardization to numerical columns. 
# For the neural don't make categoricals into factors,do dummy encoding
# BAD is included here as target.

df_processed_nn <- df[,c(1,3,5,8,10,12,13,14,17,18,19)]

# get max and min for each column
maxs <- apply(df_processed_nn,2,max)
mins <- apply(df_processed_nn,2,min)

# scale data
df_processed_nn <- as.data.frame(scale(df_processed_nn,
                                       center=mins,
                                       scale=maxs-mins))
str(df)

table(df$calculated_host_listings_count)

df_processed_nn <- cbind(df_processed_nn, df$host_response_rate=='Below 100%')
names(df_processed_nn)[12] <- 'host_response_rate_Below100'

df_processed_nn <- cbind(df_processed_nn, df$host_is_superhost=='1')
names(df_processed_nn)[13] <- 'host_is_superhost_1'

df_processed_nn <- cbind(df_processed_nn, df$host_identity_verified=='1')
names(df_processed_nn)[14] <- 'host_identity_verified_1'

df_processed_nn <- cbind(df_processed_nn, df$room_type=='Hotel room')
names(df_processed_nn)[15] <- 'room_type_Hotel'
df_processed_nn <- cbind(df_processed_nn, df$room_type=='Private room')
names(df_processed_nn)[16] <- 'room_type_Private'
df_processed_nn <- cbind(df_processed_nn, df$room_type=='Shared room')
names(df_processed_nn)[17] <- 'room_type_Shared'

df_processed_nn <- cbind(df_processed_nn, df$beds=='Low')
names(df_processed_nn)[18] <- 'beds_Low'

df_processed_nn <- cbind(df_processed_nn, df$minimum_nights=='2')
names(df_processed_nn)[19] <- 'minimum_nights_2'
df_processed_nn <- cbind(df_processed_nn, df$minimum_nights=='GT 2')
names(df_processed_nn)[20] <- 'minimum_nights_GT2'

df_processed_nn <- cbind(df_processed_nn, df$instant_bookable=='1')
names(df_processed_nn)[21] <- 'instant_bookable_1'

df_processed_nn <- cbind(df_processed_nn, df$host_identity_verified=='Low')
names(df_processed_nn)[22] <- 'host_identity_verified_Low'
