#Load packages and rent
packages <- c("jsonlite", "dplyr", "purrr", "ggplot2", "scales", "lubridate", "nnet", "randomForest", "neuralnet", "ada", "caret")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)
rent <- fromJSON("./train.json")

#Ulist every variable except 'photos and 'features' and convert to tibble
vars <- setdiff(names(rent), c("photos", "features"))
rent <- map_at(rent, vars, unlist) %>% tibble::as_tibble(.)

#Importing rent set and converting it into a rentfrmae.
rent <- fromJSON("./train.json")
vars <- setdiff(names(rent), c("photos", "features"))
rent <- map_at(rent, vars, unlist) %>%
        tibble::as_tibble(.)
head(rent,1)

##Counting the #features for each entry and  attaching it to a rent frame:
feature_count <- unlist(lapply(rent$features, length))
length(feature_count)
rent <- cbind(rent, feature_count)

##Counting the #photos for each entry and  attaching it to a rent frame:
photos_count <- unlist(lapply(rent$photos, length))
length(photos_count)
rent <- cbind(rent, photos_count)

#Time Feature included
#Extracting month and hours  from "created" column:
date_time <- strptime(rent$created, format = "%Y-%m-%d %H:%M:%S")
rent_month <- month(date_time)
rent_hour <- hour(date_time)
rent <- cbind(rent, rent_month, rent_hour)


#Interest Level Default recreated
unique(rent$interest_level)
interest_factor <- rep(NA, nrow(rent))
rent <- cbind(rent, interest_factor)
rent$interest_factor[rent$interest_level == "low"] <- 0
rent$interest_factor[rent$interest_level == "medium"] <- 1
rent$interest_factor[rent$interest_level == "high"] <- 2
rent$interest_factor <- factor(rent$interest_factor)
#Making "low" interest level as reference:
rent$interest_factor <- relevel(rent$interest_factor, ref = "0")
unique(rent$interest_factor)

#Training data/ Testing data
#70% of the sample size
smp_size <- floor(0.7 * nrow(rent))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(rent)), size = smp_size)
train <- rent[train_ind, ]
test <- rent[-train_ind, ]

### Multinomial Logistic Regression Model###

#With both features_count and photos_count
rent.lr1<- multinom(interest_factor ~ bathrooms + bedrooms + price + feature_count + photos_count + rent_month + rent_hour, data = train)
#Low interest level is the base (or reference level == 0).
summary(rent.lr1)

#With features_count and photos_count
rent.lr2<- multinom(interest_factor ~ bathrooms + bedrooms + price + feature_count + rent_month + rent_hour, data = train)
#Low interest level is the base (or reference level == 0).
summary(rent.lr2)

#With photos_count
rent.lr3<- multinom(interest_factor ~ bathrooms + bedrooms + price + photos_count + rent_month + rent_hour, data = train)
#Low interest level is the base (or reference level == 0).
summary(rent.lr3)

#Without both features_count and photos_count
rent.lr4<- multinom(interest_factor ~ bathrooms + bedrooms + rent_month + rent_hour, data = train)
#Low interest level is the base (or reference level == 0).
summary(rent.lr4)

#Making predictions on data and checking accuracy of the model:
pred.lr1 <- predict(rent.lr1, test)
pred.lr2 <- predict(rent.lr2, test)
pred.lr3 <- predict(rent.lr3, test)
pred.lr4 <- predict(rent.lr4, test)
#Creating a confusion matrix to see how many times model predicts right/wrong:
cm1 <- table(pred.lr1, test$interest_factor)
cm1
cm2 <- table(pred.lr2, test$interest_factor)
cm2
cm3 <- table(pred.lr3, test$interest_factor)
cm3
cm4 <- table(pred.lr4, test$interest_factor)
cm4

#Calculate accuracy 
mean(pred.lr1==test$interest_factor)
mean(predict(rent.lr2)==rent$interest_factor)
mean(predict(rent.lr3)==rent$interest_factor)
mean(predict(rent.lr4)==rent$interest_factor)

### Random Forest Model###
rent.rf2 <- train(interest_level ~ bathrooms + bedrooms + price + feature_count + photos_count + rent_month + rent_hour, data=rent, method=rf, mtry=c(1:6))
#Tuning mtry

rent.rf1 <- randomForest(interest_factor ~ bathrooms + bedrooms + price + feature_count + photos_count + rent_month + rent_hour,data=rent, mtry=1,ntree= 500, importance=TRUE)
#Low interest level is the base (or reference level == 0).
rent.rf1
rent.rf2 <- randomForest(interest_factor ~ bathrooms + bedrooms + price + feature_count + photos_count + rent_month + rent_hour,data=rent, mtry=2,ntree= 500, importance=TRUE)
#Low interest level is the base (or reference level == 0).
rent.rf2
rent.rf3 <- randomForest(interest_factor ~ bathrooms + bedrooms + price + feature_count + photos_count + rent_month + rent_hour,data=rent, mtry=3,ntree= 500, importance=TRUE)
#Low interest level is the base (or reference level == 0).
rent.rf3
rent.rf4 <- randomForest(interest_factor ~ bathrooms + bedrooms + price + feature_count + photos_count + rent_month + rent_hour,data=rent, mtry=4,ntree= 500, importance=TRUE)
#Low interest level is the base (or reference level == 0).
rent.rf4
rent.rf5 <- randomForest(interest_factor ~ bathrooms + bedrooms + price + feature_count + photos_count + rent_month + rent_hour,data=rent, mtry=5,ntree= 500, importance=TRUE)
#Low interest level is the base (or reference level == 0).
rent.rf5
rent.rf6 <- randomForest(interest_factor ~ bathrooms + bedrooms + price + feature_count + photos_count + rent_month + rent_hour,data=rent, mtry=6,ntree= 500, importance=TRUE)
#Low interest level is the base (or reference level == 0).
rent.rf6

#Making predictions on  data and checking accuracy of the model:
pred.rf1 <- predict(rent.rf1, rent)
pred.rf2 <- predict(rent.rf2, rent)
pred.rf3 <- predict(rent.rf3, rent)
pred.rf4 <- predict(rent.rf4, rent)
pred.rf5 <- predict(rent.rf5, rent)
pred.rf6 <- predict(rent.rf6, rent)

pred.rf2[1:20]

#Tuning ntree
rent.rf1000 <- randomForest(interest_factor ~ bathrooms + bedrooms + price + feature_count + photos_count + rent_month + rent_hour,data=rent, mtry=2,ntree= 1000, importance=TRUE)
#Low interest level is the base (or reference level == 0).
rent.rf1000

rent.rf1500 <- randomForest(interest_factor ~ bathrooms + bedrooms + price + feature_count + photos_count + rent_month + rent_hour,data=rent, mtry=2,ntree= 1500, importance=TRUE)
#Low interest level is the base (or reference level == 0).
rent.rf1500

rent.rf2000 <- randomForest(interest_factor ~ bathrooms + bedrooms + price + feature_count + photos_count + rent_month + rent_hour,data=rent, mtry=2,ntree= 2000, importance=TRUE)
#Low interest level is the base (or reference level == 0).
rent.rf2000

### Neural Network Model###
#Change interest_level into 3 catgories high(0,1), low(0,1), medium(0,1)
data.nn <- cbind(rent, class.ind(rent$interest_level))
rent.nn<-neuralnet(high + low + medium ~ bathrooms + bedrooms + price + feature_count + rent_month + rent_hour,
                   data.nn,
                        hidden=c(3),linear.output=F,err.fct="ce",act.fct="logistic")


### Ada Boosting Model ###
#Choose mfinal=5
rent.adaboost <- boosting(interest_factor ~ bathrooms + bedrooms + price + feature_count + rent_month + rent_hour, data=rent, boos=TRUE, mfinal=5)
#See importance and weight and errorevol
rent.adaboost$importance
rent.adaboost$weight
errorevol(rent.adaboost,rent)

#Plot Tree
library(tree)
t4<- rent.adaboost$tree[[4]]
plot(t4)



