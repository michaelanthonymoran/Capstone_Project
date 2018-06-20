install.packages("dummies")  
install.packages("randomForest")
install.packages("e1071")

library(readr)
library(dplyr)
library(randomForest)
library(e1071)
library(dummies)

#Replace NA values and create dummy variables for categorical variables

train <- read_csv("C:/Users/Mike/Downloads/train.csv")
newtrain <- train %>% replace(is.na(train), 0)
train1 <- dummy.data.frame(as.data.frame(newtrain), names = c("MSSubClass","BsmtCond","GarageType","Neighborhood", sep = "-")) 

#Split the data into a train set and a test set

ind<-sample(2,nrow(train1),replace=TRUE,prob=c(0.8,0.2))

trainset<-train1[ind==1,]

testset<-train1[ind==2,]


#Create Random Forest and Support Vector Machines models

set.seed(222)
rf.model <- randomForest(SalePrice ~ GrLivArea+ BedroomAbvGr+ FullBath+ OverallCond+ BsmtCond0+ BsmtCondFa+ BsmtCondGd+ BsmtCondPo+ BsmtCondTA+ 
                   GarageType0+ GarageType2Types+ GarageTypeAttchd+ GarageTypeBasment+ GarageTypeBuiltIn+ GarageTypeCarPort+ GarageTypeDetchd+
                   MSSubClass20+ MSSubClass30+ MSSubClass40+ MSSubClass45+ MSSubClass50+ MSSubClass60+ MSSubClass70+ MSSubClass75+ MSSubClass80+
                   MSSubClass85+ MSSubClass90+ MSSubClass120+ MSSubClass160+ MSSubClass180+ MSSubClass190+
                   NeighborhoodBlmngtn+ NeighborhoodBlueste+ NeighborhoodBrDale+ NeighborhoodBrkSide+ NeighborhoodClearCr+ NeighborhoodCollgCr+ NeighborhoodCrawfor+
                   NeighborhoodEdwards+ NeighborhoodGilbert+ NeighborhoodIDOTRR+ NeighborhoodMeadowV+ NeighborhoodMitchel+ NeighborhoodNAmes+ NeighborhoodOldTown+
                   NeighborhoodSawyer+ NeighborhoodSawyerW+ NeighborhoodSomerst+ NeighborhoodStoneBr+ NeighborhoodSWISU+ NeighborhoodTimber+ NeighborhoodVeenker, data = trainset )
print(rf.model)
attributes(rf.model)
varImpPlot(rf.model)

svm.model <- svm(SalePrice ~ GrLivArea+ BedroomAbvGr+ FullBath+ OverallCond+ BsmtCond0+ BsmtCondFa+ BsmtCondGd+ BsmtCondPo+ BsmtCondTA+ 
                   GarageType0+ GarageType2Types+ GarageTypeAttchd+ GarageTypeBasment+ GarageTypeBuiltIn+ GarageTypeCarPort+ GarageTypeDetchd+
                   MSSubClass20+ MSSubClass30+ MSSubClass40+ MSSubClass45+ MSSubClass50+ MSSubClass60+ MSSubClass70+ MSSubClass75+ MSSubClass80+
                   MSSubClass85+ MSSubClass90+ MSSubClass120+ MSSubClass160+ MSSubClass180+ MSSubClass190+
                   NeighborhoodBlmngtn+ NeighborhoodBlueste+ NeighborhoodBrDale+ NeighborhoodBrkSide+ NeighborhoodClearCr+ NeighborhoodCollgCr+ NeighborhoodCrawfor+
                   NeighborhoodEdwards+ NeighborhoodGilbert+ NeighborhoodIDOTRR+ NeighborhoodMeadowV+ NeighborhoodMitchel+ NeighborhoodNAmes+ NeighborhoodOldTown+
                   NeighborhoodSawyer+ NeighborhoodSawyerW+ NeighborhoodSomerst+ NeighborhoodStoneBr+ NeighborhoodSWISU+ NeighborhoodTimber+ NeighborhoodVeenker, data = trainset, cost = 100, gamma = 1)


#Generate predicted SalePrice values for the train set and the test set using the Random Forest and Support Vectior Machines models 
predict_rf.train<-predict(rf.model, newdata=trainset)
print(predict_rf.train)

predict_rf.test<-predict(rf.model, newdata=testset)
print(predict_rf.test)

predict_svm.train<-predict(svm.model, newdata=trainset)
print(predict_svm.train)

predict_svm.test<-predict(svm.model, newdata=testset)
print(predict_svm.test)

# Calculate error for the Random Forest Model
error_rf.train<- predict_rf.train - trainset$SalePrice
print(error_rf.train)

error_rf.test <- predict_rf.test - testset$SalePrice
print(error_rf.test)

# Function that returns Root Mean Squared Error for the Random Forest Model
rmse_rf.train <- sqrt(mean(error_rf.train^2))
print(rmse_rf.train)

rmse_rf.test <- sqrt(mean(error_rf.test^2))
print(rmse_rf.test)

# Function that returns Mean Absolute Error for the Random Forest Model
mae_rf.train <- mean(abs(error_rf.train))
print(mae_rf.train)

mae_rf.test <- mean(abs(error_rf.test))
print(mae_rf.test)

# Calculate error for the Support Vector Machines Model
error_svm.train <- predict_svm.train - trainset$SalePrice
print(error_svm.train)

error_svm.test <- predict_svm.test - testset$SalePrice
print(error_svm.test)

# Function that returns Root Mean Squared Error for the Support Vector Machines Model
rmse_svm.train <- sqrt(mean(error_svm.train^2))
print(rmse_svm.train)

rmse_svm.test <- sqrt(mean(error_svm.test^2))
print(rmse_svm.test)

# Function that returns Mean Absolute Error for the Support Vector Machines Model
mae_svm.train <- mean(abs(error_svm.train))
print(mae_svm.train)

mae_svm.test <- mean(abs(error_svm.test))
print(mae_svm.test)



#Evaluate the error for the Tuned Random Forest and Support Vector Machines models 

#Create tuned models
set.seed(222)
Tuned.rf.model <- randomForest(SalePrice ~ GrLivArea+ FullBath+ BedroomAbvGr+ GarageTypeAttchd+ MSSubClass60+ OverallCond+ GarageTypeDetchd+ MSSubClass20+ NeighborhoodStoneBr+ GarageType0+ NeighborhoodNAmes+ MSSubClass90+ 
                           NeighborhoodOldTown+ MSSubClass30+ NeighborhoodSomerst+ NeighborhoodEdwards, data = trainset)

Tuned.svm.model <- svm(SalePrice ~ GrLivArea+ FullBath+ BedroomAbvGr+ GarageTypeAttchd+ MSSubClass60+ OverallCond+ GarageTypeDetchd+ MSSubClass20+ NeighborhoodStoneBr+ GarageType0+ NeighborhoodNAmes+ MSSubClass90+ 
                   NeighborhoodOldTown+ MSSubClass30+ NeighborhoodSomerst+ NeighborhoodEdwards, data = trainset, cost = 100, gamma = 1)

#Generate predicted SalePrice values for the train set and the test set using the Tuned Random Forest and Support Vectior Machines models 
Tuned.predict_rf.train<-predict(Tuned.rf.model, newdata=trainset)
print(Tuned.predict_rf.train)

Tuned.predict_rf.test<-predict(Tuned.rf.model, newdata=testset)
print(Tuned.predict_rf.test)


Tuned.predict_svm.train<-predict(Tuned.svm.model, newdata=trainset)
print(Tuned.predict_svm.train)

Tuned.predict_svm.test<-predict(Tuned.svm.model, newdata=testset)
print(Tuned.predict_svm.test)


# Calculate error for the Tuned Random Forest Model
Tuned.error_rf.train<- Tuned.predict_rf.train - trainset$SalePrice
print(Tuned.error_rf.train)

Tuned.error_rf.test <- Tuned.predict_rf.test - testset$SalePrice
print(Tuned.error_rf.test)

# Function that returns Root Mean Squared Error for the Tuned Random Forest Model
Tuned.rmse_rf.train <- sqrt(mean(Tuned.error_rf.train^2))
print(Tuned.rmse_rf.train)

Tuned.rmse_rf.test <- sqrt(mean(Tuned.error_rf.test^2))
print(Tuned.rmse_rf.test)

# Function that returns Mean Absolute Error for the Tuned Random Forest Model
Tuned.mae_rf.train <- mean(abs(Tuned.error_rf.train))
print(Tuned.mae_rf.train)

Tuned.mae_rf.test <- mean(abs(Tuned.error_rf.test))
print(Tuned.mae_rf.test)


# Calculate error for the Tuned Support Vector Machines Model
Tuned.error_svm.train <- Tuned.predict_svm.train - trainset$SalePrice
print(Tuned.error_svm.train)

Tuned.error_svm.test <- Tuned.predict_svm.test - testset$SalePrice
print(Tuned.error_svm.test)

# Function that returns Root Mean Squared Error for the Tuned Support Vector Machines Model
Tuned.rmse_svm.train <- sqrt(mean(Tuned.error_svm.train^2))
print(Tuned.rmse_svm.train)

Tuned.rmse_svm.test <- sqrt(mean(Tuned.error_svm.test^2))
print(Tuned.rmse_svm.test)

# Function that returns Mean Absolute Error for the Tuned Support Vector Machines Model
Tuned.mae_svm.train <- mean(abs(Tuned.error_svm.train))
print(Tuned.mae_svm.train)

Tuned.mae_svm.test <- mean(abs(Tuned.error_svm.test))
print(Tuned.mae_svm.test)


#Evaluate the error of the retuned models

#Create retuned models

set.seed(222)
Retuned.rf.model <- randomForest(SalePrice ~ GrLivArea+ FullBath+ BedroomAbvGr+ GarageTypeAttchd+ MSSubClass60+ OverallCond+ GarageTypeDetchd+ MSSubClass20, data = trainset)

Retuned.svm.model <- svm(SalePrice ~ GrLivArea+ FullBath+ BedroomAbvGr+ GarageTypeAttchd+ MSSubClass60+ OverallCond+ GarageTypeDetchd+ MSSubClass20, data = trainset, cost = 100, gamma = 1)

#Generate predicted SalePrice values for the train set and the test set using the Retuned Random Forest and Support Vectior Machines models 
Retuned.predict_rf.train<-predict(Retuned.rf.model, newdata=trainset)
print(Retuned.predict_rf.train)

Retuned.predict_rf.test<-predict(Retuned.rf.model, newdata=testset)
print(Retuned.predict_rf.test)


Retuned.predict_svm.train<-predict(Retuned.svm.model, newdata=trainset)
print(Retuned.predict_svm.train)

Retuned.predict_svm.test<-predict(Retuned.svm.model, newdata=testset)
print(Retuned.predict_svm.test)


# Calculate error for the Retuned Random Forest Model
Retuned.error_rf.train<- Retuned.predict_rf.train - trainset$SalePrice
print(Retuned.error_rf.train)

Retuned.error_rf.test <- Retuned.predict_rf.test - testset$SalePrice
print(Retuned.error_rf.test)

# Function that returns Root Mean Squared Error for the Retuned Random Forest Model
Retuned.rmse_rf.train <- sqrt(mean(Retuned.error_rf.train^2))
print(Retuned.rmse_rf.train)

Retuned.rmse_rf.test <- sqrt(mean(Retuned.error_rf.test^2))
print(Retuned.rmse_rf.test)

# Function that returns Mean Absolute Error for the Retuned Random Forest Model
Retuned.mae_rf.train <- mean(abs(Retuned.error_rf.train))
print(Retuned.mae_rf.train)

Retuned.mae_rf.test <- mean(abs(Retuned.error_rf.test))
print(Retuned.mae_rf.test)


# Calculate error for the Retuned Support Vector Machines Model
Retuned.error_svm.train <- Retuned.predict_svm.train - trainset$SalePrice
print(Retuned.error_svm.train)

Retuned.error_svm.test <- Retuned.predict_svm.test - testset$SalePrice
print(Retuned.error_svm.test)

# Function that returns Root Mean Squared Error for the Retuned Support Vector Machines Model
Retuned.rmse_svm.train <- sqrt(mean(Retuned.error_svm.train^2))
print(Retuned.rmse_svm.train)

Retuned.rmse_svm.test <- sqrt(mean(Retuned.error_svm.test^2))
print(Retuned.rmse_svm.test)

# Function that returns Mean Absolute Error for the Retuned Support Vector Machines Model
Retuned.mae_svm.train <- mean(abs(Retuned.error_svm.train))
print(Retuned.mae_svm.train)

Retuned.mae_svm.test <- mean(abs(Retuned.error_svm.test))
print(Retuned.mae_svm.test)


###
mean(testset$SalePrice)
mean(trainset$SalePrice)

