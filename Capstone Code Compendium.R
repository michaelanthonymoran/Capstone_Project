
library(dplyr)
library(tidyr)
library(dummies)
library(readr)
library(xlsx)
library(randomForest)
library(e1071)

#Replace NA values and create dummy variables for categorical variables

train <- read_csv("C:/Users/Mike/Downloads/train.csv")
newtrain <- train %>% replace(is.na(train), 0)
train1 <- dummy.data.frame(as.data.frame(newtrain), names = c("MSSubClass","BsmtCond","GarageType","Neighborhood", sep = "-")) 

#Split the data into a train set and a test set

ind<-sample(2,nrow(train1),replace=TRUE,prob=c(0.8,0.2))

trainset<-train1[ind==1,]

testset<-train1[ind==2,]

#Base R stats
summary(train1$SalePrice)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#34900  129975  163000  180921  214000  755000 

mean(train1$SalePrice)
#180921.2
sd(train1$SalePrice)
#79442.5
var(train1$SalePrice)
#6311111264

min(train1$SalePrice)
#34900
max(train1$SalePrice)
#755000
IQR(train1$SalePrice)
#84025

#linear regression

dafk <- train1

a <- lm(dafk$GrLivArea ~ dafk$SalePrice)
summary(a)

b <- lm(dafk$BedroomAbvGr ~ dafk$SalePrice)
summary(b)

c <- lm(dafk$FullBath ~ dafk$SalePrice)
summary(c)

d <- lm(dafk$OverallCond ~ dafk$SalePrice)
summary(d)

e <- lm(dafk$GarageType0+ dafk$GarageType2Types+ dafk$GarageTypeAttchd+ dafk$GarageTypeBasment+ dafk$GarageTypeBuiltIn+ dafk$GarageTypeCarPort+ dafk$GarageTypeDetchd ~ dafk$SalePrice)
summary(e)

f <- lm(dafk$BsmtCond0+ dafk$BsmtCondFa+ dafk$BsmtCondGd+ dafk$BsmtCondPo+ dafk$BsmtCondTA ~ dafk$SalePrice)
summary(f)  

g <- lm(dafk$MSSubClass20+ dafk$MSSubClass30+ dafk$MSSubClass40+ dafk$MSSubClass45+ dafk$MSSubClass50+ dafk$MSSubClass60+ dafk$MSSubClass70+ dafk$MSSubClass75+ dafk$MSSubClass80+
          dafk$MSSubClass85+ dafk$MSSubClass90+ dafk$MSSubClass120+ dafk$MSSubClass160+ dafk$MSSubClass180+ dafk$MSSubClass190 ~ dafn$SalePrice)
summary(g)  

h <- lm(dafk$NeighborhoodBlmngtn+ dafk$NeighborhoodBlueste+ dafk$NeighborhoodBrDale+ dafk$NeighborhoodBrkSide+ dafk$NeighborhoodClearCr+ dafk$NeighborhoodCollgCr+ dafk$NeighborhoodCrawfor+
          dafk$NeighborhoodEdwards+ dafk$NeighborhoodGilbert+ dafk$NeighborhoodIDOTRR+ dafk$NeighborhoodMeadowV+ dafk$NeighborhoodMitchel+ dafk$NeighborhoodNAmes+ dafk$NeighborhoodOldTown+
          dafk$NeighborhoodSawyer+ dafk$NeighborhoodSawyerW+ dafk$NeighborhoodSomerst+ dafk$NeighborhoodStoneBr+ dafk$NeighborhoodSWISU+ dafk$NeighborhoodTimber+ dafk$NeighborhoodVeenker ~ dafk$SalePrice)
summary(h)

i <- lm(dafk$GrLivArea+ dafk$BedroomAbvGr ~ dafk$SalePrice)
summary(i)

j <- lm(dafk$GrLivArea+ dafk$BedroomAbvGr+ dafk$FullBath ~ dafk$SalePrice)
summary(j)

k <- lm (dafk$GrLivArea+ dafk$BedroomAbvGr+ dafk$FullBath+ dafk$OverallCond ~ dafk$SalePrice)
summary(k)

l <- lm(dafk$GrLivArea+ dafk$BedroomAbvGr+ dafk$FullBath+ dafk$OverallCond+ dafk$BsmtCond0+ dafk$BsmtCondFa+ dafk$BsmtCondGd+ dafk$BsmtCondPo+ dafk$BsmtCondTA ~ dafk$SalePrice)
summary(l)

m <- lm(dafk$GrLivArea+ dafk$BedroomAbvGr+ dafk$FullBath+ dafk$OverallCond+ dafk$BsmtCond0+ dafk$BsmtCondFa+ dafk$BsmtCondGd+ dafk$BsmtCondPo+ dafk$BsmtCondTA+ 
          dafk$GarageType0+ dafk$GarageType2Types+ dafk$GarageTypeAttchd+ dafk$GarageTypeBasment+ dafk$GarageTypeBuiltIn+ dafk$GarageTypeCarPort+ dafk$GarageTypeDetchd ~ dafk$SalePrice)
summary(m)

n <- lm(dafk$GrLivArea+ dafk$BedroomAbvGr+ dafk$FullBath+ dafk$OverallCond+ dafk$BsmtCond0+ dafk$BsmtCondFa+ dafk$BsmtCondGd+ dafk$BsmtCondPo+ dafk$BsmtCondTA+ 
          dafk$GarageType0+ dafk$GarageType2Types+ dafk$GarageTypeAttchd+ dafk$GarageTypeBasment+ dafk$GarageTypeBuiltIn+ dafk$GarageTypeCarPort+ dafk$GarageTypeDetchd+
          dafk$MSSubClass20+ dafk$MSSubClass30+ dafk$MSSubClass40+ dafk$MSSubClass45+ dafk$MSSubClass50+ dafk$MSSubClass60+ dafk$MSSubClass70+ dafk$MSSubClass75+ dafk$MSSubClass80+
          dafk$MSSubClass85+ dafk$MSSubClass90+ dafk$MSSubClass120+ dafk$MSSubClass160+ dafk$MSSubClass180+ dafk$MSSubClass190 ~ dafk$SalePrice)
summary(n)

o <-  lm(dafk$GrLivArea+ dafk$BedroomAbvGr+ dafk$FullBath+ dafk$OverallCond+ dafk$BsmtCond0+ dafk$BsmtCondFa+ dafk$BsmtCondGd+ dafk$BsmtCondPo+ dafk$BsmtCondTA+ 
           dafk$GarageType0+ dafk$GarageType2Types+ dafk$GarageTypeAttchd+ dafk$GarageTypeBasment+ dafk$GarageTypeBuiltIn+ dafk$GarageTypeCarPort+ dafk$GarageTypeDetchd+
           dafk$MSSubClass20+ dafk$MSSubClass30+ dafk$MSSubClass40+ dafk$MSSubClass45+ dafk$MSSubClass50+ dafk$MSSubClass60+ dafk$MSSubClass70+ dafk$MSSubClass75+ dafk$MSSubClass80+
           dafk$MSSubClass85+ dafk$MSSubClass90+ dafk$MSSubClass120+ dafk$MSSubClass160+ dafk$MSSubClass180+ dafk$MSSubClass190+
           dafk$NeighborhoodBlmngtn+ dafk$NeighborhoodBlueste+ dafk$NeighborhoodBrDale+ dafk$NeighborhoodBrkSide+ dafk$NeighborhoodClearCr+ dafk$NeighborhoodCollgCr+ dafk$NeighborhoodCrawfor+
           dafk$NeighborhoodEdwards+ dafk$NeighborhoodGilbert+ dafk$NeighborhoodIDOTRR+ dafk$NeighborhoodMeadowV+ dafk$NeighborhoodMitchel+ dafk$NeighborhoodNAmes+ dafk$NeighborhoodOldTown+
           dafk$NeighborhoodSawyer+ dafk$NeighborhoodSawyerW+ dafk$NeighborhoodSomerst+ dafk$NeighborhoodStoneBr+ dafk$NeighborhoodSWISU+ dafk$NeighborhoodTimber+ dafk$NeighborhoodVeenker ~ dafk$SalePrice)
summary(o)

#Regression outputs have been uploaded in a separate Word file 

#Machine learning
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
# 21042.74

rmse_rf.test <- sqrt(mean(error_rf.test^2))
print(rmse_rf.test)
# 36290.02

# Function that returns Mean Absolute Error for the Random Forest Model
mae_rf.train <- mean(abs(error_rf.train))
print(mae_rf.train)
# 13029.66

mae_rf.test <- mean(abs(error_rf.test))
print(mae_rf.test)
# 23357.68

# Calculate error for the Support Vector Machines Model
error_svm.train <- predict_svm.train - trainset$SalePrice
print(error_svm.train)

error_svm.test <- predict_svm.test - testset$SalePrice
print(error_svm.test)

# Function that returns Root Mean Squared Error for the Support Vector Machines Model
rmse_svm.train <- sqrt(mean(error_svm.train^2))
print(rmse_svm.train)
# 14769.06

rmse_svm.test <- sqrt(mean(error_svm.test^2))
print(rmse_svm.test)
# 56015.79

# Function that returns Mean Absolute Error for the Support Vector Machines Model
mae_svm.train <- mean(abs(error_svm.train))
print(mae_svm.train)
# 9661.835

mae_svm.test <- mean(abs(error_svm.test))
print(mae_svm.test)
# 34143.1


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
# 25679.28

Tuned.rmse_rf.test <- sqrt(mean(Tuned.error_rf.test^2))
print(Tuned.rmse_rf.test)
# 38861.16

# Function that returns Mean Absolute Error for the Tuned Random Forest Model
Tuned.mae_rf.train <- mean(abs(Tuned.error_rf.train))
print(Tuned.mae_rf.train)
# 17012.69

Tuned.mae_rf.test <- mean(abs(Tuned.error_rf.test))
print(Tuned.mae_rf.test)
# 25376.47

# Calculate error for the Tuned Support Vector Machines Model
Tuned.error_svm.train <- Tuned.predict_svm.train - trainset$SalePrice
print(Tuned.error_svm.train)

Tuned.error_svm.test <- Tuned.predict_svm.test - testset$SalePrice
print(Tuned.error_svm.test)

# Function that returns Root Mean Squared Error for the Tuned Support Vector Machines Model
Tuned.rmse_svm.train <- sqrt(mean(Tuned.error_svm.train^2))
print(Tuned.rmse_svm.train)
# 19376.01

Tuned.rmse_svm.test <- sqrt(mean(Tuned.error_svm.test^2))
print(Tuned.rmse_svm.test)
# 51942.44

# Function that returns Mean Absolute Error for the Tuned Support Vector Machines Model
Tuned.mae_svm.train <- mean(abs(Tuned.error_svm.train))
print(Tuned.mae_svm.train)
# 12079.2

Tuned.mae_svm.test <- mean(abs(Tuned.error_svm.test))
print(Tuned.mae_svm.test)
# 32016.64


#Evaluate the error of the retuned models

#Create retuned models

set.seed(222)
Retuned.rf.model <- randomForest(SalePrice ~ GrLivArea+ FullBath+ BedroomAbvGr+ GarageTypeAttchd+ MSSubClass60+ OverallCond+ GarageTypeDetchd+ MSSubClass20, data = trainset)
print(Retuned.rf.model)
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
# 34253.39

Retuned.rmse_rf.test <- sqrt(mean(Retuned.error_rf.test^2))
print(Retuned.rmse_rf.test)
# 43798.54

# Function that returns Mean Absolute Error for the Retuned Random Forest Model
Retuned.mae_rf.train <- mean(abs(Retuned.error_rf.train))
print(Retuned.mae_rf.train)
# 23375.59

Retuned.mae_rf.test <- mean(abs(Retuned.error_rf.test))
print(Retuned.mae_rf.test)
# 29373.73


# Calculate error for the Retuned Support Vector Machines Model
Retuned.error_svm.train <- Retuned.predict_svm.train - trainset$SalePrice
print(Retuned.error_svm.train)

Retuned.error_svm.test <- Retuned.predict_svm.test - testset$SalePrice
print(Retuned.error_svm.test)

# Function that returns Root Mean Squared Error for the Retuned Support Vector Machines Model
Retuned.rmse_svm.train <- sqrt(mean(Retuned.error_svm.train^2))
print(Retuned.rmse_svm.train)
# 23231.91

Retuned.rmse_svm.test <- sqrt(mean(Retuned.error_svm.test^2))
print(Retuned.rmse_svm.test)
# 53228.26

# Function that returns Mean Absolute Error for the Retuned Support Vector Machines Model
Retuned.mae_svm.train <- mean(abs(Retuned.error_svm.train))
print(Retuned.mae_svm.train)
# 14587.06

Retuned.mae_svm.test <- mean(abs(Retuned.error_svm.test))
print(Retuned.mae_svm.test)
# 31432.79

#Evaluate top performing model

#The retuned Random Forest model shows the best performance between the training set and the test set, and predicts the sale price of house to within plus or minus 15% of the sale price for the average home

retuned.rf_percent_error <- abs(Retuned.mae_rf.test- Retuned.mae_rf.train)/Retuned.mae_rf.train
print(retuned.rf_percent_error)
# 25.66%

retuned.rf_margin_of_error <-Retuned.mae_rf.test/mean(testset$SalePrice)
print(retuned.rf_margin_of_error)
# 15.41%

#Further revision of the model did not imporve the performance of the Random Forest Model

set.seed(222)
Addl.Retuned.rf.model <- randomForest(SalePrice ~ GrLivArea+ FullBath+ BedroomAbvGr+ GarageTypeAttchd+ MSSubClass60+ OverallCond, data = trainset)

#Generate predicted SalePrice values for the train set and the test set using the further retuned Random Forest models 
Addl.Retuned.predict_rf.train<-predict(Addl.Retuned.rf.model, newdata=trainset)
print(Addl.Retuned.predict_rf.train)

Addl.Retuned.predict_rf.test<-predict(Addl.Retuned.rf.model, newdata=testset)
print(Addl.Retuned.predict_rf.test)


# Calculate error for the further retuned Random Forest Model
Addl.Retuned.error_rf.train<- Addl.Retuned.predict_rf.train - trainset$SalePrice
print(Addl.Retuned.error_rf.train)

Addl.Retuned.error_rf.test <- Addl.Retuned.predict_rf.test - testset$SalePrice
print(Addl.Retuned.error_rf.test)

# Function that returns Root Mean Squared Error for the further retuned Random Forest Model
Addl.Retuned.rmse_rf.train <- sqrt(mean(Addl.Retuned.error_rf.train^2))
print(Addl.Retuned.rmse_rf.train)
# 31368.76

Addl.Retuned.rmse_rf.test <- sqrt(mean(Addl.Retuned.error_rf.test^2))
print(Addl.Retuned.rmse_rf.test)
# 42402.34

# Function that returns Mean Absolute Error for the further retuned Random Forest Model
Addl.Retuned.mae_rf.train <- mean(abs(Addl.Retuned.error_rf.train))
print(Addl.Retuned.mae_rf.train)
# 21630.91

Addl.Retuned.mae_rf.test <- mean(abs(Addl.Retuned.error_rf.test))
print(Addl.Retuned.mae_rf.test)
# 28730.82

Addl.retuned.rf_percent_error <- abs(Addl.Retuned.mae_rf.test- Addl.Retuned.mae_rf.train)/Addl.Retuned.mae_rf.train
print(Addl.retuned.rf_percent_error)
# 32.82%

Addl.retuned.rf_margin_of_error <-Addl.Retuned.mae_rf.test/mean(testset$SalePrice)
print(Addl.retuned.rf_margin_of_error)
# 15.07%

#Feature engineer the Square Footage variable to further refine the model

#Create bins for Square Footage
summary(trainset$GrLivArea)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#438    1113    1445    1503    1774    5642 

sd(trainset$GrLivArea)
#[1] 530.958

trainset$bins <- cut(trainset$GrLivArea, 10, labels = c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth"))
print(trainset$bins)

binsframe.train <- dummy.data.frame(as.data.frame(trainset), names = c("bins", sep = "-"))
print(binsframe.train)

testset$bins <- cut(testset$GrLivArea, 10, labels = c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth"))
print(testset$bins)

binsframe.test <- dummy.data.frame(as.data.frame(testset), names = c("bins", sep = "-"))
print(binsframe.test)

#create Random Forest model using binned Square Footage
set.seed(222)
binned.rf.model <- randomForest(SalePrice ~ binsFirst+ binsSecond+ binsThird+ binsFourth+ binsFifth+ binsSixth+ binsSeventh+ binsEighth+ binsNinth+ binsTenth+
                                  BedroomAbvGr+ FullBath+ OverallCond+ BsmtCond0+ BsmtCondFa+ BsmtCondGd+ BsmtCondPo+ BsmtCondTA+ 
                                  GarageType0+ GarageType2Types+ GarageTypeAttchd+ GarageTypeBasment+ GarageTypeBuiltIn+ GarageTypeCarPort+ GarageTypeDetchd+
                                  MSSubClass20+ MSSubClass30+ MSSubClass40+ MSSubClass45+ MSSubClass50+ MSSubClass60+ MSSubClass70+ MSSubClass75+ MSSubClass80+
                                  MSSubClass85+ MSSubClass90+ MSSubClass120+ MSSubClass160+ MSSubClass180+ MSSubClass190+
                                  NeighborhoodBlmngtn+ NeighborhoodBlueste+ NeighborhoodBrDale+ NeighborhoodBrkSide+ NeighborhoodClearCr+ NeighborhoodCollgCr+ NeighborhoodCrawfor+
                                  NeighborhoodEdwards+ NeighborhoodGilbert+ NeighborhoodIDOTRR+ NeighborhoodMeadowV+ NeighborhoodMitchel+ NeighborhoodNAmes+ NeighborhoodOldTown+
                                  NeighborhoodSawyer+ NeighborhoodSawyerW+ NeighborhoodSomerst+ NeighborhoodStoneBr+ NeighborhoodSWISU+ NeighborhoodTimber+ NeighborhoodVeenker, data = binsframe.train )
print(binned.rf.model)
attributes(binned.rf.model)
varImpPlot(binned.rf.model)

#Refine the Random Forest model to include important bins

set.seed(222)
Retuned.binned.rf.model <- randomForest(SalePrice ~ binsSecond+ binsEighth+ binsFourth+ binsThird+ binsFirst+ binsFifth+
                                          FullBath+ BedroomAbvGr+ GarageTypeAttchd+ MSSubClass60+ OverallCond+ GarageTypeDetchd+ MSSubClass20, data = binsframe.train)
print(Retuned.binned.rf.model)

#Generate predicted SalePrice values for the train set and the test set using the binned Random Forest model 
predict_rf.train.binned<-predict(Retuned.binned.rf.model, newdata=binsframe.train)
print(predict_rf.train)

predict_rf.test.binned<-predict(Retuned.binned.rf.model, newdata=binsframe.test)
print(predict_rf.test)

# Calculate error for the binned Random Forest Model
error_rf.train.binned<- predict_rf.train.binned - binsframe.train$SalePrice
print(error_rf.train.binned)

error_rf.test.binned <- predict_rf.test.binned - binsframe.test$SalePrice
print(error_rf.test.binned)

# Function that returns Root Mean Squared Error for the binned Random Forest Model
rmse_rf.train.binned <- sqrt(mean(error_rf.train.binned^2))
print(rmse_rf.train.binned)
#  35785.56

rmse_rf.test.binned <- sqrt(mean(error_rf.test.binned^2))
print(rmse_rf.test.binned)
# 81782.87

# Function that returns Mean Absolute Error for the binned Random Forest Model
mae_rf.train.binned <- mean(abs(error_rf.train.binned))
print(mae_rf.train.binned)
# 25097.35

mae_rf.test.binned <- mean(abs(error_rf.test.binned))
print(mae_rf.test.binned)
# 66668.48

#Refine Feature engineering for the Square Footage variable 

#Create bins for Square Footage
trainset$bins2 <- cut(trainset$GrLivArea, 5, labels = c("First", "Second", "Third", "Fourth", "Fifth"))
print(trainset$bins2)

binsframe.train2 <- dummy.data.frame(as.data.frame(trainset), names = c("bins2", sep = "-"))
print(binsframe.train2)

testset$bins2 <- cut(testset$GrLivArea, 5, labels = c("First", "Second", "Third", "Fourth", "Fifth"))
print(testset$bins)

binsframe.test2 <- dummy.data.frame(as.data.frame(testset), names = c("bins2", sep = "-"))
print(binsframe.test2)

#create Random Forest model using binned Square Footage
set.seed(222)
binned.rf.model2 <- randomForest(SalePrice ~ bins2First+ bins2Second+ bins2Third+ bins2Fourth+ bins2Fifth+ 
                                   BedroomAbvGr+ FullBath+ OverallCond+ BsmtCond0+ BsmtCondFa+ BsmtCondGd+ BsmtCondPo+ BsmtCondTA+ 
                                   GarageType0+ GarageType2Types+ GarageTypeAttchd+ GarageTypeBasment+ GarageTypeBuiltIn+ GarageTypeCarPort+ GarageTypeDetchd+
                                   MSSubClass20+ MSSubClass30+ MSSubClass40+ MSSubClass45+ MSSubClass50+ MSSubClass60+ MSSubClass70+ MSSubClass75+ MSSubClass80+
                                   MSSubClass85+ MSSubClass90+ MSSubClass120+ MSSubClass160+ MSSubClass180+ MSSubClass190+
                                   NeighborhoodBlmngtn+ NeighborhoodBlueste+ NeighborhoodBrDale+ NeighborhoodBrkSide+ NeighborhoodClearCr+ NeighborhoodCollgCr+ NeighborhoodCrawfor+
                                   NeighborhoodEdwards+ NeighborhoodGilbert+ NeighborhoodIDOTRR+ NeighborhoodMeadowV+ NeighborhoodMitchel+ NeighborhoodNAmes+ NeighborhoodOldTown+
                                   NeighborhoodSawyer+ NeighborhoodSawyerW+ NeighborhoodSomerst+ NeighborhoodStoneBr+ NeighborhoodSWISU+ NeighborhoodTimber+ NeighborhoodVeenker, data = binsframe.train2 )
print(binned.rf.model2)
attributes(binned.rf.model2)
varImpPlot(binned.rf.model2)

#Refine the Random Forest model to include important bins

set.seed(222)
Retuned.binned.rf.model2 <- randomForest(SalePrice ~ binsSecond+ binsFourth+ binsThird+ binsFirst+ 
                                           FullBath+ BedroomAbvGr+ GarageTypeAttchd+ MSSubClass60+ OverallCond+ GarageTypeDetchd+ MSSubClass20, data = binsframe.train2)
print(Retuned.binned.rf.model2)

#Generate predicted SalePrice values for the train set and the test set using the binned Random Forest model 
predict_rf.train.binned2<-predict(Retuned.binned.rf.model2, newdata=binsframe.train2)
print(predict_rf.train2)

predict_rf.test.binned2<-predict(Retuned.binned.rf.model2, newdata=binsframe.test2)
print(predict_rf.test2)

# Calculate error for the binned Random Forest Model
error_rf.train.binned2<- predict_rf.train.binned2 - binsframe.train2$SalePrice
print(error_rf.train.binned2)

error_rf.test.binned2 <- predict_rf.test.binned2 - binsframe.test2$SalePrice
print(error_rf.test.binned2)

# Function that returns Root Mean Squared Error for the binned Random Forest Model
rmse_rf.train.binned2 <- sqrt(mean(error_rf.train.binned2^2))
print(rmse_rf.train.binned2)
#  41106.26

rmse_rf.test.binned2 <- sqrt(mean(error_rf.test.binned2^2))
print(rmse_rf.test.binned2)
# 87601.99

# Function that returns Mean Absolute Error for the binned Random Forest Model
mae_rf.train.binned2 <- mean(abs(error_rf.train.binned2))
print(mae_rf.train.binned2)
# 28533.19

mae_rf.test.binned2 <- mean(abs(error_rf.test.binned2))
print(mae_rf.test.binned2)
# 66194.19

