
setwd("~/DMMG unit/Daisy")

install.packages("randomForest")
install.packages("caret", dependencies = TRUE)
install.packages("DMwR")
install.packages("remotes")
remotes::install_github("cran/DMwR", force = TRUE)
install.packages("ROCR")

library(randomForest)
library(caret)
#library(readxl)
library(DMwR)
library(kernlab)
library(ROCR)

setwd("D:\\Eric\\Manuscripts\\Random forest")
stingless_bees_Morphometircs <- read_excel("stingless bees Morphometircs_Nelly2017.csv")
names(stingless_bees_Morphometircs)
class(stingless_bees_Morphometircs)
attach(stingless_bees_Morphometircs)
stingless_bees_Morphometircs <- data.frame(stingless_bees_Morphometircs)

#change species to factor in train data, 
stingless_bees_Morphometircs[["species"]] = factor(stingless_bees_Morphometircs[["species"]])
str(stingless_bees_Morphometircs)


#split data into training set and testing set (data splicing)
#) Randomised
set.seed(2022)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(stingless_bees_Morphometircs)*.70)

# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(stingless_bees_Morphometircs), size = data_set_size)

# Assign the data to the correct sets
train.stingless_bees_Morphometircs_rf <- stingless_bees_Morphometircs[indexes,]
test.stingless_bees_Morphometircs_rf <- stingless_bees_Morphometircs[-indexes,]

?trainControl
#...................Random forest .........................................
trctrl <- trainControl(method = "cv", number = 10)

rfclassifier<- train(species ~ ., 
                     data = train.stingless_bees_Morphometircs_rf, 
                     method = "rf",
                     trControl=trctrl,
                     preProcess = c("center", "scale"),
                     tuneLength = 10)

print(rfclassifier)
plot(rfclassifier, 
     main ="Variation in CV error with no of randomly selected trees")

#b) performance on test data
predictionrf <- predict(rfclassifier, test.stingless_bees_Morphometircs_rf[,-1])
confusionMatrix(table(observed=test.stingless_bees_Morphometircs_rf[,1], predicted=predictionrf))

# variable importance
varImp_rfclassifier <- varImp(rfclassifier, scale = FALSE)
plot(varImp_rfclassifier, top = 14)


#...................Synthetic minority over-sampling technique (SMOTE)..............
set.seed(2022)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(stingless_bees_Morphometircs)*.70)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(stingless_bees_Morphometircs), size = data_set_size)
# Assign the data to the correct sets
train.stingless_bees_Morphometircs_rf <- stingless_bees_Morphometircs[indexes,]
test.stingless_bees_Morphometircs_rf <- stingless_bees_Morphometircs[-indexes,]
table(train.stingless_bees_Morphometircs_rf$species)

#using smote
library(DMwR)
train.stingless_bees_Morphometircs_rf$species <- as.factor(train.stingless_bees_Morphometircs_rf$species)
train.stingless_bees_Morphometircs_rf.data<-SMOTE(species~.,train.stingless_bees_Morphometircs_rf, perc.over=350, perc.under=700)
prop.table(table(train.stingless_bees_Morphometircs_rf$species))
#checking if the species no. have been chosen equally
table(train.stingless_bees_Morphometircs_rf$species)
table(train.stingless_bees_Morphometircs_rf.data$species)


#Random Forest classifier
trctrl <- trainControl(method = "cv", number = 10)

rfclassifier_smote <- train(species ~ ., data = train.stingless_bees_Morphometircs_rf.data, 
                            method = "rf",
                            trControl=trctrl,
                            preProcess = c("center", "scale"),
                            tuneLength = 10)

print(rfclassifier_smote)
plot(rfclassifier_smote, 
     main ="Variation in CV error with no of randomly selected trees")


#b) performance on test data
predictionrf_smote <- predict(rfclassifier_smote, test.stingless_bees_Morphometircs_rf[,-1])
confusionMatrix(table(observed=test.stingless_bees_Morphometircs_rf[,1], predicted=predictionrf_smote))

# variable importance
varImp_rfclassifier_smote <- varImp(rfclassifier_smote, scale = FALSE)
plot(varImp_rfclassifier_smote, top = 14)


#.......................................... SVM Radial..........................................

train.stingless_bees_Morphometircs_rf_svmr <- train.stingless_bees_Morphometircs_rf 
test.stingless_bees_Morphometircs_rf_svmr <- test.stingless_bees_Morphometircs_rf

svm_Radial <- train(species ~., 
                    data = train.stingless_bees_Morphometircs_rf_svmr, 
                    method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 20)
svm_Radial

test_pred_svmr <- predict(svm_Radial, newdata = test.stingless_bees_Morphometircs_rf_svmr)
confusionMatrix(table(test_pred_svmr, test.stingless_bees_Morphometircs_rf$species))


# variable importance
varImp_rfclassifier_svm <- varImp(svm_Radial, scale = FALSE)
plot(varImp_rfclassifier_svm, top = 14)










###### (B) stingless bees Morphometrics using using fruitfly code

#############################################

fruitfly_Morphometircs <- read.csv("stingless bees Morphometircs_Nelly2017.csv")
attach(fruitfly_Morphometircs)
fruitfly_Morphometircs <- data.frame(fruitfly_Morphometircs)

#change species to factor in train data, 
fruitfly_Morphometircs[["species"]] = factor(fruitfly_Morphometircs[["species"]])
str(fruitfly_Morphometircs)


#split data into training set and testing set (data splicing)
#) Randomised
set.seed(2026)
train <- createDataPartition(fruitfly_Morphometircs$species,
                             p = 0.7, # % of data going to training
                             times = 1,
                             list = F)
train.origffly <- fruitfly_Morphometircs[ train,]
testffly       <- fruitfly_Morphometircs[-train,]

train.fruitfly_Morphometircs_rf <- train.origffly 
test.fruitfly_Morphometircs_rf <- testffly



#...................Random forest .........................................
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

rfclassifier_ffly<- train(species ~ ., 
                          data = train.fruitfly_Morphometircs_rf, 
                          method = "rf",
                          trControl=trctrl,
                          preProcess = c("center", "scale"),
                          tuneLength = 10)

print(rfclassifier_ffly)
plot(rfclassifier_ffly,main ="", xlab="Number of randomly selected predictors",lwd=2,col="black")

#b) performance on test data
# Validation set assessment #1: looking at confusion matrix
predictionrf <- predict(rfclassifier_ffly, test.fruitfly_Morphometircs_rf[,-1])
confusionMatrix(table(observed=test.fruitfly_Morphometircs_rf[,1], predicted=predictionrf))


# variable importance
varImp_rfclassifier_ffly <- varImp(rfclassifier_ffly, scale = FALSE)
plot(varImp_rfclassifier_ffly, top = 14)
print(varImp_rfclassifier_ffly, top = 14)

# Ploting area under the curve
library(pROC)
roc_RF <- multiclass.roc(response = test.fruitfly_Morphometircs_rf[,1], predictor =as.numeric(predictionrf))
roc_RF



library(UBL) ##SMOTE algorithm for unbalanced classification problems
set.seed(2026)
train <- createDataPartition(fruitfly_Morphometircs$species,
                             p = 0.7, # % of data going to training
                             times = 1,
                             list = F)
train.orig <- fruitfly_Morphometircs[ train,]
test       <- fruitfly_Morphometircs[-train,]
train.fruitfly_Morphometircs_rf_sc <- train.orig 
test.fruitfly_Morphometircs_rf_sc <- test



train.fruitfly_Morphometircs_rf.data_sc<-SmoteClassif(species~.,
                                                      train.fruitfly_Morphometircs_rf_sc, 
                                                      C.perc = "balance", k = 5, 
                                                      repl = FALSE,
                                                      dist = "Euclidean", #"Euclidean", "Manhattan", "Canberra", "Chebyshev", "p-norm"; #"Overlap"; "HEOM", "HVDM".
                                                      p = 2)

table(train.fruitfly_Morphometircs_rf_sc$species)## original data
table(train.fruitfly_Morphometircs_rf.data_sc$species)## smoted data


#Random Forest classifier with SMOTE
trctrl <- trainControl(method = "cv", number = 10)

rfclassifier_smote_ffly_sc <- train(species ~ ., data = train.fruitfly_Morphometircs_rf.data_sc, 
                                    method = "rf",
                                    trControl=trctrl,
                                    preProcess = c("center", "scale"),
                                    tuneLength = 10)

print(rfclassifier_smote_ffly_sc)
plot(rfclassifier_smote_ffly_sc, 
     main ="Variation in CV error with no of randomly selected trees")


#b) performance on test data
# Validation set assessment #1: looking at confusion matrix
predictionrf_smote_ffly_sc <- predict(rfclassifier_smote_ffly_sc, test.fruitfly_Morphometircs_rf_sc[,-1])
confusionMatrix(table(observed=test.fruitfly_Morphometircs_rf_sc[,1], predicted=predictionrf_smote_ffly_sc))

# Variable importance under smote
# variable importance
varImp_rfclassifier_sting_smote <- varImp(rfclassifier_smote_ffly_sc, scale = FALSE)
plot(varImp_rfclassifier_sting_smote, top = 14)
print(varImp_rfclassifier_sting_smote, top = 14)

##AUC - SMOTE ====
roc_RF_SMOTE <- multiclass.roc(response =test.fruitfly_Morphometircs_rf_sc[,1] , predictor =as.numeric(predictionrf_smote_ffly_sc))
roc_RF_SMOTE


## ADASDYN classifier {UBL}=============

train.fruitfly_Morphometircs_rf.data_ac<- AdasynClassif(species~.,
                                                        train.fruitfly_Morphometircs_rf_sc,
                                                        baseClass = NULL, 
                                                        beta = 1, dth = 0.95,
                                                        k = 5, dist = "Euclidean", p = 2)

table(train.fruitfly_Morphometircs_rf$species)
table(train.fruitfly_Morphometircs_rf.data_ac$species)


#Random Forest classifier with ADASYN
trctrl <- trainControl(method = "cv", number = 10)

rfclassifier_smote_ffly_ac <- train(species ~ ., data = train.fruitfly_Morphometircs_rf.data_ac, 
                                    method = "rf",
                                    trControl=trctrl,
                                    preProcess = c("center", "scale"),
                                    tuneLength = 10)

print(rfclassifier_smote_ffly_ac)
plot(rfclassifier_smote_ffly_ac, 
     main ="Variation in CV error with no of randomly selected trees")
# Variable importance under ADASYN

varImp_rfclassifier_sting_ADASYN <- varImp(rfclassifier_smote_ffly_ac, scale = FALSE)
plot(varImp_rfclassifier_sting_ADASYN, top = 14)
print(varImp_rfclassifier_sting_ADASYN, top = 14)

#b) performance on test data ADASYN
# Validation set assessment #1: looking at confusion matrix
predictionrf_smote_ffly_ac <- predict(rfclassifier_smote_ffly_ac, test.fruitfly_Morphometircs_rf_sc[,-1])
confusionMatrix(table(observed=test.fruitfly_Morphometircs_rf_sc[,1], predicted=predictionrf_smote_ffly_ac))

##AUC - ADASYN ====
roc_RF_ADA <- multiclass.roc(response =test.fruitfly_Morphometircs_rf_sc[,1], 
                             predictor =as.numeric(predictionrf_smote_ffly_ac))
roc_RF_ADA





#.......................................... SVM Radial..........................................
trctrl <- trainControl(method = "cv", number = 10)
set.seed(2022)
train <- createDataPartition(fruitfly_Morphometircs$species,
                             p = 0.7, # % of data going to training
                             times = 1,
                             list = F)
train.orig <- fruitfly_Morphometircs[ train,]
test       <- fruitfly_Morphometircs[-train,]

train.fruitfly_Morphometircs_rf_svmr <- train.orig 
test.fruitfly_Morphometircs_rf_svmr <- test



svm_Radial <- train(species ~., 
                    data = train.fruitfly_Morphometircs_rf_svmr, 
                    method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 20)
svm_Radial

plot(svm_Radial)

test_pred_svmr <- predict(svm_Radial, newdata = test.fruitfly_Morphometircs_rf_svmr)
confusionMatrix(table(test_pred_svmr, test.fruitfly_Morphometircs_rf$species))


# variable importance for SVM
varImp_rfclassifier_svm <- varImp(svm_Radial, scale = FALSE)
plot(varImp_rfclassifier_svm, top = 14)

##AUC - SVM ====
roc_SVM <- multiclass.roc(response =test.fruitfly_Morphometircs_rf_svmr[,1], 
                             predictor =as.numeric(test_pred_svmr))
roc_SVM

### SVM with smote========

SVMclassifier_smote_ffly_sc <- train(species ~ ., data = train.fruitfly_Morphometircs_rf.data_sc, 
                                    method = "svmRadial",
                                    trControl=trctrl,
                                    preProcess = c("center", "scale"),
                                    tuneLength = 10)

print(SVMclassifier_smote_ffly_sc)
#plot(SVMclassifier_smote_ffly_sc) 
     
#b) performance on test data smote + SVM
# Validation set assessment #1: looking at confusion matrix
predictionSVM_smote_ffly_sc <- predict(SVMclassifier_smote_ffly_sc, test.fruitfly_Morphometircs_rf_sc[,-1])
confusionMatrix(table(observed=test.fruitfly_Morphometircs_rf_sc[,1], predicted=predictionSVM_smote_ffly_sc))

roc_SVM_smote <- multiclass.roc(response =test.fruitfly_Morphometircs_rf_sc[,1], 
                          predictor =as.numeric(predictionSVM_smote_ffly_sc))
roc_SVM_smote

### SVM with with ADASYN*****************************************
set.seed(1995)
train.fruitfly_Morphometircs_SVM.data_ac<- AdasynClassif(species~.,
                                                        train.fruitfly_Morphometircs_rf_sc,
                                                        baseClass = NULL, 
                                                        beta = 1, dth = 0.95,
                                                        k = 5, dist = "Euclidean", p = 2)

table(train.fruitfly_Morphometircs_rf$species)
table(train.fruitfly_Morphometircs_SVM.data_ac$species)


#SVM classifier with ADASYN
trctrl <- trainControl(method = "cv", number = 10)

SVMclassifier_ffly_ac <- train(species ~ ., data = train.fruitfly_Morphometircs_SVM.data_ac, 
                                    method = "svmRadial",
                                    trControl=trctrl,
                                    preProcess = c("center", "scale"),
                                    tuneLength = 10)

print(SVMclassifier_ffly_ac)
##plot(SVMclassifier_ffly_ac, 
     main ="Variation in CV error with no of randomly selected trees")
# Variable importance under ADASYN

varImp_SVMclassifier_sting_ADASYN <- varImp(SVMclassifier_ffly_ac, scale = FALSE)
plot(varImp_SVMclassifier_sting_ADASYN, top = 14)
print(varImp_SVMclassifier_sting_ADASYN, top = 14)

#b) performance on test data ADASYN
# Validation set assessment #1: looking at confusion matrix
predictionSVM_ffly_ac <- predict(SVMclassifier_ffly_ac, test.fruitfly_Morphometircs_rf_sc[,-1])
confusionMatrix(table(observed=test.fruitfly_Morphometircs_rf_sc[,1], predicted=predictionSVM_ffly_ac))

##AUC - ADASYN ====
roc_SVM_ADA <- multiclass.roc(response =test.fruitfly_Morphometircs_rf_sc[,1], 
                             predictor =as.numeric(predictionSVM_ffly_ac))
roc_SVM_ADA
