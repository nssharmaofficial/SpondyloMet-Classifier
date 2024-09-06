library(randomForest)
library(pROC)
library(caret)
library(kernlab)
library(e1071)
library(readxl)


features <- read_excel("../data/features.xlsx")

# change the quality column to a factor type
features$Disease <- as.factor(features$Disease)

#using Boruta for variable selection
library(Boruta)
boruta_output <- Boruta(factor(Disease) ~ ., data=na.omit(features), doTrace=2) 
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])
print(boruta_signif)
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 
vypis <- attStats(boruta_output)
vypis

# split the dataframe into train and test sets
index <- sample(1:nrow(features),size = 0.7*nrow(features))
train.split <- features[index,]
test.split <- features[-index,]
names(train.split) <- make.names(names(train.split))

#Random Forest/ Train,Test split = BEST MODEL

rf <- randomForest(factor(Disease)~GLCM_homogeneity
                   +GLCM_contrast
                   +GLCM_dissimilarity
                   +GLRLM_SRE
                   +GLZLM_GLNU
                   +GLZLM_ZP
                   +NGLDM_Busyness
                   +GLRLM_LGRE
                   +GLRLM_SRLGE
                   +GLZLM_SZHGE
                   +GLRLM_GLNU
                   +GLRLM_RLNU
                   +GLRLM_RP
                   +GLZLM_LZE
                   +GLZLM_LZLGE
                   +NGLDM_Coarseness
                   +NGLDM_Contrast
                   +GLZLM_SZE,data=train.split, ntree=400, mtry=4, na.action = na.omit, importance=TRUE)
print(rf)                   
varImpPlot(rf)

rf_pred <- predict(rf, test.split, type = "prob")
print(rf_pred)

confusionMatrix(as.factor(rf_pred), as.factor(test.split$Disease))

ROC_rf <- roc(factor(test.split$Disease) ,rf_pred[ ,2])
plot(ROC_rf, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     max.auc.polygon=TRUE,print.thres=TRUE,
     auc.polygon.col="skyblue")

save(rf , file = 'MyML.rda')

# VALIDATION
library(readxl)
validation <- read_excel("../data/validation.xlsx")
valid <- predict(rf, validation, type = "prob")
ROC_rf_valid <- roc(factor(validation$Disease) ,valid[ ,2])
plot(ROC_rf_valid, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     max.auc.polygon=TRUE,print.thres=TRUE,
     auc.polygon.col="skyblue")
ci.auc(ROC_rf_valid)


confusionMatrix(as.factor(valid), as.factor(validation$Disease))

# Logistic Regression/ Train,Test split

lr <- glm(factor(Disease)~GLCM_homogeneity
          +GLCM_contrast
          +GLCM_dissimilarity
          +GLRLM_SRE
          +GLZLM_GLNU
          +GLZLM_ZP
          +NGLDM_Busyness
          +GLRLM_LGRE
          +GLRLM_SRLGE
          +GLZLM_SZHGE
          +GLRLM_GLNU
          +GLRLM_RLNU
          +GLRLM_RP
          +GLZLM_LZE
          +GLZLM_LZLGE
          +NGLDM_Coarseness
          +NGLDM_Contrast
          +GLZLM_SZE, data = train.split, family = "binomial", control= list(maxit=150))
print(lr)

lr_pred <- predict(lr, test.split, type = "response")
lr_pred

ROC_lr <- roc(factor(test.split$Disease), lr_pred)
plot(ROC_lr, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     max.auc.polygon=TRUE,print.thres=TRUE,auc.polygon.col="skyblue")


#svm/ Train,Test split

svm_traintest <- svm(factor(Disease)~GLCM_homogeneity
                     +GLCM_contrast
                     +GLCM_dissimilarity
                     +GLRLM_SRE
                     +GLZLM_GLNU
                     +GLZLM_ZP
                     +NGLDM_Busyness
                     +GLRLM_LGRE
                     +GLRLM_SRLGE
                     +GLZLM_SZHGE
                     +GLRLM_GLNU
                     +GLRLM_RLNU
                     +GLRLM_RP
                     +GLZLM_LZE
                     +GLZLM_LZLGE
                     +NGLDM_Coarseness
                     +NGLDM_Contrast
                     +GLZLM_SZE,data=train.split)
svm_traintest_pred <- predict(svm_traintest, test.split, type = "prob")

ROC_svm1 <- roc(factor(test.split$Disease), as.ordered(svm_traintest_pred))
plot(ROC_svm1,print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     max.auc.polygon=TRUE,print.thres=TRUE,auc.polygon.col="skyblue")


roc.rf1 <- plot.roc(factor(test.split$Disease), rf_pred[ ,2], main="Statistical comparison of method: Train/Test split", col="1", percent=TRUE,grid=c(0.1, 0.2))
roc.lr1 <- lines.roc(factor(test.split$Disease), lr_pred, col="2", percent=TRUE)
roc.svm1 <- lines.roc(factor(test.split$Disease), as.ordered(svm_traintest_pred), col="4", percent=TRUE)

test.trainsplit <- roc.test(ROC_lr, ROC_rf)

text(50, 50, labels=paste("p-value =", format.pval(test.trainsplit$p.value)),adj=c(0, 5))
legend("bottomright", legend=c("Random Forest, AUC=98.61%", "Logistic Regression, AUC=84.03%", "Support Vector Machines, AUC=87.5%"), col=c("1", "2", "4"), lwd=2)

auc(roc.rf1)
auc(roc.lr1)
auc(roc.svm1)



###############################################################################


# Random Forest + Logistic Regression + svm/ Leave one out cross validation
test=rep(0, times=80)
test_lm=rep(0, times=80)
test_svm=rep(0, times=80)

for(i in seq(1,80)){
  train = features[-i,]
  rf_fit<-randomForest(factor(Disease)~GLCM_homogeneity
                       +GLCM_contrast
                       +GLCM_dissimilarity
                       +GLRLM_SRE
                       +GLZLM_GLNU
                       +GLZLM_ZP
                       +NGLDM_Busyness
                       +GLRLM_LGRE
                       +GLRLM_SRLGE
                       +GLZLM_SZHGE
                       +GLRLM_GLNU
                       +GLRLM_RLNU
                       +GLRLM_RP
                       +GLZLM_LZE
                       +GLZLM_LZLGE
                       +NGLDM_Coarseness
                       +NGLDM_Contrast
                       +GLZLM_SZE,
                       data=train, mtry=3, ntree=500)
  lm_fit <- glm(factor(Disease)~GLCM_homogeneity
                +GLCM_contrast
                +GLCM_dissimilarity
                +GLRLM_SRE
                +GLZLM_GLNU
                +GLZLM_ZP
                +NGLDM_Busyness
                +GLRLM_LGRE
                +GLRLM_SRLGE
                +GLZLM_SZHGE
                +GLRLM_GLNU
                +GLRLM_RLNU
                +GLRLM_RP
                +GLZLM_LZE
                +GLZLM_LZLGE
                +NGLDM_Coarseness
                +NGLDM_Contrast
                +GLZLM_SZE,
                data=train, 
                family="binomial")
  svm_fit <- svm(factor(Disease)~GLCM_homogeneity
                 +GLCM_contrast
                 +GLCM_dissimilarity
                 +GLRLM_SRE
                 +GLZLM_GLNU
                 +GLZLM_ZP
                 +NGLDM_Busyness
                 +GLRLM_LGRE
                 +GLRLM_SRLGE
                 +GLZLM_SZHGE
                 +GLRLM_GLNU
                 +GLRLM_RLNU
                 +GLRLM_RP
                 +GLZLM_LZE
                 +GLZLM_LZLGE
                 +NGLDM_Coarseness
                 +NGLDM_Contrast
                 +GLZLM_SZE,
                 data=train)
  prediction <- predict(rf_fit, features[i,], type="prob")
  prediction <- prediction[,2]
  test[i] = prediction
  prediction_lm <- predict(lm_fit, features[i,], type="response")
  test_lm[i] = prediction_lm
  prediction_svm <- predict(svm_fit, features[i, ], type = "prob")
  test_svm[i] = prediction_svm
}

save(rf_fit , file = 'MyML2.rda')

ROC_rf2 <- roc(factor(features$Disease) ,test)
plot(ROC_rf2, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     max.auc.polygon=TRUE,print.thres=TRUE,
     auc.polygon.col="skyblue")

ROC_lr2 <- roc(factor(features$Disease), test_lm)
plot(ROC_lr2, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     max.auc.polygon=TRUE,
     auc.polygon.col="skyblue")

ROC_svm2 <- roc(factor(features$Disease), as.ordered(test_svm))
plot(ROC_svm2, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     max.auc.polygon=TRUE,
     auc.polygon.col="skyblue")

roc.rf2 <- plot.roc(features$Disease, test, main="Statistical comparison of method: Leave one out cross validation", col="1", percent=TRUE,grid=c(0.1, 0.2))
roc.lr2 <- lines.roc(features$Disease, test_lm, col="2", percent=TRUE)
roc.svm2 <- lines.roc(factor(features$Disease), as.ordered(test_svm), col="4", percent=TRUE)

test.leaveone<- roc.test(roc.rf2,roc.lr2)

text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)),adj=c(0, 5))
legend("bottomright", legend=c("Random Forest, AUC=92.31%", "Logistic Regression, AUC=76.25%","Support Vector Machines, AUC=82.5%"), col=c("1", "2","4"), lwd=2)

auc(roc.rf2)
auc(roc.lr2)
auc(roc.svm2)

###################################################################################

# Run algorithms using 10-fold cross validation

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

set.seed(20)
glm.cross <- train(Disease~., data=train.split, method="glm",family="binomial", metric=metric, trControl=control)
set.seed(20)
svm.cross <- train(Disease~., data=train.split, method="svmRadial", metric=metric, trControl=control)
set.seed(20)
rf.cross <- train(Disease~., data=train.split, method="rf", metric=metric, trControl=control)

results <- resamples(list(glm=glm.cross,svm=svm.cross, rf=rf.cross))
summary(results)
dotplot(results)

predictions_glm.cross <- predict(glm.cross, test.split)
confusionMatrix(predictions_glm.cross, factor(test.split$Disease))
pred.glm <- as.numeric(predictions_glm.cross)

predictions_svm.cross <- predict(svm.cross, test.split)
confusionMatrix(predictions.svm, factor(test.split$Disease))
pred.svm <- as.numeric(predictions_svm.cross)

predictions_rf.cross <- predict(rf.cross, test.split)
confusionMatrix(predictions_rf.cross, factor(test.split$Disease))
pred.rf <- as.numeric(predictions_rf.cross)

roc.rf3 <- lines.roc(test.split$Disease, pred.rf, col="1", percent=TRUE)
roc.lr3 <- plot.roc(test.split$Disease, pred.glm, main="Statistical comparison of method: K-fold cross validation", col="2", percent=TRUE,grid=c(0.1, 0.2))
roc.svm3 <- lines.roc(test.split$Disease, pred.svm, col="4", percent=TRUE)
legend("bottomright", legend=c("Random Forest, AUC=83.33%", "Logistic Regression, AUC=87.5%","Support Vector Machines, AUC=87.5%"), col=c("1", "2","4"), lwd=2)

auc(roc.lr3)
auc(roc.svm3)
auc(roc.rf3)


