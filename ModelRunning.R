#Model Running for Final Case

load("TOTALCASH.CLEAN.Rdata")
library(gbm)
library(caret)
library(parallel)
library(doParallel)
library(xgboost)
library(kernlab)
library(rpart)
library(regclass)

#turning on parallelization (optional)
cluster <- makeCluster(detectCores() - 1) 
registerDoParallel(cluster) 

#GLMNet
fitControl <- trainControl(method="cv",number=5, allowParallel = TRUE) 
glmnetGrid <- expand.grid(alpha = seq(0,.5,by=0.05),lambda = 10^seq(-2,1,length=20)) 
GLMnet <- train(LOG_TOTAL_CASH~.,data=CASH.TRAIN.CLEAN,method='glmnet',tuneGrid=glmnetGrid,
                trControl=fitControl,preProc=c("center", "scale") )
plot(GLMnet)
GLMnet$bestTune
GLMnet$results[rownames(GLMnet$bestTune),] 

predictions <- predict(GLMnet,newdata=CASH.KAGGLE.CLEAN)
D <- data.frame(ID=CASH.KAGGLE.CLEAN$ID,LOG_TOTAL_CASH=predictions)
write.csv(D,row.names=FALSE,file="GLMNET_CASH.csv")

#K Nearest Neighbors
knnGrid <- expand.grid(k=1:40)
KNN <- train(LOG_TOTAL_CASH~.,data=CASH.TRAIN.CLEAN,method='knn', tuneGrid=knnGrid,
             trControl=fitControl, preProc = c("center", "scale"))

plot(KNN)  #See how error varies with k
KNN$bestTune #Gives best parameters
KNN$results[rownames(KNN$bestTune),]

predictions <- predict(KNN,newdata=CASH.KAGGLE.CLEAN)
D <- data.frame(ID=CASH.KAGGLE.CLEAN$ID,LOG_TOTAL_CASH=predictions)
write.csv(D,row.names=FALSE,file="KNN_CASH.csv")

#Random Forest
forestGrid <- expand.grid(mtry=c(1:32))
fitControl <- trainControl(method = "cv",number = 5, allowParallel = TRUE)
FOREST <- train(LOG_TOTAL_CASH~.,data=CASH.TRAIN.CLEAN,method="rf",preProc=c("center","scale"),
                trControl=fitControl,tuneGrid=forestGrid, importance=TRUE)

FOREST$results[rownames(FOREST$bestTune),] 

predictions <- predict(FOREST,newdata=CASH.KAGGLE.CLEAN)
D <- data.frame(ID=CASH.KAGGLE.CLEAN$ID,LOG_TOTAL_CASH=predictions)
write.csv(D,row.names=FALSE,file="FOREST_CASH.csv")

#Boosted Tree
gbmGrid <- expand.grid(n.trees=c(850,900,1000),
                       interaction.depth=c(15,49,35,11),
                       shrinkage=c(.01,.02,0.009),
                       n.minobsinnode=c(16,18,20))
fitControl <- trainControl(method = "cv",number = 5, allowParallel = TRUE)
GBM <- train(LOG_TOTAL_CASH~.,data=CASH.TRAIN.CLEAN,method="gbm",trControl=fitControl,
             tuneGrid=gbmGrid,preProc=c("center","scale"),verbose=FALSE)

plot(GBM)
GBM$results[rownames(GBM$bestTune),]

THIRD_PARTY <- CASH.TRAIN.CLEAN[,-c(17:20,22:24)]
gbmGrid <- expand.grid(n.trees=c(850),
                       interaction.depth=c(15),
                       shrinkage=c(0.009),
                       n.minobsinnode=c(16))
fitControl <- trainControl(method = "cv",number = 5, allowParallel = TRUE)
GBM1 <- train(LOG_TOTAL_CASH~.,data=THIRD_PARTY,method="gbm",trControl=fitControl,
             tuneGrid=gbmGrid,preProc=c("center","scale"),verbose=FALSE)
GBM1$results[rownames(GBM1$bestTune),]


predictions <- predict(GBM,newdata=CASH.KAGGLE.CLEAN)
D <- data.frame(ID=CASH.KAGGLE.CLEAN$ID,LOG_TOTAL_CASH=predictions)
write.csv(D,row.names=FALSE,file="GBM_CASH.csv")

#XGBoost
xgbGrid <- expand.grid(nrounds = c(2400:2500), 
                       max_depth = 8:10,
                       eta = 0.003:0.01,
                       colsample_bytree = 0.75:0.9,
                       min_child_weight = 3:5,
                       subsample = 0.5:0.8,
                       gamma = 0.005:0.01)

fitControl <- trainControl(method = "cv",number = 5, allowParallel = TRUE)
XGB <- train(LOG_TOTAL_CASH~.,data=CASH.TRAIN.CLEAN,method="xgbTree",trControl=fitControl,
             tuneGrid=xgbGrid,preProc=c("center","scale"),verbose=FALSE)
plot(XGB)
XGB$results[rownames(XGB$bestTune),]

predictions <- predict(XGB,newdata=CASH.KAGGLE.CLEAN)
D <- data.frame(ID=CASH.KAGGLE.CLEAN$ID,LOG_TOTAL_CASH=predictions)
write.csv(D,row.names=FALSE,file="XGB_CASH.csv")

#Neural Net
fitControl <- trainControl(method="cv", number=5, verboseIter = FALSE) 
nnetGrid <- expand.grid( size=5:20, decay=10^( seq(-5, 5, by=.5) ) )

NNET <- train(LOG_TOTAL_CASH~.,data=CASH.TRAIN.CLEAN,method='nnet', tuneGrid=nnetGrid,
              trControl=fitControl, preProc = c("center", "scale"),
              trace = FALSE, linout = TRUE)

plot(NNET)
NNET$results[rownames(NNET$bestTune),]

predictions <- predict(NNET,newdata=CASH.KAGGLE.CLEAN)
D <- data.frame(ID=CASH.KAGGLE.CLEAN$ID,LOG_TOTAL_CASH=predictions)
write.csv(D,row.names=FALSE,file="NNET_CASH.csv")

#Linear SVM
paramGrid <- expand.grid(C=10^seq(-2,3,length=21))
SVM.linear <- train(LOG_TOTAL_CASH~.,data=CASH.TRAIN.CLEAN, method='svmLinear',
                    trControl=fitControl,tuneGrid=paramGrid,preProc=c("center", "scale"))

plot(SVM.linear)
SVM.linear$results[rownames(SVM.linear$bestTune),]

#LOG_TOTAL_CASH NAIVE
train.rows <- sample(1:nrow(CASH.TRAIN.CLEAN),0.5*nrow(CASH.TRAIN.CLEAN))
KNOWN <- CASH.TRAIN.CLEAN[train.rows,]
UNKNOWN <- CASH.TRAIN.CLEAN[-train.rows,]
y.predicted <- mean(KNOWN$LOG_TOTAL_CASH)
sqrt( mean( (y.predicted-UNKNOWN$LOG_TOTAL_CASH)^2 ) ) #Best guess of RMSE generalization error on naive model

#Vanilla Partition
rpartGrid <- expand.grid(cp=10^seq(-1,1,length=50))
RPART <- train(LOG_TOTAL_CASH~.,data=CASH.TRAIN.CLEAN,method="rpart",trControl=fitControl,
               tuneGrid=rpartGrid,preProc=c("center","scale"))

plot(RPART)  #See how performance varies with cp parameters
RPART$results[rownames(RPART$bestTune),]

CASH.TRAIN.CLEAN$TOTAL_CASH <- 10^CASH.TRAIN.CLEAN$LOG_TOTAL_CASH
write.csv(CASH.TRAIN.CLEAN,row.names=FALSE,file="totalcash.csv")
varImp(GBM)


#Combos
CASH.TRAIN.CLEAN$TOTAL_CASH <- NULL
TREE <- rpart(LOG_TOTAL_CASH ~ .,
              data=CASH.TRAIN.CLEAN,cp=0.01652374 )
TREE$cptable
summarize_tree(TREE)
visualize_model(TREE)
TREE
10^2.24; 10^1.51
10^2.05; 10^2.55
10^2.29; 10^3.16
10^1.8; 10^2.3

