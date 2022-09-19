load("TOTALCASH.CLEAN.RData")
load("DONATEDIN2021.CLEANVERSION1.RData")
CASH.TRAIN.CLEAN$ID <- NULL
DONATED.TRAIN.CLEAN$ID <- NULL




##Understanding
library("DALEX")

#Make special datasets for examining models
TRAIN.PREDICTORS <- CASH.TRAIN.CLEAN
TRAIN.PREDICTORS$LOG_TOTAL_CASH <- NULL
TRAIN.PREDICTORS$TOTAL_CASH <- NULL
TRAIN.TARGET <- CASH.TRAIN.CLEAN$LOG_TOTAL_CASH

#Example building an explainer with GBM
shop_explainer <- explain(GBM, data = TRAIN.PREDICTORS,  y = TRAIN.TARGET)
shop_vi <- model_parts(shop_explainer, loss_function = loss_root_mean_square)
shop_vi
plot(shop_vi)

plot( model_profile(shop_explainer,type = "accumulated"), variables = "BEQUEST_LIKELIHOOD") 

profile_group <- model_profile(explainer = shop_explainer, 
                               variables = c("BEQUEST_LIKELIHOOD"), 
                               groups = "DONATED_LAST_10YEARS", type = "partial")  
plot(profile_group)

profile_group <- model_profile(explainer = shop_explainer, 
                               variables = c("BEQUEST_LIKELIHOOD"), 
                               groups = "FavGame", type = "partial")  
plot(profile_group)


profile_group <- model_profile(explainer = shop_explainer, 
                               variables = c("BEQUEST_LIKELIHOOD"), 
                               groups = "DEGREE_1_GRAD_YEAR", type = "partial")  
plot(profile_group)

profile_group <- model_profile(explainer = shop_explainer, 
                               variables = c("OpenReceiveRatio"), 
                               groups = "DONATED_LAST_10YEARS", type = "partial")  
plot(profile_group)

predictions <- predict(GBM,newdata=CASH.KAGGLE.CLEAN)
which(predictions > 3.8)
which(predictions < 1.2)

specific.alumnus <- CASH.KAGGLE.CLEAN[1618,]
plot( predict_parts(explainer = shop_explainer, new_observation = specific.alumnus, type = "break_down") )

specific.alumnus1 <- CASH.KAGGLE.CLEAN[1718,]
plot( predict_parts(explainer = shop_explainer, new_observation = specific.alumnus1, type = "break_down") )




###DONATED IN 21


library("DALEX")

#Make special datasets for examining models
TRAIN.PREDICTORS <- DONATED.TRAIN.CLEAN
TRAIN.PREDICTORS$DonatedIn2021 <- NULL
TRAIN.TARGET <- as.numeric(DONATED.TRAIN.CLEAN$DonatedIn2021=="Yes")

#Example building an explainer with random forest
library(regclass)
FOREST <- randomForest(DonatedIn2021~.,data=DONATED.TRAIN.CLEAN,mtry=30 )
# donated_ranger <- ranger::ranger(Donated19~., data = DONATED.TRAIN.CLEAN, num.trees = 500, mtry=40,
#                                  probability = TRUE)
# donated_ranger_exp <- explain(donated_ranger, data = TRAIN.PREDICTORS, y = TRAIN.TARGET)
donated_forest_exp <- explain(FOREST,data = TRAIN.PREDICTORS, y = TRAIN.TARGET)
shop_explainer_donate <- donated_forest_exp
shop_vi <- model_parts(shop_explainer_donate)
shop_vi
plot(shop_vi)



plot( model_profile(shop_explainer_donate,type = "accumulated"), variables = "BEQUEST_LIKELIHOOD") 
plot( model_profile(shop_explainer_donate,type = "accumulated"), variables = "DEGREE_1_GRAD_YEAR") 
plot( model_profile(shop_explainer_donate,type = "accumulated"), variables = "FIRST_GIFT_AMT_UT") 

profile_group <- model_profile(explainer = shop_explainer_donate, 
                               variables = c("BEQUEST_LIKELIHOOD"), 
                               groups = "BirthGradFlag", type = "partial")  
plot(profile_group)

profile_group <- model_profile(explainer = shop_explainer_donate, 
                               variables = c("BEQUEST_LIKELIHOOD"), 
                               groups = "TotalActivities", type = "partial")  
plot(profile_group)


profile_group <- model_profile(explainer = shop_explainer_donate, 
                               variables = c("BEQUEST_LIKELIHOOD"), 
                               groups = "ESTIMATED_GIFT_CAPACITY", type = "partial")  
plot(profile_group)

profile_group <- model_profile(explainer = shop_explainer_donate, 
                               variables = c("CRT_LIKELIHOOD"), 
                               groups = "GENDER", type = "partial")  
plot(profile_group)

predictions <- predict(FOREST,newdata=DONATED.KAGGLE.CLEAN,type="prob")[,2]
which(predictions > 0.95 & predictions < 0.96)
which(predictions > 0.05 & predictions < 0.053)


specific.alumnus <- DONATED.KAGGLE.CLEAN[6804,]
plot( predict_parts(explainer = shop_explainer_donate, new_observation = specific.alumnus, type = "break_down") )

specific.alumnus <- DONATED.KAGGLE.CLEAN[5080 ,]
plot( predict_parts(explainer = shop_explainer_donate, new_observation = specific.alumnus, type = "break_down") )


