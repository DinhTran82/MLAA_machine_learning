rm(list=ls())
dev.off()



library(tidyverse)
library(ggplot2)
library(caret)
library(e1071)
library(ROCR)
library(ROSE)
library(gbm)
library(pdp)
library(parallel)
library(doParallel)
library(corrplot)
library(ranger)
library(randomForest)

#read the data
repurchase <-read_csv("repurchase_training.csv")
summary(repurchase)





# checking proportion of each group in each column (except the ID column)
col_names <- names(repurchase)

for (colname in col_names[2:17]){
  tab_temp<-table(repurchase[,colname])
  print(colname)
  print(prop.table(tab_temp))
}


repurchase$Target <- as.factor(repurchase$Target)
repurchase$model <- as.numeric(str_extract(repurchase$car_model,"\\d+"))
options(scipen = 100, digits = 4)

ggplot(data=repurchase)+aes(x=Target)+geom_bar()



sum(repurchase$model==2)


#EDA and plots

 ggplot(data=repurchase)+aes(x=car_segment, fill=car_segment)+geom_bar()+
  facet_wrap(~Target, scales="free")+theme(legend.position="top", legend.box = "horizontal")

ggplot(data=repurchase)+aes(x=model, fill=as.factor(model))+geom_bar()+scale_fill_discrete(name = "Car model")+labs(x="car model")+theme(legend.position="top", legend.box = "horizontal")+facet_wrap(~Target, scales="free")

ggplot(data=repurchase)+aes(x=as.factor(gender), fill=as.factor(gender))+geom_bar()+scale_fill_discrete(name = "Gender")+labs(x="car model")+theme(legend.position="top", legend.box = "horizontal")+facet_wrap(~Target, scales="free")

ggplot(data=repurchase)+aes(x=as.factor(age_band), fill=as.factor(age_band))+geom_bar()+scale_fill_discrete(name = "Age band")+labs(x="car model")+theme(legend.position="top", legend.box = "horizontal")+facet_wrap(~Target, scales="free")

ggplot(data=repurchase)+aes(x=as.factor(age_of_vehicle_years), fill=as.factor(age_of_vehicle_years))+
  geom_bar()+ 
  scale_fill_discrete(name = "Age of vehicle")+
  labs(x="Age of vehicle in deciles")+theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~Target, scales="free")


ggplot(data=repurchase)+aes(x=as.factor(sched_serv_warr), fill=as.factor(sched_serv_warr))+
  geom_bar()+ 
  scale_fill_discrete(name = "Numbers of scheduled services")+
  labs(x="Numbers of scheduled services indeciles")+theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~Target, scales="free")

ggplot(data=repurchase)+aes(x=as.factor(non_sched_serv_warr), fill=as.factor(non_sched_serv_warr))+
  geom_bar()+ 
  scale_fill_discrete(name = "Numbers of non scheduled services")+
  labs(x="Numbers non of scheduled services in deciles")+theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~Target, scales="free")

ggplot(data=repurchase)+aes(x=as.factor(sched_serv_paid), fill=as.factor(sched_serv_paid))+
  geom_bar()+ 
  scale_fill_discrete(name = "Amount paid for scheduled services")+
  labs(x="Amount paid for scheduled services in deciles")+theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~Target, scales="free")


ggplot(data=repurchase)+aes(x=as.factor(non_sched_serv_paid), fill=as.factor(non_sched_serv_paid))+
  geom_bar()+ 
  scale_fill_discrete(name = "Amount paid for non scheduled services")+
  labs(x="Amount paid for scheduled services in deciles")+theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~Target, scales="free")


ggplot(data=repurchase)+aes(x=as.factor(total_paid_services), fill=as.factor(total_paid_services))+
  geom_bar()+ 
  scale_fill_discrete(name = "Total paid services")+
  labs(x="Amount paid in total for services in deciles")+theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~Target, scales="free")



ggplot(data=repurchase)+aes(x=as.factor(total_services), fill=as.factor(total_services))+
  geom_bar()+ 
  scale_fill_discrete(name = "Total services")+
  labs(x="Number of total services indeciles")+theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~Target, scales="free")


ggplot(data=repurchase)+aes(x=as.factor(mth_since_last_serv), fill=as.factor(mth_since_last_serv))+
  geom_bar()+ 
  scale_fill_discrete(name = "Number of months since the last service")+
  labs(x="The number of months since the last service in deciles")+theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~Target, scales="free")


ggplot(data=repurchase)+aes(x=as.factor(annualised_mileage), fill=as.factor(annualised_mileage))+
  geom_bar()+ 
  scale_fill_discrete(name = "Annualised vehicle milage")+
  labs(x="Annualised vehicle milage in deciles")+theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~Target, scales="free")


ggplot(data=repurchase)+aes(x=as.factor(num_dealers_visited), fill=as.factor(num_dealers_visited))+
  geom_bar()+ 
  scale_fill_discrete(name = "Number of different dealers visited")+
  labs(x="Number of different dealers visited for servicing in deciles")+theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~Target, scales="free")

ggplot(data=repurchase)+aes(x=as.factor(num_serv_dealer_purchased), fill=as.factor(num_serv_dealer_purchased))+
  geom_bar()+ 
  scale_fill_discrete(name = "Number of services at the purchased dealer")+
  labs(x="Number of services had at the same dealer where the vehicle was purchased, in deciles")+theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~Target, scales="free")


#remove ID, gender and age columns 
names(repurchase)
trainset <- repurchase[,-c(1, 3,4,5)]
class(trainset$model)

names(trainset)

trainset$car_segment <-as.factor(trainset$car_segment)
levels(trainset$car_segment) <- c("Small-Medium", "Large-SUV", "LCV", "Other")


#### UPSAMPLING ########
set.seed(42)

trainup <- upSample(x = trainset[,-1], y = trainset$Target)
names(trainup)[names(trainup) == 'Class'] <- 'Target'
table(trainup$Target)


#### DOWN SAMPLING #######
set.seed(42)

traindown <- downSample(x = trainset[,-1], y = trainset$Target)
names(traindown)[names(traindown) == 'Class'] <- 'Target'
table(traindown$Target)


##### USING ROSE ##########
set.seed(42)
trainrose <- ROSE(Target ~ ., data  = trainset)$data                         
table(trainrose$Target) 


######################### backward selection for linear classification ##################

# GLM #
glm1 <- glm(Target ~ ., family = binomial(logit), data = trainset)
summary(glm1)


#stepwise selection
step(glm1, direction = "backward")

#best model found from above
step_glm <- glm(formula = Target ~ car_segment + sched_serv_warr + sched_serv_paid + 
                  non_sched_serv_paid + total_paid_services + total_services + 
                  mth_since_last_serv + annualised_mileage + num_dealers_visited + 
                  num_serv_dealer_purchased + model, family = binomial(logit), 
                data = trainset)

summary(step_glm)


# ###### Best model  after removing variables which are not significant (from model found by backward selection) 

###### equation for the model ########################
eq<- Target ~ sched_serv_warr + sched_serv_paid + 
  non_sched_serv_paid + total_paid_services + total_services + 
  mth_since_last_serv + annualised_mileage + num_dealers_visited + 
  num_serv_dealer_purchased+model
set.seed(42)
logit <- glm(formula=eq, family = binomial(logit), 
                 data = trainset)
summary(logit)

p1logit <-predict(logit, newdata=trainset,type = "response")
prediction <-as.integer(p1logit>0.49)
confusionMatrix(data=as.factor(prediction), reference=as.factor(trainset$Target), positive="1")






###### Using k fold cross validation for linear classification #####
set.seed(42)
fitControl <- trainControl(method="cv", number=5)



#### Linear classification  ###########
#.............without subsampling...............
glm_model <- train(Target~., data = trainset, method = "glm",family = "binomial", trControl = fitControl)

up_model <- train(Target~., data = trainup, method = "glm",family = "binomial", trControl = fitControl)

down_model <- train(Target~.,  data = traindown,  method = "glm", family = "binomial", trControl = fitControl)

rose_model <- train( Target~.,  data = traindown,  method = "glm", family = "binomial", trControl = fitControl)



###### Confusion Matrix for these models ######################

p1_glm <-predict(glm_model, trainset)
cfm_glm <-confusionMatrix(data=p1_glm, reference=as.factor(trainset$Target), positive="1")

p1_up <-predict(up_model, trainset)
cfm_up <- confusionMatrix(data=p1_up, reference=as.factor(trainset$Target), positive="1")

p1_down <-predict(down_model, trainset)
cfm_down <- confusionMatrix(data=p1_down, reference=as.factor(trainset$Target), positive="1")

p1_rose <-predict(rose_model, trainset)
cfm_rose<- confusionMatrix(data=p1_rose, reference=as.factor(trainset$Target), positive="1")





### Building confusion matrix and calculate accuracy, recall, precision manually #########

cfm_glm
glm_cfm <- table( predicted=p1_glm,true =trainset$Target)
glm_cfm

#Accuracy
glm_accuracy <- (glm_cfm[2,2]+glm_cfm[1,1])/sum(glm_cfm)
glm_accuracy

#Precision
glm_precision <- glm_cfm[2,2]/(glm_cfm[2,2]+glm_cfm[2,1])
glm_precision

#Recall
glm_recall <- glm_cfm[2,2]/(glm_cfm[2,2]+glm_cfm[1,2])
glm_recall

#f1
glm_f1 <- 2*(glm_precision*glm_recall/(glm_precision+glm_recall))
glm_f1




cfm_down
down_cfm <- table( predicted=p1_down,true =trainset$Target)
down_cfm

#Accuracy
down_accuracy <- (down_cfm[2,2]+down_cfm[1,1])/sum(down_cfm)
down_accuracy

#Precision
down_precision <- down_cfm[2,2]/(down_cfm[2,2]+down_cfm[2,1])
down_precision

#Recall
down_recall <- down_cfm[2,2]/(down_cfm[2,2]+down_cfm[1,2])
down_recall

#f1
down_f1 <- 2*(down_precision*down_recall/(down_precision+down_recall))
down_f1


cfm_down
cfm_rose


############  ROC curve ############################
library(ROCR)
glm_prob <- predict(glm_model, trainset, type = "prob")$"1"

up_prob <- predict(up_model, trainset, type = "prob")$"1"

down_prob <- predict(down_model, trainset, type = "prob")$"1"

rose_prob <- predict(rose_model, trainset, type = "prob")$"1"


# Compute AUC metrics for cv_model1 and cv_model3
perf_glm <- prediction(glm_prob, trainset$Target) %>% performance(measure = "tpr", x.measure = "fpr")

perf_up <-prediction(up_prob, trainset$Target) %>% performance(measure = "tpr", x.measure = "fpr")

perf_down <- prediction(down_prob, trainset$Target) %>% performance(measure = "tpr", x.measure = "fpr")

perf_rose <-prediction(rose_prob, trainset$Target) %>% performance(measure = "tpr", x.measure = "fpr")

##............... Plot ROC curves for these models................................
plot(perf_glm, col = "black", lty = 2)
plot(perf_up, add = TRUE, col = "blue")
plot(perf_down, add = TRUE, col = "green")
plot(perf_rose, add = TRUE, col = "red")
abline(a=0,b=1)


legend(0.7, 0.3, legend = c("without subsampling", "upsampling", "downsampling", "both:using ROSE"),
       col = c("black", "blue", "green", "red"), lty = 2:1, cex = 0.6)

## ROC curve





#### AUC####
glm_auc <-performance(prediction(glm_prob, trainset$Target),"auc")
glm_auc<-unlist(slot(glm_auc, "y.values"))


up_auc <-performance(prediction(up_prob, trainset$Target),"auc")
up_auc<-unlist(slot(up_auc, "y.values"))


down_auc <-performance(prediction(down_prob, trainset$Target),"auc")
down_auc<-unlist(slot(down_auc, "y.values"))


rose_auc <-performance(prediction(rose_prob, trainset$Target),"auc")
rose_auc<-unlist(slot(rose_auc, "y.values"))




c(glm_auc, up_auc, down_auc, rose_auc)

#### important variables ######
varImp(glm_model)
varImp(up_model)
varImp(rose_model)
varImp(down_model)




### GBM Model #####
set.seed(42)
control<- trainControl(method = "cv",
                       number = 5, #Making a simple cv for speed here
                       search="grid",
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       allowParallel = TRUE
)


### downsampling 
levels(traindown$Target) <- c("No", "Yes")


#####default grid
gbm_down1 = train( x = traindown[, -ncol(traindown)],  y = traindown$Target,  
                   method = "gbm", 
                  trControl = control, verbose = FALSE)
gbm_down1

#EXPAND GRID
gbm_grid =  expand.grid(
  interaction.depth = c(3,4),
  n.trees = (1:5)*100, 
  shrinkage = 0.01,
  n.minobsinnode = 10
)
set.seed(42)
 gbm_down = train( x = traindown[, -ncol(traindown)],  y = traindown$Target, 
                   method = "gbm", 
                  trControl = control, verbose = FALSE,
                  tuneGrid= gbm_grid)

gbm_down
 
 
levels(trainset$Target) <- c("No", "Yes")
p1_gbm <-predict(gbm_down, trainset)
cfm_gbm <- confusionMatrix(data=p1_gbm, reference=as.factor(trainset$Target), positive="Yes")
cfm_gbm


gbm_prob <- predict(gbm_down, trainset, type = "prob")$"Yes"
perf_gbm <- prediction(gbm_prob, trainset$Target) %>% performance(measure = "tpr", x.measure = "fpr")
plot(perf_gbm, col = "blue", lty = 2, main="ROC curve on the repurchase training set using gbm")
abline(a=0,b=1)


gbm_auc <-performance(prediction(gbm_prob, trainset$Target),"auc")
gbm_auc<-unlist(slot(gbm_auc, "y.values"))
gbm_auc



#############  variable importance

imp = varImp(gbm_down)
# As a table
imp
# As a plot
plot(imp, main="Important variables in the gbm model")

##### partial dependency plot #####


grid.arrange(
  partial(gbm_down, pred.var = "sched_serv_warr", plot = TRUE, rug = TRUE, 
          type="classification", prob=TRUE, parallel=F, which.class = "Yes",
          train=trainset),
  partial(gbm_down, pred.var = "mth_since_last_serv", plot = TRUE, rug = TRUE, 
          type="classification", prob=TRUE,parallel=F, which.class="Yes",train=trainset),
  partial(gbm_down, pred.var = "total_services", plot = TRUE, rug = TRUE, 
          type="classification", prob=TRUE,parallel=F, which.class="Yes",train=trainset),
  ncol = 3
)

grid.arrange(
  partial(gbm_down, pred.var = "annualised_mileage", plot = TRUE, rug = TRUE, 
          type="classification", prob=TRUE,parallel=F, which.class="Yes",train=trainset),
  partial(gbm_down, pred.var = "sched_serv_paid", plot = TRUE, rug = TRUE, 
          type="classification", prob=TRUE,parallel=F, which.class="Yes",train=trainset),
  ncol=3
)




########  down sampling###############

##   Random forest ###

levels(trainset$Target) <- c("0" ,"1")
levels(traindown$Target) <- c("0" ,"1")

forest <- train(Target~., data=traindown, method="rf", trControl=fitControl, Importance=TRUE)
print(forest)
plot(forest)

pdown <- predict(forest, trainset, type="raw")
confusionMatrix(pdown, trainset$Target, positive = "1")

plot(varImp(forest), main="Important features plot") 


#custom tunning grid
# Define the tuning grid: tuneGrid
tuneGrid <- expand.grid(.mtry=c(2,8,15))


# Fit random forest: model
forest_tune <- train(
  Target ~ .,
  tuneGrid = tuneGrid,
  data = traindown, 
  method = "rf",
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = FALSE)
)

print(forest_tune)
plot(forest_tune)

ptunedown <- predict(forest_tune, trainset, type="raw")
confusionMatrix(ptunedown, trainset$Target, positive = "1")
plot(varImp(forest_tune)) 

# ####on ROSE sample....
# forest_rose <- train(Target~., data=trainrose, method="rf", trControl=fitControl, Importance=TRUE)
# print(forest_rose)
# plot(forest_rose)
# 
# prose <- predict(forest_rose, trainset, type="raw")
# confusionMatrix(prose, trainset$Target, positive = "1")




########### working on validation set ###############
validation <-read_csv("repurchase_validation.csv")
summary(validation)

validation$model <- as.numeric(str_extract(validation$car_model,"\\d+"))
#View(validation)
testset <- validation[,-c( 2,3,4)]

names(testset)

testset$car_segment <-as.factor(testset$car_segment)
levels(testset$car_segment) <- c("Small-Medium", "Large-SUV", "LCV", "Other")



ID_column <- testset$ID
a<- c("0", "1")
testset$ID <-sample(a,1)
colnames(testset)[which(names(testset) == "ID")] <- "Target"

levels(testset$Target)<- a


#using forest model 
pred_1<- predict(forest, testset, type="prob")
pred_1

pred_2<-as.data.frame(predict(forest, testset, type="raw"))
test_rf <-cbind(ID_column, pred_1,pred_2)

test_rf <-test_rf[,-2]
column_names <- c("ID", "target_probability", "target_class")
colnames(test_rf) <-column_names

tab_temp<-table(test_rf[,3])
prop.table(tab_temp)

write.csv(test_rf, "repurchase_validation_14382497.csv")

#using forest_tune model 
pred_tune<- predict(forest_tune, testset, type="prob")
pred_tune

pred_tune2<-as.data.frame(predict(forest_tune, testset, type="raw"))
test_rf2 <-cbind(ID_column, pred_tune,pred_tune2)

test_rf2 <-test_rf2[,-2]
column_names <- c("ID", "target_probability", "target_class")
colnames(test_rf2) <-column_names
View(test_rf2)

tab_temp<-table(test_rf2[,3])
prop.table(tab_temp)



# 
# 
# ##### Using a random forest model ###########
# rfdown <- randomForest(Target~., data = traindown, mtry=8, ntrees=300, proximity=TRUE)
# print(rfdown)
# plot(rfdown)
# 
# 
# confusionMatrix(predict(rfdown, trainset), trainset$Target, positive = '1')
# 
# 
# varImpPlot(rfdown, main="Plot of importance")
# partialPlot(rfdown, as.data.frame(trainset), sched_serv_warr,"1")
# 
# 
# 
# partialPlot(rfdown,  as.data.frame(trainset), sched_serv_warr,"0")
# 
# varUsed(rfdown)
# 
# hist(treesize(rfdown), main="Number of nodes for the trees", col="green")
# varImpPlot(rfdown)
# 
# 
# 
