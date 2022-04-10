library(tidyverse)
library(lubridate)
library(ggplot2)
library(caret)
library(gridExtra)

#read the data
trans_dt <-read_csv("transactions.csv")
#data summary
View(trans_dt)
str(trans_dt)
summary(trans_dt)

###part 1: EDA
#remove duplicated rows if any
distinct_dt <- trans_dt %>% distinct()

#it shows no duplicated rows
nrow(distinct_dt)
nrow(trans_dt)



#adding 0 to date and create newdate column with format yyyy-mm-dd
trans_dt$date <-as.Date(paste("0",trans_dt$date, sep=""), format="%d/%m/%Y")
trans_dt$newdate <- trans_dt$date


max(trans_dt$monthly_amount)
min(trans_dt$monthly_amount)
summary(trans_dt$monthly_amount)


#Only use transactions which are higher than 0
trans_dt <-subset(trans_dt, monthly_amount > 0)
min(trans_dt$monthly_amount)



#number of transactions by industry and location

p1 <- ggplot(data=trans_dt)+aes(x=as.factor(location))+geom_bar()+labs( x="Location", y="No. of transactions", title="Nnumber of transactions in each location")
p2 <-ggplot(data=trans_dt)+aes(x=as.factor(industry))+geom_bar()+labs( x="Industry", y="No. of transactions", title="Number of transactions in each industry")
grid.arrange(p1,p2, ncol=2)



######## group by industry (with sum monthly ammount  and mean monthly amount in  4 years of data)
industry <- trans_dt %>% group_by(industry) %>%summarise_at(vars(monthly_amount), funs(sum_monthly=sum, mean_monthly=mean))


######## group by location (with sum monthly ammount  and mean monthly amount in  4 years of data)
location <- trans_dt %>% group_by(location) %>%summarise_at(vars(monthly_amount), funs(sum_monthly=sum, mean_monthly=mean))


View(industry)
View(location)



### group by location and date
location_dt <- trans_dt %>% group_by(newdate,location) %>%summarise_at(vars(monthly_amount), funs(sum_monthly=sum, mean_monthly=mean))


#### Some EDA
ggplot(data=location_dt) +aes(x=newdate, y=round(sum_monthly/10^6, digits=4), colour=as.factor(location)) +geom_line()+labs( x="Date", y="Monthly total amount in million", title="Total monthly amount (in million) in each location")+scale_color_discrete(name="Location")



ggplot(data=location_dt) +aes(x=newdate, y=round(sum_monthly/10^6, digits=4), colour=as.factor(location)) +geom_line()+labs( x="Date", y="Monthly total amount in million",title="Total monthly amount (in million) by location")+scale_color_discrete(name="Location")+facet_wrap(~as.factor(location))


ggplot(data=location_dt) +aes(x=newdate, y=mean_monthly, colour=as.factor(location)) +geom_line()+labs( x="Date", y="Monthly mean amount", title="Mean monthly amount in each location")+scale_color_discrete(name="Location")+facet_wrap(~as.factor(location))




###    Group by date and industry   ##################
industry_dt <- trans_dt %>% group_by(newdate,industry) %>%summarise_at(vars(monthly_amount), funs(sum_monthly=sum, mean_monthly=mean))


ggplot(data=industry_dt) +aes(x=newdate, y=round(sum_monthly/10^6, digits=4), colour=as.factor(industry)) +geom_line()+labs( x="Date", y="Total monthly amount in million", title="Total monthly amount (in million) in each industry")+scale_color_discrete(name="Industry")

ggplot(data=industry_dt) +aes(x=newdate, y=round(sum_monthly/10^6, digits=4), colour=as.factor(industry)) +geom_line()+labs( x="Date", y="Monthly total amount in million", title="Monthly total amount in million by industry")+scale_color_discrete(name="industry")+facet_wrap(~as.factor(industry))


ggplot(data=industry_dt) +aes(x=newdate, y=mean_monthly, colour=as.factor(industry)) +geom_line()+labs( x="Date", y="Monthly mean amount", title="Mean monthly amount in million in each industry")+scale_color_discrete(name="Industry")+facet_wrap(~as.factor(industry))

ggplot(data=industry_dt) +aes(x=newdate, y=mean_monthly, colour=as.factor(industry)) +geom_line()+labs( x="Date", y="Monthly mean amount", title="Mean monthly amount in million in each industry")+scale_color_discrete(name="Industry")





#*****************************************************************************
######## Part 2: Linear regression#########################
#create an aggregated dataset using the fileds dates, industry and location with a mean of monthly_amount
agg_dt<- trans_dt %>% group_by(newdate, industry,location) %>%summarise(mean_monthly_amount=mean(monthly_amount))


#adding month and year columns to the data
agg_dt <- agg_dt %>%  mutate(year=year(newdate)) 
agg_dt$month <-month(agg_dt$newdate)


# adding month number column to the data, the base month is 2013-01-01 (the first date in the data)
agg_dt <- agg_dt %>%
  mutate(month_number = (interval(ymd("2013-01-01"), 
                                       ymd(newdate))) %/% months(1)+1)
#View(agg_dt)

#mean_dt$month <- months(mean_dt$newdate)




#filter for location 1 and industry 1
agg_11 <- agg_dt %>%  filter(industry==1 & location==1)



#plot mean month =ly amount for location1 and industry 1 and fit a linear line in the 
 ggplot(agg_11,  aes(x = newdate, y = mean_monthly_amount))+
   geom_point(color = "blue") +
  geom_line()  + 
   geom_smooth(method = lm, se = FALSE, colour="red")+
   scale_x_date(date_labels = "%b-%Y") +
   labs( x="Month-Year", y="Average monthly amount", title="Mean monthly amount- industry 1 & location 1 with the fitted linear regression line")




#splitting data into train set the first ceiling(0.75*47) months and test set is the rest.
trainset_size <- ceiling(0.75 * nrow(agg_11))

trainset <- agg_11[1:trainset_size,]
testset<-anti_join(agg_11, trainset, by = 'newdate')

#double check if number of rows in train+set=total
nrow(trainset)
nrow(testset)
nrow(agg_11)


#linear model: mean_monthly vs date
lm_11 <- lm(mean_monthly_amount~newdate, data=trainset)
summary(lm_11)
coef(lm_11)
AIC(lm_11)

#checking the goodness of fit of this model on the train set
plot(lm_11)



#calculate RMSE, and R2, MAE on the train set 
predict_train <-predict(lm_11, trainset)
trainRMSE <- RMSE(predict_train,trainset$mean_monthly_amount)
trainR2 <- R2(predict_train,trainset$mean_monthly_amount)
trainMAE <- MAE(predict_train,trainset$mean_monthly_amount)


#prediction on the test set
predict <-predict(lm_11, testset)
testset$prediction<-predict


#calculate R2, RMSE on the test set
testRMSE <- RMSE(predict,testset$mean_monthly_amount)
testR2 <- R2(predict,testset$mean_monthly_amount)
testMAE <- MAE(predict,testset$mean_monthly_amount)



#plot actual data and prediction on the test set. 
ggplot() +
  geom_point(data = agg_11, aes(x = newdate, y = mean_monthly_amount), color = "blue") + geom_line(data = agg_11, aes(x = newdate, y = mean_monthly_amount), color = "blue") +
  geom_point(data = testset,aes(x = newdate, y = prediction), color = "red")+scale_x_date(date_labels = "%b-%Y")




#create a data frame for December using the same column names as the aggregated data
col_names <- c(names(agg_11))
col_names
dec_row <- c("2016-12-01",1,1,0,2016, 12,48)
Dec_dt <- data.frame(matrix(nrow = 1,data = dec_row))
colnames(Dec_dt) <- col_names
Dec_dt$newdate <-as.Date(Dec_dt$newdate)

#Prediction for Dec 2016
predict_Dec <-predict(lm_11, Dec_dt)
Dec_dt$prediction <-predict_Dec
Dec_16 <-Dec_dt$prediction


#plot actual data and prediction for December on the same data.
ggplot() +
  geom_point(data = agg_11, aes(x = newdate, y = mean_monthly_amount), color = "blue") +
  geom_line(data = agg_11, aes(x = newdate, y = mean_monthly_amount), color = "green") +
  geom_point(data = Dec_dt,aes(x = newdate, y = prediction), color = "red")+
  geom_line(aes(x=as.Date(c("2016-11-01","2016-12-01")),y=c(agg_11$mean_monthly_amount[47],Dec_dt$prediction[1])))+
  scale_x_date(date_labels = "%b-%Y")+labs( x="Month-Year", y="mean monthly amount", title="Monthly average transaction & prediction for Dec 2016-industry 1 & location 1")



#########################################################################################
#adding month and year in the model
#new model
newlm_11 <- lm(mean_monthly_amount~., data=trainset)
summary(newlm_11)

newlm_11 <- lm(mean_monthly_amount~newdate+month+year, data=trainset)
summary(newlm_11)
AIC(newlm_11)

#calculate R2,RMSE on the train set
predict_train <-predict(newlm_11, trainset)
newtrainRMSE <-RMSE(predict_train,trainset$mean_monthly_amount)
newtrainR2 <- R2(predict_train,trainset$mean_monthly_amount)


#prediction on the test set
newpredict <-predict(newlm_11, testset)
testset$newprediction<-newpredict

#calculate R2, RMSE on the test set
newtestRMSE <- RMSE(newpredict,testset$mean_monthly_amount)
newtestR2 <-R2(newpredict,testset$mean_monthly_amount)


#plot actual data and prediction on the test set. 
ggplot() +
  geom_point(data = agg_11, aes(x = newdate, y = mean_monthly_amount), color = "blue") + geom_line(data = agg_11, aes(x = newdate, y = mean_monthly_amount), color = "blue") +
  geom_point(data = testset,aes(x = newdate, y = newprediction), color = "red")+scale_x_date(date_labels = "%b-%Y")





#make sure some variables in the new model as the same type as the one in the original data frame
Dec_dt$year <-as.numeric(Dec_dt$year)
Dec_dt$month <-as.numeric(Dec_dt$month)





#Prediction for Dec 2016
newpredict_Dec <-predict(newlm_11, Dec_dt)
Dec_dt$newprediction <-newpredict_Dec
newDec_16 <- Dec_dt$newprediction



#plot actual data and prediction for December on the same data.
ggplot() +
  geom_point(data = agg_11, aes(x = newdate, y = mean_monthly_amount), color = "blue") +
  geom_line(data = agg_11, aes(x = newdate, y = mean_monthly_amount), color = "green") +
  geom_point(data = Dec_dt,aes(x = newdate, y = newprediction), color = "red")+
  geom_line(aes(x=as.Date(c("2016-11-01","2016-12-01")),y=c(agg_11$mean_monthly_amount[47],Dec_dt$newprediction[1])))+
  scale_x_date(date_labels = "%b-%Y")+labs( x="Month-Year", y="mean monthly amount", title="Monthly average transaction & prediction for Dec 2016-industry 1 & location 1")


#compare two models
options(scipen=999)
c(trainRMSE, trainR2, testRMSE, testR2, Dec_16)
c(newtrainRMSE, newtrainR2, newtestRMSE, newtestR2, newDec_16)


#***************************************************************************
### part 3 advanced model fitting    ##############################



newagg<- trans_dt %>% group_by(newdate, industry,location) %>%summarise(mean_monthly_amount=mean(monthly_amount))

ind_list <- sort(unique(trans_dt$industry))
loc_list <- sort(unique(trans_dt$location))

#just checking the number of rows when filter by industry and location
# sometimes we get the empty data frame

number_rows<-function(industry_list, location_list){
  t <- 0 
  for (ii in industry_list) {
    for (jj in location_list) {
      newdata <- newagg %>% filter(industry==ii & location==jj )
      
      print(nrow(newdata))
      
      if (nrow(newdata)>36) {
        t = t+1
      }
      else {
        t=t
      }
      
    }
  }
  return (t)
}


row_numbers_all <- number_rows(ind_list, loc_list)
row_numbers_all


# the cut off for number of rows is 36


auto_prediction<- function(industry_list, location_list){
  out_dt <-data.frame()
  for (ii in industry_list) {
    for (jj in location_list) {
      newdata <- newagg %>% filter(industry==ii & location==jj )
      
      if(nrow(newdata)>36) {
      
      #split to train and set 
      n <- nrow(newdata)
      train_size <- ceiling(0.75 * n)
      train <-newdata[1:train_size,]
      test<-anti_join(newdata, train, by = 'newdate')
      
      #double check number of rows in train and test
      nrow(train)
      nrow(test)
      n
      
      #model
      auto_model <-lm(mean_monthly_amount~newdate, data=train)
      summary(auto_model)
      
      #getting coefficients for the model
      beta_0 <- coef(auto_model)[[1]]
      beta_1 <-coef(auto_model)[[2]]
      
      
      
      
      #calculate R2,RMSE on the train set
      pred_train <-predict(auto_model, train)
      train_RMSE <-RMSE(pred_train,train$mean_monthly_amount)
      train_R2 <- R2(pred_train,train$mean_monthly_amount)
      
      
      #prediction on the test set
      pred_test <-predict(auto_model, test)
      test$prediction<-pred_test
      
      #calculate R2, RMSE on the test set
      test_RMSE <- RMSE(pred_test,test$mean_monthly_amount)
      test_R2 <-R2(pred_test,test$mean_monthly_amount)
      
      #create Dec 2016 data frame
      Dec16_dt <-data.frame(newdate="2016-12-01", industry=ii, location=jj, mean_monthly_amount=0)
      Dec16_dt$newdate <-as.Date(Dec16_dt$newdate)
      
      #prediction for December 16
      pred_Dec16 <-predict(auto_model, Dec16_dt)
      Dec16_prediction <- pred_Dec16[[1]]
      
      temp_out <-c (ii, jj,beta_0, beta_1,  train_RMSE, train_R2, test_RMSE, test_R2, Dec16_prediction)
      }
      else {
        temp_out <-c (ii, jj,NA, NA,  NA, NA, NA, NA, NA)
      }
      out_dt <-rbind(out_dt, temp_out)
  }
  }   
    out_colnames <- c("industry", "location" ,"intercept", "coeff.date", "train.RMSE", "train.R2","test.RMSE","test.R2", "Dec16.prediction" )  
    colnames(out_dt)<-out_colnames
    return(out_dt)
}


ind_list <- sort(unique(trans_dt$industry))
loc_list <- sort(unique(trans_dt$location))

prediction_all <- auto_prediction(ind_list, loc_list)


ggplot(data=prediction_all)+aes(x=industry, y=location, fill=test.RMSE)+geom_tile()+scale_x_continuous(breaks = c(1:10))+scale_y_continuous(breaks = c(1:10))+scale_fill_gradient(low = "grey", high = "red", na.value = NA)+labs(title="RMSE of the test set")



ggplot(data=prediction_all)+aes(x=industry, y=location, fill=test.R2)+geom_tile()+geom_tile()+scale_x_continuous(breaks = c(1:10))+scale_y_continuous(breaks = c(1:10))+scale_fill_gradient(low = "grey", high = "red", na.value = NA)+labs(title="R2 of the test set")

pred_arrange <- arrange(prediction_all, desc(test.RMSE))
View(pred_arrange)

agg_61 <- agg_dt %>%  filter(industry==6 & location==1)
Dec16_61 <- pred_arrange$Dec16.prediction[1]
Dec16_61



ggplot() +
  geom_point(data = agg_61, aes(x = newdate, y = mean_monthly_amount), color = "blue") +
  geom_line(data = agg_61, aes(x = newdate, y = mean_monthly_amount), color = "green") +
  geom_point(aes(x = as.Date("2016-12-01"), y = Dec16_61), color = "red")+
  geom_line(aes(x=as.Date(c("2016-11-01","2016-12-01")),y=c(agg_61$mean_monthly_amount[nrow(agg_61)],Dec16_61)))+
  scale_x_date(date_labels = "%b-%Y")+labs( x="Month-Year", y="mean monthly amount", title="Monthly average transaction & prediction for Dec 2016-industry 6 & location 1")


#plot mean month =ly amount for location1 and industry 1 and fit a linear line in the 
agg_810 <- agg_dt %>%  filter(industry==8 & location==10)

ggplot(agg_810,  aes(x = newdate, y = mean_monthly_amount))+
  geom_point(color = "blue") +
  geom_line()  + 
  geom_smooth(method = lm, se = FALSE, colour="red")+
  scale_x_date(date_labels = "%b-%Y") +
  labs( x="Month-Year", y="Average monthly amount", title="Monthly average transaction-industry 10 & location 8 with the fitted linear regression line")

data_810 <-trans_dt %>% filter(industry==8 &location==10)
View(data_810)




#using spline
library(splines)
agg_11$splines <- ns(agg_11$month_number,knots=c(13,25))
model <- lm(mean_monthly_amount~splines,agg_11)
lines(agg_11$month_number,predict(model))


summary(model)




#time series approach
library(forecast)
ts_11 <-ts(agg_11$mean_monthly_amount,start = c(2013,1), end = c(2016,11), frequency = 12)
autoplot(ts_11)
training = window(ts_11, end = c(2015,12))
test = window(ts_11, start = c(2016,1))
ts_11
plot(decompose(ts_11))
acf(diff(ts_11,12))
pacf(diff(ts_11,12))


autoplot(diff(ts_11,12))
