# Handling Class Imbalance Problem in R: Improving Predictive Model Performance

data=read.csv('C:\\Users\\hamza jamil\\Downloads\\binary.csv')
str(data)
#here we convert over class column into factor
data$admit = as.factor(data$admit)
summary(data)
str(data$admit)
#here we check class Imbalancing
prop.table(table(data$admit))
#Class Imbalancing Plot
barplot(prop.table(table(data$admit)),col = rainbow(2),ylim = c(0,0.7),main = 'class DISTR')
set.seed(123)
ind=sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
train=data[ind==1,]
test=data[ind==2,]

#data for developing predictive model
table(train$admit) #here we count how many 0 and 1 belong with train
#0   1 
#189  87  for 0 189 *2 for oversampling
prop.table(table(train$admit)) #here we find average of 0 and 1 in train
summary(train)
#here we create random forest model
mod=randomForest(admit ~ .,data = train)
mod
#model evalution to the help of test data
ev=predict(mod,test)
ev
library(caret)
library(e1071)
confusionMatrix(ev,test$admit)

confusionMatrix(predict(mod,test),test$admit,positive = '1')
#library(ROSE)
# here we work on oversampling better  senstivity
over=ovun.sample(admit~.,data = train,method ='over',N=376)$data

table(over$admit)
summary(over)
#now here we create over model to the help of over
ranover=randomForest(admit~.,data = over)
ranover
confusionMatrix(predict(ranover,test),test$admit,positive = '1')
# here we work on underampling better  senstivity
# 0   1 
#188  97 
under=ovun.sample(admit~.,data = train,method ='under',N=194)$data
under
table(under$admit)
ranunder=randomForest(admit~.,data = under)
ranunder

confusionMatrix(predict(ranunder,test),test$admit,positive = '1')
#both sampling method
both=ovun.sample(admit~.,data = train,method ='both',p=0.5,seed=222,N=285)$data
table(both$admit)
ranboth=randomForest(admit~.,data = both)
ranboth

confusionMatrix(predict(ranboth,test),test$admit,positive = '1')

#here we use ROSE method
rose=ROSE(admit~.,data = train,N=500,seed=111)$data
table(rose$admit)
summary(rose)
ranrose=randomForest(admit~.,data = rose)
ranrose
confusionMatrix(predict(ranrose,test),test$admit,positive = '1')
