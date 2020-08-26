filereadernew=read.csv('C:\\Users\\hamza jamil\\Desktop\\All dcouemnnt\\fishdata.csv')
boxplot(filereadernew)
ln=lm(Weight~.,data = filereadernew)
summary(ln)
#0.9365,	Adjusted R-squared:  0.9313 

removeoutlier=filereadernew[filereadernew$Weight>quantile(filereadernew$Weight,0.25) - 1.5*IQR(filereadernew$Weight) & filereadernew$Weight<quantile(filereadernew$Weight,0.75)+1.5*IQR(filereadernew$Weight),]
boxplot(removeoutlier)
ml= lm(Weight~.,data =removeoutlier )
summary(ml)
#d:  0.9454,	Adjusted R-squared:  0.9408

library(faraway)
vif(ln)


creatingsample=sample(2,nrow(filereadernew),replace = TRUE,prob = c(0.7,0.3))

training1=filereadernew[creatingsample==1,] #why are we using a comma here? Are we creating samples here?
testing1=filereadernew[creatingsample==2,] #why are we using a comma here?

linearmodel=lm(Weight~.,data=training1)
summary(linearmodel) #summary show the accuracy of models and structure of model

predictor=predict(linearmodel,testing1) # why are we using testing data here? why is it not showing results?
summary()
#making a linear model with the removal of outliers:
sum(is.na(filereadernew)) 
filereadernew[duplicated(filereadernew),] #why do we put a comma here? Syntax?
removeoutlier=filereadernew[filereadernew$Weight>quantile(filereadernew$Weight,0.25) - 1.5*IQR(filereadernew$Weight) & filereadernew$Weight<quantile(filereadernew$Weight,0.75)+1.5*IQR(filereadernew$Weight),]
boxplot(removeoutlier)

creatingsample=sample(2,nrow(removeoutlier),replace = TRUE,prob = c(0.7,0.3))

training1=removeoutlier[creatingsample==1,] #why are we using a comma here? Are we creating samples here?
testing1=removeoutlier[creatingsample==2,] #why are we using a comma here?

l1=lm(Weight~.,data = training1)
summary(l1)
predict1=predict(linearmodel,testing1)
boxplot(predict1,col='red',main='Predicted data for the file',border='black')
#how do I see the accuracy of the model?