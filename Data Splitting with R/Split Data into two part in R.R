veh = read.csv('C:\\Users\\hamza jamil\\Downloads\\vehicle.csv',header = T)
veh
head(veh)
#this is way to convert numeric value into factor
read.csv('C:\\Users\\hamza jamil\\Downloads\\vehicle.csv',header = T,colClasses ='factor')
#here we split data into trainiing and validation/test
ind = sample(2,nrow(veh),replace = TRUE,prob = c(0.8,0.2))
tdata=veh[ind==1,]
vdata=veh[ind==2,]
head(tdata)
head(vdata)

# here we create multiple linear regression
result = lm(lc~Mileage+lh,tdata)
result
summary(result)
result$coefficients
coef(result)

# predicton 

pred = predict(result,vdata)
pred
head(pred)
tab = table(pred,vdata$lc)
tab
1-sum(diag(tab))/sum(tab)
result$