getwd()
binary = read.csv('C:\\Users\\hamza jamil\\Downloads\\binary.csv')
str(binary)

# multi-nomial logistic  model
library(nnet)
mymodel = multinom(admit~.,data = binary)

#Misclassification rate
p = predict(mymodel,binary)
tab =table(p,binary$admit)
tab
sum(diag(tab)) / sum(tab) # accuracy
1-sum(diag(tab)) / sum(tab) # error ratee

table(binary$admit)

# Model perfomance Evaluation
library(ROCR)
pred = predict(mymodel,binary,type = 'prob')
pred
pred = prediction(pred,binary$admit)
eval = performance(pred,'acc')
plot(eval)
abline(h=0.705,v=0.45)

# Identify best line
max =  which.max(slot(eval,"y.values")[[1]])
max
acc = slot(eval,"y.values")[[1]][max]
acc
cut = slot(eval,"x.values")[[1]][max]
print(c(Accuracy=acc,Cutoff=cut))


# Receving operating characteristic  ROC Curve
roc = performance(pred,"tpr","fpr")
plot(roc,colorize=T,main='roc curve',ylab = 'sensitiviy',xlab='specifcy')
abline(a=0,b=1)




# Identify best line
max =  which.max(slot(eval,"y.values")[[1]])
max
acc = slot(eval,"y.values")[[1]][max]
acc
cut = slot(eval,"x.values")[[1]][max]
print(c(Accuracy=acc,Cutoff=cut))


# Receving operating characteristic  ROC Curve
roc = performance(pred,"tpr","fpr")
plot(roc,colorize=T,main='roc curve',ylab = 'sensitiviy',xlab='specifcy')
abline(a=0,b=1)
#Area under curve

auc = performance(pred,'auc')
auc = unlist(slot(auc,"y.values"))
auc = round(auc,4)
legend(.6,.2,auc,title = "AUC",cex=1)


