#library
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
data = read.csv("C:\\Users\\hamza jamil\\Downloads\\binary.csv")
head(data)
str(data)
xtabs(~admit+rank,data = data)
data$rank = as.factor(data$rank)
data$admit = as.factor(data$admit)

#Visualization
pairs.panels(data[-1])
data %>%
  ggplot(aes(x=admit,y=gpa,fill=admit))+
  geom_boxplot()+
  ggtitle('box plot')

# Density plot
data %>%
  ggplot(aes(x=gpa,fill=admit))+
  geom_density(alpha=0.8,color='black')+
  ggtitle('Density plot')

# Data Partition
ind = sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train = data[ind == 1,]
test = data[ind == 2,]

# Naive bysian Algorthm
model = naive_bayes(admit~.,data = train)# parameter  usekernel=T
model

train %>%
  filter(admit=='1') %>%
  summarise(mean(gre),sd(gre))
plot(model)

# Predict
p = predict(model,train,type='prob')
head(cbind(p,train))

# Confusion Metrix traing Data
p1 = predict(model,train)
p1
(tab = table(p1,train$admit))

1-sum(diag(tab)) / sum(tab)


# Confusion Metrix test Data
p2 = predict(model,test)
p2
tab1 = table(p1,test$admit)
tab1
1-sum(diag(tab1)) / sum(tab1)
