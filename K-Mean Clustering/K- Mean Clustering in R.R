att
data("iris")
uti =iris
View(uti)
#plot
plot(Sepal.Length~Petal.Length,uti)
#here we ddrop 5th column
#Normalization
z=uti[,-c(5,5)]
#mean
m=apply(z,2,mean)
#standard deviation
s=apply(z,2,sd)
z=scale(z,m,s)
z

#now euclidean distance
dit = dist(z)
dit
print(dit,digits = 3)

# Hirerical Clustering using Dendrogram with complete linkage
hc.c=hclust(dit)
hc.c
plot(hc.c)
#Hirerical Clustering using Dendrogram with average linkage
hc.a =hclust(dit,method = 'average')
hc.a
plot(hc.a,hang = -1)

# Clustering memebership
member.c = cutree(hc.c,3)
member.a = cutree(hc.a,3)
table(member.c,member.a)
# cluster mean
aggregate(z,list(member.c),mean)
aggregate(uti[,-c(5,5)],list(member.c),mean)

# Silthout plot
library(cluster)
plot(silhouette(cutree(hc.c,3),dit))

# Scree plot
wss = (nrow(z)-1)*sum(apply(z,2,var))
wss
for(i in 2:20) wss[i] = sum(kmeans(z,centers = i)$withinss)
plot(1:20,wss,type = 'b',xlab = 'number of cluster',ylab = 'wihtiin group ss')

# K-mean clustering non-herical
kc = kmeans(z,3)
kc
kc$cluster
kc$centers
plot(Sepal.Width~ Petal.Width,uti,col = kc$cluster)
