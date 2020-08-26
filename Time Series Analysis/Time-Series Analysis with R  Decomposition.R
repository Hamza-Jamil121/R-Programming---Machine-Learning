# time series data
data("AirPassengers")
str(AirPassengers)
Ap = AirPassengers
View(Ap)
head(Ap)
ts(Ap,frequency = 12,start=c(1951,2))
attributes(Ap)
plot(Ap)
# log transformatoin
Ap= log(Ap)
plot(Ap)
# Decomposion of addditive time series
decomp = decompose(Ap)
decomp$figure
plot(decomp$figure,
     type='b',
     xlab='month',
     ylab= 'sensability index',
     col='blue',
     las=2)

plot(decomp)
