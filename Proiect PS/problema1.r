data("quakes")
attach(quakes)
head(quakes)
library(e1071)
#Medie pentru variabilele latitudine, longitudine, adancime si nr de statii care au inregistrat cutremurul
summary(quakes)
mean(lat)
mean(long)
mean(depth)
mean(mag)
mean(stations)

#Varianta pentru variabile
var(lat)
var(long)
var(depth)
var(mag)
var(stations)


#boxplot
boxplot(lat)
boxplot(long)
boxplot(depth)
boxplot(mag,sub=paste("Outlier rows: ", boxplot.stats(quakes$mag)$out))
plot(density(mag),sub=paste("Skewness:", round(e1071::skewness(quakes$mag), 2)))
boxplot(stations)
cor(stations,mag)


summary(lat)
summary(long)
summary(depth)
summary(mag)
summary(stations)
