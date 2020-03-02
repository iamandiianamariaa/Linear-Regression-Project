d=read.table("/Users/anamaria/Downloads/F2.dat", header=T)
d
attach(d)

y
medie<-mean(y)
hist(y,col="magenta",freq=F)
t<-seq(14,32,0.001)
lines(t,dnorm(t,23.25,4),col="blue")
q1<-quantile(y,1/4)
q3<-quantile(y,3/4)
q2<-quantile(y,2/4)
mediana<-median(y)
ceva<-quantile(y,23/47)
ceva
toateq<-quantile(y,c(1/4,2/4,3/4))
toateq

boxplot(y)
#MIN(max,q3+1.5*IQR), IQR=q3-q1
#MAX(min,q1-1.5*IQR)

y1<-c(y,50,2)
boxplot(y1)
q1<-quantile(y1,1/4)
q3<-quantile(y1,3/4)
maxim<-q3+1.5*(q3-q1)


d1=read.table("/Users/anamaria/Downloads/F3.dat", header=T)
attach(d1)
d1

summary(y)
