arr <- c(1,2,3,4,6)    #ks algoritm
arr <-sort(arr)
len <-length(arr)
d <- vector()
for(i in 1:len)
{
  d <-  c(d, abs(  i/len -  punif(arr[i],0,7)))
  
}
max(d)

# interval de incredere 99% pentru m cu dispersia cunoscuta
#2017

x <- c(1,2,3,7)
alpha <- 0.01
x_bar <- mean(x)
std <- sqrt(3)
u <- qnorm(1-alpha/2,0,1)
n <- length(x)
left <- x_bar - (std*u)/sqrt(n)
right <- x_bar + (std*u)/sqrt(n)


dbinom(8,100,1/6)
sqrt(2)/2
x <- c(1,2,3,11)
alpha <- 0.01
x_bar <- mean(x)
std <- sqrt(3)
u <- qnorm(1-alpha/2,0,1)
n <- length(x)
left <- x_bar - (std*u)/sqrt(n)
right <- x_bar + (std*u)/sqrt(n)
print(left)
print(right)

x <- c(1,2,3,11)
alpha <- 0.05
x_bar <- mean(x)
n <- length(x)
s<-var(x)
s1<-(1-x_bar)*(1-x_bar)
s2<-(2-x_bar)*(2-x_bar)
s3<-(3-x_bar)*(3-x_bar)
s4<-(11-x_bar)*(11-x_bar)
s<-sum(s1,s2,s3,s4)*(1/(n-1))
print(s)
left <- ((n-1)*s)/qchisq(n-1,alpha/2)
right <- ((n-1)*s)/qchisq(n-1,1-alpha/2)
print(left)
print(right)

c=rbind(c(1, 1,2,3,11), c(1,1,2,3,11),c(2,2,4,6,22),c(3,3,6,9,33),c(11,11,22,33,121))  
det(c)
