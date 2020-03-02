#Repartitia Cauchy
x<-seq(-10,10,by=0.1)
par(mfrow=c(1, 2))
plot(x, pcauchy(x, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE),type="l")        #functia de repartitie
plot(x, dcauchy(x, location = 0, scale = 1, log = FALSE),type="l")      #functia densitate de probabilitate
