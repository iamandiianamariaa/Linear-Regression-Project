data("quakes")
attach(quakes)
head(quakes)

#impartim in training data si test data
set.seed(100) 
trainingRowIndex <- sample(1:nrow(quakes), 0.8*nrow(quakes))  # indicele liniilor pentru training data
trainingData <- quakes[trainingRowIndex, ]  #training data
testData  <- quakes[-trainingRowIndex, ]   #test data


#density plot pentru training data
plot(density(trainingData$mag), main="Density Plot: Magnitude", ylab="Frequency")
polygon(density(trainingData$mag), col="red")
plot(density(trainingData$stations), main="Density Plot: Stations", ylab="Frequency") 
polygon(density(trainingData$stations), col="blue")

cor(trainingData$mag, trainingData$stations)
lmMod <- lm(stations ~ mag, data=trainingData)  #construim modelul pt regresia simpla
                                                #pe training data
print(lmMod)  #prezicem nr de statii care inregistreaza cutremurul
                            #in functie de magnitudinea lui

plot(lmMod)

plot(trainingData$mag, trainingData$stations,     #plot pentru training data
     ylab = "# of Stations Reporting",
     xlab = "Magnitude",
     main = "Fiji Earthquakes")

abline(-179.28, 46.03, col="red",lwd=2)    #regression line
summary(lmMod)
Pred <- predict(lmMod, testData)  # prezicem nr de statii
AIC(lmMod)
BIC(lmMod)

actuals_preds <- data.frame(cbind(actuals=testData$stations, predicteds=Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)    #prediction accuracy
print(correlation_accuracy)
head(actuals_preds)

lmMultip <- lm(stations ~ mag + depth,data=trainingData)    #construim modelul pentru regresia multipla
print(lmMultip)       #prezicem nr de statii care inregistreaza cutremurul 
summary(lmMultip)     #dupa magnitudine si adancimea cutremurului
PredMultip <- predict(lmMultip, testData)  # prezicem nr de statii
AIC(lmMultip)         #pentru regresia multipla
BIC(lmMultip)

actuals_preds_multip <- data.frame(cbind(actuals=testData$stations, predicteds=PredMultip))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds_multip)    #prediction accuracy
print(correlation_accuracy)
head(actuals_preds_multip)

plot(lmMultip)


anova(lmMod,lmMultip)


#generam o variabila aleatoare noua a.i. sa exista o relatie de dependenta directa
# unde X v.a descrie distanta pe care simtit cutremurul
# distanta >> => numar de statii care il inregistreaza creste
# cum magnitudinea si nr de statii sunt intr-o relatie liniara
# multiplicam magnitudinea cu o constanta pozitiva a.i
# rezulta o distanta in relatie liniara pozitiva cu numarul de statii
x <- vector()
diff <- 10
for( i in 1:length(mag)){
    
  if(i%% 2 == 0)
      x <- c(x,( mag[i])*100 + diff)
  else{
    x <- c(x, (mag[i])*100 - diff)
  }
  
  
}
plot(stations ~x)
quakes["distance"] <- x
newModel <- lm( stations ~ x)
abline(-1.683e+02,4.356e-01, col="red",lwd=2)
summary(newModel)
