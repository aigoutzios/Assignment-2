library(AICcmodavg)
setwd("C:/Users/aigou/OneDrive/Desktop/ALL the things/Msc/Modeling/Assignment/Final")
PumpingData <- read.csv(file=file.path("Data","PDFIXED.csv"))

y <- PumpingData$x
x <- PumpingData$y

plot(x,y, main="Concentrations as Pumping function")


i_max <- 15 # define highest order of the polynomial function you would like to consider

# Create empty vectors to store the selection criteria 
BIC.v <-  rep(NaN,i_max) 
AIC.v <-  rep(NaN,i_max) 
AICc.v <- rep(NaN,i_max) 

#i<-1 # Use to test the loop
for(i in 1:i_max){
  print(paste("Fitting polynomial of degree ",i,sep=""))
  model <- lm(y ~ poly(x, i, raw=TRUE)) # Notice that variable "i" denotes the degree of the polynoom.
  
  png(filename = file.path("plots",paste("degree",i,".png",sep="")))
  plot(x,y, main=paste("Degree ",i,sep = ""))
  #Display model fit on the plot 
  lines(x,model$fit,col='green',lwd=2)
  
  #Predicts 95% confidence model error bounds (P = 0.05)
  predicted.intervals <- predict(model,data.frame(x),interval='confidence',level=0.95)
  
  #Add model error bounds
  lines(x,predicted.intervals[,2],col='red',lwd=1, lty = "longdash")
  lines(x,predicted.intervals[,3],col='red',lwd=1, lty = "longdash")
  dev.off()
  
  #Model selection criteria computation
  #BIC
  BIC.v[i] <- BIC(model)
  #AIC
  AIC.v[i] <- AIC(model)
  #AICc
  AICc.v[i] <- AICc(model)
}

# Plots displying model fit criteria values
model.order <- seq(1:i_max)

plot(model.order,BIC.v,col='blue',ylab='BIC',xlab='Order',main='BIC')
lines(model.order,BIC.v,col='blue')

plot(model.order,AIC.v,col='red',ylab='AIC',xlab='Order',main='AIC')
lines(model.order,AIC.v,col='red')

plot(model.order,AICc.v,col='green',ylab='AICc',xlab='Order',main='AICc')
lines(model.order,AICc.v,col='green')



model <- lm(x ~ poly(y, 4, raw=TRUE))


new1 <- data.frame(y=seq(from = 0.15, to = 0.25, length.out = 37))
predictions <- predict.lm(model, newdata=new1)
Concetration.Predictions <- data.frame(predictions,new1)

write.csv(Concetration.Predictions,"Data\\Predictions.csv", row.names = FALSE)

fake <- predict(model)
