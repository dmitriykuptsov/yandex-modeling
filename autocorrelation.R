yandex<-scan("yandex.csv", sep="");
yandex<-data.frame(weeks=seq(1, 282), visits=yandex);
model<-lm(formula = visits ~ weeks, data = yandex);
#level<-668358233+1798283*yandex$weeks;
levels<-predict(model, newdata=data.frame(weeks=seq(1, 282)));
pdf("autocorrelation.pdf");
# Just as correlation measures the extent of a linear relationship 
# between two variables, autocorrelation measures the linear relationship 
# between lagged values of a time series.
acf(yandex$visits-levels, lag.max=282, main="Autocorrelation", xlab="Lag", ylab="ACF");
grid(col="black");
dev.off();


