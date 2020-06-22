yandex<-scan("yandex.csv");
yandex<-scan("yandex.csv", sep="");
yandex<-data.frame(weeks=seq(1, 282), visits=yandex);
model<-lm(formula = visits ~ weeks, data = yandex);
#level<-668358233+1798283*yandex$weeks;
levels<-predict(model, newdata=data.frame(weeks=seq(1, 282)));
pdf("autocorrelation.pdf");
acf(yandex$visits-levels, lag=282);
grid(col="black");
dev.off();


