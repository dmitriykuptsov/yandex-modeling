library(forecast)
library(fma)

# Load the data
yandex<-scan("yandex.csv");
google<-scan("google.csv");

# Output file
pdf("prediction.pdf", height=10, width=12);

# Split screen into two rows and one column
par(mfrow=c(2, 1))

# Plot original data
plot(seq(1, 282, 1), yandex/1000000, col="dark red", type="l", main="Real data vs Predictions (Yandex)", xlab="Week", ylab="Number of requests (millions)", lwd=6, xlim=c(1, 334), ylim=c(300, 2000));

# Build Holt-Winters model
hw<-HoltWinters(ts(yandex/1000000, frequency=52));

# Put data into data frame
yandex<-data.frame("week" = seq(1, 282, 1), "visits" = yandex/1000000);

# Fit linear regression model
fit<-tslm(visits~week, ts(yandex));

# Convert data into time series object
yandex<-ts(yandex/1000000, frequency=52);

# Draw linear regression model
abline(fit, col="dark blue", lwd=6);

# Print model parameters
print(fit);

# Predict using Holt-Winters model
p <- predict(hw, 52, prediction.interval = FALSE);

# Plot predicted data
points(seq(283, 334), p, col="light blue", lwd=6, type="l")

# Plot linear regression
abline(fit, col="dark blue", lwd=6);

# Add grid to the plot
grid(col="black");

# Add legend
legend("topleft", c("Real data (Yandex)", "Linear regression", "Holt-Winters prediction"), col=c("dark red", "dark blue", "light blue"), bty="n", lwd=6);

plot(seq(1, 282, 1), google/1000000, col="dark red", type="l", main="Real data vs Predictions (Google)", xlab="Week", ylab="Number of requests (millions)", lwd=6, xlim=c(1, 334), ylim=c(300, 1400));
hw<-HoltWinters(ts(google/1000000, frequency=52));
google<-data.frame("week" = seq(1, 282, 1), "visits" = google/1000000);
fit<-tslm(visits~week, ts(google));
google<-ts(google/1000000, frequency=52);
abline(fit, col="dark blue", lwd=6);
print(fit);
p <- predict(hw, 52, prediction.interval = FALSE);
points(seq(283, 334), p, col="light blue", lwd=6, type="l")
grid(col="black");
legend("topleft", c("Real data (Google)", "Linear regression", "Holt-Winters prediction"), col=c("dark red", "dark blue", "light blue"), bty="n", lwd=6);

# Write plots to the file
dev.off();
