## Forecasting Monthly Retail Trade and Food Services Data in the United States

## use forecast library
library(forecast)
library(zoo)

## Set working directory for locating files.
setwd("/Users/sindhujaarivukkarasu/Documents/BAN 673 Time Series Analytics/Project")

## create data frame 
Retail.data <- read.csv("Retail.csv")

# See the first 6 records of the file.
head(Retail.data)
tail(Retail.data)

## Time series data set
retail.ts <- ts(Retail.data$Value, start = c(1992, 1), end = c(2023, 12), freq = 12)
retail.ts

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder (level and noise component).
retail.stl <- stl(retail.ts, s.window = "periodic")
autoplot(retail.stl, main = "Retail Trade and Food Services - Time Series Components")

# Set option to prevent scientific notation in plot
options(scipen = 10)
## Use plot() to plot time series data  
plot(retail.ts, 
     xlab = "Time", ylab = "Monthly Sales", 
     ylim = c(162000, 703000), xaxt = 'n',
     main = "Retail Trade and Food Services", col = "blue")
# Establish x-axis scale interval for time in months.
axis(1, at = seq(1992, 2024, 1), labels = format(seq(1992, 2024, 1)))

## Acf() function to identify autocorrelation and plot autocorrelation for different lags.
autocor <- Acf(retail.ts, lag.max = 12, main = "Autocorrelation for Retail Trade and Food Services Data")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

## Data Partition in the training and validation sets: nTrain and nValid
# Total number of period length(ridership.ts) = 384.
# nvalid = 84 months (7 years), from January 2017 to December 2023
# nTrain = 300 months (25 years), from January 1992 to December 2016.
length(retail.ts) 
nValid <- 84
nTrain <- length(retail.ts) - nValid
nTrain
train.ts <- window(retail.ts, start = c(1992, 1), end = c(1992, nTrain))
valid.ts <- window(retail.ts, start = c(1992, nTrain + 1), 
                   end = c(1992, nTrain + nValid))
length(train.ts) 
length(valid.ts) 

# Plot the time series data and visualize partitions. 
plot(train.ts, 
     xlab = "Time", ylab = "Sales (in 000s)", ylim = c(162000, 730000), 
     bty = "l", xlim = c(1992, 2025.25), xaxt = 'n', main = "", lwd = 2, col = "blue") 
axis(1, at = seq(1992, 2025, 1), labels = format(seq(1992, 2025, 1)))
lines(valid.ts, col = "blue", lty = 1, lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 720000))
lines(c(2024, 2024), c(0, 720000))
text(2008, 700000, "Training", pos = 3)  # Adjusted y-coordinate and pos
text(2020, 700000, "Validation", pos = 3)  # Adjusted y-coordinate and pos
text(2025.5, 700000, "Future", pos = 3)  # Adjusted y-coordinate and pos
arrows(1992, 700000, 2015.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.1, 700000, 2023.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 700000, 2027.3, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

###############################################################################################

## Model 1: REGRESSION MODELS

## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY:
# Use tslm() function to create linear trend and seasonal model.
train.lin.trend.season <- tslm(train.ts ~ trend  + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.trend.season)

# Apply forecast() function to make predictions for ts with linear trend and seasonality data in validation set.  
train.lin.trend.season.pred <- forecast(train.lin.trend.season, h = nValid, level = 0)
train.lin.trend.season.pred

## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY:
# Use tslm() function to create quadratic trend and seasonal model.
train.quad.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.trend.season)

# Apply forecast() function to make predictions for ts with quadratic trend and seasonality data in validation set.  
train.quad.trend.season.pred <- forecast(train.quad.trend.season, h = nValid, level = 0)
train.quad.trend.season.pred

## Performance measures of these 2 forecasts on validation data
# Use accuracy() function to identify common accuracy measures
round(accuracy(train.lin.trend.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.trend.season.pred$mean, valid.ts),3)

## Time series forecast for the regression models on the entire data set

## 1.LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonal model.
lin.trend.season <- tslm(retail.ts ~ trend + season)

# See summary of linear trend and seasonality equation and associated parameters.
summary(lin.trend.season)

# Apply forecast() function to make predictions for ts with trend and seasonality data in the future 12 months.  
lin.trend.season.pred <- forecast(lin.trend.season, h = 12, level = 0)
lin.trend.season.pred

# Plot ts data, regression model with linear trend and seasonality data, 
# and predictions for future 12 periods.
plot(lin.trend.season.pred$mean, 
     xlab = "Time", ylab = "Retail Sales", 
     ylim = c(162000, 730000), bty = "l",
     xlim = c(1992, 2025.25), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality and Forecast for Future Periods", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1992, 2025.25,1), labels = format(seq(1992, 2025.25,1)))
lines(lin.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(retail.ts)
legend(1992,700000, legend = c("Retail Sales", 
                             "Linear Trend and Seasonality Model for Entire Data",
                             "Linear and Seasonality Forecast for Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(0, 730000))
text(2005, 700000, "Data Set", pos = 3)
text(2025.2, 700000, "Future", pos = 3)
arrows(1992, 700000, 2023.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 700000, 2026.3, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## 2.QUADRATIC TREND AND SEASONALITY MODEL
# Use tslm() function to create quadratic trend and seasonality model.
quad.trend.season <- tslm(retail.ts ~ trend + I(trend^2)+ season)

# See summary of quadratic trend and seasonality equation and associated parameters.
summary(quad.trend.season)

# Apply forecast() function to make predictions for ts with trend and seasonality data in the future 12 months.  
quad.trend.season.pred <- forecast(quad.trend.season, h = 12, level = 0)
quad.trend.season.pred

# Plot ts data, regression model with linear trend and seasonality data, 
# and predictions for future 12 periods.
plot(quad.trend.season.pred$mean, 
     xlab = "Time", ylab = "Retail Sales", 
     ylim = c(162000, 730000), bty = "l",
     xlim = c(1992, 2025.25), xaxt = "n",
     main = "Regression Model with Quadratic Trend and Seasonality and Forecast for Future Periods", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(1992, 2025.25,1), labels = format(seq(1992, 2025.25,1)))
lines(quad.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(retail.ts)
legend(1992,700000, legend = c("Retail Sales", 
                               "Quadratic Trend and Seasonality Model for Entire Data",
                               "Quadratic and Seasonality Forecast for Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(0, 730000))
text(2005, 700000, "Data Set", pos = 3)
text(2025.2, 700000, "Future", pos = 3)
arrows(1992, 700000, 2023.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 700000, 2026.3, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## Performance measure of the forecasts on entire data
round(accuracy(lin.trend.season.pred$fitted, retail.ts),3)
round(accuracy(quad.trend.season.pred$fitted, retail.ts),3)

#########################################################################################################
## Model 2: Two-level Forecating

## Create trailing moving average with window widths of k = 4, 6, and 12.
# In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_6 <- rollmean(train.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

## Create forecast for the validation data for the window widths of k = 4, 6, and 12. 
ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_4.pred

ma.trail_6.pred <- forecast(ma.trailing_6, h = nValid, level = 0)
ma.trail_6.pred

ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred

## Use accuracy() function to identify common accuracy measures.
round(accuracy(ma.trail_4.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_6.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)

## GENERATE PLOT FOR PARTITION DATA AND TRAILING MA.

# Plot original data and forecast for training and validation partitions
# using trailing MA with window widths of k = 4 and k = 12.
plot(retail.ts, 
     xlab = "Time", ylab = "Sales (in 000s)", 
     ylim = c(162000, 730000), bty = "l", xaxt = "n",
     xlim = c(1992, 2025.25), main = "Trailing Moving Average") 
axis(1, at = seq(1992, 2025, 1), labels = format(seq(1992, 2025, 1)) )
lines(ma.trailing_4, col = "brown", lwd = 2, lty = 1)
lines(ma.trail_4.pred$mean, col = "brown", lwd = 2, lty = 2)
lines(ma.trailing_12, col = "blue", lwd = 2, lty = 1)
lines(ma.trail_12.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1992,700000, legend = c("Sales Data", 
                               "Trailing MA, k=4, Training Partition", 
                               "Trailing MA, k=4, Validation Partition",
                              "Trailing MA, k=12, Training Partition", 
                              "Trailing MA, k=12, Validation Partition"), 
       col = c("black", "brown","brown","blue", "blue"), 
       lty = c(1, 1, 2, 1, 2), lwd =c(1, 2, 2, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017, 2017), c(0, 720000))
lines(c(2024, 2024), c(0, 720000))
text(2008, 700000, "Training", pos = 3)  # Adjusted y-coordinate and pos
text(2020, 700000, "Validation", pos = 3)  # Adjusted y-coordinate and pos
text(2025.5, 700000, "Future", pos = 3)  # Adjusted y-coordinate and pos
arrows(1992, 700000, 2015.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2017.1, 700000, 2023.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 700000, 2027.3, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#############################################################################################

## Fit a regression model with linear trend and seasonality for training partition. 
lin.trend.seas <- tslm(train.ts ~ trend  + season)
summary(lin.trend.seas)

# Create regression forecast with linear trend and seasonality for validation period.
lin.trend.seas.pred <- forecast(lin.trend.seas, h = nValid, level = 0)
lin.trend.seas.pred

## Identify and display residuals based on the regression model in training period.
lin.trend.seas.res <- lin.trend.seas$residuals
lin.trend.seas.res

# Apply trailing MA for residuals with window width k = 12. 
ma.trail.res <- rollmean(lin.trend.seas.res, k = 12, align = "right")
ma.trail.res

# Regression residuals in validation period.
lin.trend.seas.res.valid <- valid.ts - lin.trend.seas.pred$mean
lin.trend.seas.res.valid

# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

## Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level <- lin.trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level

# Create a table for validation period: validation data, regression 
# forecast, trailing MA for residuals and total forecast.
valid.df <- round(data.frame(valid.ts, lin.trend.seas.pred$mean, 
                             ma.trail.res.pred$mean, 
                             fst.2level), 3)
names(valid.df) <- c("Retail Sales", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

# Use accuracy() function to identify common accuracy measures.
round(accuracy(fst.2level, valid.ts), 3)

## Fit a regression model with linear trend and seasonality for entire data set.
tot.trend.seas <- tslm(retail.ts ~ trend + season)
summary(tot.trend.seas)

# Create regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 12, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                          tot.fst.2level)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

## Use accuracy() function to identify common accuracy measures.
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, retail.ts), 3)
round(accuracy((snaive(retail.ts))$fitted, retail.ts), 3)

## GENERATE PLOT OF ORIGINAL DATA AND REGRESSION FORECAST, AND PREDICTIONS
## IN FUTURE 12 PERIODS.
## GENERATE PLOT OF REGRESSION RESIDUALS, TRAILING MA FOR RESIDUALS, AND 
## TRAILING MA FORECAST IN FUTURE 12 PERIODS.

# Plot original Ridership time series data and regression model.
plot(retail.ts, 
     xlab = "Time", ylab = "Sales (in 000s)", ylim = c(162000, 730000), 
     bty = "l", xlim = c(1992, 2025.25), lwd =1, xaxt = "n",
     main = "Retail Sales Data and Regression with Trend and Seasonality") 
axis(1, at = seq(1992, 2025.25, 1), labels = format(seq(1992, 2025.25, 1)))
lines(tot.trend.seas$fitted, col = "blue", lwd = 2)
lines(tot.trend.seas.pred$mean, col = "blue", lty =5, lwd = 2)
legend(1992,700000, legend = c("Retail Sales", "Regression",
                             "Regression Forecast for Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(0, 730000))
text(2005, 700000, "Data Set", pos = 3)
text(2025.2, 700000, "Future", pos = 3)
arrows(1992, 700000, 2023.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 700000, 2026.3, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Plot regression residuals data and trailing MA based on residuals.
plot(tot.trend.seas.res, 
     xlab = "Time", ylab = "Sales (in 000s)", ylim = c(-81000, 81000), 
     bty = "l", xaxt = "n", xlim = c(1992, 2025.25), lwd =1, col = "brown", 
     main = "Regression Residuals and Trailing MA for Residuals") 
axis(1, at = seq(1992, 2025.25, 1), labels = format(seq(1992, 2025.25, 1)))
lines(tot.ma.trail.res, col = "blue", lwd = 2, lty = 1)
lines(tot.ma.trail.res.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1992,50000, legend = c("Regresssion Residuals", 
                             "Trailing MA (k=12) for Residuals", 
                             "Trailing MA Forecast (k=12) for Future 12 Periods"), 
       col = c("brown", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(-81000, 81000))
text(2005, 50000, "Data Set", pos = 3)
text(2025.2, 50000, "Future", pos = 3)
arrows(1992, 70000, 2023.9, 70000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 70000, 2026.3, 70000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#######################################################################################################################

## Fit a regression model with quadratic trend and seasonality for training partition. 

quad.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(quad.trend.season)

# Create regression forecast with linear trend and seasonality for validation period.
quad.trend.season.pred <- forecast(quad.trend.season, h = nValid, level = 0)
quad.trend.season.pred

## Identify and display residuals based on the regression model in training period.
quad.trend.season.res <- quad.trend.season$residuals
quad.trend.season.res

# Apply trailing MA for residuals with window width k = 12. 
ma.trail.res1 <- rollmean(quad.trend.season.res, k = 12, align = "right")
ma.trail.res1

# Regression residuals in validation period.
quad.trend.season.res.valid <- valid.ts - quad.trend.season.pred$mean
quad.trend.season.res.valid

# Create residuals forecast for validation period.
ma.trail.res.pred1 <- forecast(ma.trail.res1, h = nValid, level = 0)
ma.trail.res.pred1

## Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level1 <- quad.trend.season.pred$mean + ma.trail.res.pred1$mean
fst.2level1

# Create a table for validation period: validation data, regression 
# forecast, trailing MA for residuals and total forecast.
valid.df1 <- round(data.frame(valid.ts, quad.trend.season.pred$mean, 
                             ma.trail.res.pred1$mean, 
                             fst.2level1), 3)
names(valid.df1) <- c("Retail Sales", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df1

# Use accuracy() function to identify common accuracy measures.
round(accuracy(fst.2level, valid.ts), 3)
round(accuracy(fst.2level1, valid.ts), 3)

## Fit a regression model with quadratic trend and seasonality for entire data set.
tot.trend.seas1 <- tslm(retail.ts ~ trend + I(trend^2) + season)
summary(tot.trend.seas1)

# Create regression forecast for future 12 periods.
tot.trend.seas.pred1 <- forecast(tot.trend.seas1, h = 12, level = 0)
tot.trend.seas.pred1

# Identify and display regression residuals for entire data set.
tot.trend.seas.res1 <- tot.trend.seas1$residuals
tot.trend.seas.res1

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res1 <- rollmean(tot.trend.seas.res1, k = 12, align = "right")
tot.ma.trail.res1

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred1 <- forecast(tot.ma.trail.res1, h = 12, level = 0)
tot.ma.trail.res.pred1

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future 12 periods.
tot.fst.2level1 <- tot.trend.seas.pred1$mean + tot.ma.trail.res.pred1$mean
tot.fst.2level1

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df1 <- data.frame(tot.trend.seas.pred1$mean, tot.ma.trail.res.pred1$mean, 
                           tot.fst.2level1)
names(future12.df1) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df1

## GENERATE PLOT OF ORIGINAL DATA AND REGRESSION FORECAST, AND PREDICTIONS
## IN FUTURE 12 PERIODS.
## GENERATE PLOT OF REGRESSION RESIDUALS, TRAILING MA FOR RESIDUALS, AND 
## TRAILING MA FORECAST IN FUTURE 12 PERIODS.

# Plot original Ridership time series data and regression model.
plot(retail.ts, 
     xlab = "Time", ylab = "Sales (in 000s)", ylim = c(162000, 730000), 
     bty = "l", xlim = c(1992, 2025.25), lwd =1, xaxt = "n",
     main = "Retail Sales Data and Regression with Quadratic Trend and Seasonality") 
axis(1, at = seq(1992, 2025.25, 1), labels = format(seq(1992, 2025.25, 1)))
lines(tot.trend.seas1$fitted, col = "blue", lwd = 2)
lines(tot.trend.seas.pred1$mean, col = "blue", lty =5, lwd = 2)
legend(1992,700000, legend = c("Retail Sales", "Regression",
                               "Regression Forecast for Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(0, 730000))
text(2005, 700000, "Data Set", pos = 3)
text(2025.2, 700000, "Future", pos = 3)
arrows(1992, 700000, 2023.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 700000, 2026.3, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Plot regression residuals data and trailing MA based on residuals.
plot(tot.trend.seas.res1, 
     xlab = "Time", ylab = "Sales (in 000s)", ylim = c(-81000, 81000), 
     bty = "l", xaxt = "n", xlim = c(1992, 2025.25), lwd =1, col = "brown", 
     main = "Regression Residuals and Trailing MA for Residuals") 
axis(1, at = seq(1992, 2025.25, 1), labels = format(seq(1992, 2025.25, 1)))
lines(tot.ma.trail.res1, col = "blue", lwd = 2, lty = 1)
lines(tot.ma.trail.res.pred1$mean, col = "blue", lwd = 2, lty = 2)
legend(1992,50000, legend = c("Regresssion Residuals", 
                              "Trailing MA (k=12) for Residuals", 
                              "Trailing MA Forecast (k=12) for Future 12 Periods"), 
       col = c("brown", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(-81000, 81000))
text(2005, 50000, "Data Set", pos = 3)
text(2025.2, 50000, "Future", pos = 3)
arrows(1992, 70000, 2023.9, 70000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 70000, 2026.3, 70000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## Performance measures
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, retail.ts), 3)
round(accuracy(tot.trend.seas.pred1$fitted+tot.ma.trail.res1, retail.ts), 3)

#######################################################################################################################

## Model 3: Advanced Exponential Smoothing models - Holt-Winters model

## Use ets() function with model = "ZZZ"- automated selection of
# error, trend, and seasonality options & use optimal alpha, beta, & gamma
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

# Use forecast() function to make predictions using this HW model with validation period (nValid).
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

## Performance measures for hw forecast
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)

# Use ets() function with model = "ZZZ" for entire data
HW.ZZZ <- ets(retail.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this hw model for 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# plot HW predictions for original data, optimal smoothing parameters.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Sales (in 000s)", ylim = c(162000, 730000), 
     bty = "l", xlim = c(1992, 2025.25), xaxt = "n",
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(1992, 2025.25, 1), labels = format(seq(1992, 2025.25, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(retail.ts)
legend(1992,700000, 
       legend = c("Retail Sales", 
                  "Holt-Winter'sModel for Entire Data Set",
                  "Holt-Winter's Model Forecast, Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2024, 2024), c(0, 730000))
text(2005, 700000, "Data Set", pos = 3)
text(2025.2, 700000, "Future", pos = 3)
arrows(1992, 700000, 2023.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 700000, 2026.3, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## Performance measures for HW forecast
round(accuracy(HW.ZZZ.pred$fitted, retail.ts), 3)

####################################################################################################################

## Model 4:  ARIMA MODELS

## Utilize auto.arima() function to automatically identify the ARIMA model structure and parameters. 
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

train.auto.arima.pred <- forecast(train.auto.arima, 
                                  h = nValid, level = 0)
train.auto.arima.pred

## Accuracy measures for the  auto ARIMA models
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

## ARIMA models to fit the entire dataset
retail.auto.arima <- auto.arima(retail.ts)
summary(retail.auto.arima)
retail.auto.arima.pred <- forecast(retail.auto.arima, h = 12, level = 0)
retail.auto.arima.pred

# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 12 future periods.
plot(retail.ts, 
     xlab = "Time", ylab = "Sales", 
     ylim = c(162000, 730000), xaxt = "n", 
     bty = "l", xlim = c(1992, 2025.25), lwd = 2,
     main = "Auto ARIMA Model for Entire Dataset") 
axis(1, at = seq(1992, 2025.25, 1), labels = format(seq(1992, 2025.25, 1)))
lines(retail.auto.arima$fitted, col = "blue", lwd = 2)
lines(retail.auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(1992,700000, legend = c("Retail Sales", 
                             "Auto ARIMA Forecast", 
                             "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
# lines(c(2004.25 - 3, 2004.25 - 3), c(0, 2600))
lines(c(2024, 2024), c(0, 730000))
text(2005, 700000, "Data Set", pos = 3)
text(2025.2, 700000, "Future", pos = 3)
arrows(1992, 700000, 2023.9, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024.1, 700000, 2026.3, 700000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


## Use accuracy() function to identify common accuracy measures for:
round(accuracy(retail.auto.arima.pred$fitted, retail.ts), 3)

#####################################################################################################

#Final Performance comparison

## Linear and Quadratic regression models with seasonality
round(accuracy(lin.trend.season.pred$fitted, retail.ts),3)
round(accuracy(quad.trend.season.pred$fitted, retail.ts),3)

## Two-level forecast for linear and Quadratic regression models with seasonality
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, retail.ts), 3)
round(accuracy(tot.trend.seas.pred1$fitted+tot.ma.trail.res1, retail.ts), 3)

## Performance measures for HW forecast
round(accuracy(HW.ZZZ.pred$fitted, retail.ts), 3)

## Performance measures for HW forecast
round(accuracy(retail.auto.arima.pred$fitted, retail.ts), 3)

##naive and snaive
round(accuracy((naive(retail.ts))$fitted, retail.ts), 3)
round(accuracy((snaive(retail.ts))$fitted, retail.ts), 3)

#############################################################################################
