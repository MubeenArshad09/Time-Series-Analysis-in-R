
install.packages("readxl")
library(readxl)
install.packages("DT")
library(DT)
install.packages("forecast")
library(forecast)
install.packages("TTR")
library(TTR) 


# Read the Excel file
marriage_data <- read_excel("Vital statistics in the UK.xlsx", sheet = "Marriage")

# remove first 4 rows
marriage_data <- marriage_data[-c(1:4), ]

# Set the 5th row as column names
colnames(marriage_data) <- as.character(unlist(marriage_data[1, ]))
marriage_data <- marriage_data[-1, ]  # Remove the new header row from data
head(marriage_data)


marriage_data <- marriage_data[order(marriage_data$Year), ]

# Inspect the data
datatable(head(marriage_data))
datatable(tail(marriage_data))
summary(marriage_data)
dim(marriage_data)
colnames(marriage_data)
sapply(marriage_data, class)

#marriage_data$England <- as.character(marriage_data$England)       # Convert to character
#marriage_data$England <- gsub("[,:]", "", marriage_data$England)   # Remove commas and ":"


# Convert column to numeric

marriage_data$England <- as.numeric(marriage_data$England)
Marriage_England <- marriage_data[4]
Marriage_England <- na.omit(Marriage_England)
head(Marriage_England)


## Convert into timeseries
Marriage_England_Timeseries <- ts(Marriage_England, frequency= 1,start=c(1974),end=c(2019))
plot.ts(Marriage_England_Timeseries)


# decomposing with SMA

Marriage_England_TimeseriesSMA <- SMA(Marriage_England_Timeseries,n=5)
plot.ts(Marriage_England_TimeseriesSMA)



Marriage_England_Model <- HoltWinters(Marriage_England_Timeseries, gamma=FALSE, 
                                      l.start=363137, b.start=-3176)


plot(Marriage_England_Model)

Marriage_England_Model


Marriage_England_Model$SSE




#Usesmodel#Uses these smoothed components (level and trend) to forecast future values
library(forecast)


Marriage_England_Forcasts <- forecast(Marriage_England_Model, h=20)
plot(Marriage_England_Forcasts)


acf(na.omit(Marriage_England_Forcasts2$residuals), lag.max=20)
Box.test(na.omit(Marriage_England_Forcasts2$residuals), lag=20, 
         type="Ljung-Box")


plot.ts(Marriage_England_Forcasts2$residuals)


# Histogram of residuals

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

# Histogram of Forecast errors
Marriage_England_Forcasts$residuals <-Marriage_England_Forcasts$residuals[
                    !is.na(Marriage_England_Forcasts$residuals)]

#Add a normal distribution curve
plotForecastErrors(Marriage_England_Forcasts$residuals)

mean(Marriage_England_Forcasts$residuals)


#################################################################

### ARIMA Model

auto.arima(Marriage_England )



Marriage_England_Timeseriesdiff1 <- diff(Marriage_England_Timeseries,
                                         differences=1)
plot.ts(Marriage_England_Timeseriesdiff1)



acf(Marriage_England_Timeseriesdiff1, lag.max=20)
acf(Marriage_England_Timeseriesdiff1, lag.max=20, plot=FALSE)

pacf(Marriage_England_Timeseriesdiff1, lag.max=20)
pacf(Marriage_England_Timeseriesdiff1, lag.max=20, plot=FALSE)


Marriage_England_Timeseriesarima <- arima(Marriage_England_Timeseries,
                                    order=c(0,1,0)) # fit an ARIMA(0,1,1) model

Marriage_England_Timeseriesarima


Marriage_England_Timeseriesforcast <- forecast(Marriage_England_Timeseriesarima,h=20)
Marriage_England_Timeseriesforcast
plot(Marriage_England_Timeseriesforcast)

acf(Marriage_England_Timeseriesforcast$residuals, lag.max=20)
Box.test(Marriage_England_Timeseriesforcast$residuals, lag=20, type="Ljung-Box")

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plot.ts(Marriage_England_Timeseriesforcast$residuals)


plotForecastErrors(Marriage_England_Timeseriesforcast$residuals)

mean(Marriage_England_Timeseriesforcast$residuals)

