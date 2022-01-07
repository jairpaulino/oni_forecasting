# Algorithm: ONI index modelling and forecasting
# Author: Jair Paulino
# Date: January 06 2021 

# Clear R environment
rm(list=ls())

# Libraries
library(forecast)

# Import functions
source("R/web_scraping.R")

# 01: Web scraping and data analysis ----
oni_data = web_scraping()

# ONI boxplot
boxplot(oni_data$oni_data[,(-1)], ylab="ONI index", xlab="Média trimestral")

# Oni time series
plot.ts(oni_data$oni_ts, lwd=1.5, xlab="Observações", ylab="ONI index")
abline(h=c(0.5), col=2, lty=3, lwd=3)
abline(h=c(-0.5), col=4, lty=3, lwd=3)
#abline(h=0, col=4, lty=3, lwd=2)
#abline(h=c(1,-1), col=4, lty=3, lwd=2)
#abline(h=c(2,-2), col=2, lty=3, lwd=2)

# Pre-processing ----

# Modelling ----

# Metrics ----

