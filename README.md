---
---
title: "ITS"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages
```{r}
library(lavaan)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(paran)
library(caret)
library(ggplot2)
library(smooth)
library(TSA)
library(pracma)
```
Load the data
Outcome = aces
Outcome is a count
```{r}
setwd("C:/Users/Matthew.Hanauer/Desktop")
ITSTest = read.csv("ZSData.csv", header = TRUE) 
head(ITSTest)
```
Steps I need to take
1. Get a year variable
2. Get a month variable

```{r}
setwd("C:/Users/Matthew.Hanauer/Desktop")
ITSTest = read.csv("ZSData.csv", header = TRUE)
ITSTest$Month

### use the gsub function to break off -02 part, then get rid of -, then you have the year
ITSTest$MonthNum =  gsub("\\d", "", ITSTest$Month)
### Get rid of -0x part 
ITSTest$MonthNum = substr(ITSTest$MonthNum, start = 1, stop= 3)

ITSTest$Year = gsub("\\D", "", ITSTest$Month)

ITSTest$Year = as.numeric(ITSTest$Year)

ITSTest[,1]
```
Figure out how to create a ts object with the current data
```{r}

ITSTest = ts(ITSTest)
```


Ok now let's start with the autocorrelation tests
You need to check the residuals for autocorrelation 
So put together a model first 

Helpful introduction to ARIMA modeling: https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials

Need to test for autocorrelation via plots of residuals and ACF plots
ACF plots show how much relationship there between time points 

The bands on the plots show 95% confidence intervals for whether there is significant autocorrelation or not


```{r}
modelAuto = glm(aces ~ time*smokban, family = "poisson", data = ITSTest)  
residModelAuto = residuals(modelAuto)
plot(residModelAuto, ITSTest$time)

acf(residModelAuto)
pacf(residModelAuto)
```
Now try getting rid of seasonality
Change into time series object
Then get rid of seasonality
Good website for seasonality: https://anomaly.io/seasonal-trend-decomposition-in-r/

Is the seasonality multiplicatie or addative, so basically is the seasonality constant over time or does it grow or shrink over time

Before you can use decompose you need to know the frequency of the seasonality

To find the seasonality, you need to find the freq with the highest spec value. Think about the clock, this is the value when we look at time when highest points for the different frequencies

What to do if you do not know what the seasonabilty is: https://anomaly.io/detect-seasonality-using-fourier-transform-r/

To get rid of seasonal effect you extract the seasonal effect then

Here is how you adjust for seasonality
```{r}
decompose_ITSTest_ts = decompose(ITSTest_ts, "additive")

plot(decompose_ITSTest_ts$trend)

adjustITSTest_ts = ITSTest_ts - decompose_ITSTest_ts$seasonal 
plot(adjustITSTest_ts)

ITSTest_ts[,3]
```
Here I am demonstrating how to identify what the frequency should be is you are unsure (I guess I should know, because it will likely be each month)???


