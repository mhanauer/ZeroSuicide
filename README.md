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
library(pracma)
library(AER)
library(pscl)
library(TSA)
library(TTR)
library(smooth)
library(greybox)
library(tseries)
library(descr)
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
3. Add a time variable
4. Get descriptives: Counts per month, counts per year, plot over time
5. Add intervention variable Intervention starts January 2014
6. Figure out how to evaluate seasonal effect
7. Figure out how to get rid of if necessary seasonal effect
8. Evaluate the regular model for autocorrelation
9. Figure out if necessary how to get rid of auto
```{r}
### use the gsub function to break off -02 part, then get rid of -, then you have the year
ITSTest$MonthNum =  gsub("\\d", "", ITSTest$Month)
### Get rid of -0x part 
ITSTest$MonthNum = substr(ITSTest$MonthNum, start = 1, stop= 3)

ITSTest$Year = gsub("\\D", "", ITSTest$Month)

ITSTest$Year = as.numeric(ITSTest$Year)

ITSTest$Month = NULL
head(ITSTest)

### Add a time variable that is 1:length of data set see Bernal article
ITSTest$Time= 1:dim(ITSTest)[1]
dim(ITSTest)
head(ITSTest)
ITSTest
ITSTest[144,]

Intervention= c(rep(0,143), rep(1,194-143))
length(Intervention)

ITSTest$Intervention = Intervention
head(ITSTest)
ITSTest[143:144,]

## Get rid of year two, because we are not starting out in January and the ts object is thinking that we are starting in January 
ITSTest = subset(ITSTest, Year != 2)
describe.factor(ITSTestTest$Year)
ITSTest[120:145,]

```
Just look at descirptives
We are missing some values for certain months not sure why
```{r}
describe(ITSTest)
```


Get counts by month and year 
We are missing some months?  Why?

Getting the total number of suicide deaths by month, then year
```{r}
sucByYear = aggregate(Suicides ~ Suicides + Year, data = ITSTest, sum)
sucByMonth = aggregate(Suicides ~ Suicides + MonthNum, data = ITSTest, sum)

sucByYear
sucByMonth

plot(sucByYear$Year, sucByYear$Suicides)

```
Let's try to graph suicides over time with some indication of intervention
```{r}
interaction.plot(x.factor = ITSTest$Time, trace.factor = ITSTest$Intervention, response = ITSTest$Suicides)
```
Look at means from pre and post intervention
```{r}
compmeans(ITSTest$Suicides, ITSTest$Intervention)
```
Testing overdispersion and autocorrelation

Dispersion not significantly different from one by test and visual inspection of value 


Autocorrelation good link: https://www.ibm.com/support/knowledgecenter/en/SS3RA7_15.0.0/com.ibm.spss.modeler.help/timeseries_acf_pacf.htm

Shows the correlation on the y-axis between different time orders.  For example, time one 
```{r}
modelP = glm(Suicides ~ Time*Intervention, family = "poisson", data = ITSTest)  
modelQP = glm(Suicides ~ Time*Intervention, family = "quasipoisson", data = ITSTest)  
dispersiontest(modelP, alternative = "two.sided")
mean(ITSTest$Suicides)
sd(ITSTest$Suicides)

summary(modelP)

### Testing autocorrelation

residModelP = residuals(modelP)
plot(residModelP, ITSTest$time)
acf(residModelP)
pacf(residModelP)
```

Try the hurdle model and test for auto
```{r}

modelH= hurdle(Suicides ~ Time*Intervention, dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelH)
# Needs to be a ts function and doesn't work like the example says
modelHH = hurdle(Suicides ~ Time*Intervention, dist = "poisson", zero.dist = "binomial", data = ITSTest)  

## Not trying cubic spline, not sure what is happening
# When I model time as cubed as not raw then ok 
modelHC = hurdle(Suicides ~ poly(Time, 3, raw = FALSE)*Intervention, dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelHC)

#Checking autocorrelation
residModelH = residuals(modelH)
plot(ITSTest$Time, residModelH)
acf(residModelH)
pacf(residModelH)
```

Try modeling with moving average and using linear regression?  Check residuals and suicides over time

Need to difference data points

Good information on time series in R: https://datascienceplus.com/time-series-analysis-in-r-part-2-time-series-transformations/
```{r}
## Moving average starting at December 2004

Suicides_ma = SMA(ITSTest$Suicides, n = 12)
ITSTest$Suicides_ma = Suicides_ma
ITS_ma = na.omit(ITSTest)
## Now difference the data, high autocorrelation
ITS_ma$Suicides_ma_diff = diff(ITS_ma$Suicides_ma)
dim(ITS_ma)
head(ITS_ma)
```
Get plots and descriptives with new moving average
```{r}
describe(ITS_ma)
interaction.plot(x.factor = ITS_ma$Time, trace.factor = ITS_ma$Intervention, response = ITS_ma$Suicides_ma)

compmeans(ITS_ma$Suicides_ma, ITS_ma$Intervention) 


```



Try model with moving average
```{r}
hist(ITSTest$Suicides_ma)

model_lm = lm(Suicides_ma ~ Time*Intervention, data = ITS_ma)  
summary(model_lm)


#Checking autocorrelation
residmodel_lm= residuals(model_lm)
plot(ITS_ma$Time, residmodel_lm)
acf(residmodel_lm)
pacf(residmodel_lm)


### Not significant may be too much going on
#model_poly = lm(Suicides_ma ~ poly(Time, 3, raw = FALSE)*Intervention, data = ITS_ma)  
#summary(model_poly)

```


Extra information

Try getting the outcome smoothed
```{r}

test = log(test)
hist(test)
```

```{r}
decompose_ITSTest_ts = decompose(ITSTest_ts, "additive")

plot(decompose_ITSTest_ts$trend)

adjustITSTest_ts = ITSTest_ts - decompose_ITSTest_ts$seasonal 
plot(adjustITSTest_ts)

ITSTest_ts[,3]
```
Here I am demonstrating how to identify what the frequency should be is you are unsure (I guess I should know, because it will likely be each month)???
Try turning into a TS object
No way to account for inflated zeros with tscount model.

Maybe take the moving average then model
```{r}

ITSTest$MonthNum = NULL
ITS_ts = ts(ITSTest, start = 3, frequency = 12)
ITS_ts
window(ITS_ts, start = c(4, 12), end = c(6, 5))

plot.ts(ITS_ts[,1])
```

