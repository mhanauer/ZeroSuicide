---
---
title: "ITS"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages

Good time series information: https://www.itl.nist.gov/div898/handbook/pmc/section4/pmc4431.htm

More good time series information: https://otexts.org/fpp2/stationarity.html

Good time series: https://datascienceplus.com/time-series-analysis-in-r-part-1-the-time-series-object/

Good information on time series in R: https://datascienceplus.com/time-series-analysis-in-r-part-2-time-series-transformations/
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
library(urca)
library(forecast)
```
Load the data
```{r}
setwd("C:/Users/Matthew.Hanauer/Desktop")
ITSTest = read.csv("ZSData.csv", header = TRUE) 
head(ITSTest)
```
Cleaning data
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
describe.factor(ITSTest$Year)
ITSTest[120:145,]


### Changing the month names to numbers so we can plot
ITSTest$MonthNum = ifelse(ITSTest$MonthNum == "Jan", 1, ifelse(ITSTest$MonthNum == "Feb", 2, ifelse(ITSTest$MonthNum == "Mar", 3, ifelse(ITSTest$MonthNum=="Apr", 4, ifelse(ITSTest$MonthNum == "May", 5, ifelse(ITSTest$MonthNum == "Jun", 6, ifelse(ITSTest$MonthNum == "Jul", 7, ifelse(ITSTest$MonthNum == "Aug", 8, ifelse(ITSTest$MonthNum == "Sep", 9, ifelse(ITSTest$MonthNum == "Oct", 10, ifelse(ITSTest$MonthNum == "Nov", 11, ifelse(ITSTest$MonthNum == "Dec", 12, ITSTest$MonthNum))))))))))))

write.csv(ITSTest, "ITSTest.csv", row.names = FALSE)

ITSTest = read.csv("ITSTest.csv", header = TRUE)
```
Just look at descirptives
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
plot(sucByMonth$Month, sucByMonth$Suicides)

```
Model: First difference log with constant
Cleaning data for this analysis
```{r}

# You can exp the log to get the original value, then you can subtract the constant from before
t1 = 10

t1_log = log(t1)

t1_exp = exp(t1_log); t1_exp

t1_exp-5



ITS_diff_log = ITSTest

## Now add some constant to subtract later
Suicides_c = 6+ITS_diff_log$Suicides
length(Suicides_c)
# Now get rid first observation to rbind later
ITS_diff_log = ITS_diff_log[-1,] 
dim(ITS_diff_log)
## Make sure the numbers are the same (need to comment the above to work)
# Use 12, because that is seasonal differencing
# Seasonal differencing makes the numbers not line, so maybe try that later
ITS_diff_log$Suicides_diff_log =  diff(exp(log(Suicides_c))-6)
head(ITS_diff_log)
hist(ITS_diff_log$Suicides_diff_log)
```
Model: First difference log with constant 
Checking assumptions 

ur.kpps test assumes the data are stationary and if the test statistic is larger than those at different levels listed in the output, then you reject the null hypothesis.  If the data are stationary then, we don't have to worry about seasonality or autocorrelation
```{r}

interaction.plot(x.factor = ITS_diff_log$Time, trace.factor = ITS_diff_log$Intervention, response = ITS_diff_log$Suicides_diff_log)

mean_station =  ur.kpss(ITS_diff_log$Suicides_diff_log, type="mu", lags="short")
summary(mean_station)

trend_station =  ur.kpss(ITS_diff_log$Suicides_diff_log, type="tau", lags="short")
summary(trend_station)

season_station = nsdiffs(ITS_diff_log$Suicides_diff_log)

```
Model: First difference log with constant 
Now look at the interaction plot and compare means
```{r}
interaction.plot(x.factor = ITS_diff_log$Time, trace.factor = ITS_diff_log$Intervention, response = ITS_diff_log$Suicides_diff_log)
```
Model: First difference log with constant 
Now try robust linear regression
```{r}
library(MASS)
model_diff_log_robust_nointer = rlm(Suicides_diff_log ~ Time + Intervention, data = ITS_diff_log)
summary(model_diff_log)

model_diff_log_robust_inter = rlm(Suicides_diff_log ~ Time*Intervention, data = ITS_diff_log)
summary(model_diff_log_inter)

model_diff_log_lm_nointer = lm(Suicides_diff_log ~ Time + Intervention, data = ITS_diff_log)
summary(model_diff_log_lm_nointer)

model_diff_log_lm_inter = lm(Suicides_diff_log ~ Time*Intervention, data = ITS_diff_log)
summary(model_diff_log_lm_inter)

```
Model 2: Moving average model with difference scores
Cleaning data
```{r}
## Moving average starting at December 2004

Suicides_ma = SMA(ITSTest$Suicides, n = 12)
ITSTest$Suicides_ma = Suicides_ma
ITS_ma = na.omit(ITSTest)
## Now difference the data, high autocorrelation
Suicides_ma_diff = diff(ITS_ma$Suicides_ma)
## Now drop first row so we can rbind 
dim(ITS_ma)
ITS_ma = ITS_ma[-1,] 
dim(ITS_ma)
## Now put back together the data sets
ITS_ma$Suicides_ma_diff = Suicides_ma_diff
dim(ITS_ma)
head(ITS_ma)
```
Model 2: Moving average model
Get plots and descriptives with new moving average
```{r}
describe(ITS_ma)
interaction.plot(x.factor = ITS_ma$Time, trace.factor = ITS_ma$Intervention, response = ITS_ma$Suicides_ma_diff)
mean(Suicides_ma_diff)
sd(Suicides_ma_diff)
compmeans(ITS_ma$Suicides_ma_diff, ITS_ma$Intervention) 
```
Model 2: Moving average model
Check assumptions
```{r}
mean_station =  ur.kpss(ITS_ma$Suicides_ma, type="mu", lags="short")
summary(mean_station)

trend_station =  ur.kpss(ITS_ma$Suicides_ma, type="tau", lags="short")
summary(trend_station)



```
Model 2: Moving average model

Running the model and checking for autocorrelation

Seems like differing is needed there is autocorrelation
```{r}
model_lm = lm(ITS_ma$Suicides_ma ~ Time*Intervention, data = ITS_ma)  
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
Model 3: Moving average plus differencing (use data set generated before)
```{r}
describe(ITS_ma)
interaction.plot(x.factor = ITS_ma$Time, trace.factor = ITS_ma$Intervention, response = ITS_ma$Suicides_ma_diff)
hist(Suicides_ma_diff)
qqnorm(Suicides_ma_diff)
compmeans(ITS_ma$Suicides_ma_diff, ITS_ma$Intervention) 

```
Model 3: Moving average and differencing
Checking if data is stationary
```{r}
mean_station =  ur.kpss(ITS_ma$Suicides_ma_diff, type="mu", lags="short")
summary(mean_station)

trend_station =  ur.kpss(ITS_ma$Suicides_ma_diff, type="tau", lags="short")
summary(trend_station)


```
Model 3: Moving average and differencing
Running linear and robust models
```{r}
model_lm = lm(ITS_ma$Suicides_ma_diff ~ Time*Intervention, data = ITS_ma)  
summary(model_lm)


#Checking autocorrelation
residmodel_lm= residuals(model_lm)
plot(ITS_ma$Time, residmodel_lm)
acf(residmodel_lm)
pacf(residmodel_lm)
```
Model 4: Poisson and hurdle with counts
Testing assumptions
```{r}
mean_station =  ur.kpss(ITSTest$Suicides, type="mu", lags="short")
summary(mean_station)

trend_station =  ur.kpss(ITSTest$Suicides, type="tau", lags="short")
summary(trend_station)

interaction.plot(x.factor = ITSTest$Time, trace.factor = ITSTest$Intervention, response = ITSTest$Suicides)

compmeans(ITSTest$Suicides, ITSTest$Intervention) 
```
Model 4: Poisson and hurdle with counts
Running models

Testing for seasonality

Testing overdispersion and autocorrelation

Dispersion not significantly different from one by test and visual inspection of value 


Autocorrelation good link: https://www.ibm.com/support/knowledgecenter/en/SS3RA7_15.0.0/com.ibm.spss.modeler.help/timeseries_acf_pacf.htm

Shows the correlation on the y-axis between different time orders.  For example, time one 
```{r}
modelP = glm(Suicides ~ Time+ Intervention, family = "poisson", data = ITSTest)  
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
modelH = hurdle(Suicides ~ Time*Intervention, dist = "poisson", zero.dist = "binomial", data = ITSTest)  

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
Model 5: CRI's moving average data



