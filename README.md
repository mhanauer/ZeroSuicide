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


Here I am just loading a bunch of packages.
```{r}
library(ggplot2)
library(pracma)
library(AER)
library(TSA)
library(TTR)
library(descr)
library(urca)
library(pscl)
library(tsModel) ; library(Epi)
library(splines) ; library(vcd)

```
Loading the data.  I created two data sets.  First is just the raw data.  It starts in Februray 2002 (no data for January) and ends in March 2018.  The data is by the count of suicides per month.  

The rollowing data set is the rolling average that is used by the Centerstone team.
```{r}

#Zero Suicide Data
rm(list=ls())
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/ZeroSuicide")
ITSTest = read.csv("ZSData.csv", header = TRUE, na.strings = "N/A")
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/ZeroSuicide")
ITSRolling = read.csv("ZeroSuicideRollingSum.csv", header = TRUE, na.strings = "N/A")
head(ITSTest)
```
We need to get the date into a form that is analyziable.  I want two variable for date one with month and another with year.

Then I want to create a time variable which is just the number of time points in the study.

Then I created an intervention variable, which is zero until the intervention start point which is Jan 2014.

Then I am renaming the months so they are numbers.
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
ITSTest[144:150,]

#Start Jan 2014 intervention starts
Intervention= c(rep(0,143), rep(1,194-143))
length(Intervention)

ITSTest$Intervention = Intervention
head(ITSTest)
ITSTest[143:145,]


### Changing the month names to numbers so we can plot
ITSTest$MonthNum = ifelse(ITSTest$MonthNum == "Jan", 1, ifelse(ITSTest$MonthNum == "Feb", 2, ifelse(ITSTest$MonthNum == "Mar", 3, ifelse(ITSTest$MonthNum=="Apr", 4, ifelse(ITSTest$MonthNum == "May", 5, ifelse(ITSTest$MonthNum == "Jun", 6, ifelse(ITSTest$MonthNum == "Jul", 7, ifelse(ITSTest$MonthNum == "Aug", 8, ifelse(ITSTest$MonthNum == "Sep", 9, ifelse(ITSTest$MonthNum == "Oct", 10, ifelse(ITSTest$MonthNum == "Nov", 11, ifelse(ITSTest$MonthNum == "Dec", 12, ITSTest$MonthNum))))))))))))

```
Just look at descirptives
```{r}
describe(ITSTest)
```
Evaluating if the average number of suicides is different between intervention and baseline phase.

Suicides went slightly up during intervention phase.
```{r}
compmeans(ITSTest$Suicides, ITSTest$Intervention)
```
Harmonic mean explained: https://towardsdatascience.com/on-average-youre-using-the-wrong-average-geometric-harmonic-means-in-data-analysis-2a703e21ea0



```{r}
library(psych)
x = c(1:5)
mean(x)
harmonic.mean(x)
27^7
```


Model: Count
I created four hurdle models
1. Just intervention with respect to time
2. Intervention + time

I am first testing the hurdle model with just intervention, then intervention + time, then intervention*time for a Possion versus negative biomonal.

I am comparing each level (i.e. intervention only Possion versus negative binomial) first to evaluate whether the Possion or negative binomial model is a better fit.  Then I am comparing the best fitting of those two across the more complex models (i.e. internvetion only versus intervention + time) to find out which model is the best fit.  If there was no statistically significant difference, then I defaulted to the simplier model (i.e. less covariate, poisson over negbin).

Information on the hurdle model: https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf
```{r}

modelH_int_p = hurdle(Suicides ~ Intervention, dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelH_int_p)

modelH_int_n = hurdle(Suicides ~ Intervention, dist = "negbin", zero.dist = "binomial", data = ITSTest)  
summary(modelH_int_n)

## No diff go with Poisson simplier model
lrtest(modelH_int_p,modelH_int_n)
######

modelH_int_covar_p = hurdle(Suicides ~ Intervention + Time, dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelH_int_covar_p)

modelH_int_covar_n = hurdle(Suicides ~ Intervention + Time, dist = "negbin", zero.dist = "binomial", data = ITSTest)  
summary(modelH_int_covar_n)

## No diff go with Poisson simplier model
lrtest(modelH_int_covar_p,modelH_int_covar_n)


modelH_int_interact_p = hurdle(Suicides ~ Intervention*Time, dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelH_int_interact_p)

modelH_int_interact_n = hurdle(Suicides ~ Intervention*Time, dist = "negbin", zero.dist = "binomial", data = ITSTest)  
summary(modelH_int_interact_n)

## No diff go with Poisson simplier model
lrtest(modelH_int_interact_p,modelH_int_interact_n)

 
### Now compare all the models
lrtest(modelH_int_p, modelH_int_covar_p, modelH_int_interact_p)

```
Now checking the autocorrelation and sesaonaility of the best fitting model
```{r}
#Checking autocorrelation
residModelH = residuals(modelH_int_p)
plot(ITSTest$Time, residModelH)
acf(residModelH)
pacf(residModelH)

### Use other test
mean_station_short =  ur.kpss(ITSTest$Suicides, type="mu", lags="short")
summary(mean_station_short)

trend_station_short =  ur.kpss(ITSTest$Suicides, type="tau", lags="short")
summary(trend_station_short)

mean_station_long =  ur.kpss(ITSTest$Suicides, type="mu", lags="long")
summary(mean_station_long)

trend_station_long =  ur.kpss(ITSTest$Suicides, type="tau", lags="long")
summary(trend_station_long)

```






Then, because it could be the case that seaonalitiy is affecting the results (although I have evidence that it is not), I also did the analysis as above with a Possion and negative binomal without the hurdle and added a harmonic mean component.  The harmonic mean is used in this article about interrupted time series to account for sesasonality: https://academic.oup.com/ije/article/46/1/348/2622842
```{r}
### Compare seasonal models won't work with hurdle
## 
model_p_harm <- glm(Suicides ~ Intervention + harmonic(MonthNum,2,12),
  family="poisson", data = ITSTest)

model_p <- glm(Suicides ~ Intervention,
  family="poisson", data = ITSTest)
summary(model_p_harm)
summary(model_p)

lrtest(model_p_harm,model_p)
anova(model_p_harm, model_p)

### Test hurdle versus regular
lrtest(modelH_int_p, model_p_harm)
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
library(descr)
compmeans(ITS_diff_log$Suicides_diff_log, ITS_diff_log$Intervention)
```
Model: First difference log with constant 
Now try robust linear regression
```{r}
model_diff_log_lm_nointer = lm(Suicides_diff_log ~ Time + Intervention, data = ITS_diff_log)
summary(model_diff_log_lm_nointer)

model_diff_log_lm_inter = lm(Suicides_diff_log ~ Time*Intervention, data = ITS_diff_log)
summary(model_diff_log_lm_inter)

#Checking autocorrelation
residModelH = residuals(model_diff_log_lm_nointer)
plot(ITS_diff_log$Time, residModelH)
acf(residModelH)
pacf(residModelH)

```
Model 2: Moving average model with difference scores
Cleaning data
```{r}
## Moving average starting at December 2004
library(TTR)
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
compmeans(ITS_ma$Suicides_ma, ITS_ma$Intervention) 
```
Model 2: Moving average model
Check assumptions
```{r}
mean_station =  ur.kpss(ITS_ma$Suicides_ma_diff, type="mu", lags="short")
summary(mean_station)

trend_station =  ur.kpss(ITS_ma$Suicides_ma, type="tau", lags="short")
summary(trend_station)

```
Model 2: Moving average model

Running the model and checking for autocorrelation

Seems like differing is needed there is autocorrelation
```{r}
model_lm = lm(Suicides_ma ~Time*Intervention, data = ITS_ma)  
summary(model_lm)
library(jtools)


interact_plot(model_lm, pred = "Time", modx = "Intervention", data = ITS_ma)

interaction.plot(x.factor = ITS_diff_log$Time, trace.factor = ITS_diff_log$Intervention, response = ITS_diff_log$Suicides_diff_log)

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
Model 5: CRI's rolling sum
Load the data
```{r}
setwd("C:/Users/Matthew.Hanauer/Desktop")
ITSRolling = read.csv("ZeroSuicideRollingSum.csv", header = TRUE)
head(ITSRolling)
```
Model 5: CRI's rolling sum
Cleaning data
```{r}
### use the gsub function to break off -02 part, then get rid of -, then you have the year
ITSRolling$MonthNum =  gsub("\\d", "", ITSRolling$Date)
### Get rid of -0x part 
ITSRolling$MonthNum = substr(ITSRolling$MonthNum, start = 2, stop= 4)

ITSRolling$Year = gsub("\\D", "", ITSRolling$Date)

ITSRolling$Year = as.numeric(ITSRolling$Year)

ITSRolling$Date = NULL
head(ITSRolling)

### Add a time variable that is 1:length of data set see Bernal article
ITSRolling$Time= 1:dim(ITSRolling)[1]
dim(ITSRolling)
head(ITSRolling)
ITSRolling
ITSRolling[144:145,]

Intervention= c(rep(0,144), rep(1,195-144))
length(Intervention)

ITSRolling$Intervention = Intervention
head(ITSRolling)
ITSRolling[144:145,]


### Changing the month names to numbers so we can plot
ITSRolling$MonthNum = ifelse(ITSRolling$MonthNum == "Jan", 1, ifelse(ITSRolling$MonthNum == "Feb", 2, ifelse(ITSRolling$MonthNum == "Mar", 3, ifelse(ITSRolling$MonthNum=="Apr", 4, ifelse(ITSRolling$MonthNum == "May", 5, ifelse(ITSRolling$MonthNum == "Jun", 6, ifelse(ITSRolling$MonthNum == "Jul", 7, ifelse(ITSRolling$MonthNum == "Aug", 8, ifelse(ITSRolling$MonthNum == "Sep", 9, ifelse(ITSRolling$MonthNum == "Oct", 10, ifelse(ITSRolling$MonthNum == "Nov", 11, ifelse(ITSRolling$MonthNum == "Dec", 12, ITSRolling$MonthNum))))))))))))

write.csv(ITSRolling, "ITSRolling.csv", row.names = FALSE)

ITSRolling = read.csv("ITSRolling.csv", header = TRUE)
```
Model 5: CRI's rolling sum

Just look at descirptives
```{r}
describe(ITSRolling)
```
Model 5: CRI's rolling sum
If test statistic is below the critical levels, then we reject the null hypothesis of being stationary (this is bad)

```{r}
mean_station =  ur.kpss(ITSRolling$RollingSumTN, type="mu", lags="short")
summary(mean_station)

trend_station =  ur.kpss(ITSRolling$RollingSumTN, type="tau", lags="short")
summary(trend_station)

interaction.plot(x.factor = ITSRolling$Time, trace.factor = ITSRolling$Intervention, response = ITSRolling$RollingSumTN)

compmeans(ITSRolling$RollingSumTN, ITSRolling$Intervention) 
```
Model 5: CRI's rolling sum
```{r}
library(jtools)
model_lm = lm(RollingSumTN ~ Intervention*Time, data = ITSRolling)  
summary(model_lm)

interact_plot(model_lm, pred = "Time", modx = "Intervention")

### Testing autocorrelation
residModelP = residuals(model_lm)
plot(ITSRolling$Time, residModelP)
acf(residModelP)
pacf(residModelP)
hist(predict.lm(model_lm))


```
Can you compare two models with an intervention variable where one group gets the intervention and the other does not and they each have their own outcome variables? Sure do some model comparison?? 
Won't work because the outcome variable is different.

Maybe we can compare parameter estimates.  The values should be the same.  If we have the parameter estimate and the standard error, then we compare them via a t test??? The outcome is different numerically, but the same thing.  Think about if differences in degrees of freedom will make a difference
```{r}
ITSRollingComp = na.omit(ITSRolling)

dim(ITSRollingComp)

model_TN = lm(RollingSumTN ~ Time*Intervention, data = ITSRolling)
summary(model_TN)
model_IN = lm(RollingSumIN ~ Time*Intervention, data = ITSRolling)
summary(model_IN)

```
Do t-tests and demonstrate how to do this

(x1=x2)/sqrt(s1^2/n1 + s2^2/n2)
```{r}

```
Try GLS model
```{r}
head(ITSRolling)
library(nlme)
ITSRollingGLS = data.frame(RollingSumTN = ITSRolling$RollingSumTN, Intervention = ITSRolling$Intervention)
gls_rollingTN = gls(RollingSumTN ~ Intervention, correlation  = corAR1(), data = ITSRolling)
summary(gls_rollingTN)
```

