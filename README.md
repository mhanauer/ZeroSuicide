---
---
title: "ITS"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages
Here I am just loading a bunch of packages.
```{r}
library(ggplot2)
library(AER)
library(TSA)
library(TTR)
library(descr)
library(urca)
library(pscl)
library(tsModel) ; library(Epi)
library(splines) ; library(vcd)
library(psych)
```
Loading the data.  I created two data sets.  First is just the raw data.  It starts in Februray 2002 (no data for January) and ends in March 2018.  The data is by the count of suicides per month. 
```{r}
#Zero Suicide Data
#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/ZeroSuicide")
ITSTest = read.csv("ZSData.csv", header = TRUE, na.strings = "N/A")
head(ITSTest)
```
We need to get the date into a form that is analyzable.  I want two variables for date one with month and another with year.

Then I want to create a time variable which is just the number of time points in the study.

Then I created an intervention variable, which is zero until the intervention start point which is Jan 2014.

Then I am renaming the months so they are numbers.

Also created a quarter variable to account for the seasonality
```{r}
ITSTest$Year = gsub("\\D", "", ITSTest$Month)
ITSTest$Year = as.numeric(ITSTest$Year)

head(ITSTest)
### use the gsub function to break off -02 part, then get rid of -, then you have the year
ITSTest$MonthNum =  gsub("\\d", "", ITSTest$Month)
### Get rid of -0x part 
ITSTest$MonthNum = substr(ITSTest$MonthNum, start = 2, stop= 4)
ITSTest$Month = NULL

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

ITSTest$Quarter = ifelse(ITSTest$MonthNum <= 3, 1, ifelse(ITSTest$MonthNum >= 4 & ITSTest$MonthNum <= 6, 2, ifelse(ITSTest$MonthNum >= 7 & ITSTest$MonthNum <= 9, 3, ifelse(ITSTest$MonthNum >= 10, 4, ITSTest$MonthNum))))

```
Just look at descriptives 
```{r}
describe(ITSTest)
describe.factor(ITSTest$Intervention)
```
Getting the

Evaluating if the average number of suicides is different between intervention and baseline phase.

Suicides went slightly up during intervention phase.
```{r}
compmeans(ITSTest$Suicides, ITSTest$Intervention)
```
Model: Counts
I am first testing the hurdle model with just intervention, then intervention + 
quarter, then intervention*quarter for a Poisson versus negative biomonal.

I am comparing each level (i.e. intervention only Poisson versus negative binomial) first to evaluate whether the Poisson or negative binomial model is a better fit.  Then I am comparing the best fitting of those two across the more complex models (i.e. intervention only versus intervention + quarter) to find out which model is the best fit.  If there was no statistically significant difference, then I defaulted to the simpler model (i.e. less covariates, poisson over negbin).

Information on the hurdle model: https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf


Need to include a quarter variable for seasonality: https://books.google.com/books?id=64vt5TDBNLwC&pg=PA369&dq=seasonality+dummy+variable&hl=en&sa=X&ved=0ahUKEwimksvS8ujhAhUGHqwKHZEICI4Q6AEIKDAA#v=onepage&q=seasonality%20dummy%20variable&f=false

I also tested whether a regular Poisson (better model fit) was better or worse than the final hurdle count model.
```{r}
model_p = glm(Suicides ~ Intervention, family = "poisson", data = ITSTest)
summary(model_p)

library(MASS)

model_nb = glm.nb(Suicides ~ Intervention, data = ITSTest)
summary(model_nb)
AIC(model_p)
AIC(model_nb)
BIC(model_p)
BIC(model_nb)

anova(model_p, model_nb)
pchisq(2 * (logLik(model_p) - logLik(model_nb)), df = 1, lower.tail = FALSE)

model_p_quart = glm(Suicides ~ Intervention + factor(Quarter), family = "poisson", data = ITSTest)
sum_model_p_quart = summary(model_p_quart)
exp(sum_model_p_quart$coefficients[,1])

model_p = glm(Suicides ~ Intervention, family = "poisson", data = ITSTest)
summary(model_p)

modelH_int_p = hurdle(Suicides ~ Intervention, dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelH_int_p)

modelH_int_n = hurdle(Suicides ~ Intervention, dist = "negbin", zero.dist = "binomial", data = ITSTest)  
summary(modelH_int_n)

## No diff go with Poisson simplier model
lrtest(modelH_int_p,modelH_int_n)
######


modelH_int_covar_p = hurdle(Suicides ~ Intervention + factor(Quarter), dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelH_int_covar_p)
exp(0.5827)
exp(-0.06691)
modelH_int_covar_n = hurdle(Suicides ~ Intervention + factor(Quarter), dist = "negbin", zero.dist = "binomial", data = ITSTest)  
summary(modelH_int_covar_n)

## No diff go with Poisson simplier model
lrtest(modelH_int_covar_p,modelH_int_covar_n)


modelH_int_interact_p = hurdle(Suicides ~ Intervention*factor(Quarter), dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelH_int_interact_p)

modelH_int_interact_n = hurdle(Suicides ~ Intervention*factor(Quarter), dist = "negbin", zero.dist = "binomial", data = ITSTest)  
summary(modelH_int_interact_n)

## No diff go with Poisson simplier model
lrtest(modelH_int_interact_p,modelH_int_interact_n)
 
### Now compare all the models
lrtest(modelH_int_p, modelH_int_covar_p, modelH_int_interact_p)

### Try best model with Poisson not hurdle them compare
model_p_<- glm(Suicides ~ Intervention + factor(Quarter), 
family= "poisson", data = ITSTest)

#### Hurdle most is better
lrtest(modelH_int_covar_p, model_p_)

```
Now checking the autocorrelation and seasonality of the best fitting model
Some small amount of auto at 17ish, but not confirmed by other stationary tests at the .05 alpha level

Good explanation of roots: https://www.statisticshowto.datasciencecentral.com/unit-root/
Change in distribution of means and trends (non-linear changes)
```{r}
#Checking autocorrelation
residModelH = residuals(modelH_int_covar_p)
plot(ITSTest$Time, residModelH)
acf(residModelH)
pacf(residModelH)

### Use other test to test whether the data is stationary
lag_n_short = c(2:10)
mean_station_short = list()
for(i in 1:length(lag_n_short)){
mean_station_short[[i]]  =  ur.kpss(ITSTest$Suicides, type="tau", use.lag
 = lag_n_short[[i]])
mean_station_short[[i]] = summary(mean_station_short[[i]])
}
mean_station_short

lag_n_long = c(11:20)
mean_station_long = list()
for(i in 1:length(lag_n_long)){
mean_station_long[[i]]  =  ur.kpss(ITSTest$Suicides, type="tau", use.lag
 = lag_n_long[[i]])
mean_station_long[[i]] = summary(mean_station_long[[i]])
}
mean_station_long

lag_n_short = c(2:10)
trend_station_short = list()
for(i in 1:length(lag_n_short)){
trend_station_short[[i]]  =  ur.kpss(ITSTest$Suicides, type="tau", use.lag
 = lag_n_short[[i]])
trend_station_short[[i]] = summary(trend_station_short[[i]])
}
trend_station_short

lag_n_long = c(11:20)
trend_station_long = list()
for(i in 1:length(lag_n_long)){
trend_station_long[[i]]  =  ur.kpss(ITSTest$Suicides, type="tau", use.lag
 = lag_n_long[[i]])
trend_station_long[[i]] = summary(trend_station_long[[i]])
}
trend_station_long
```
Because there is minimal evidence of autocorrelation and to another way of analyzing time series data to account for autocorrelation, I tried a moving average with first differences.  The moving average can help with the seasonality and the first difference can help with the autocorrelation: 

Also data looks normally distrbuted

Model 2: Moving average model with difference scores
Cleaning data

Interpretation
```{r}
## Moving average starting at December 2004
library(TTR)
Suicides_ma = SMA(ITSTest$Suicides, n = 12)
ITSTest$Suicides_ma = Suicides_ma
ITS_ma = na.omit(ITSTest)
dim(ITSTest)
dim(ITS_ma)
### Checking moving average
mean(head(ITSTest$Suicides, 12))
head(ITSTest$Suicides_ma, 15)
1.416667 - 1.500000
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
### Check distribution
hist(Suicides_ma_diff)
qqnorm(Suicides_ma_diff)
```
Model 2: Moving average model
Get plots and descriptives with new moving average
```{r}
describe(ITS_ma)
mean(Suicides_ma_diff)
sd(Suicides_ma_diff)
compmeans(ITS_ma$Suicides_ma_diff, ITS_ma$Intervention) 
0.017973856--0.003180662
```

Model 2: Moving average model

Running the model and checking for autocorrelation

Seems like differing is needed there is autocorrelation
```{r}
model_lm = lm(Suicides_ma_diff ~Intervention, data = ITS_ma)  
model_lm_cov = lm(Suicides_ma_diff ~Intervention + factor(Quarter), data = ITS_ma) 
model_lm_inter = lm(Suicides_ma_diff ~Intervention*factor(Quarter), data = ITS_ma) 

## Inter only is best model
anova(model_lm, model_lm_cov, model_lm_inter)
summary(model_lm)


### compare hurdle with simple model
AIC(model_lm)
AIC(modelH_int_covar_p)
BIC(model_lm)
BIC(modelH_int_covar_p)
```
Model 2: Moving average model
Check assumptions
```{r}
#Checking autocorrelation
residModelH = residuals(model_lm)
plot(ITS_ma$Time, residModelH)
acf(residModelH)
pacf(residModelH)

### Use other test to test whether the data is stationary
lag_n_short = c(2:10)
mean_station_short = list()
for(i in 1:length(lag_n_short)){
  mean_station_short[[i]]  =  ur.kpss(ITS_ma$Suicides_ma_diff, type="tau", use.lag
                                      = lag_n_short[[i]])
  mean_station_short[[i]] = summary(mean_station_short[[i]])
}
mean_station_short

lag_n_long = c(11:20)
mean_station_long = list()
for(i in 1:length(lag_n_long)){
  mean_station_long[[i]]  =  ur.kpss(ITS_ma$Suicides_ma_diff, type="tau", use.lag
                                     = lag_n_long[[i]])
  mean_station_long[[i]] = summary(mean_station_long[[i]])
}
mean_station_long

lag_n_short = c(2:10)
trend_station_short = list()
for(i in 1:length(lag_n_short)){
  trend_station_short[[i]]  =  ur.kpss(ITS_ma$Suicides_ma_diff, type="tau", use.lag
                                       = lag_n_short[[i]])
  trend_station_short[[i]] = summary(trend_station_short[[i]])
}
trend_station_short

lag_n_long = c(11:20)
trend_station_long = list()
for(i in 1:length(lag_n)){
  trend_station_long[[i]]  =  ur.kpss(ITS_ma$Suicides_ma_diff, type="tau", use.lag
                                      = lag_n[[i]])
  trend_station_long[[i]] = summary(trend_station_long[[i]])
}
trend_station_long


```
########################################################################################
Another way that (Bernal, 2013; Bernal, 2017) has found to account for seasonality is include a harmonic mean for the month.  I am not sure why this helps, how to interpret the results or how many sine cosines to include, but the results still demonstrate no differences in the pre and post intervention.
########################################################################################
Does not pass lrtest; however, one of the "months" is significant.  Not sure what this means.
```{r}
modelH_q = hurdle(Suicides ~ Intervention + factor(Quarter), dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelH_q)

modelH_harm = hurdle(Suicides ~ Intervention + factor(Quarter) + harmonic(MonthNum,2,12), dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelH_harm)

lrtest(modelH_q, modelH_harm)
```
Now trying with the moving average differenced data.  No improvement.
```{r}
model_lm = lm(Suicides_ma_diff ~Intervention, data = ITS_ma)
model_lm_harm = lm(Suicides_ma_diff ~Intervention + harmonic(MonthNum,2,12), data = ITS_ma)

anova(model_lm, model_lm_harm)
lrtest(model_lm, model_lm_harm)
```






