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
ITSTest$MonthNum = substr(ITSTest$MonthNum, start = 1, stop= 3)
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

2002, 2006, 2010, 2014, 2018
```{r}

compmeans(ITSTest$Suicides, ITSTest$Intervention)
library(ggplot2)
plot(ITSTest$MonthNum, ITSTest$Suicides)

plot_year = aggregate(Suicides ~ Year, data = ITSTest, sum)
plot_year
ggplot(plot_year, aes(x = Year, y = Suicides))+
  geom_line()+
  geom_smooth(method = "lm")+
  labs(title="Suicides by Year")+  
  geom_vline(xintercept = 14, colour="red")


  

ITSTest[190:200,]  
  
ggplot(ITSTest, aes(x = Time, y = Suicides))+
  geom_line()+
  geom_smooth(method = "lm")+
  labs(title="Suicides by Month per Year")+
  geom_vline(xintercept = 144, colour="red")
```
Simluate good version
```{r}

suc_pre = c(10, 9, 8,7,6,5,4,3,2)
suc_good = c(5,4,3,2,1,rep(0,5))

good_suc = data.frame(Time = 1:200, Suicides = c(sample(suc_pre, 144, replace = TRUE), sample(suc_good, 200-144, replace = TRUE)))

ggplot(good_suc, aes(x = Time, y = Suicides))+
  geom_line()+
  geom_smooth(method = "lm")+
  labs(title="Suicides by Month per Year")+
  geom_vline(xintercept = 144, colour="red")
```


Model: Counts
I am comparing the Poisson and negative binomal model. I am comparing the best fitting of those two across the more complex models (i.e. intervention only versus intervention + quarter) to find out which model is the best fit.  If there was no statistically significant difference, then I defaulted to the simpler model (i.e. less covariates, poisson over negbin).


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
pchisq(2 * (logLik(model_p) - logLik(model_nb)), df = 1, lower.tail = FALSE)
#### Poisson not any different go with Poisson

### Compare Poisson with intervention + and intervention interact
model_p_quart = glm(Suicides ~ Intervention + factor(Quarter), family = "poisson", data = ITSTest)
model_p_quart_inter = glm(Suicides ~ Intervention*factor(Quarter), family = "poisson", data = ITSTest)
#### Although not statistically significantly different quarter 3 is significant so keep that variable in the model
lrtest(model_p,model_p_quart)
######
lrtest(model_p_quart, model_p_quart_inter)
######
```
Final model results
```{r}
model_p_quart = glm(Suicides ~ Intervention + factor(Quarter), family = "poisson", data = ITSTest)
sum_model_p_quart = summary(model_p_quart)
sum_model_p_quart
exp(sum_model_p_quart$coefficients[,1])

```
Now checking the autocorrelation and seasonality of the best fitting model
Some small amount of auto at 17ish, but not confirmed by other stationary tests at the .05 alpha level

Good explanation of roots: https://www.statisticshowto.datasciencecentral.com/unit-root/
Change in distribution of means and trends (non-linear changes)
```{r}
#Checking autocorrelation
residModelH = residuals(model_p_quart)
plot(ITSTest$Time, residModelH)
range(exp(residModelH))
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
########################################################
Now try just a year after with new intervention variable
April 2014 - May 2015: 147 to 160

```{r}
Intervention_April_May = c(rep(0,147), rep(1,160-147))
length(Intervention_April_May)
ITSTest_April_May = ITSTest[1:160,]
ITSTest_April_May$Intervention_April_May = Intervention_April_May

ITSTest_April_May[140:150,]
compmeans(ITSTest_April_May$Suicides, ITSTest_April_May$Intervention_April_May)
percent_change =  round((0.7692308-1.0272109)/1.0272109, 2)
percent_change

ITSTest[140:160,]
Intervention_Jan_Jan = c(rep(0,144), rep(1,156-144))
length(Intervention_Jan_Jan)
ITSTest_Jan_Jan = ITSTest[1:156,]
ITSTest_Jan_Jan$Intervention_Jan_Jan = Intervention_Jan_Jan
compmeans(ITSTest_Jan_Jan$Suicides, ITSTest_Jan_Jan$Intervention_Jan_Jan)
percent_change =  round((0.6666667-1.0208333)/1.0208333, 2)
percent_change
```
Now try statistical tests for just first year
```{r}
model_p_jan_jan = glm(Suicides ~ Intervention_Jan_Jan + factor(Quarter), family = "poisson", data = ITSTest_Jan_Jan)
summary(model_p_jan_jan)
sum_model_jan_jan = summary(model_p_jan_jan)
exp(sum_model_jan_jan$coefficients[,1])
```


########################################################################################
Does not pass lrtest; however, one of the "months" is significant.  Not sure what this means.
```{r}
model_p_plus = glm(Suicides ~ Intervention + factor(Quarter), family = "poisson", data = ITSTest)  
summary(model_p_plus)

model_p_plus_harm = glm(Suicides ~ Intervention + factor(Quarter) + harmonic(MonthNum,2,13), family = "poisson", data = ITSTest)  
summary(model_p_plus_harm)

modelH_harm = hurdle(Suicides ~ Intervention + factor(Quarter) + harmonic(MonthNum,2,12), dist = "poisson", zero.dist = "binomial", data = ITSTest)  
summary(modelH_harm)

anova(model_p_plus, modelH_harm)
AIC(modelH_harm)
BIC(modelH_harm)
AIC(model_p_plus)
BIC(model_p_plus)
```





