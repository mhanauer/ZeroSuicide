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
library(prettyR)
library(MASS)
```

We need to get the date into a form that is analyzable.  I want two variables for date one with month and another with year.

Then I want to create a time variable which is just the number of time points in the study.

Then I created an intervention variable, which is zero until the intervention start point which is Jan 2014.



Then I am renaming the months so they are numbers.

Also created a quarter variable to account for the seasonality

```{r}
ITSTest$Month
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
ITSTest[134:140,]
ITSTest[144:150,]
#Start Jan 2014 intervention starts
#Intervention= c(rep(0,143), rep(1,194-143))
## Try April 2013
Intervention= c(rep(0,134), rep(1,194-134))
length(Intervention)

ITSTest$Intervention = Intervention
head(ITSTest)
ITSTest[143:145,]
ITSTest[134:140,]


### Changing the month names to numbers so we can plot
ITSTest$MonthNum = ifelse(ITSTest$MonthNum == "Jan", 1, ifelse(ITSTest$MonthNum == "Feb", 2, ifelse(ITSTest$MonthNum == "Mar", 3, ifelse(ITSTest$MonthNum=="Apr", 4, ifelse(ITSTest$MonthNum == "May", 5, ifelse(ITSTest$MonthNum == "Jun", 6, ifelse(ITSTest$MonthNum == "Jul", 7, ifelse(ITSTest$MonthNum == "Aug", 8, ifelse(ITSTest$MonthNum == "Sep", 9, ifelse(ITSTest$MonthNum == "Oct", 10, ifelse(ITSTest$MonthNum == "Nov", 11, ifelse(ITSTest$MonthNum == "Dec", 12, ITSTest$MonthNum))))))))))))
ITSTest$MonthNum = as.numeric(ITSTest$MonthNum)

ITSTest$Quarter = ifelse(ITSTest$MonthNum <= 3, 1, ifelse(ITSTest$MonthNum >= 4 & ITSTest$MonthNum <= 6, 2, ifelse(ITSTest$MonthNum >= 7 & ITSTest$MonthNum <= 9, 3, ifelse(ITSTest$MonthNum >= 10, 4, ITSTest$MonthNum))))
describe.factor(ITSTest$Quarter)
```
Just look at descriptives 
```{r}
describe(ITSTest)
describe.factor(ITSTest$Intervention)
aggregate(Suicides ~ Intervention, data = ITSTest, sum)
aggregate(Suicides ~ Intervention, data = ITSTest, range)

```
Evaluating if the average number of suicides is different between intervention and baseline phase.

Suicides went slightly up during intervention phase.

2002, 2006, 2010, 2014, 2018
```{r}

compmeans(ITSTest$Suicides, ITSTest$Intervention)
library(ggplot2)
plot(ITSTest$MonthNum, ITSTest$Suicides)

plot_year = aggregate(Suicides ~ Year, data = ITSTest, sum)
describe.factor(plot_year$Year)
plot_year$Year = ifelse(plot_year$Year == 2, 2002, ifelse(plot_year$Year == 3, 2003, ifelse(plot_year$Year == 4, 2004, ifelse(plot_year$Year == 5, 2005, ifelse(plot_year$Year == 6, 2006, ifelse(plot_year$Year == 7, 2007, ifelse(plot_year$Year == 8, 2008, ifelse(plot_year$Year == 9, 2009, ifelse(plot_year$Year== 10, 2010, ifelse(plot_year$Year == 11, 2011, ifelse(plot_year$Year == 12, 2012, ifelse(plot_year$Year == 13, 2013, ifelse(plot_year$Year== 14,  2014, ifelse(plot_year$Year == 15, 2015, ifelse(plot_year$Year == 16, 2016, ifelse(plot_year$Year == 17, 2017, ifelse(plot_year$Year == 18, 2018, plot_year$Year)))))))))))))))))

ggplot(plot_year, aes(x = Year, y = Suicides))+
  geom_line()+
  geom_smooth(method = "glm")+
  labs(title="Suicides by Year")+  
  geom_vline(xintercept = 2014, colour="red")


  

ITSTest[50:200,]  
  
ggplot(ITSTest, aes(x = Time, y = Suicides))+
  geom_line()+
  geom_smooth(method = "glm")+
  labs(title="Suicides by Month per Year")+
  geom_vline(xintercept = 144, colour="red")


### Plot pre and post intervention seperately and put them side by side

```
Simluate good version
```{r}

suc_pre = c(10, 9, 8,7,6,5,4,3,2)
suc_good = c(5,4,3,2,1,rep(0,5))

good_suc = data.frame(Time = 1:200, Suicides = c(sample(suc_pre, 144, replace = TRUE), sample(suc_good, 200-144, replace = TRUE)))

ggplot(good_suc, aes(x = Time, y = Suicides))+
  geom_line()+
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
coeftest(model_p, vcov = sandwich)
con_robust =  coefci(model_p, vcov = sandwich)
exp(con_robust[2,1:2])
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
summary(model_p_quart)
model_p_quart_inter = glm(Suicides ~ Intervention*factor(Quarter), family = "poisson", data = ITSTest)
summary(model_p_quart_inter)
####Try a month variable nothing significant
model_p_m = glm(Suicides ~ Intervention + factor(MonthNum), family = "poisson", data = ITSTest)
summary(model_p_m)
#### 
lrtest(model_p,model_p_quart)
######
lrtest(model_p_quart, model_p_quart_inter)
######
```
Final model results
```{r}
model_p = glm(Suicides ~ Intervention, family = "poisson", data = ITSTest)
summary(model_p)
coeftest(model_p, vcov = sandwich)
con_robust =  coefci(model_p, vcov = sandwich)
exp(con_robust[2,1:2])
```
Now checking the autocorrelation and seasonality of the best fitting model
Some small amount of auto at 17ish, but not confirmed by other stationary tests at the .05 alpha level

Good explanation of roots: https://www.statisticshowto.datasciencecentral.com/unit-root/
Change in distribution of means and trends (non-linear changes)
```{r}
#Checking autocorrelation
residModelH = residuals(model_p)
plot(ITSTest$Time, residModelH)
range(predict.glm(model_p))
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
Let's try with the 2009 data.  Just line it up with the previous data and won't need to merge the dates

ITSTest$MonthNum =  gsub("\\d", "", ITSTest$Month)
### Get rid of -0x part 
ITSTest$MonthNum = substr(ITSTest$MonthNum, start = 1, stop= 3)

Let's do the first points, then say if "-" the delete?

Then get rid of the data until 2009, because we do not have counts for those
Then 
```{r}
TotalCount$Month = substr(TotalCount$MonthYear, start = 1, stop = 2)
head(TotalCount$Month)
TotalCount$Month =  gsub("\\D", "", TotalCount$Month)
TotalCount$Month = as.numeric(TotalCount$Month)

TotalCount$Year = substr(TotalCount$MonthYear, start = 3, stop = 7)
TotalCount$Year = gsub("\\D", "", TotalCount$Year)
TotalCount$Year = as.numeric(TotalCount$Year)
head(TotalCount)

TotalCount = TotalCount[order(TotalCount$Year, TotalCount$Month),]
head(TotalCount)
mean(TotalCount$Year, na.rm = TRUE)
TotalCount = TotalCount[-c(1:72),]
head(TotalCount)
ITSTest[170:200,]
```



Let's try the offset.  Maybe run some simulations with it to see if differences in rates would matter much.
Range is from 39k (round up to 40K) up to 60k

How to use offsets: 
https://stats.stackexchange.com/questions/11182/when-to-use-an-offset-in-a-poisson-regression
http://www.stat.umn.edu/geyer/5931/mle/seed2.pdf
See Bernal

Without offsets it is the ratio so the expcted percentage change of the expected counts: https://stats.idre.ucla.edu/stata/output/poisson-regression/

With offsets it is the expected percentage change in the rate: https://stats.idre.ucla.edu/stata/dae/poisson-regression/ 

We are getting the rate per year.  So we are getting the percentage change in the rate per year when we exp the results.

Ok we have two numbers at the end.  Split the data in half and the early half gets 40 to 50 and the second get 50 to 60.  

10,000 times in some loop


Then get the number of times there is a significant decrease and then increase maybe
Could use this formula to do a 15% increase for every 5 years: 5*(60000-40000)/40000 / (2018-2002)
### Now get the data for 
```{r}
dim(ITSTest)
total_n_fun = function(){
total_samp_low = seq(from = round(40000/12,0), to = round(50000/12,0), by= 10) 
total_low = sample(total_samp_low, replace = TRUE, size = 194/2)
total_samp_high = seq(from = round(50000/12,0), to = round(60000/12,0), by= 10) 
total_high = sample(total_samp_high, replace = TRUE, size = 194/2)
total_n = c(total_low, total_high)
total_n
}
total_n = replicate(10000, total_n_fun())
total_n_mat = matrix(data = total_n, nrow = 194, ncol = 10000, byrow = FALSE)
dim(total_n_mat)
total_n_mat[,1]
```
Ok now create a for loop to run with the log of n in the results
```{r}
model_p_results_n = apply(total_n_mat, 2, function(x){glm(Suicides ~ Intervention + offset(log((x))), family = "poisson", data = ITSTest)})
model_p_results_n[1]
model_p_results_test = summary(model_p_results_n[[1]])
model_p_results_test$coefficients
model_p_results_test$coefficients[2,4]

summary_dat = lapply(model_p_results_n, summary)
p_values = lapply(summary_dat, function(x)x$coefficients[2,4])
head(p_values)
range(p_values)
p_values_sig = ifelse(p_values < .05,1,0)
describe.factor(p_values_sig)
sum(p_values_sig)
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
Describe first year
```{r}
aggregate(Suicides ~ Intervention, data = ITSTest_Jan_Jan, sum)
aggregate(Suicides ~ Intervention, data = ITSTest_Jan_Jan, range)

```


Now try statistical tests for just first year
```{r}
model_p_jan_jan_quart = glm(Suicides ~ Intervention_Jan_Jan + factor(Quarter), family = "poisson", data = ITSTest_Jan_Jan)
summary(model_p_jan_jan_quart)

model_p_jan_jan = glm(Suicides ~ Intervention_Jan_Jan, family = "poisson", data = ITSTest_Jan_Jan)
summary(model_p_jan_jan)

sum_model_jan_jan = summary(model_p_jan_jan)
exp(sum_model_jan_jan$coefficients[,1])
```
########################################################################################
Trying to test time and harmonic term here.  Does not seem to have an impact choose the simplier model
```{r}
model_p_plus = glm(Suicides ~ Intervention + factor(Quarter), family = "poisson", data = ITSTest)  
summary(model_p_plus)

model_p_plus_harm = glm(Suicides ~ Intervention + factor(Quarter) + harmonic(MonthNum,2,12), family = "poisson", data = ITSTest)  
summary(model_p_plus_harm)

model_p_harm = glm(Suicides ~ Intervention + harmonic(MonthNum,2,12), family = "poisson", data = ITSTest)  
summary(model_p_harm)

model_p_time_plus = glm(Suicides ~ Intervention + Time, family = "poisson", data = ITSTest)  
summary(model_p_time_plus)

model_p_time_inter = glm(Suicides ~ Intervention*Time, family = "poisson", data = ITSTest)  
summary(model_p_time_inter)

model_p_time_inter_quarter = glm(Suicides ~ Intervention*Time + factor(Quarter), family = "poisson", data = ITSTest)  
summary(model_p_time_inter_quarter)


anova(model_p, model_p_harm)

AIC(model_p)
BIC(model_p)
AIC(model_p_harm)
BIC(model_p_harm)
```
ARIMA Model
Create a time ts object
Run model
Data starts in Feburary, but probably does not matter if we assume Februray is January just a label.
```{r}
ITS_ts = ts(ITSTest, start = c(2, 1), frequency = 12)
head(ITS_ts)
Intervention_ts = ITS_ts[,5]
ITS_ts = ITS_ts[,1]
library(forecast)
library(lmtest)
ITS_arima = Arima(ITS_ts, order = c(0,0,1), xreg = Intervention_ts)
summary(ITS_arima)
coeftest(ITS_arima)
### There was evidence of a spike at 17
ITS_arima_17 = Arima(ITS_ts, order = c(17,0,17), xreg = Intervention_ts)
summary(ITS_arima_17)
coeftest(ITS_arima_17)
```
Try tscount model, because ARIMA assumes nomral distribution
```{r}
library(tscount)
ITS_ts
inter_cov = interv_covariate(n = length(ITS_ts), tau = 144, delta = c(1))
ts_model_1 = tsglm(ITS_ts, model = list(past_obs = 1, past_mean = 12), xreg = inter_cov, distr = "nbinom")

### Doesn't seem to work no evidence of season anyways
ts_model_p = tsglm(ITS_ts, model = list(past_obs = 1, past_mean = 12), xreg = inter_cov, distr = "poisson")

ts_model = tsglm(ITS_ts, model = list(past_obs = 1), xreg = inter_cov, distr = "poisson", link = "log")

summary(ts_model)

interv_test(ts_model, tau = 144, delta = 0, est_interv = TRUE)
```


Maybe add Indiana Grant
```{r}
head(InSuicides)
InSuicides$Year = gsub("\\D", "", InSuicides$Month)
InSuicides$Year = as.numeric(InSuicides$Year)

head(InSuicides)
### use the gsub function to break off -02 part, then get rid of -, then you have the year
InSuicides$MonthNum =  gsub("\\d", "", InSuicides$Month)
### Get rid of -0x part 
InSuicides$MonthNum = substr(InSuicides$MonthNum, start = 2, stop= 4)
InSuicides$Month = NULL

### Add a time variable that is 1:length of data set see Bernal article
InSuicides$Time= 1:dim(InSuicides)[1]
dim(InSuicides)
head(InSuicides)
InSuicides$MonthNum = ifelse(InSuicides$MonthNum == "Jan", 1, ifelse(InSuicides$MonthNum == "Feb", 2, ifelse(InSuicides$MonthNum == "Mar", 3, ifelse(InSuicides$MonthNum=="Apr", 4, ifelse(InSuicides$MonthNum == "May", 5, ifelse(InSuicides$MonthNum == "Jun", 6, ifelse(InSuicides$MonthNum == "Jul", 7, ifelse(InSuicides$MonthNum == "Aug", 8, ifelse(InSuicides$MonthNum == "Sep", 9, ifelse(InSuicides$MonthNum == "Oct", 10, ifelse(InSuicides$MonthNum == "Nov", 11, ifelse(InSuicides$MonthNum == "Dec", 12, InSuicides$MonthNum))))))))))))

InSuicides$MonthNum = as.numeric(InSuicides$MonthNum)
describe(InSuicides$MonthNum)

InSuicides$Quarter = ifelse(InSuicides$MonthNum <= 3, 1, ifelse(InSuicides$MonthNum <= 6, 2, ifelse(InSuicides$MonthNum <= 9, 3, ifelse(InSuicides$MonthNum >= 10, 4, InSuicides$MonthNum))))


describe.factor(InSuicides$Quarter)
describe.factor(ITSTest$MonthNum)
head(InSuicides)
InSuicides[40:60,]
### Generate pre / post intervention variable for starts at 49 for the IN data set
InSuicides$Intervention = c(rep(0,49), rep(1, dim(InSuicides)[1]-49))
IN_TN_dat =  ITSTest[96:194,]
IN_TN_dat$State = rep("TN", dim(IN_TN_dat)[1])
InSuicides$State = rep("IN", dim(InSuicides)[1])
IN_TN_dat = rbind(IN_TN_dat, InSuicides)

```
N for state analysis
```{r}
head(IN_TN_dat)
dim(IN_TN_dat)
describe.factor(IN_TN_dat$State)
```


Combine the IN and TN data at the right time point
```{r}
compmeans(IN_TN_dat$Suicides, IN_TN_dat$State)
IN_TN_dat
model_in_tn = glm(Suicides ~ State*Intervention, family = "poisson", data = IN_TN_dat)
summary(model_in_tn)
exp(-0.3058)

model_in_tn_fac = glm(Suicides ~ State*Intervention + factor(Quarter), family = "poisson", data = IN_TN_dat)
summary(model_in_tn)

model_in_tn_nb = glm.nb(Suicides ~ State*Intervention, data = IN_TN_dat)
summary(model_in_tn_fac)

model_in_tn_harm = glm(Suicides ~ State*Intervention + harmonic(MonthNum,2,12),family = "poisson", data = IN_TN_dat)
summary(model_in_tn_harm)
```
Just try logistic regression 
```{r}
ITSTest$bin_Suicides = ifelse(ITSTest$Suicides > 0, 1, 0)

logit_suc_model = glm(bin_Suicides ~ Intervention, family = "binomial", data = ITSTest)
summary(logit_suc_model)

library(Zelig)
re_model = zelig(bin_Suicides ~ Intervention, model = "relogit", data = ITSTest)
summary(re_model)

```
