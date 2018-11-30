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
3. Add a time variable
4. Get descriptives: Counts per month, counts per year, plot over time
5. Add intervention variable
Baseline until January 2014, "Medication Only" protocol September 2015 

6. Evaluate the regular model for autocorrelation
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

Intervention= c(rep(0,144), rep(1,194-144))
length(Intervention)

ITSTest$Intervention = Intervention
head(ITSTest)
ITSTest[144:145,]


```
Just look at descirptives
```{r}
describe(ITSTest)
```


Get counts by month and year 
We are missing some months?  Why?

I want the total number of people who died by suicide for each month 
```{r}
sucByYear = aggregate(Suicides ~ Suicides + Year, data = ITSTest, sum)
sucByMonth = aggregate(Suicides ~ Suicides + MonthNum, data = ITSTest, sum)
sucByMonth = data.frame(sucByMonth)
plot(sucByMonth$Suicides, sucByMonth$MonthNum)
plot(sucByYear$Year, sucByYear$Suicides)

```
Testing autocorrelation and overdispersion
```{r}
modelP = glm(Suicides ~ Time*Intervention, family = "poisson", data = ITSTest)  
modelQP = glm(Suicides ~ Time*Intervention, family = "quasipoisson", data = ITSTest)  
summary(modelQP)


residModel1 = residuals(model1)
plot(residModel1, ITSTest$time)

mean(ITSTest$Suicides)
sd(ITSTest$Suicides)

acf(residModel1)
pacf(residModel1)


summary(model1)
```
```{r}
decompose_ITSTest_ts = decompose(ITSTest_ts, "additive")

plot(decompose_ITSTest_ts$trend)

adjustITSTest_ts = ITSTest_ts - decompose_ITSTest_ts$seasonal 
plot(adjustITSTest_ts)

ITSTest_ts[,3]
```
Here I am demonstrating how to identify what the frequency should be is you are unsure (I guess I should know, because it will likely be each month)???


