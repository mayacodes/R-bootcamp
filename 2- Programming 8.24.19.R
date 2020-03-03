#---------------------------------------------------------
#         CODE 2: PROGRAMMING
#---------------------------------------------------------
# title: "Programming"
# author: "Maya Luetke"
# date: "August 24, 2019"
# last update: "August 24, 2019"
#---------------------------------------------------------



#---------------------------------------------------------
#         Set directory
#---------------------------------------------------------
getwd()
setwd("C:/Users/luetke/Downloads")
#---------------------------------------------------------



#---------------------------------------------------------
#         Import data
#---------------------------------------------------------
wnv <- read.csv('wnv.csv')
#---------------------------------------------------------



#---------------------------------------------------------
#         Look at data
#---------------------------------------------------------
names(wnv)
head(wnv)
wnv$Year
#---------------------------------------------------------



#---------------------------------------------------------
#         Histogram of WNV cases
#---------------------------------------------------------
#https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=Total), binwidth=20) + 
  labs(x='Total cases', y='Frequency', title='Distribution of WNV cases per year',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------



#---------------------------------------------------------
#         Histogram of natural log of WNV cases
#---------------------------------------------------------
#WAY ONE
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=log(Total))) + 
  labs(x='Log of total cases', y='Frequency', title='Distribution of WNV cases per year',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#WAY TWO
str(wnv$Total)
wnv$total_log <- log(wnv$Total)
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=total_log)) + 
  labs(x='Log of total cases', y='Frequency', title='Distribution of WNV cases per year',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------



#---------------------------------------------------------
#         Histogram of the calcated CFRs
#---------------------------------------------------------
wnv$case_fatality <- wnv$Fatal/wnv$Total
library(ggplot2)
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=case_fatality)) + 
  labs(x='Case fatality', y='Frequency', title='Distribution of WNV case fatality',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#wnv$State
#install.packages("dplyr")
library(dplyr)
#install.packages("magrittr")
library(magrittr)

library(dplyr)
wnv %>%
  mutate(cfr=Fatal/Total) %>%
  ggplot() +
  geom_histogram(mapping=aes(x=cfr)) + 
  labs(x='Case fatality', y='Frequency', title='Distribution of WNV case fatality',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------



#---------------------------------------------------------
#         Calc annual casecount (round down to nearest dozen) #calculate errors in roudning
#---------------------------------------------------------
names(wnv)
head(wnv)
wnv$calc.cases <- wnv$EncephMen + wnv$Fever + wnv$Other
#round down to the nearest dozen
calc.cases.rd <- trunc(wnv$calc.cases/12) * 12
#calculate # of discrepancies
total.discrepancies <- sum(calc.cases.rd !=wnv$Total)
print(paste('Total discrepancies:', total.discrepancies))

wnv$dozens <- wnv$Total %/% 12
wnv$errors <- wnv$Total %% 12
#---------------------------------------------------------



#---------------------------------------------------------
#         Comparing case fatality rates in the E vs. W US
#---------------------------------------------------------
library(ggplot2)

ggplot(data=wnv, mapping=aes(x=region, y=case_fatality)) + 
  geom_point() + 
  labs(x='region', y='case_fatality',
       title='WNV case fatality rates by region')

ggplot(data=wnv, mapping=aes(x=Longitude, y=case_fatality)) + 
  geom_point() + 
  geom_smooth() +
  labs(x='Longitude', y='case_fatality',
       title='WNV case fatality rates by longitude')

#GOOD ONE
ggplot(data=wnv, mapping=aes(x=Longitude, y=case_fatality)) + 
  geom_point(mapping = aes(color=region)) + 
  geom_smooth() +
  labs(x='Longitude', y='case_fatality',
       title='WNV case fatality rates by longitude')

#OR... add state?
ggplot(data=wnv, mapping=aes(x=Longitude, y=case_fatality)) + 
  geom_point(mapping = aes(color=State)) + 
  geom_smooth() +
  labs(x='Longitude', y='case_fatality',
       title='WNV case fatality rates by longitude')

#ORRRR FREQUENCY
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=case_fatality, color=State)) + 
  labs(x='Case fatality rate', y='Frequency', title='Distribution of WNV cases per year')

#WAY TWO
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=case_fatality)) + 
  facet_grid(case_fatality ~ region) + 
  labs(x='Case fatality', y='Frequency', title='Distribution of WNV case fatality',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------

