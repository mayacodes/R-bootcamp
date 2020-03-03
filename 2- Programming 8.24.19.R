# title: "Programming"
# author: "Maya Luetke"
# date: "July 3, 2019"
# last update: "July 3, 2019"

####--------------------------####
#Exercise. 
#Write a script to load the West Nile virus data and use ggplot to create a histogram 
#for the total number of cases in each state in each year. 
#Follow the format of the prototypical script advocated in the presentation: 
#Header, Load Packages, Declare Functions, Load Data, Perform Analysis.
####--------------------------####


#---------------------------------
#         set directory
#---------------------------------
getwd()
setwd("C:/Users/luetke/Downloads")
#---------------------------------


#---------------------------------
#         Import data
#---------------------------------
wnv <- read.csv('wnv.csv')
#---------------------------------


#---------------------------------
#         Look at data
#---------------------------------
names(wnv)
head(wnv)
wnv$Year
#---------------------------------


##################################
##################################
########     SCRIPTS    ##########
##################################
##################################


#---------------------------------
#         Histogram of WNV cases
#---------------------------------
#https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=Total), binwidth=20) + 
  labs(x='Total cases', y='Frequency', title='Distribution of WNV cases per year',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------


####--------------------------####
#Exercise. 
#The state-level and case burden is evidently highly skewed. 
#Plot a histogram for the logarithm of the number of cases. 
#Do this two different ways.
####--------------------------####


#---------------------------------
#         Histogram of natural log of WNV cases
#---------------------------------
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
#---------------------------------


####--------------------------####
#Exercise. 
#Use arithmetic operators to calculate the raw case fatality rate (CFR) 
#in each state in each year. Plot a histogram of the calcated CFRs. 
####--------------------------####


#---------------------------------
#         Histogram of the calcated CFRs
#---------------------------------
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
#---------------------------------


####--------------------------####
#Exercise. 
#Use arithmetic operators, logical operators, and the function sum to verify
#that the variable Total is simply the sum of the number of febrile cases, 
#neuroinvasive cases, and other cases.
####--------------------------####
names(wnv)
head(wnv)


####--------------------------####
#Exercise. 
#Use modular arithmetic to provide an annual case count for each state 
#rounded (down) to the nearest dozen. Use modular arithmetic to extract 
#the rounding errors associated with this calculate, then add the errors 
#to obtain the total error.
####--------------------------####
wnv$calc.cases <- wnv$EncephMen + wnv$Fever + wnv$Other
#round down to the nearest dozen
calc.cases.rd <- trunc(wnv$calc.cases/12) * 12
#calculate # of discrepancies
total.discrepancies <- sum(calc.cases.rd !=wnv$Total)
print(paste('Total discrepancies:', total.discrepancies))

#calculate # of discrepancies
error <- if calc.cases.rd !=wnv$Total
then:
  (calc.cases.rd-wnv$Total)


wnv$dozens <- wnv$Total %/% 12
wnv$errors <- wnv$Total %% 12

##################################
##################################
#########     PIPES    ###########
##################################
##################################


####--------------------------####
#Exercise. Write a function to calculate the mean and standard error 
#(standard deviation divided by the square root of the sample size) of the 
#neuroinvasive disease rate for all the states in a given list and given set
#of years. Follow the Google R style and remember to place the function near
#the top of your script. Use your function to calculate the average severe 
#disease rate in California, Colorado, and New York.
####--------------------------####


#mean and standard error 
nr.of.appearances <- aggregate(x = values, 
                               by = list(unique.values = values$value), 
                               FUN = length)



####--------------------------####
#Exercise. 
#Use ggplot to show the neurovinvasive disease rate for these states as a 
#bar graph with error bars to show the standard deviation.
####--------------------------####








####--------------------------####
#Exercise.
#Use your function and ggplot to show the neurovinvasive disease rate for 
#all states.
####--------------------------####




##################################
##################################
#####     CONTROL OF FLOW    #####
##################################
##################################


####--------------------------####
#Exercise. 
#Choose a longitude to designate the "center" of the country. Use the function 
#ifelse to assign each state to an "Eastern" region or a "Western" region.
####--------------------------####
hist(wnv$Longitude)
wnv$region <- ifelse(wnv$Longitude>=-98.5795,"E","W")
table(wnv$region)


####--------------------------####
#Exercise.
#Analyse your data to compare case fatality rates in the Eastern vs. Western 
#United States.
####--------------------------####
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

#---------------------------------






#Create empty variables
Year <- c(1999:2007)
state.number <- rep(NA,length(Year))
case.total <- rep(NA,length(Year))
fatal.total <- rep(NA,length(Year))
cfr <-rep(NA,length(Year))

for(i in c (1:length(Year))) {
  state.number[i] <- length(unique(wnv$State[wnv$Year==Year[i]]))
  case.total[i] <- sum (wnv$Total[wnv$Year== Year[i]])
  fatal.total[i] <- sum
  (wnv$Fatal[wnv$Year== Year[i]])
  cfr[i] <- mean(wnv$cfr[wnv$Year==Year[i]])
}

##################################
##################################
#######     USING HELP    ########
##################################
##################################


####--------------------------####
#Exercise.
#We may interpret raw case fatality rate (i.e. ratio of the number of deaths, x, to number 
#of infections, n) as a realization from a binomial process with n trials and x "successes" 
#generated by an unknown rate parameter p. This p may be the quantity truly of interest (for 
#instance, if we wish to ask if the case fatality rate in California is significantly 
#different from the case fatality rate in Colorado. In R, the estimated rate and its 
#confidence interval can be obtained using the function prop.test for testing equal 
#proportions. Use the help to determine the proper usage of prop.test and calculate 
#confidence intervals for the case fatality rates in all states for which there have been 
#reported cases of WNV.
####--------------------------####
table(wnv$Total)
table(wnv$State)
prop.test(wnv$Total, wnv$State)

table(wnv$Fatal)
names(wnv)

prop.test(x, n, p = case_fatality,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)


summarised_state_totals <- 
  wnv %>%
  group_by (State) %>%
  summarise(alive = sum(Total-Fatal), fatal = sum(Fatal))

prop.test <- prop.test(summarised_state_totals$fatal, summarised_state_totals$alive)
prop.test

#states have significantly different casefatality rates

####--------------------------####
#Exercise.
#The "See Also" section of the help for prop.test states that a different function might be 
#useful for an exact test of our hypotheses. Use the help to identify what this function is, 
#learn how to use it, and compare the differences.
####--------------------------####
?prop.test
?binom.test()
binom.test <- binom.test(summarised_state_totals$fatal, summarised_state_totals$alive)
binom.test

