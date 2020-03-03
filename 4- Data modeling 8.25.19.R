# title: "Data modeling"
# author: "Maya Luetke"
# date: "Aug 25, 2019"
# last update: "Aug 25, 2019"


#---------------------------------
#         set directory
#---------------------------------
getwd()
setwd("C:/Users/luetke/Downloads")
#---------------------------------


#---------------------------------
#         Load packages
#---------------------------------
#install.packages('tidyverse')
library(tidyverse)
#install.packages('magrittr')
library(magrittr)
#install.packages('GGally')
library(GGally)
#---------------------------------


#---------------------------------
#         Import data
#---------------------------------
# SOLUTION
load("get_LdPrismPop.Rda")
#or
read_csv("LdPrismPop.csv")
#---------------------------------


#---------------------------------
#         Data visualization
#---------------------------------
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","size","cases"))
#---------------------------------


#---------------------------------
#         Data visualization
#---------------------------------
ld.prism.pop %<>% mutate(log10size=log10(size))
ld.prism.pop %<>% mutate(log10cases=log10(cases+1))
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","log10size","log10cases"))
#---------------------------------


#---------------------------------
#         Simple linear model
#---------------------------------
set.seed(222)
smallData <- ld.prism.pop %>% sample_n(100)
plotSmall <- ggplot(smallData,aes(x=prcp,y=avtemp))+geom_point() 
plotSmall

#add a regression line
plotSmall + geom_smooth(method="lm")

#linear regression
lm_prcp_avtemp <- lm(avtemp ~ prcp, data = smallData)
summary(lm_prcp_avtemp)

summary(myModel)$coefficients[2,1] 
summary(myModel)$coefficients[2,4]

summary(lm_prcp_avtemp)$coefficients[2,c(1,4)]
#---------------------------------


#---------------------------------
#         The modelr package
#---------------------------------
ld.prism.pop %>% group_by(year) %>% summarise(total_pop=sum(size)) %>%
  ggplot() + 
  geom_point(aes(x=year,y=total_pop))
#---------------------------------


by_state <- ld.prism.pop %>% group_by(state)
by_state

by_state %<>% nest
by_state

by_state$data[[10]]

#Create function for simple linear regression
lin_reg_fxn <- function(df){
  lm(size ~ year, data = df)
}

#Create map --- make sure not using map from "maps" package
detach("package:maps", unload=TRUE)
library(tidyverse)
#install.packages("purrr")
library(purrr)
models <- purrr::map(by_state$data, lin_reg_fxn)
models

#install.packages("magrittr")
library(magrittr)
#install.packages("dplyr")
library(dplyr)
#Group by state
by_state %<>% mutate(model = map(data, lin_reg_fxn))


library(modelr)
by_state %<>% mutate(resids = map2(data, model, add_residuals))

#---------------------------------
#Task 14: Write a function that accepts an object of the type in the resids list, and returns 
#a sum of the absolute values, i.e. ignoring sign: abs(3)+abs(-2)=5. Use the function to add 
#a column called totalResid to by_state that provides the total size of residuals summed over 
#counties and years.
#---------------------------------

print(by_state$resids)

abs(-9)

#function
abs_sum_resids <- function(x){
  sum(abs(x$resid))
  }

names(by_state)

by_state %<>% mutate(asum_resids=map(resids,abs_sum_resids))

#code from solution
#sum_resids <- function(x){
#  sum(abs(x$resid))
#}
#by_state %<>% mutate(totalResid = map(resids,sum_resids))
#---------------------------------


#---------------------------------
#Task 15: Write a function that accepts a linear model and returns the slope (model M has slope 
#M$coefficients[2]) and then use this function to create a new column called slope in the by_state 
#data frame, that is the slope for each state.
#---------------------------------

m_slope <- function(m){
  m$coefficients[2]
}

by_state %<>% mutate(slope=map(model,m_slope))
print(by_state$slope)

#---------------------------------
#Task 16: Plot the growth rate (slope value) for all states.
#---------------------------------
names(by_state)
slopes <- unnest(by_state, slope)

#ugly plot
ggplot(slopes) + geom_point(aes(x=state,y=slope))

#pretty plot
ggplot(slopes) + geom_point(aes(x=state,y=slope)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#---------------------------------


#---------------------------------
#Task 17: Plot the total resisduals for all states.
##---------------------------------
totalResids <- unnest(by_state, asum_resids)

#ugly plot
ggplot(totalResids) + geom_point(aes(x=state,y=asum_resids))

#pretty plot
ggplot(totalResids) + geom_point(aes(x=state,y=asum_resids)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#---------------------------------


#---------------------------------
#Task 18: Repeat Tasks 9 and 10 using a different data frame name, by_state2.
#---------------------------------
names(by_state)

#ONE WAY TO DO IT
by_state <- ld.prism.pop %>% group_by(state)
by_state

by_state %<>% nest
by_state

#ANOTHER WAY TO DO IT
by_state2 <- ld.prism.pop %>% group_by(state) %>% nest
#---------------------------------


#---------------------------------
#Task 19: Write a function that accepts an element of the by_state2$data list-column and returns the 
#spearman correlation coefficient between Lyme disease cases and precipitation
#---------------------------------
?cor.test
names(by_state2)

by_state2 %>% cor.test(x, y,
         alternative = c("two.sided", "less", "greater"),
         method = c("pearson", "kendall", "spearman"),
         exact = NULL, conf.level = 0.95, continuity = FALSE, ...)


#BAD CODE
#corfxn <- function(df){
#  df %>% unnest(by_state, data) %>%
#    cor.test(prcp,cases) 
#}
#corfxn(by_state2)

corfxn <- function(df){
  suppressWarnings(cor.test(df$cases,df$prcp, method = "spearman")$estimate)
}

by_state2 %<>% mutate(Corr = map(data, corfxn))

corrs <- unnest(by_state2, Corr)

corrs %<>% arrange(desc(Corr))
print(corrs)

#relevel, reorder, unique --> makes them not automatically ordered
#NEEEED this so that can order by Y-axis
corrs$state <- factor(corrs$state, levels=unique(corrs$state))

#NOW PLOT
#ugly plot
ggplot(corrs) + geom_point(aes(x=state,y=Corr))

#pretty plot
ggplot(corrs) + geom_point(aes(x=state,y=Corr)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
