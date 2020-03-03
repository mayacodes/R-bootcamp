#---------------------------------------------------------
#         CODE 1: DATA VISUALIZATION
#---------------------------------------------------------
# title: "Programming"
# author: "Maya Luetke"
# date: "August 24, 2019"
# last update: "August 24, 2019"
#---------------------------------------------------------



#---------------------------------------------------------
#         set directory
#---------------------------------------------------------
getwd()
setwd("C:/Users/luetke/Downloads")
#---------------------------------------------------------



#---------------------------------------------------------
#         Import data
#---------------------------------------------------------
mers <- read.csv('cases.csv')
#---------------------------------------------------------



#---------------------------------------------------------
#         Data checks
#---------------------------------------------------------
head(mers)
class(mers$onset)

mers$hospitalized[890] <- c('2015-02-20')
mers <- mers[-471,]
#---------------------------------------------------------



#---------------------------------------------------------
#         Convert factor to date
#---------------------------------------------------------
#change to date format
#install.packages("lubridate")
library(lubridate)
mers$onset2 <- ymd(mers$onset)
#---------------------------------------------------------



#---------------------------------------------------------
#         Recode errors
#---------------------------------------------------------
mers$hospitalized[mers$hospitalized=="???"] <- NA
mers$hospitalized[mers$hospitalized=="3/5/14, 5/5/14"] <- NA

which(mers$hospitalized != is.date)
which(!is.Date(mers$hospitalized))
#---------------------------------------------------------



#---------------------------------------------------------
#         Convert factor to date
#---------------------------------------------------------
#change to date format
library(lubridate)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)
#---------------------------------------------------------



#---------------------------------------------------------
#         Create some dates
#---------------------------------------------------------
#find earliest date
day0 <- min(na.omit(mers$onset2))

#create epidemic date
mers$epi.day <- as.numeric(mers$onset2 - day0)
#---------------------------------------------------------



#---------------------------------------------------------
#         Plotting
#---------------------------------------------------------
#install.packages("ggplot2")
library(ggplot2)
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------



#---------------------------------------------------------
#         Plotting
#---------------------------------------------------------
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#add position="fill"
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country), position="fill") +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")


#add coord_flip()
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") + 
  coord_flip() 


#add coord_flip() and coord_polar()
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") + 
  coord_flip() + coord_polar()
#---------------------------------------------------------



#---------------------------------------------------------
#         Create new variable
#---------------------------------------------------------
mers$infectious.period <- mers$hospitalized2-mers$onset2    # calculate "raw" infectious period
class(mers$infectious.period)           # these data are class "difftime"
mers$infectious.period <- as.numeric(mers$infectious.period, units = "days") # convert to days
#---------------------------------------------------------



#---------------------------------------------------------
#         Univariate plot
#---------------------------------------------------------
#https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period)) + 
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------



#---------------------------------------------------------
#         Recode for the cases that occurred BEFORE hospitalization
#---------------------------------------------------------
mers$infectious.period2 <- ifelse(mers$infectious.period<0,0,mers$infectious.period)
#---------------------------------------------------------



#---------------------------------------------------------
#         Try univariate plots again
#---------------------------------------------------------
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2)) + 
  labs(x='Infectious period', y='Frequency',
       title='Distribution of calculated MERS infectious period (positive values only)', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------



#---------------------------------------------------------
#         Explore frequency of hospital acquired infections
#---------------------------------------------------------
ggplot(data=mers) + 
  geom_density(mapping=aes(x=infectious.period2)) + 
  labs(x='Infectious period', y='Frequency',
       title='Probability density for MERS infectious period (positive values only)', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------



#---------------------------------------------------------
#         Explore frequency of hospital acquired infections wiht AREA under curve
#---------------------------------------------------------
ggplot(data=mers) + 
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------



#---------------------------------------------------------
#         Explore geom_dotplot and geom_bar.
#---------------------------------------------------------
#geom_dotplot
ggplot(data=mers) + 
  geom_dotplot(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#geom_bar
ggplot(data=mers) + 
  geom_bar(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------



#---------------------------------------------------------
#         Delete outlier for following graphs
#---------------------------------------------------------
#delete outlier
mers$infectious.period2[mers$infectious.period2>150] <- NA
#---------------------------------------------------------



#---------------------------------------------------------
#         Bivariate plots
#---------------------------------------------------------
library(ggplot2)
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, y=infectious.period2)) +
  labs(x='Epidemic day', y='Infectious period', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 

ggplot(data=mers) + 
  geom_point(aes(epi.day,infectious.period2)) +
  labs(x='Time', y='Infectious period',
       title='Plot for MERS infectious period over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) + 
  geom_point(aes(x=epi.day,y=infectious.period2)) +
  labs(x='Time', y='Infectious period',
       title='Plot for MERS infectious period over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(mers, aes(x=epi.day,y=infectious.period2)) + geom_line() +
  labs(x='Time', y='Infectious period', title='Plot for MERS infectious period over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(mers, aes(x=epi.day,y=infectious.period2)) + geom_line() +
  labs(x='Time', y='Infectious period', title='Plot for MERS infectious period over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")


# Add the regression line
ggplot(mers, aes(x=epi.day,y=infectious.period2)) + 
  geom_point()+
  geom_smooth(method=lm)

# Remove the confidence interval
ggplot(mers, aes(x=epi.day,y=infectious.period2)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

# Loess method
ggplot(mers, aes(x=epi.day,y=infectious.period2)) + 
  geom_point()+
  geom_smooth()
#---------------------------------------------------------

# Change point shapes, colors and sizes
ggplot(mers, aes(x=epi.day,y=infectious.period2, color=country, scale_y_continuous())) +
  geom_point() +
  geom_smooth(method=loess, scale_y_continuous())

ggplot(mers, aes(x=epi.day,y=infectious.period2, color = country)) +
  geom_line(size = 1) + 
  #geom_ribbon(aes(ymin = mers$CI.lower, ymax = mers$CI.upper, fill = country)) +
  scale_x_continuous(breaks = c(1:12,1)) + 
  facet_grid(. ~ avg.sh.aud.by.reg.month$year)

ggplot(mers, aes(x=epi.day,y=infectious.period2, color=country)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=country))
#---------------------------------------------------------



#---------------------------------------------------------
#         Faceting     
#---------------------------------------------------------
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) + 
  geom_point(mapping = aes(color=country)) +
  facet_wrap(~ country) + 
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period (positive values only) over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------



#---------------------------------------------------------
#         Recode gender values    
#---------------------------------------------------------
table(mers$gender)
mers$gender[mers$gender=="?"] <- NA
mers$gender[mers$gender=="M?"] <- NA
table(mers$gender2)
#---------------------------------------------------------



#---------------------------------------------------------
#         More faceting     
#---------------------------------------------------------
ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea','UAE')), mapping=aes(x=epi.day, y=infectious.period2)) + 
  geom_point(mapping = aes(color=country)) +
  facet_grid(gender ~ country) + 
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period by gender and country', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------



#---------------------------------------------------------
#         Exercise. Study variation in the case fatality rate (the fraction of cases that end in death) over time and across countries.     
#---------------------------------------------------------
install.packages("ggplot2-exts")
library(ggplot2-exts)

names(mers)

table(mers$outcome)
outcome
mers$fatal <- ifelse(mers$outcome)
#case-fatality rate
mers$death
merse$CFR <- mers$death/mers$death

ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea','UAE')), mapping=aes(x=epi.day, y=infectious.period2)) + 
  geom_point(mapping = aes(color=country)) +
  facet_grid(gender ~ country) + 
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period by gender and country', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#---------------------------------------------------------



#---------------------------------------------------------
#         Make plots interactive using plotly (#ggsave)
#---------------------------------------------------------
#install.packages("plotly")
library(plotly)
### EPI CURVE INTERACTIVE PLOT ###
epi.curve <- ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplotly(epi.curve)

### INFECTIOUS PERIOD INTERACTIVE PLOT ###
infectious_plot <- ggplot(mers, aes(x=epi.day,y=infectious.period2, color=country)) +
  geom_point() + 
  geom_smooth(method=loess)
ggplotly(infectious_plot)
#---------------------------------------------------------
