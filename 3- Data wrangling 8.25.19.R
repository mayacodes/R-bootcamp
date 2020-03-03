#---------------------------------------------------------
#         CODE 3: DATA WRANGLING
#---------------------------------------------------------
# title: "Data wrangling"
# author: "Maya Luetke"
# date: "August 25, 2019"
# last update: "August 25, 2019"
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
pop <- read.csv('pop.csv')
ld <- read.csv('lyme.csv')
prism <- read.csv('climate.csv')
#---------------------------------------------------------



#---------------------------------------------------------
#         Load packages
#---------------------------------------------------------
#install.packages("tidyverse")
library(tidyverse)
#install.packages("magrittr")
library(magrittr)
#install.packages("dplyr")
library(dplyr)
#install.packages("stringr")
library(stringr)
#install.packages("GGally")
library(GGally)
#install.packages("maptools")
library(maptools)
#install.packages("maptools")
library(maptools)
#install.packages("maps")
library(maps)
#install.packages("plotly")
library(plotly)
#---------------------------------------------------------



#---------------------------------------------------------
#         Look at data
#---------------------------------------------------------
names(pop)
head(pop)
print(pop)
#---------------------------------------------------------



#---------------------------------------------------------
#         Tidy data
#---------------------------------------------------------
#install.packages("dplyr")
library(dplyr)
#install.packages("stringr")
library(stringr)

library(dplyr)
pop %<>% select(fips,starts_with("pop2"))
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit
table(pop$str_year)
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))
pop %<>% mutate(year=as.integer(year))
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))
pop %<>% mutate(fips=as.integer(fips))
#---------------------------------------------------------



#---------------------------------------------------------
#         Look at data
#---------------------------------------------------------
names(ld)
head(ld)
print(ld)
#---------------------------------------------------------



#---------------------------------------------------------
#         Clean data
#---------------------------------------------------------
library(dplyr)
#ld %<>% select(STNAME, CTYNAME ,starts_with("Cases2"))
#ld %<>% gather(starts_with("Cases2"),key="str_year", value="CTYNAME") %>% na.omit
#ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))
#ld %<>% mutate(year=as.integer(year))
#ld %<>% mutate(fips=str_replace_all(fips,"^0",""))
#ld %<>% mutate(fips=as.integer(fips))

ld %<>% gather(starts_with("Cases"),key="str_year",value="cases") 
ld %<>% mutate(year=str_replace_all(str_year,"Cases","")) 
ld %<>% mutate(year=as.integer(year))
ld %<>% rename(state=STNAME,county=CTYNAME)
#---------------------------------------------------------



#---------------------------------------------------------
#         Pad fips with zeros
#---------------------------------------------------------
fips.builder<-function(st,ct){
  if (str_length(ct)==3){
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
}

ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE)) # takes about 10 seconds
ld %<>% select(-c(STCODE,CTYCODE,str_year))
#---------------------------------------------------------



#---------------------------------------------------------
#         Merge datasets
#---------------------------------------------------------
#mydata2 <- merge(ld,prism,by=c("fips","year"))
#mydata3 <- merge(mydata2,pop,by=c("fips","year"))

ld.prism <- inner_join(ld,prism,by=c("fips","year"))
ld.prism.pop<-inner_join(ld.prism,pop,by=c("fips","year"))
#ld.prism2 <- full_join(ld,prism, by=c("fips","year"))

#length(ld.prism$fips)
#length(ld.prism2$fips)
#length(mydata2$fips)
#---------------------------------------------------------



#---------------------------------------------------------
#         Getting summary info
#---------------------------------------------------------
#average cases per year
ld %>% ungroup %>% group_by(year) %>% summarise(avg_cases_year=mean(cases)) %>% 
    arrange(desc(avg_cases_year))
  
  
#average cases per state
ld %>% ungroup %>% group_by(state) %>% summarise(avg_cases_state=mean(cases)) %>% 
    arrange(desc(avg_cases_state))

#total cases per year
cases_by_year <- ld %>% ungroup %>% group_by(year) %>% 
    summarize(total=sum(cases)) %>% arrange(desc(total))
#---------------------------------------------------------



#---------------------------------------------------------
#         Save Rda data file
#---------------------------------------------------------
write_csv(ld.prism.pop,"LdPrismPop.csv")
save(ld.prism.pop,file="get_LdPrismPop.Rda")
#---------------------------------------------------------



#---------------------------------------------------------
#         Mapping
#---------------------------------------------------------
#get map data for US counties and states
county_map <- map_data("county")
state_map <- map_data("state")

#setting up map
ag.fips <- group_by(ld.prism.pop,fips) 
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
ld.16y<-left_join(select(ld.prism.pop,c(state,county,fips)),ld.16y)
ld.16y<-distinct(ld.16y)
ld.16y %<>% rename(region=state,subregion=county)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
ld.16y$region<-tolower(ld.16y$region)
ld.16y$subregion<-tolower(ld.16y$subregion)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," parish","")
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
map.ld.16y<-left_join(county_map,ld.16y)
#make map
ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) + 
  scale_colour_gradientn(colours=rev(rainbow(10)))

#setting up map
county_map <- map_data("county")
state_map <- map_data("state")
ag.fips <- group_by(ld.prism.pop,fips)
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
ld.16y<-left_join(select(ld.prism.pop,c(state,county,fips)),ld.16y)
ld.16y<-distinct(ld.16y)
ld.16y %<>% rename(region=state,subregion=county)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
ld.16y$region<-tolower(ld.16y$region)
ld.16y$subregion<-tolower(ld.16y$subregion)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," parish","")
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
map.ld.16y<-left_join(county_map,ld.16y)
#make map
ggplot2 <- ggplot(map.ld.16y)+geom_polygon(aes(long,lat,group=group,fill=log10cases),color="gray",lwd=0.2) +
  scale_fill_gradientn(colours=rev(heat.colors(10)))
ggplot2

#malke previous map interactive
ggplotly(ggplot2)

#make STATE map
county_map <- map_data("county")
state_map <- map_data("state")
ag.fips <- group_by(ld.prism.pop,fips)
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
ld.16y<-left_join(select(ld.prism.pop,c(state,county,fips)),ld.16y)
ld.16y<-distinct(ld.16y)
ld.16y %<>% rename(region=state,subregion=county)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
ld.16y$region<-tolower(ld.16y$region)
ld.16y$subregion<-tolower(ld.16y$subregion)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," parish","")
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
map.ld.16y<-left_join(state_map,ld.16y)

ggplot4 <- ggplot(map.ld.16y)+geom_polygon(aes(long,lat,group=group,fill=log10cases),color="gray",lwd=0.2) +
  scale_fill_gradientn(colours=rev(heat.colors(10)))
ggplot4

#Map figure
p <- ggplot(data = map,
            aes(x = long, y = lat,
                group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)

ggplotly(ggplot2)
#---------------------------------------------------------
