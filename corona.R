library(dplyr)
library(tidyr)
library(ggplot2)

setwd("c:/Projects/R")

# Read in data
df <- read.delim("corona.dat", header=TRUE,sep=";")
# Convert date from string to date type
df$date <- as.POSIXct(strptime(df$date, format="%Y-%m-%d %H:%M"))
df$bins <- cut(df$date, breaks="1 day")

df %>% group_by(bins) %>% summarise(max = max(number))

test <- ggplot(df, aes(x = date, y = number, group=1)) +  geom_line()+
  geom_point()

# hopkins dataset
###https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

days <- c(
    "1/22/20",       
    "1/23/20",
    "1/24/20",
    "1/25/20",
    "1/26/20",       
    "1/27/20",
    "1/28/20",
    "1/29/20",
    "1/30/20",       
    "1/31/20",
    "2/1/20" ,
    "2/2/20" ,
    "2/3/20" ,       
    "2/4/20" ,
    "2/5/20" ,
    "2/6/20" ,
    "2/7/20" ,       
    "2/8/20" ,
    "2/9/20" ,
    "2/10/20",
    "2/11/20",       
    "2/12/20",
    "2/13/20",
    "2/14/20",
    "2/15/20",       
    "2/16/20",
    "2/17/20",
    "2/18/20",
    "2/19/20",       
    "2/20/20",
    "2/21/20",
    "2/22/20",
    "2/23/20",       
    "2/24/20",
    "2/25/20",
    "2/26/20",
    "2/27/20",       
    "2/28/20",
    "2/29/20",
    "3/1/20" ,
    "3/2/20" ,       
    "3/3/20" ,
    "3/4/20",
    "3/5/20")

dfhop <- read.table("time_series_19-covid-Confirmed_06_03.csv", check.names=FALSE,  header=TRUE,sep=",")
dfhop <- gather(dfhop,all_of(days),key="date",value="number")
dfhop$date <- as.POSIXct(strptime(dfhop$date, format="%m/%d/%y"))



dfhop <- dfhop[dfhop$"Country/Region" == "Germany"
              & dfhop$"Country/Region" == "Italy",]
test <- ggplot(dfhop, aes(x = date, y = number, group=1)) + geom_line()+
  geom_point()
