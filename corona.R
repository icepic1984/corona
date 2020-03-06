library(dplyr)
library(tidyr)
library(ggplot2)

# hopkins dataset
###https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

filename <- "data/time_series_19-covid-Confirmed_05_03_20.csv"
start_date <- as.POSIXct("2020-01-22")
number_of_days <- 44

# Read in data
dfhop <- read.table(filename, check.names=FALSE,  header=TRUE,sep=",")

# Generate timestamp
days <- format(seq(start_date, by="day", length.out=number_of_days), "%m/%d/%y")
days<-gsub("0(\\d)", "\\1", days)

# Convert dates into varibale 
dfhop <- gather(dfhop,all_of(days),key="date",value="number")

# Convert dates from string to date type
dfhop$date <- as.POSIXct(strptime(dfhop$date, format="%m/%d/%y"))

# Prettyprint column names
colnames(dfhop) <- c("Province/State", "Country","Lat","Long","Date","Number")


dfhop <- dfhop[dfhop$Country == "Germany" |
               dfhop$Country == "Italy"  |
               dfhop$Country == "South Korea" |
               dfhop$Country == "UK"  |
               dfhop$Country == "Iran" ,]

plot <- ggplot(dfhop, aes(x = Date, y = Number)) + geom_line(aes(color=Country)) + geom_point()
       
test <- ggplot(dfhop, aes(x = date, y = number, group=1)) + geom_line()+
  geom_point()
