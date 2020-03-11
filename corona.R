library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)

# hopkins dataset
###https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

filename <- "data/time_series_19-covid-Confirmed_10_03_20.csv"
start_date <- as.POSIXct("2020-01-22")
number_of_days <- 49

# Generate timestamp
days <- format(seq(start_date, by="day", length.out=number_of_days), "%m/%d/%y")
days<-gsub("0(\\d)", "\\1", days)

# Read in data
dfhop <- read.table(filename, check.names=FALSE,  header=TRUE,sep=",")

# Add new row with total cases in us
us_total <- dfhop[dfhop$Country == "US",]
us_total <- colSums(us_total[days])
us_total <- data.frame("Province/State"="","Country/Region"="US Total",Lat="",Long="",t(us_total),check.names=FALSE)
dfhop <- rbind(dfhop, us_total)

china_total <- dfhop[dfhop$Country == "Mainland China",]
china_total <- colSums(china_total[days])
china_total <- data.frame("Province/State"="","Country/Region"="China Total",Lat="",Long="",t(china_total),check.names=FALSE)
dfhop <- rbind(dfhop, china_total)



# Convert dates into varibale
dfhop <- gather(dfhop,all_of(days),key="date",value="number")

# Convert dates from string to date type
dfhop$date <- as.POSIXct(strptime(dfhop$date, format="%m/%d/%y"))

# Prettyprint column names
colnames(dfhop) <- c("Province/State", "Country","Lat","Long","Date","Number")


dfhop <- dfhop[dfhop$Country == "Germany"
             |  dfhop$Country == "Italy"
             |  dfhop$Country == "Spain"
             |  dfhop$Country == "France"
             |  dfhop$Country == "Austria"
             |  dfhop$Country == "South Korea"
             |  dfhop$Country == "UK"
             |  dfhop$Country == "US Total"
             |  dfhop$Country == "China Total"
             |  dfhop$Country == "Iran"
              ,]


dfhop_lastx <- dfhop[dfhop$Date > as.POSIXct(Sys.Date(),format="%m/%d/%y")
                     - as.difftime(15, unit = "days"),]

growth_rate = dfhop %>%
  # first sort by year
  arrange(Date) %>% filter(Country == "Germany") %>%
  mutate(Diff_Date = Date - lag(Date),  
         Diff_Growth = Number - lag(Number),
         Rate_percent = (Diff_Growth / 1)/Number * 100) 

growth_rate <- rbind(growth_rate,dfhop %>%
  arrange(Date) %>% filter(Country == "Italy") %>%
  mutate(Diff_Date = Date - lag(Date), 
         Diff_Growth = Number - lag(Number),
         Rate_percent = (Diff_Growth / 1)/Number * 100))

growth_rate <- rbind(growth_rate,dfhop %>%
  arrange(Date) %>% filter(Country == "France") %>%
  mutate(Diff_Date = Date - lag(Date), 
         Diff_Growth = Number - lag(Number),
         Rate_percent = (Diff_Growth / 1)/Number * 100))

growth_rate <- rbind(growth_rate,dfhop %>%
  arrange(Date) %>% filter(Country == "US Total") %>%
  mutate(Diff_Date = Date - lag(Date), 
         Diff_Growth = Number - lag(Number),
         Rate_percent = (Diff_Growth / 1)/Number * 100))

growth_rate <- rbind(growth_rate,dfhop %>%
  arrange(Date) %>% filter(Country == "Spain") %>%
  mutate(Diff_Date = Date - lag(Date), 
         Diff_Growth = Number - lag(Number),
         Rate_percent = (Diff_Growth / 1)/Number * 100))


plot <- ggplot(dfhop, aes(x = Date, y = Number)) +
    geom_line(aes(color=Country)) +
    geom_point()

plot_lastx <- ggplot(dfhop_lastx, aes(x = Date, y = Number)) +
    geom_line(aes(color=Country)) +
    geom_point()


plot_growth <- ggplot(growth_rate, aes(x = Date, y = Diff_Growth)) +
    geom_line(aes(color=Country)) +
    geom_point()
