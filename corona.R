library(dplyr)
library(tidyr)
library(ggplot2)

generate_days <- function(start_date, count)
{
    days <- format(seq(start_date, by="day", length.out=count), "%m/%d/%y")
    days<-gsub("0(\\d)", "\\1", days)
}

calculate_total <- function(df,days,country,row_name)
{
    total <- df[df$Country == country,]
    total <- colSums(total[days])
    total <- data.frame("Province/State"="","Country/Region"=row_name,Lat="",Long="",t(total),check.names=FALSE)
    rbind(df, total)
}

restructure <- function(df,days,name)
{
    # Convert dates into varibale
    df <- gather(df,all_of(days),key="date",value="number")

    # Convert dates from string to date type
    df$date <- as.POSIXct(strptime(df$date, format="%m/%d/%y"))

    # Prettyprint column names
    colnames(df) <- c("Province/State", "Country","Lat","Long","Date",name)
    df
}

calculate_growth <- function(dfhop,country)
{
    growth_rate = dfhop %>%
        arrange(Date) %>% filter(Country == country) %>%
        mutate(
            Growth_Recovered = Recovered - lag(Recovered),
            Rate_Recovered = (Growth_Recovered / 1)/Recovered * 100,
            Growth_Death = Death - lag(Death),
            Rate_Death = (Growth_Death / 1)/Death * 100,
            Growth_Confirmed = Confirmed - lag(Confirmed),
            Rate_Confirmed = (Growth_Confirmed / 1)/Confirmed * 100)

    growth_rate
}
# hopkins dataset
###https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv

confirmed <- "data/time_series_19-covid-Confirmed.csv"
death <- "data/time_series_19-covid-Deaths.csv"
recovered <- "data/time_series_19-covid-Recovered.csv"

days <- generate_days(as.POSIXct("2020-01-22"),53)

# Read in data
dfhop_confirmed <- read.table(confirmed, check.names=FALSE,  header=TRUE,sep=",", quote = "\"")
dfhop_death <- read.table(death, check.names=FALSE,  header=TRUE,sep=",", quote="\"")
dfhop_recovered <- read.table(recovered, check.names=FALSE,  header=TRUE,sep=",",quote = "\"")

dfhop_confirmed <- calculate_total(dfhop_confirmed,days, "US", "US Total")
dfhop_confirmed <- calculate_total(dfhop_confirmed,days, "China", "China Total")
dfhop_confirmed <- calculate_total(dfhop_confirmed,days, "Denmark", "Denmark Total")
dfhop_confirmed <- calculate_total(dfhop_confirmed,days, "France", "France Total")

dfhop_death <- calculate_total(dfhop_death,days, "US", "US Total")
dfhop_death <- calculate_total(dfhop_death,days, "China", "China Total")
dfhop_death <- calculate_total(dfhop_death,days, "Denmark", "Denmark Total")
dfhop_death <- calculate_total(dfhop_death,days, "France", "France Total")

dfhop_recovered <- calculate_total(dfhop_recovered,days, "US", "US Total")
dfhop_recovered <- calculate_total(dfhop_recovered,days, "China", "China Total")
dfhop_recovered <- calculate_total(dfhop_recovered,days, "Denmark", "Denmark Total")
dfhop_recovered <- calculate_total(dfhop_recovered,days, "France", "France Total")


dfhop_confirmed <- restructure(dfhop_confirmed,days,"Confirmed")
dfhop_death <- restructure(dfhop_death,days,"Death")
dfhop_recovered <- restructure(dfhop_recovered,days,"Recovered")

dfhop <- cbind(dfhop_confirmed,dfhop_death$Death)
dfhop <- cbind(dfhop,dfhop_recovered$Recovered)
colnames(dfhop)[7] <- "Death"
colnames(dfhop)[8] <- "Recovered"



dfhop_lastx <- dfhop[dfhop$Date > as.POSIXct(Sys.Date(),format="%m/%d/%y")
                     - as.difftime(15, unit = "days"),]

dfhop <- rbind(
    calculate_growth(dfhop,"Germany"),
    calculate_growth(dfhop,"Italy"),
    calculate_growth(dfhop,"Spain"),
    calculate_growth(dfhop,"UK"),
    calculate_growth(dfhop,"US Total"),
    calculate_growth(dfhop,"Korea, South"),
    calculate_growth(dfhop,"Sweden"),
    calculate_growth(dfhop,"Denmark Total"),
    calculate_growth(dfhop,"Norway"),
    calculate_growth(dfhop,"Netherlands"),
    calculate_growth(dfhop,"France Total"),
    calculate_growth(dfhop,"Iran (Islamic Republic of)"),
    calculate_growth(dfhop,"China Total")
      )

#moving average
dfhop_mean <- dfhop %>% filter(Country == "Korea, South") %>%
    mutate(Mean_Growth=rollapply(
               Growth_Confirmed,4,mean,align='right',fill=NA),
           Mean_Rate=rollapply(
               Rate_Confirmed,4,mean,align='right',fill=NA))

plot_mean <- ggplot(dfhop_mean, aes(x = Date, y = Mean_Rate)) +
    geom_line(aes(color=Country)) +
    geom_point()

plot_confirmed <- ggplot(dfhop, aes(x = Date, y = Confirmed)) +
    geom_line(aes(color=Country)) +
    geom_point()

plot_confirmed_growth <- ggplot(dfhop, aes(x = Date, y = Growth_Confirmed)) +
    geom_line(aes(color=Country)) +
    geom_point()


plot_death <- ggplot(dfhop, aes(x = Date, y = Death)) +
    geom_line(aes(color=Country)) +
    geom_point()

plot_death_growth <- ggplot(dfhop, aes(x = Date, y = Growth_Death)) +
    geom_line(aes(color=Country)) +
    geom_point()
