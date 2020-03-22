library(dplyr)
library(tidyr)
library(ggplot2)
library(RCurl)

generate_days <-function(start, end)
{
    count <- as.numeric(end - start) + 1
    print(count)
    days <- format(seq(start, by="day", length.out=count), "%m/%d/%y")
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
            Growth_Rate = lag(Growth_Confirmed) / Growth_Confirmed,
            Rate_Confirmed = (Growth_Confirmed / 1)/Confirmed * 100)

    growth_rate
}

#confirmed <- "data/time_series_19-covid-Confirmed.csv"
#death <- "data/time_series_19-covid-Deaths.csv"
#recovered <- "data/time_series_19-covid-Recovered.csv"

confirmed <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
death <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
recovered <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

# Read in data
dfhop_confirmed <- read.table(text=confirmed, check.names=FALSE,  header=TRUE,sep=",", quote = "\"")
dfhop_death <- read.table(text=death, check.names=FALSE,  header=TRUE,sep=",", quote="\"")
dfhop_recovered <- read.table(text=recovered, check.names=FALSE,  header=TRUE,sep=",",quote = "\"")


#Generate list of days from fith column (start date) and last column name (end date)
start_day <- names(dfhop_confirmed)[5]
start_day <- as.POSIXct(strptime(start_day, format="%m/%d/%y"))
last_day <- names(dfhop_confirmed)[length(names(dfhop_confirmed))]
last_day <- as.POSIXct(strptime(last_day, format="%m/%d/%y"))
days <- generate_days(start_day, last_day)

dfhop_confirmed <- calculate_total(dfhop_confirmed,days, "US", "US Total")
dfhop_confirmed <- calculate_total(dfhop_confirmed,days, "China", "China Total")
dfhop_confirmed <- calculate_total(dfhop_confirmed,days, "Denmark", "Denmark Total")
dfhop_confirmed <- calculate_total(dfhop_confirmed,days, "France", "France Total")
dfhop_confirmed <- calculate_total(dfhop_confirmed,days, "Netherlands", "Netherlands Total")

# Set first day of china to zero in order to merge all countrys into one plot
dfhop_confirmed[dfhop_confirmed$Country == "China Total",]$"1/22/20" <- 0

dfhop_death <- calculate_total(dfhop_death,days, "US", "US Total")
dfhop_death <- calculate_total(dfhop_death,days, "China", "China Total")
dfhop_death <- calculate_total(dfhop_death,days, "Denmark", "Denmark Total")
dfhop_death <- calculate_total(dfhop_death,days, "France", "France Total")
dfhop_death <- calculate_total(dfhop_death,days, "Netherlands", "Netherlands Total")

dfhop_recovered <- calculate_total(dfhop_recovered,days, "US", "US Total")
dfhop_recovered <- calculate_total(dfhop_recovered,days, "China", "China Total")
dfhop_recovered <- calculate_total(dfhop_recovered,days, "Denmark", "Denmark Total")
dfhop_recovered <- calculate_total(dfhop_recovered,days, "France", "France Total")
dfhop_recovered <- calculate_total(dfhop_recovered,days, "Netherlands", "Netherlands Total")

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
    calculate_growth(dfhop,"Belgium"),
    calculate_growth(dfhop,"UK"),
    calculate_growth(dfhop,"US Total"),
    calculate_growth(dfhop,"Korea, South"),
    calculate_growth(dfhop,"Sweden"),
    calculate_growth(dfhop,"Denmark Total"),
    calculate_growth(dfhop,"Switzerland"),
    calculate_growth(dfhop,"Norway"),
    calculate_growth(dfhop,"Netherlands Total"),
    calculate_growth(dfhop,"France Total"),
    calculate_growth(dfhop,"Iran"),
    calculate_growth(dfhop,"China Total")
)

dfhop_histo <- dfhop %>%
    filter(#Country=="China Total" |
        Country == "Italy" |
        Country == "Spain" |
        Country == "China Total" |
        Country == "Germany") %>%
    select(Date,Country,Growth_Confirmed, Growth_Death)

#Change ordering of df
level.order <- c("Italy","Spain","Germany","China Total")
dfhop_histo <- dfhop_histo[order(match(dfhop_histo$Country,level.order)),]

p<-ggplot(dfhop_histo, aes(x=Date,y=Growth_Confirmed, fill=Country, color=Country, alpha=Country)) +
    geom_bar(stat="identity" , position="identity") +
    scale_colour_manual(values=c("Germany"="blue","Italy"="red","Spain"="green","China Total"="black")) +
    scale_fill_manual(values=c("Germany"="lightblue","Italy"="pink","Spain"="lightgreen","China Total"="gray")) +
    scale_alpha_manual(values=c("Germany"=0.1, "Italy"=1,"Spain"=0.5,"China Total"=0.7))    


p<-ggplot(dfhop_histo, aes(x=Date,y=Growth_Death, fill=Country, color=Country, alpha=Country)) +
    geom_bar(stat="identity" , position="identity") +
    scale_colour_manual(values=c("Germany"="blue","Italy"="red","Spain"="green","China Total"="black")) +
    scale_fill_manual(values=c("Germany"="lightblue","Italy"="pink","Spain"="lightgreen","China Total"="gray")) +
    scale_alpha_manual(values=c("Germany"=0.1, "Italy"=1,"Spain"=0.5,"China Total"=0.7))    


p<-ggplot(dfhop_histo, aes(x=Date)) +
    geom_bar(aes(y=Growth_Confirmed,fill=Country), alpha=c(0.3,0.9),stat="identity" , position="identity")
   # geom_bar(aes(y=Growth_Death,fill=Country),stat="identity",alpha=0.9)
    

p<-ggplot(dfhop_histo, aes(x=Date)) +
    geom_bar(aes(y=Growth_Confirmed,fill=Country),stat="identity",alpha=.3,fill='lightblue',color='lightblue4') +
    geom_bar(aes(y=Growth_Death,fill=Country),stat="identity",alpha=.3,fill='red',color='red')


#moving average
dfhop_mean <- dfhop %>% filter(Country == "Spain") %>%
    mutate(Mean_Growth=rollapply(
               Growth_Confirmed,4,mean,align='center',fill=NA),
           Mean_Rate=rollapply(
               Rate_Confirmed,4,mean,align='center',fill=NA))

plot_mean <- ggplot(dfhop_mean, aes(x = Date, y = Mean_Rate)) +
    geom_line(aes(color=Country)) +
    geom_point()


dfhop_growth_rate <- dfhop %>% filter(Country == "Germany")

dfhop_growth_rate <- dfhop_growth_rate  %>%
    mutate(Mean_Growth_Rate=rollapply(
               Growth_Rate,4,mean,align='center',fill=NA))


plot_growth_rate <- ggplot(dfhop_growth_rate, aes(x = Date, y = Mean_Growth_Rate)) +
    geom_line() +
    geom_point()

plot_confirmed <- ggplot(dfhop, aes(x = Date, y = Confirmed)) +
    geom_line(aes(color=Country)) +
    geom_point()

zero <- dfhop %>%
    filter(Confirmed < 100)  %>%
    group_by(Country) %>%
    mutate(id = 0,
           Confirmed = 0) 

dfhop_big <- dfhop %>%
    filter(Confirmed > 100) %>%
    group_by(Country) %>%
    mutate(id = row_number() + 1)


dfhop_big <- rbind(zero,dfhop_big)


base_breaks <- function(n = 10){
    function(x) {
        axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
}

plot_same_start <- ggplot(dfhop_big, aes(x = id, y = Confirmed)) +
    geom_line(aes(color=Country)) +
    geom_point() +
    scale_y_continuous(trans = 'log10', breaks = base_breaks(),
                        labels = prettyNum)  + 
    theme(panel.grid.minor = element_blank())

plot_same_start <- ggplot(dfhop_big, aes(x = id, y = Confirmed)) +
    geom_line(aes(color=Country)) +
    geom_point() +
    theme(panel.grid.minor = element_blank())

    ## scale_y_continuous(trans = log2_trans(),
    ##                     breaks = trans_breaks("log2", function(x) 2^x),
    ##                     labels = trans_format("log2", math_format(2^.x)))
#scale_y_continuous(trans='log10')



dfhop_filtered <- dfhop  %>% filter(Country == "Germany" |
                                    Country == "Spain" |
                                    Country == "Switzerland" |
                                    Country == "Norway" |
                                    Country == "France" |
                                    Country == "Netherlands Total" |
                                    Country == "France Total" |
                                    Country == "Belgium" |
                                    Country == "US Total")

plot_filtered <- ggplot(dfhop_filtered, aes(x = Date, y = Confirmed)) +
    geom_line(aes(color=Country)) +
    geom_point()


plot_death <- ggplot(dfhop, aes(x = Date, y = Death)) +
    geom_line(aes(color=Country)) +
    geom_point()

plot_death_growth <- ggplot(dfhop, aes(x = Date, y = Growth_Death)) +
    geom_line(aes(color=Country)) +
    geom_point()
