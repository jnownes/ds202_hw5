library(tidyr)
library(lubridate)

#1 and 2
dat = readr::read_csv("2019_Iowa_Liquor_Sales.csv")
dat = dat %>%
  separate(col="Store Location", into=c("Point", "Longitude", "Latitude"), sep=" ") %>%
  subset(select = -c(Point))
dat$Longitude = gsub("\\(","",dat$Longitude)
dat$Latitude = gsub("\\)","",dat$Latitude)
dat$Longitude = as.numeric(dat$Longitude)
dat$Latitude = as.numeric(dat$Latitude)
dat$Date = mdy(dat$Date)
dat$Month = month(dat$Date)
dat$Day = day(dat$Date)
dat$Year = year(dat$Date)