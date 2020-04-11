library(tidyverse)
library(dplyr)
library(lubridate)
library(forcats)

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
dat$Month = month(dat$Date, label = TRUE)
dat$Day = day(dat$Date)
dat$Year = year(dat$Date)
dat$`Category Name` = as.factor(dat$`Category Name`)
dat$`Category Name` = dat$`Category Name` %>%
  fct_collapse("American Distilled Spirit Specialty" = c("American Distilled Spirit Specialty","American Distilled Spirits Specialty")) %>%
  fct_collapse("American Vodkas" = c("American Vodka","American Vodkas")) %>%
  fct_collapse("Cocktails/RTD" = c("Cocktails / RTD","Cocktails /RTD")) %>%
  fct_collapse("Imported Cordials & Liqueurs" = c("Imported Cordials & Liqueur","Imported Cordials & Liqueurs")) %>%
  fct_collapse("Imported Distilled Spirit Specialty" = c("Imported Distilled Spirit Specialty","Imported Distilled Spirits Specialty")) %>%
  fct_collapse("Imported Vodkas" = c("Imported Vodka","Imported Vodkas")) %>%
  fct_collapse("Temporary & Specialty Packages" = c("Temporary &  Specialty Packages","Temporary & Specialty Packages"))


#3
dat %>%
  ggplot(aes(x=Longitude, y = Latitude)) + geom_point() + labs(title= "2019 Liquor Store locations in Ames, IA", x = "Longitude (Degrees)", y = "Latitude (Degrees)")
dat %>%
  drop_na(`Category Name`) %>%
  group_by(`Category Name`) %>%
  summarize("Volume Sold (Gallons)" = sum(`Volume Sold (Gallons)`)) %>%
  ggplot(aes(y = `Volume Sold (Gallons)`, x = fct_rev(`Category Name`))) + geom_col() + coord_flip() + labs(x = 'Liquor Category', title = '2019 Liquor Sales in Ames, IA ')

#4
dat %>%
  group_by(Date) %>%
  summarize("Number of Sales" = n(),
            "Volume Sold (Gallons)" = sum(`Volume Sold (Gallons)`),
            "Amount of Money Spent" = sum(`Sale (Dollars)`))

#5
dat %>%
  group_by(Date) %>%
  summarize("Volume Sold (Gallons)" = sum(`Volume Sold (Gallons)`)) %>%
  ungroup() %>%
  complete(Date = seq.Date(mdy('01-01-2019'), max(Date), by="day")) %>%
  mutate(`Volume Sold (Gallons)` = replace_na(`Volume Sold (Gallons)`, 0)) %>%
  ggplot(aes(x= day(Date), y = `Volume Sold (Gallons)`)) + facet_wrap(~month(Date, label = TRUE)) + geom_smooth(span = 0.22) + labs(x = "Day of Month", title = "2019 Liquor Sales in Ames, IA")
