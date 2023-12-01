#============================================================================
# BANL 6100
# Project
# Movies Galore
# Fall 2023

setwd("~/arodsclass")
remove(list = ls())
graphics.off()
options(digits = 3, scipen = 9999)

library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)

#' I have assembled data on the top 100 Biggest Movie budgets and Performance
#' of all times.  I took it from:

url = "https://www.the-numbers.com/movie/budgets/all"

#' the dataset is attached: i called it my moviedata
#' i also created a subset: moviedata1. The subset contains sum of worldwide 
#' gross by year (YearGross); and it also contains number of films 
#' per year (NumberFilms).


#' 1. Show the Worldwide Year Gross per year in a ggplot barchart - properly labeled
#' 2. Show the Number of Films per year in a ggplot barchart or column chart
#' 3. Calculate a third variable PerFilm = YearGross/NumberFilms and display 
#'    in a barchart.
#'4.  Repeat the task in 1 above but only films from 2019 and after.
#'5.  2019 seems to be a banner year; repeat the task in 4 but only for the year
#'    2019.
#'6.  Provide a ggplot column chart listing the titles of the 2019 movies and
#'    their Worldwide Gross

#'7.  Provide a ggplot column chart listing the titles of movies from both 2015 
#'    2019 and their Worldwide Gross
#'     
#'8. The reported worldwide gross is in nominal dollars; use the CPI from FRED
#'    and convert the
#'   2015 and 2019 Worldwide Gross to 2023 dollars; call this new variable
#'   RealWorldwideGross.  Redo the graph in 7 above.
#'   
# ======================================  #  
library(readr)
moviedata1 <- read_csv("moviedata1.csv")
View(moviedata1)
# 1. Show the Worldwide Year Gross per year in a ggplot barchart - properly labeled
ggplot(moviedata1, aes(x = Year, y = YearGross)) +
  geom_bar(stat = "identity", fill = "olivedrab4") +
  labs(title = "Worldwide Year Gross per Year",
       x = "Year",
       y = "Worldwide Gross")

# 2. Show the Number of Films per year in a ggplot barchart or column chart
ggplot(moviedata1, aes(x = Year, y = NumberFilms)) +
  geom_bar(stat = "identity", fill = "springgreen4") +
  labs(title = "Number of Films per Year",
       x = "Year",
       y = "Number of Films")

# 3. Calculate a third variable PerFilm = YearGross / NumberFilms and display in a barchart
moviedata1$PerFilm <- moviedata1$YearGross / moviedata1$NumberFilms
ggplot(moviedata1, aes(x = Year, y = PerFilm)) +
  geom_bar(stat = "identity", fill = "darkseagreen3") +
  labs(title = "PerFilm (Year Gross per Film) per Year",
       x = "Year",
       y = "PerFilm")

# 4. Repeat the task in 1 above but only for films from 2019 and after.
ggplot(moviedata1[moviedata1$Year >= 2019, ], aes(x = Year, y = YearGross)) +
  geom_bar(stat = "identity", fill = "yellowgreen") +
  labs(title = "Worldwide Year Gross per Year (2019 and after)",
       x = "Year",
       y = "Worldwide Gross")

# 5. Repeat the task in 4 but only for the year 2019.
ggplot(moviedata1[moviedata1$Year == 2019, ], aes(x = Year, y = YearGross)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Worldwide Year Gross for 2019",
       x = "Year",
       y = "Worldwide Gross")

# 6. Provide a ggplot column chart listing the titles of the 2019 movies and their Worldwide Gross
library(readr)
moviedata <- read_csv("moviedata.csv")
View(moviedata)
movies_2019 <- moviedata[moviedata$Year == 2019, ]
ggplot(movies_2019, aes(x = Movie, y = WorldwideGross)) +
  geom_col(fill = "seagreen") +
  labs(title = "Worldwide Gross of 2019 Movies",
       x = "Movie Title",
       y = "Worldwide Gross") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Provide a ggplot column chart listing the titles of movies from both 2015 and 2019 and their Worldwide Gross
movies_2015_2019 <- moviedata[moviedata$Year %in% c(2015, 2019), ]
ggplot(movies_2015_2019, aes(x = Movie, y = WorldwideGross, fill = as.factor(Year))) +
  geom_col(position = "dodge") +
  labs(title = "Worldwide Gross of 2015 and 2019 Movies",
       x = "Movie Title",
       y = "Worldwide Gross") +
  theme(legend.title = element_blank())+coord_flip()

# 8. The reported worldwide gross is in nominal dollars; use the CPI from FRED to convert the 2015 and 2019 Worldwide Gross to 2023 dollars; call this new variable RealWorldwideGross and redo the graph in 7 above.
# You'll need to download CPI data, adjust the values, and perform the currency conversion, which may require additional code and data sources. You can use the "quantmod" package to get CPI data from FRED and then adjust the worldwide gross values to 2023 dollars before plotting.

library(readxl)
file_path <- "~/arodsclass/CPIAUCSL.xls"
cpi_data1 <- read_excel(file_path)
summary(cpi_data1)
#cpi_data1 = read.csv("CPIAUCSL.csv")
print(cpi_data1)

Cpiyear <- format(as.Date(cpi_data1$observation_date, format = "%m-%d-%Y"), "%Y")
#cpi_data1$YearNumeric <- as.numeric(format(as.Date(cpi_data1$DATE, format = "%m-%d-%Y"), "%Y"))
print(Cpiyear)
print(cpi_data1)

yearnumeric <- as.numeric(Cpiyear)

class(yearnumeric)
y<-yearnumeric

names(y) <- "y"
y

df<-data.frame(y=y)

merged_data<-cbind(cpi_data1,y)

merged_data

merged_data$CPICAUCSL <- merged_data$CPICAUCSL
class(merged_data)

cpi_2015 <- merged_data[merged_data$y == "2015", "CPIAUCSL"]
cpi_2019 <- merged_data[merged_data$y == "2019", "CPIAUCSL"]
cpi_2022 <- merged_data[merged_data$y == "2022", "CPIAUCSL"]

as.numeric(cpi_2015)
as.numeric(cpi_2019)
as.numeric(cpi_2022)

inflation_2015_to_2022 <- cpi_2022 / cpi_2015
inflation_2019_to_2022 <- cpi_2022 / cpi_2019

moviedata$RealWorldwideGross <-ifelse(moviedata$Year ==2015,
                                      moviedata$WorldwideGross*inflation_2015_to_2022,
                                      ifelse(moviedata$Year==2019,
                                             moviedata$WorldwideGross*inflation_2019_to_2022,moviedata$WorldwideGross))

filter7<- moviedata%>%filter(Year %in% c(2015, 2019))

ggplot(data = filter7, aes(x = reorder(Movie,RealWorldwideGross), y = RealWorldwideGross)) +
  geom_bar(stat = "identity", fill = "Purple") +
  labs(x = "Year", y = "Worldwide Gross in 2022") +
  ggtitle("Adjusted Worldwide Gross in 2022 By Year") +
  coord_flip()
#---------------------------------------------------------------------------------



#

