#R Programming
#Fall 23
# READING DATA =====================

remove(list = ls())  # clear the workspace
graphics.off()
options(digits = 3, scipen = 9999)
#setwd("C:/Users/arodriguez/Dropbox/classes/BANL6100_Fall23/Class2_AlmanacPlus")
setwd("~/arodsclass")

##installing and loading packages ================
#install.packages("pacman")
library(pacman)

#Alternatively
if (!"pacman" %in% installed.packages()) install.packages("pacman")
# install.packages("pacman")  # Pacman is a package management tool 
library(pacman)

p_load(tidyverse, rio, pdfetch,GGally, janitor, pdfetch)
# ----------------------------------------#
# Read Data packed with R packages ==============      
library(datasets)
data(package = "datasets")

data(faithful)
faithful
dim(faithful)
? faithful
head(faithful)

str(faithful)  
summary(faithful)

#Select the variable waiting alone.

mean(faithful$eruptions); median(faithful$eruptions); sd(faithful$eruptions)  # mean median and standard deviation
colSums(is.na(faithful)) # data missing? everytime there is missing data in R it records with na
mean(faithful$eruptions, na.rm = T)  # remove any NA values, if applicable

iqr = IQR(faithful$eruptions)  # the interquartile range
iqr
boxplot(faithful$eruptions)

max_eruptions = max(faithful$eruptions)
min_eruptions = min(faithful$eruptions)
range_eruptions = max_eruptions - min_eruptions
range_eruptions  # the range

#' 
#' SUMMARIZING DATA USING HISTOGRAMS
#' 
#' A histogram is a commonly used tool to visualize data.  
#' Simply put, a histogram tells you how many data points fall in each 
#' particular range, or segment, or "bucket" of your data.  
#' For example, consider eruptions. 
#' 
#' A histogram created from the eruptions might show the number (or frequency) 
#' of eruptions occurring with the 2 to 2.5 minute range, the 4 to 4.5 range, 
#' the 4.5 to 5 range, and so on.  The ranges in which you group data are 
#' referred to as bin ranges. 
#' 

hist(faithful$eruptions)
plot(faithful$eruptions~ faithful$waiting)
hist(faithful$waiting)

#' Skewness and Kurtosis
#' Some distributions look a little "off-center" or a little "squat" relative to 
#' the bell cure - the standard normal distribution.  

#' What we call "skew" is the 
#' metric that measure the symmetry of the distribution - and allows us to 
#' establish how much it is off-center.  Thus, a distribution can display 
#' either positive or negative skew.  The "direction" of the skew refers to the 
#' direction of the longer tail, not to where the bulk of the data are located.  
#' A curve has a skew of "0" if it is symmetric. 

#' Kurtosis refers to the "peakedness" of the distribution - that is, 
#' how "squat" or "peaked" it is relative to the normal distribution.  
#' 

# CORRELATION ===================== =
#' 
#' The correlation between any pair of variables can provide insights into how 
#' the two variables move up and down in value together.  
#' And although correlation may not be causation - 
#' the reverse is necessarily true.  
#' 
#' The correlation between variables X and Y is a unit-free measure of 
#' the strength of a linear-relationship between X and Y.  
#' It is unit free because we don't want the metric to vary depending on 
#' whether the series is measured in Yen or in Dollars.  
#' Or whether they are measure in wages or hours.
#' 
#' The correlation ranges between -1 and +1.  
#' 
#' A correlation near +1 means that x and y have a strong positive 
#' linear relationship.  
#'

cor(faithful$eruptions, faithful$waiting)

#' A correlation near zero means that X and Y have a weak linear relationship.
#' 
GGally::ggpairs(faithful)
#' 
