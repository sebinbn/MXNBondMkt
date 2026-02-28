# Loading required packages

library(readxl)       #for importing data from xlsx
library(reshape2)     #for using melt() which converts data to long format convenient for ggplot
library(ggplot2)      #for plots
library(zoo)          #for na.approx() used in 1_4_dailyDataCreate.R
library(urca)         #for ur.df used in 2_1_ADFTest.R

message("Necessary packages loaded into enviroment")
