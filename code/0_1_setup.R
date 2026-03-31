# Loading required packages


if (!requireNamespace("tvReg", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  library(remotes)      #for installing an R package from Github
  remotes::install_github("sebinbn/tvReg")
}

library(readxl)       #for importing data from xlsx
library(lubridate)    #for adding dates by a year/month
library(dplyr)        #used for group_by(), filter(), arrange()
library(reshape2)     #for using melt() which converts data to long format convenient for ggplot
library(ggplot2)      #for plots
library(zoo)          #for na.approx() used in 1_4_dailyDataCreate.R
library(urca)         #for ur.df used in 2_1_ADFTest.R
library(systemfit)    #for SUR estimation using systemfit()
library(tvReg)        #for tvvar and tvirf


message("Necessary packages loaded into enviroment")
