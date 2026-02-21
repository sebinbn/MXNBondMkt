# PURPOSE:
#   1. Import Bloomberg yield curve data and clean it.
#   2. Identify the first available date for each yield tenor.
#
# INPUT:  <data_path>/MXYC_1mo30y_PLAST.xlsx  (downloaded from Bloomberg)
# OUTPUT: BBYield (data frame), start_date_yield (data frame)
# CALLED BY: MXNBnd_Replicate.R


# 1. Import and Clean Bloomberg Yield Data ------------------------------------

# Row 1 of the Excel file contains Bloomberg metadata, so first row skipped.
BBYield <- read_xlsx(
  file.path(DATA_RAW, "MXYC_1mo30y_PLAST.xlsx"),
  skip = 1
)

BBYield$Date   <- as.Date(BBYield$Date)          # Convert POSIXct to Date
BBYield[, -1]  <- lapply(BBYield[, -1], as.numeric)

# Assign clean tenor names following the column order of raw data (1W, 1M, 3M, 6M, 1Y, ..., 30Y)
colnames(BBYield)[-1] <- c(
  "MXY01Y",  "MXY02Y",  "MXY03Y",  "MXY04Y",  "MXY05Y",  "MXY06Y",
  "MXY07Y",  "MXY08Y",  "MXY09Y",  "MXY10Y",  "MXY15Y",  "MXY20Y",
  "MXY30Y",  "MXY01M",  "MXY03M",  "MXY01W",  "MXY09M","MXY06M" )

BBYield <- BBYield[order(BBYield$Date), ]
# Note: NA values indicate days when the market was closed.


# 2. Identify Start Date for Each Yield Tenor ---------------------------------
# Used downstream for sample selection in estimation scripts.

#first_row stores row number of first value of each yield
first_row      <- apply(!is.na(BBYield[, -1]), 2, function(x) which(x)[1])

BBYield_startdate <- data.frame(
  Variable   = names(first_row),
  Start_Date = BBYield$Date[first_row]
)

rm(first_row)