# PURPOSE:
#   1. Import Bloomberg yield curve data and clean it.
#   2. Identify the first available date for each yield tenor.
#
# INPUT: DATA_RAW/MXYC_1mo30y_PLAST.xlsx  (downloaded from Bloomberg)
# OUTPUT: Yield_Data (data frame), Yield_Data_startdate (data frame)
# CALLED BY: MXNBnd_Replicate.R


# 1. Import and Clean Bloomberg Yield Data ------------------------------------

# Row 1 of the Excel file contains Bloomberg metadata, so first row skipped.
Yield_Data <- read_xlsx(
  file.path(DATA_RAW, "MXYC_1mo30y_PLAST.xlsx"),
  skip = 1
)

Yield_Data$Date   <- as.Date(Yield_Data$Date)          # Convert POSIXct to Date
Yield_Data[, -1]  <- lapply(Yield_Data[, -1], as.numeric)

# Assign clean tenor names following the column order of raw data (1W, 1M, 3M, 6M, 1Y, ..., 30Y)
colnames(Yield_Data)[-1] <- c(
  "MXY01Y",  "MXY02Y",  "MXY03Y",  "MXY04Y",  "MXY05Y",  "MXY06Y",
  "MXY07Y",  "MXY08Y",  "MXY09Y",  "MXY10Y",  "MXY15Y",  "MXY20Y",
  "MXY30Y",  "MXY01M",  "MXY03M",  "MXY01W",  "MXY09M","MXY06M" )

Yield_Data <- Yield_Data[order(Yield_Data$Date), ]
# Note: NA values indicate days when the market was closed.


# 2. Identify Start Date for Each Yield Tenor ---------------------------------
# Used downstream for sample selection in estimation scripts.

#first_row stores row number of first value of each yield
first_row      <- apply(!is.na(Yield_Data[, -1]), 2, function(x) which(x)[1])

Yield_Data_startdate <- data.frame(
  Variable   = names(first_row),
  Start_Date = Yield_Data$Date[first_row]
)
save(Yield_Data, file = file.path(DATA_CLEAN,"MXYield.RData"))
rm(first_row)

message(sprintf("Data on bond yields cleaned.Cleaned data saved in %s",
                paste(getwd(),DATA_CLEAN, sep = "/")
) )