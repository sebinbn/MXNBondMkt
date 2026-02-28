# PURPOSE:
#   1. Import EFFR (US Fed Funds Rate) and IIP (US Industrial Production) downloaded
#       from FRED and TIIE (MXN overnight rate) and MXN/USD exchange rate downloaded
#       from Banxico.
#   2. Clean the data which is then used in 1_5_MonthlyDataCreate.R and 
#       1_4_DailyDataCreate.R
#
# INPUT:  <DATA_RAW>/EFFR.csv
#         <DATA_RAW>/INDPRO.csv
#         <DATA_RAW>/TIIE_daily.xlsx
#         <DATA_RAW>/MXN_USD_daily.xlsx
# OUTPUT: EFFR, IIP, TIIE, MXN (all data frame)
# CALLED BY: MXNBnd_Replicate.R


# 1. Import Raw Data -----------------------------------------------------------

EFFR        <- read.csv(file.path(DATA_RAW, "EFFR.csv"))
colnames(EFFR)[colnames(EFFR) == "DATE"] = "Date"
EFFR$Date   <- as.Date(EFFR$Date)
EFFR$EFFR   <- as.numeric(EFFR$EFFR)

IIP         <- read.csv(file.path(DATA_RAW, "INDPRO.csv"))
colnames(IIP)[colnames(IIP) == "DATE"] = "Date"
IIP$Date    <- as.Date(IIP$Date)
# Note that IIP Date is the first of the month. According to FRED convention, the
# first date is provided of the period in question. Monthly data with date 1-Jan-2023
# includes everything that happened in all of Jan.
# Read more - https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/date-format/

# Row 1–17 of the Banxico xlsx files contain metadata, so skip to row 18.
TIIE           <- read_xlsx(file.path(DATA_RAW, "TIIE_daily.xlsx"), skip = 17)
TIIE$Date      <- as.Date(TIIE$Date)
TIIE[, -1]     <- lapply(TIIE[, -1], as.numeric)
colnames(TIIE)[-1] <- c("Tgt_rate", "TIIE")

MXN            <- read_xlsx(file.path(DATA_RAW, "MXN_USD_daily.xlsx"), skip = 17)
MXN$Date       <- as.Date(MXN$Date)
colnames(MXN)[-1] <- "MXN_USD"


save(EFFR, IIP, TIIE, MXN, 
     file = file.path(DATA_CLEAN,"EFFR_IIP_TIIE_MXN_Clean.RData"))

message(sprintf("Data on EFFR, US IIP, TIIE, MXN-USD cleaned. Cleaned data saved in %s",
                paste(getwd(),DATA_CLEAN, sep = "/")
) )
