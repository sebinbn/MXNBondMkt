# PURPOSE:
#   1. Import EFFR (US Fed Funds Rate) and IIP (US Industrial Production) downloaded
#       from FRED and TIIE (MXN overnight rate) and MXN/USD exchange rate downloaded
#       from Banxico.
#   2. Clean the data which is then used in 1_4_MonthlyDataCreate.R and 
#       1_5_DailyDataCreate.R
#
# INPUT:  <DATA_RAW>/EFFR.csv
#         <DATA_RAW>/INDPRO.csv
#         <DATA_RAW>/TIIE_daily.xlsx
#         <DATA_RAW>/MXN_USD_daily.xlsx
# OUTPUT: EFFR, IIP, TIIE, MXN (all data frame)
# CALLED BY: MXNBnd_Replicate.R


# 1. Import Raw Data -----------------------------------------------------------

EFFR        <- read.csv(file.path(DATA_RAW, "EFFR.csv"))
EFFR$DATE   <- as.Date(EFFR$DATE)
EFFR$EFFR   <- as.numeric(EFFR$EFFR)

IIP         <- read.csv(file.path(DATA_RAW, "INDPRO.csv"))
IIP$DATE    <- as.Date(IIP$DATE)

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

message(sprintf("Data on EFFR, US IIP, TIIE, MXN-USD cleaned.\n
                Cleaned data saved in %s", paste(getwd(),DATA_CLEAN, sep = "/")
) )
