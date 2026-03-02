# PURPOSE:
#   Build a monthly-frequency panel by aggregating daily data and merging
#   it with the monthly IIP series.
#
# INPUTS  (must already be in the R environment):
#   Yield_Data     -- daily yields
#   Own_Data    -- ownership data (Date, F_Own, F_Own_p, ...)
#   EFFR        -- daily federal-funds rate (DATE, EFFR)
#   IIP         -- monthly industrial production (DATE, INDPRO)
#   TIIE        -- daily policy / overnight rate (Date, Tgt_rate, TIIE)
#   MXN         -- daily exchange rate (Date, MXN_USD)
#
# OUTPUT:
#   Mex_m -- monthly data frame with columns:
#                  Date, EFFR, d_ln_IIP, TIIE, MXN_USD, F_Own, F_Own_p,
#                  + all yield columns from Yield_Data
#   Mex_m_diff  -- first difference of Mex_m except for for d_ln_IIP which is 
#                 copied as is since it is already differenced.



# 1. Build a monthly date spine (last calendar day of each month) ---------------

# Note: TIIE starts only from 2006-01-02 and so has an NA for 2006-01-01 which is 
# a Sunday.
# seq(as.Date("2006-01-31"), as.Date("2023-12-31"), by = "month") #does not create 
# end of months. 
month_ends <- seq(as.Date("2006-02-01"), as.Date("2024-01-01"), by = "month") - 1


# 2. Helper: last non-NA observation within 10 calendar days of month end ------
#    Works for any daily data frame whose first column is 'Date'.

last_obs_monthly <- function(daily_df, date_col = "Date", n_lookback = 10) {
  dates  <- daily_df[[date_col]]
  values <- daily_df[, -which(names(daily_df) == date_col), drop = FALSE]
  
  result <- data.frame(Date = month_ends)
  for (col in names(values)) {
    result[[col]] <- vapply(month_ends, function(m) {
      window <- values[[col]][dates <= m & dates > (m - n_lookback)]
      window <- window[!is.na(window)]
      if (length(window) > 0) tail(window, 1) else NA_real_
    }, numeric(1))
  }
  result
}


# 3. Aggregate daily series to monthly ----------------------------------------

EFFR_m  <- last_obs_monthly(EFFR)
TIIE_m  <- last_obs_monthly(TIIE)
MXN_m   <- last_obs_monthly(MXN)         
Yield_m <- last_obs_monthly(Yield_Data)
F_Own_m   <- last_obs_monthly(Own_Data[,c("Date", "F_Own", "F_Own_p")])


# 4. IIP: compute 12-month log-difference of industrial production -------------
# Since IIP data is already monthly, no need to use last_obs_monthly. However,
# the dates are the first days of the month. So, subsetting uses first of month

d_ln_IIP <- diff(log(IIP[IIP$Date >= as.Date("2005-01-01") &
                             IIP$Date <= as.Date("2023-12-01"), "INDPRO" ]),
                 lag = 12) * 100
IIP_m   <- data.frame(Date = month_ends,
                           d_ln_IIP = d_ln_IIP)


# 5. Merge all series into one monthly panel ----------------------------------

Mex_m <- cbind(
  EFFR_m[c("Date", "EFFR")],
  TIIE  = TIIE_m$TIIE,
  MXN_USD = MXN_m$MXN_USD,
  F_Own   = F_Own_m$F_Own,
  F_Own_p = F_Own_m$F_Own_p,
  d_ln_IIP = IIP_m$d_ln_IIP,
  Yield_m[, names(Yield_m) != "Date"]
)

Vars_diff <- names(Mex_m)[!names(Mex_m) %in% c("Date", "d_ln_IIP")]

Mex_m_diff <- data.frame(
  Date = Mex_m$Date[-1],
  lapply(Mex_m[, Vars_diff], diff)
)

# Clean up intermediate objects --------------------------------------------

message(sprintf("Monthly data from %s to %s generated from daily data and stored in Mex_m, and its first difference stored in Mex_m_diff",
                month_ends[1], tail(month_ends,1) ))

rm(EFFR_m, TIIE_m, MXN_m, F_Own_m, IIP_m, Yield_m, 
   month_ends, d_ln_IIP, last_obs_monthly)


