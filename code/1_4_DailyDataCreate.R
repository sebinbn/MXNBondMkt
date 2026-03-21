# PURPOSE:
#   1. Merge data to create daily frequency dataframe Mex_d and its first
#       difference Mex_d_diff.
#      Period: weekday grid from 2008-04-01 to 2022-12-31, 
#      with key variables linearly interpolated across missing weekdays 
#      (e.g. market holidays). This dataframe is used in TV-VAR analysis.
#
# Choice of sample period: 2008-04-01 to 2022-12-31
#   - Start: MXY10Y has many missing before mid-Feb 2008;MXY01Y starts only in 
#             01-04-2008 (April '08)
#   - End:   MPTBA (1mo T-bill) is only available through March 2023; so ending 
#     Dec 2022.
#
# INPUT:  TIIE, MXN, Yield_Data, Own_Data
# OUTPUT: Mex_d, Mex_d_diff, Mex_d_NA (a count of how many NAs in sample).
#         Notes: Yields converted from pps to bps.
#                F_Own converted from Mns Pesos to Bns Pesos
# CALLED BY: MXNBnd_Replicate.R

# 1. Merging and Creating Daily Data -----------------------------------------

sample_start_end = c(start = as.Date("2008-04-01"), end = as.Date("2022-12-31"))

all_weekdays = data.frame(
  Date = {
    all_days = seq.Date(from = sample_start_end["start"],
                        to   = as.Date("2023-12-31"), 
                        by   = "day")
    #MXY06Y and MXY08Y have missing values in 2022, but values available in 2023.
    #To use 2023 values to interpolate, ending date is 2023. It is later subsetted to 2022.
    all_days[!weekdays(all_days) %in% c("Saturday", "Sunday")]
  }
)

Mex_d  = merge(all_weekdays, Yield_Data[,names(Yield_Data) != "MXY01W"],
               by = "Date", all.x = T)
Mex_d = merge(Mex_d, Own_Data[,c("Date", "F_Own", "F_Own_p")], all.x = T)
Mex_d  = merge(Mex_d, MXN, all.x = T)
Mex_d  = merge(Mex_d, TIIE[,names(TIIE) != "Tgt_rate"], all.x = T)
Mex_d = merge(Mex_d, EFFR, all.x = T)

## 1.1 handling NA Values ---------------------------------------------
obs_count = nrow(Mex_d[Mex_d$Date <= sample_start_end["end"], ])
na_count = colSums(is.na(Mex_d[Mex_d$Date <= sample_start_end["end"], ])) 
Mex_d_NA = data.frame(
  Variable        = names(na_count),
  NA_count        = na_count,
  NA_pct          = sprintf("%.1f %%", na_count/obs_count*100),
  row.names       = NULL
)
message(sprintf(
  "In the daily data sample from %s to %s with %d observations, missing values by variable:",
  sample_start_end["start"], sample_start_end["end"], obs_count
))
print(Mex_d_NA, row.names = FALSE)

Vars_d_NA = names(Mex_d)[colMeans(is.na(Mex_d)) > 0]
# Linearly interpolate over missing weekdays (market holidays) for key variables.
# na.rm = FALSE preserves leading/trailing NAs rather than extrapolating.
Mex_d[Vars_d_NA] = lapply(Mex_d[Vars_d_NA],na.approx, na.rm = FALSE)

# subsetting to choose sample period
Mex_d = Mex_d[Mex_d$Date <= sample_start_end["end"],]

Vars_d_NA = names(Mex_d)[colMeans(is.na(Mex_d)) > 0]
message(sprintf("Despite interpolation, %s has missing values because of trailing NAs.",
                paste(Vars_d_NA, collapse = ", ")
))

# MXY09Y is missing after 2018-05-22. these are now filled in as the average of the 
# 8 and 10 year yields.
Mex_d$MXY09Y[is.na(Mex_d$MXY09Y)] = rowMeans(
  Mex_d[is.na(Mex_d$MXY09Y), c("MXY08Y", "MXY10Y")])
message("These NAs filled as the average of neighboring yields.")

## 1.2 Adjusting units ---------------------------------------------

yield_cols = !colnames(Mex_d) %in% c("Date", "F_Own", "F_Own_p", "MXN_USD")
Mex_d[yield_cols] = Mex_d[yield_cols] * 100 #converting to bps
Mex_d["F_Own"] = Mex_d["F_Own"]/1000  #converting to Bns of Pesos
message("Yields converted to bps and F_Own converted to Bns of MXN Pesos")

# 2. Creating First Differenced Data -----------------------------------------

# lapply required as diff() does not work on dataframes
Mex_d_diff <- data.frame(
  Date = Mex_d$Date[-1],
  lapply(Mex_d[, names(Mex_d) != "Date"], diff)
)

# removing intermediate variables no longer necessary
rm(all_weekdays, all_days, Vars_d_NA, obs_count, na_count)

message("Weekday dataframe (Mex_d) and its first difference (Mex_d_diff) created.")