# PURPOSE:
#   1. Merge data to create daily frequency dataframe Mex_d and its first
#       difference Mex_d_diff.
#      This is a complete weekday grid (2008-03-01 to 2022-12-31) 
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
# OUTPUT: Mex_d, Mex_d_diff
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

Mex_d  = merge(all_weekdays, Yield_Data, by = "Date", all.x = TRUE)
Mex_d = merge(Mex_d, Own_Data[,c("Date", "F_Own", "F_Own_p")], all.x = TRUE)
Mex_d  = merge(Mex_d, MXN, all.x = TRUE)
Mex_d  = merge(Mex_d, TIIE[,names(TIIE) != "Tgt_rate"], all.x = TRUE)

# Linearly interpolate over missing weekdays (market holidays) for key variables.
# na.rm = FALSE preserves leading/trailing NAs rather than extrapolating.
Vars_d_NA = names(Mex_d)[colMeans(is.na(Mex_d)) > 0]
Mex_d[Vars_d_NA] = lapply(Mex_d[Vars_d_NA],na.approx, na.rm = FALSE)

# subsetting to choose sample period
Mex_d = Mex_d[Mex_d$Date <= sample_start_end["end"],]

Vars_d_NA = names(Mex_d)[colMeans(is.na(Mex_d)) > 0]
message(sprintf("Despite subsetting from %s to %s, %s have missing values which are linearly interpolated using na.approx()",
                sample_start_end["start"], sample_start_end["end"],
                paste(Vars_d_NA, collapse = ", ")
))

message("Dataframe with daily data for analysis created.")


# Creating First differences (lapply required as diff() does not work on dataframes)
Mex_d_diff <- data.frame(
  Date = Mex_d$Date[-1],
  lapply(Mex_d[, Vars_TVVAR], diff)
)

# removing intermediate variables no longer necessary
rm(all_weekdays, all_days, Vars_d_NA)

message("Complete weekday dataframe with interpolated values (Mex_d_sub) and its first difference (Mex_d_sub) created.")