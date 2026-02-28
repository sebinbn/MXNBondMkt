# PURPOSE:
#   1. Merge data to create daily frequency dataframe.
#   2. Create Mex_d_sub and its first difference Mex_d_sub_diff.
#      This is a complete weekday grid (2008-03-01 to 2022-12-31) 
#      with key variables linearly interpolated across missing weekdays 
#      (e.g. market holidays). This dataframe is used in TV-VAR analysis.
#
# INPUT:  TIIE, MXN, Yield_Data, Own_Data
# OUTPUT: Mex_d, Mex_d_sub
# CALLED BY: MXNBnd_Replicate.R

# 1. Merging and Creating Daily Data -----------------------------------------

Mex_d  = merge(Yield_Data, Own_Data[,c("Date", "F_Own", "F_Own_p")])
Mex_d  = merge(Mex_d, MXN)
Mex_d  = merge(Mex_d, TIIE, all= T)

message("Dataframe with daily data for analysis created.")

# 2. Creating Mex_d_sub and Mex_d_sub_diff - subset used in TV-VAR analysis------------
#
# Mex_d contains data only on dates where observations exist. For TV_VAR,
# a complete sequence of weekdays is needed so that lag structures are 
# meaningful (e.g. lag-5 = one trading week). Missing values
# on market holidays are filled via linear interpolation (na.approx).
#
# Sample period: 2008-03-01 to 2022-12-31
#   - Start: GMXN10Y (10yr yield) is missing before mid-Feb 2008; starting 
#     March 2008 avoids a long leading block of NAs.
#   - End:   MPTBA (1mo T-bill) is only available through March 2023; so ending 
#     Dec 2022.

Vars_TVVAR = c("MXY01M", "MXY03M", "MXY06M", "MXY01Y","MXY10Y", "MXY30Y",
             "TIIE", "MXN_USD","F_Own", "F_Own_p")

all_weekdays = data.frame(
  Date = {
    all_days = seq.Date(from = as.Date("2008-03-01"),
                        to   = as.Date("2022-12-31"),
                        by   = "day")
    all_days[!weekdays(all_days) %in% c("Saturday", "Sunday")]
  }
)

Mex_d_sub = merge(all_weekdays, Mex_d[,c("Date", Vars_TVVAR)], by = "Date", all.x = TRUE)

# Linearly interpolate over missing weekdays (market holidays) for key variables.
# na.rm = FALSE preserves leading/trailing NAs rather than extrapolating.
Mex_d_sub[Vars_TVVAR] = lapply(Mex_d_sub[Vars_TVVAR],
                                na.approx, na.rm = FALSE)

# Creating First differences (lapply required as diff() does not work on dataframes)
Mex_d_sub_diff <- data.frame(
  Date = Mex_d_sub$Date[-1],
  lapply(Mex_d_sub[, Vars_TVVAR], diff)
)

# removing intermediate variables no longer necessary
rm(all_weekdays)

message("Complete weekday dataframe with interpolated values (Mex_d_sub) and its first difference (Mex_d_sub) created.")