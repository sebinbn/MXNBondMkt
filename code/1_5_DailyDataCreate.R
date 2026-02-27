# PURPOSE:
#   1. Merge data to create daily frequency dataframe for further analysis
#      
#
# INPUT:  TIIE, MXN, Yield_Data, Own_Data
# OUTPUT: Mex_d
# CALLED BY: MXNBnd_Replicate.R

# 1. Mergeing and Creating Daily Data -----------------------------------------

Mex_d  = merge(Yield_Data, Own_Data[,c("Date", "F_Own", "F_Own_p")])
Mex_d  = merge(Mex_d, MXN)
Mex_d  = merge(Mex_d, TIIE, all= T)
