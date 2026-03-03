# PURPOSE:
#   1. Run ADF test at levels and first difference on daily and monthly data,
#      and save results.
#   Daily data ADF run for select variables considered for TVVAR and its robustness.
#
# INPUT:  Mex_d, Mex_d_diff, Mex_m, Mex_m_diff
# OUTPUT: ADF_d_tab, ADF_m_tab
# CALLED BY: MXNBnd_Replicate.R

# Function to run ADF Test and tabulate results -------------------------------

# Run ADF tests on levels and first differences
run_adf <- function(data, vars) {
  lapply(vars, function(v) {
    res <- summary(ur.df(data[[v]], type = "none", selectlags = "AIC"))
    round(res@testreg$coefficients[1, c("t value", "Pr(>|t|)")],3)
    })
}

tabulate_adf <- function(levels, diff, vars) {
  ADF_tab <- data.frame(
    Variable      = vars,
    Level_tstat = sapply(levels, function(x) x[["t value"]]),
    Level_pval  = sapply(levels, function(x) x[["Pr(>|t|)"]]),
    Diff_tstat  = sapply(diff,  function(x) x[["t value"]]),
    Diff_pval   = sapply(diff,  function(x) x[["Pr(>|t|)"]])
  )
  colnames(ADF_tab) <- c("Variable", "Level: t-stat", "Level: p-value",
                           "Diff: t-stat",  "Diff: p-value")
  ADF_tab
}

# ADF Test on Daily Data used in TVVAR -----------------------------------------

Vars_TVVAR = c("MXY01M", "MXY03M", "MXY06M", "MXY10Y", "MXY30Y",
               "TIIE", "MXN_USD","F_Own", "F_Own_p")

adf_levels <- run_adf(Mex_d,  Vars_TVVAR)
adf_diff  <- run_adf(Mex_d_diff, Vars_TVVAR)

ADF_d_tab <- tabulate_adf(adf_levels, adf_diff, Vars_TVVAR)

# Save table
write.csv(ADF_d_tab, file = file.path(TAB_PATH, "ADF_results_daily.csv"),
          row.names = FALSE)


# ADF Test on Monthly Data ------------------------------------------------

# Tests for unit roots in levels and first differences for all Mex_m variables.

Vars_m_ADF = names(Mex_m)[!names(Mex_m) %in% "Date"]

adf_levels <- run_adf(Mex_m,  Vars_m_ADF)
adf_diff  <- run_adf(Mex_m_diff, Vars_m_ADF)

ADF_m_tab <- tabulate_adf(adf_levels, adf_diff, Vars_m_ADF)

# Save table
write.csv(ADF_m_tab, file = file.path(TAB_PATH, "ADF_results_monthly.csv"), 
          row.names = FALSE)



# removing intermediate variables no longer necessary -------------------------

rm(adf_levels, adf_diff, run_adf, tabulate_adf, Vars_m_ADF)

message(sprintf("ADF test run on daily and monthly data. Result saved in %s, daily results in %s and monthly results in %s",
                paste(getwd(),TAB_PATH,sep = "/"), "ADF_results_daily.csv",
                "ADF_results_monthly.csv") )

