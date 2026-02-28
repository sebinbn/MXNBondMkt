# PURPOSE:
#   1. Run ADF test at levels and first difference on daily data, and save results.
#
# INPUT:  Mex_d_sub, Mex_d_sub_diff
# OUTPUT: ADF_tab
# CALLED BY: MXNBnd_Replicate.R

# Running ADF Test --------------------------------------------------------

# Run ADF tests on levels and first differences
run_adf <- function(data, vars) {
  lapply(vars, function(v) {
    res <- summary(ur.df(data[[v]], type = "none", selectlags = "AIC"))
    round(res@testreg$coefficients[1, c("t value", "Pr(>|t|)")],3)
    })
}

adf_levels <- run_adf(Mex_d_sub,  Vars_TVVAR)
adf_diffs  <- run_adf(Mex_d_sub_diff, Vars_TVVAR)

# Build results table
ADF_tab <- data.frame(
  Variable      = Vars_TVVAR,
  Level_tstat   = sapply(adf_levels, `[[`, "t value"),
  Level_pval    = sapply(adf_levels, `[[`, "Pr(>|t|)"),
  Diff_tstat    = sapply(adf_diffs,  `[[`, "t value"),
  Diff_pval     = sapply(adf_diffs,  `[[`, "Pr(>|t|)")
)

# Save table
colnames(ADF_tab) <- c("Variable",
                       "Level: t-stat", "Level: p-value",
                       "Diff: t-stat",  "Diff: p-value")
write.csv(ADF_tab, file = file.path(TAB_PATH, "ADF_results.csv"), row.names = FALSE)

# removing intermediate variables no longer necessary
rm(adf_levels, adf_diffs, run_adf)

message(sprintf("ADF test run on daily data. Result saved in %s",
                paste(getwd(),paste0(TAB_PATH,"ADF_results.csv"), sep = "/")
) )

