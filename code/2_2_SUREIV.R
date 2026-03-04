# PURPOSE:
#   1. Estimate the SURE-IV model for 17 Mexican government bond yields
#      using the first-stage fitted Value of foreign ownership as the endogenous
#      regressor. Applies tF-corrected standard errors to account for weak 
#      instruments.
#   2. Conducts Wald tests for coefficient equality between short-term yields 
#      (1mo, 3mo, 1yr) and long-term yields (10Y, 20Y, 30Y).
#
# INPUT:  Mex_m_diff
# OUTPUT: SUREIV_tab  (data.frame: coef, SE, tF-corrected SE, p-value per yield)
#         wald_tests    (list of marginaleffects::hypotheses() Wald test results)
#         Stg1_result - lm() output of Stage 1 regresion
#         SUR_result  - systemfit() output of Stage 2 SUR
# CALLED BY: MXNBnd_Replicate.R

## 1. Setting up data -------------------------------------------------

yield_names = c("MXY01M", "MXY03M", "MXY06M", "MXY09M", "MXY01Y", "MXY02Y",
                "MXY03Y", "MXY04Y", "MXY05Y", "MXY06Y", "MXY07Y", "MXY08Y",
                "MXY09Y", "MXY10Y", "MXY15Y", "MXY20Y", "MXY30Y")

SURData = Mex_m_diff
SURData[yield_names] = SURData[yield_names] * 100 #convert pp to bps
SURData["TIIE"] = SURData["TIIE"] * 100

## 2. First stage regression -------------------------------------------------

Stg1_result = lm(F_Own ~ 0 + EFFR + d_ln_IIP, data = SURData)
summary(Stg1_result)

# For weak instrument, tF se = se * tf correction factor, where the correction
# factor depends on the 1st stage F. 
# Below, I use Table 3A from Lee et.al (2022) in AER. This table shows correction
# factor for few Stage 1 Fs, and the values in between can be interpolated.
# From the Table,
# Stage 1 F - tF correction
# 7.940     - 2.025
# 8.196     - 1.980
# 8.473     - 1.935
# 8.773     - 1.892
# Since Stage 1 F-stat falls between 8.196 and 8.473, the precise correction 
# factor is interpolated in tFCorr 
Stg1_F = summary(Stg1_result)$fstatistic["value"] 
tFCorr = 1.935 + (8.473 - Stg1_F)/(8.473 - 8.196)* (1.98-1.935)

# 3. Second stage SURE -------------------------------------------------

SURData$F_Own_fit = Stg1_result$fitted.values

### SURE Estimation ---------------------------------------------------------


# build each equation formula
eq_list = lapply(yield_names, function(y) {
  as.formula( paste0( y, " ~ 0 + F_Own_fit + TIIE") )
})
names(eq_list) = yield_names

SUR_result = systemfit(eq_list, data = SURData, method = "SUR")
summary(SUR_result)

### Tabulate results --------------------------------------------------------

# Stg1_result_fitted is the 1st regressor in each equation => every other coefficient.
F_Own_fit_idx <- seq(1, length(SUR_result$coefficients), by = 2)

SUREIV_tab <- data.frame(
  Var  = yield_names,
  F_Own_coef = SUR_result$coefficients[F_Own_fit_idx],
  se   = sqrt(diag(SUR_result$coefCov))[F_Own_fit_idx],
  row.names = NULL
)
SUREIV_tab$tFse   <- SUREIV_tab$se * tFCorr
SUREIV_tab$pValue <- (1 - pnorm(abs(SUREIV_tab$F_Own_coef) / SUREIV_tab$tFse)) * 2

# 4. Wald Tests ---------------------------------------------------------

wald_hypotheses <- c(
  "MXY01M_F_Own_fit - MXY30Y_F_Own_fit = 0",
  "MXY03M_F_Own_fit - MXY30Y_F_Own_fit = 0",
  "MXY01Y_F_Own_fit - MXY30Y_F_Own_fit = 0",
  "MXY01M_F_Own_fit - MXY20Y_F_Own_fit = 0",
  "MXY03M_F_Own_fit - MXY20Y_F_Own_fit = 0",
  "MXY01Y_F_Own_fit - MXY20Y_F_Own_fit = 0",
  "MXY01M_F_Own_fit - MXY10Y_F_Own_fit = 0",
  "MXY03M_F_Own_fit - MXY10Y_F_Own_fit = 0",
  "MXY01Y_F_Own_fit - MXY10Y_F_Own_fit = 0"
)

wald_tests <- lapply(wald_hypotheses,
                     function(h) hypotheses(SUR_result, hypothesis = h))
names(wald_tests) <- wald_hypotheses

print(wald_tests)
# Remove intermediate variables ----------------------------------------------

rm(yield_names, SURData, Stg1_F, tFCorr, F_Own_fit_idx)