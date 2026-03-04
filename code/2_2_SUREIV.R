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
SURData["F_Own"] = SURData["F_Own"] / 10 #convert to tens of Bns of Pesos 
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

# build each equation formula
eq_list = lapply(yield_names, function(y) {
  as.formula( paste0( y, " ~ 0 + F_Own_fit + TIIE") )
})
names(eq_list) = yield_names

SUR_result = systemfit(eq_list, data = SURData, method = "SUR")

### Tabulate results --------------------------------------------------------

# Index picking F_Own_Fit coefficients
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

# Maturity pairs to test: each short-term vs. each long-term yield
short_term_yields <- c("MXY01M", "MXY03M", "MXY01Y")
long_term_yields  <- c("MXY10Y", "MXY20Y", "MXY30Y")

# linearHypothesis() uses a constraint matrix over all SUR coefficients. For 9 
# hypothesis on the 34 estimated SUR coefficients, I generate a 9 * 34 matrix
H <- matrix(0, nrow = 9, ncol = length(coef_names),
            dimnames = list(
              paste0(rep(short_term_yields, each = 3), " = ",
                     long_term_yields),
              coef_names
            ))

for (i in 1:nrow(H)) {
  H[i, paste0(short_term_yields[ceiling(i/3)], "_F_Own_fit")] <-  1
  H[i, paste0(long_term_yields[((i-1) %% 3) + 1], "_F_Own_fit")] <- -1
}

wald_tests <- lapply(1:nrow(H), function(i) {
  linearHypothesis(SUR_result, hypothesis.matrix = H[i, , drop = FALSE])
})
names(wald_tests) <- rownames(H)

### Tabulate results --------------------------------------------------------
Wald_tab <- data.frame(
  Hypothesis = names(wald_tests),
  Difference_in_bps = sapply(1:nrow(H), function(i) (H[i, ] %*% coef(SUR_result))[1]),
  F_stat     = sapply(wald_tests, function(x) x$F[2]),
  p_value    = sapply(wald_tests, function(x) x$"Pr(>F)"[2]),
  row.names  = NULL
)

write.csv(Wald_tab, file.path(TAB_PATH, "Wald_Tests.csv"), row.names = FALSE)

# Remove intermediate variables ----------------------------------------------

rm(yield_names, SURData, Stg1_F, tFCorr, F_Own_fit_idx)