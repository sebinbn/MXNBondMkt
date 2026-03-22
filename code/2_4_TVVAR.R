# PURPOSE:
#   1. Estimate the TV-VAR model with coefficients as functions of the share of
#      foreign ownership of Mexican government bonds.
#   2. Estimate cumulative impulse responses for the baseline 1-month yield 
#       specification and the 6-month yield robustness specification.
#   3. Calculate wild-bootstrap confidence intervals for IRF.
#   4. Save estimation results in .RData files so the bootstrap
#      does not need to be rerun every time the figure is generated.
#
# INPUT:  Mex_d, Mex_d_diff
# OUTPUT: TVVAR_1mo, TVVAR_6mo
# CALLED BY: MXNBnd_Replicate.R

# Helper to estimate IRF and CI for a VAR specification ------------------------

run_tvvar_irf = function(short_yield, impulses, responses, save_file) {
  
  VAR_vars = c("TIIE", short_yield, "MXY30Y", "MXN_USD")
  
  
  start_time = Sys.time()
  var_fit = tvVAR(Mex_d_diff[VAR_vars],p = 5, type = "none", tkernel = "Epa",
        z= Mex_d$F_Own_p[-1], bw = rep(0.05,length(VAR_vars)))
  message(sprintf( "TV-VAR with %s estimated in %.2f mins",
    short_yield,
    as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  ))
  
  start_time = Sys.time()
  irf_fit = tvIRF(var_fit, cumulative = T, bw.cov = 0.05, unit.shock = T,
                  impulse = impulses, response = responses )
  message(sprintf( "TV-IRF for %s estimated in %.2f mins",
    short_yield,
    as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  ))
  
  start_time = Sys.time()
  ci_fit = confint(irf_fit, level = 0.68, tboot = "wild")
  message(sprintf("Wild-bootstrap confidence intervals for %s estimated in %.2f mins",
    short_yield,
    as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  ))
  
  tvvar_results = list(
    VAR = var_fit,
    CumIRF = irf_fit,
    CI = ci_fit
  )
  
  save(tvvar_results, file = file.path(TAB_PATH, save_file))
  message(sprintf("Saved TV-VAR results to %s", save_file))
  
  tvvar_results
}

# Baseline estimation -----------------------------------

TVVAR_1mo = run_tvvar_irf(short_yield = "MXY01M",
                          impulses = "TIIE",
                          responses = c("MXY01M", "MXY30Y"),
                          save_file = "TVVAR_1mo.RData" )

# Robustness Test -----------------------------------

TVVAR_6mo = run_tvvar_irf(short_yield = "MXY06M",
                               impulses = "TIIE",
                               responses = c("MXY06M", "MXY30Y"),
                               save_file = "TVVAR_6mo.RData" )

