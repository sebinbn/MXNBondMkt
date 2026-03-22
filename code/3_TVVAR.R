# PURPOSE:
#   1. Use the results from TVVAR analysis to generate two pictures.
#   1.1 CIRF at 90th and 10th percentile along with 1 std devn bands.
#   1.2 3-D CIRF
#
# INPUT:  TVVAR_1mo or 
# OUTPUT: IRF_9010, 3D-CIRF
# CALLED BY: MXNBnd_Replicate.R

# IRF plotting by quantile ----------------------------------------------------
#
#   To generate IRFs as local averages and not the results of one VAR estimated 
#   at a particlular quantile,the IRF at any quintile is calculated as the mean
#   over a local range.
#   Eg: If qntle = 0.9 and gap = 0.05 is input, it will provide the mean of IRFs
#   between 0.9 ± 0.05.
#
# Inputs:
#   obj    - tvIRF result list with elements irf, Lower, Upper (each [obs x resp x horizon])
#   qntle   - quintile corresponding to which the IRF is generated. Eg: 0.9 would
#             generate the IRF corresponding to 90th quintile of propn. of Foreign
#             ownership.
#   gap     - half-width around each quantile. e.g. 0.05 gives [qntle ± 0.05]
#             

plot_TIIE_IRF_Qntle <- function(obj,F_Own, qntle, gap) {
  
  obs <- dim(obj$irf$TIIE)[1]
  
  #Find value of F_Own_p at each quintile
  F_Own_qntle = quantile(F_Own, probs = c(qntle - gap, qntle + gap))
  message(c(qntle - gap, qntle + gap))
  # Row indices for the low-F_Own and high-F_Own windows
  #lowRows  <- (ceiling((qntle[1] - gap) * obs) + 1) : ceiling((qntle[1] + gap) * obs)
  lowRows  <- which(F_Own > F_Own_qntle[1] & F_Own < F_Own_qntle[3])
  #highRows <- (ceiling((qntle[2] - gap) * obs) + 1) : ceiling((qntle[2] + gap) * obs)
  highRows  <- which(F_Own > F_Own_qntle[2] & F_Own < F_Own_qntle[4])
  
  # Helper: mean across the window for a given array (obs x 1 x horizon -> horizon)
  windowMean <- function(arr, rows) apply(arr[rows, 1, ], 2, mean)
  
  irf_df <- data.frame(
    Period   = rep(0:10, 2),
    Regime   = rep(c("Low FO period", "High FO period"), each = 11),
    Response = c(windowMean(obj$irf$TIIE,   lowRows),
                 windowMean(obj$irf$TIIE,   highRows)),
    Lower    = c(windowMean(obj$Lower$TIIE, lowRows),
                 windowMean(obj$Lower$TIIE, highRows)),
    Upper    = c(windowMean(obj$Upper$TIIE, lowRows),
                 windowMean(obj$Upper$TIIE, highRows))
  )
  
  ggplot(irf_df, aes(x = Period, y = Response, color = Regime, fill = Regime)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, linewidth = 0) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = c(0, 5, 10)) +
    labs(x = "Day", y = "Cumulative IRF") +
    theme_minimal() +
    theme(
      axis.text        = element_text(size = 14),
      axis.title       = element_text(size = 14),
      legend.title     = element_blank(),
      legend.text      = element_text(size = 12),
      legend.position  = "bottom"
    )
}

# Fig 1 - CIRF at low vs high FO periods -----------------------------------


Fig_CIRF_LowHigh <- plot_TIIE_IRF_Qntle(obj  = TVVAR_1mo$CI, 
                                        F_Own = TVVAR_1mo$VAR$z,
                                        qntle = c(0.10, 0.90), gap  = 0.05)
Fig_CIRF_LowHigh

# Fig 2 - 3D surface of CIRF across horizon and FO proportion --------------

# Rows index observations sorted by FO proportion (obj$x$z);
# columns index horizon (0-10); shown for the 1-month bond (resp = 2).

obj_1mo <- CI_1mo

Fig_CIRF3D <- plot_ly(
  x = ~0:10,
  y = ~obj_1mo$x$z,
  z = ~obj_1mo$irf$TIIE[ , 2, ],
  type       = "surface",
  colorscale = "Viridis",
  colorbar   = list(
    title    = list(text = "Cumulative IRF", font = list(size = 16)),
    tickfont = list(size = 14),
    len      = 0.7,
    x        = 0.95,
    xanchor  = "left"
  )
) %>%
  layout(
    scene = list(
      xaxis = list(title = list(text = "Horizon (0\u201310)",       font = list(size = 18)),
                   tickfont = list(size = 14)),
      yaxis = list(title = list(text = "Proportion of FO Bonds",    font = list(size = 18)),
                   tickfont = list(size = 14)),
      zaxis = list(title = list(text = "Cumulative IRF",            font = list(size = 18)),
                   tickfont = list(size = 14))
    )
  )
Fig_CIRF3D

# Export figures -----------------------------------------------------------

ggsave(filename = file.path(FIG_PATH, "CIRF_LowHighFO.png"),
       plot = Fig_CIRF_LowHigh, width = 6, height = 5, dpi = 300)

htmlwidgets::saveWidget(Fig_CIRF3D,
                        file = file.path(FIG_PATH, "TVVAR_CIRF_3D.html"),
                        selfcontained = TRUE)

