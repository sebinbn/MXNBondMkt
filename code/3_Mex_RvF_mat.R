# PURPOSE:
#   1. Import Ownership data by bond maturity & holder type downloaded from banxico.
#   2. Plot and save figure of long-term share over time for residents, non-residents,
#      and all holders combined.
#
# INPUT:  <DATA_RAW>/Mex_GD_Maturity.xlsx
# OUTPUT: <FIG_PATH>/Fig_Mat.png
# CALLED BY: MXNBnd_Replicate.R
# NOTE: Packages are loaded in 0_1_setup.R. Variables created here are not used elsewhere.


# 1. Import ---------------------------------------------------------------

# Rows 1-17 are Banxico metadata; row 18 is the header, data starts row 19.
BM_Mat <- read_xlsx(
  file.path(DATA_RAW, "Mex_GD_Maturity.xlsx"), skip = 17)
BM_Mat$Date <- seq.Date(as.Date("2012-12-01"), as.Date("2024-02-01"), by = "month")


# 2. Select and Rename Columns --------------------------------------------

# Subsetting columns for Residents, Non-Resdients, Total by maturity
Mex_mat <- BM_Mat[, c("Date", "SF224646", "SF224647", "SF224648",
                      "SF224697", "SF224698", "SF224699",
                      "SF224748", "SF224749", "SF224750")]

colnames(Mex_mat)[-1] <- c("R_Own_Tot",  "R_Own_short",  "R_Own_long",
                           "F_Own_Tot",  "F_Own_short",  "F_Own_long",
                           "GG_Tot",     "GG_short",     "GG_long")

# 3. Compute Long-Term Share ----------------------------------------------

Mex_mat$R_lt_share  <- Mex_mat$R_Own_long / Mex_mat$R_Own_Tot   # Residents
Mex_mat$F_lt_share  <- Mex_mat$F_Own_long / Mex_mat$F_Own_Tot   # Non-residents
Mex_mat$GG_lt_share <- Mex_mat$GG_long    / Mex_mat$GG_Tot      # All holders


# 4. Plot -----------------------------------------------------------------

mat_long <- melt(
  Mex_mat[, c("Date", "R_lt_share", "F_lt_share", "GG_lt_share")],
  id.vars = "Date"
)

Mat_plot <- ggplot(data = mat_long, aes(x = Date, y = value, color = variable)) +
  geom_line(linewidth = 1.25) +
  scale_color_discrete(labels = c("Residents", "Non-Residents", "Total")) +
  labs(x = NULL, y = "Share of long term bonds in total holdings") +
  theme(
    axis.text         = element_text(size = 14),
    axis.title        = element_text(size = 14),
    legend.position   = c(0.1, 0.9),
    legend.title      = element_blank(),
    legend.background = element_rect(linetype = "solid", colour = "black")
  )


# 5. Save -----------------------------------------------------------------
filename = "Mex_RvF_Mat.png"
ggsave(file.path(FIG_PATH,filename), plot = Mat_plot,
       width = 10, height = 6, dpi = 300)

rm(BM_Mat, Mex_mat, mat_long, filename)

message(sprintf("Plot of share of long term bonds held by various classes saved as %s",
                paste(getwd(),FIG_PATH,filename,sep = "/")
) )
