# PURPOSE:
#   1. Generate summary statistics for every variable in the daily
#      and monthly datasets.
#   2. Export the daily summary-statistics table to .csv.
#
# INPUT:  Mex_d, Mex_m
# OUTPUT: SummStats_d_tab
# CALLED BY: MXNBnd_Replicate.R

# Function to generate and tabulate summary statistics -------------------------

summ_stats <- function(df) {
  
  df = df[, names(df) != "Date", drop = F]
  Stats_tab = data.frame(matrix(nrow = length(names(df)), ncol = 6))
  colnames(Stats_tab) = c("Variable", "Mean", "Median", "Std. Devn", "Min", "Max")
  
  for (i in seq_along(df)) {
   Stats_tab[i, ] = c(names(df)[i],
                       round(mean(df[[i]]), 2),
                       round(median(df[[i]]), 2),
                       round(sd(df[[i]]), 2),
                       round(min(df[[i]]), 2),
                       round(max(df[[i]]), 2))
  }
  
  Stats_tab
}

# Calculate Summary statistics  -------------------------------

SummStats_tab = summ_stats(Mex_d)
SummStats_tab = rbind(SummStats_tab, summ_stats(Mex_m["d_ln_IIP"]) )

# Save results, Remove intermediates-------------------------------------------
filename = "Summary_stats.csv"
write.csv(SummStats_tab,
          file = file.path(TAB_PATH, filename),
          row.names = FALSE)

message(sprintf("Summary statistics generated and saved to %s",
                file.path(TAB_PATH, filename)))

rm(summ_stats, filename)