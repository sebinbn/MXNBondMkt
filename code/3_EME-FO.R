# PURPOSE:
#   1. Plot annual total foreign-owned government debt
#   2. Plot annual foreign-owned government debt as share of GDP
#
# INPUT:  GDebt_FO_EM, GDebt_shares
#
# OUTPUT: <FIG_PATH>/FOInGDebt_2004-24.png
# OUTPUT: <FIG_PATH>/FOInGDebt_share_2004-24.png
#
# CALLED BY: MXNBnd_Replicate.R

# Setting figure theme controls ---------------------------------------

fig_theme <- theme_minimal() + theme(
  axis.text.y = element_text(size = 17,angle = 90),
  axis.title = element_text(size = 20),
  axis.text.x = element_text(size = 15)
)

# 1. Plot and save the annual EME total ----------------------------------------

PlotData = GDebt_FO_EM
PlotData$Year = as.Date(paste0(PlotData$Year, "-01-01"))

GDebt_FO_plot <- ggplot(data = PlotData, aes(x = Year, y = Total, group = 1)) +
  geom_line(color = "darkblue", linewidth = 1.25) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(y = "Billions of $", x = NULL) +
  fig_theme

GDebt_FO_plot

filename = "FOInGDebt_2004-24.png"
ggsave(file.path(FIG_PATH, filename),  plot = GDebt_FO_plot,
        width = 9,  height = 6,  dpi = 300)

message(sprintf("Plot of EM's FO GovDebt over time saved in %s",
                file.path(getwd(), FIG_PATH, filename) ))

# 2. Plot and save Annual FO in GDebt as  share of GDP ---------------------

PlotData = GDebt_shares
PlotData$Year = as.Date(paste0(PlotData$Year, "-01-01"))

GDebt_Share_plot <- ggplot(data = PlotData, aes(x = Year, y = Total, group = 1)) +
  geom_line(color = "forestgreen", linewidth = 1.25) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(
    breaks = seq(0.06, max(PlotData$Total, na.rm = TRUE), by = 0.02)
  ) +
  labs(y = "Share of GDP", x = NULL) +
  fig_theme

GDebt_Share_plot

filename = "FOInGDebt_share_2004-24.png"
ggsave(file.path(FIG_PATH, filename), plot = GDebt_Share_plot,
       width = 9,  height = 6,  dpi = 300)

message(sprintf("Plot of EM's FO GovDebt as share of GDP over time saved in %s",
                file.path(getwd(), FIG_PATH, filename) ))


rm(PlotData, filename)