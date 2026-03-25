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

# 1. Plot and save the annual EME total ----------------------------------------

PlotData = GDebt_FO_EM
PlotData$Year = as.Date(paste0(PlotData$Year, "-01-01"))

GDebt_FO_plot <- ggplot(data = PlotData, aes(x = Year, y = Total, group = 1)) +
  geom_line(color = "darkblue", linewidth = 1.25) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(y = "Billions of $", x = NULL) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text.y = element_text(angle = 90)
  )

GDebt_FO_plot

filename = "FOInGDebt_2004-24.png"
ggsave(file.path(FIG_PATH, filename),  plot = GDebt_FO_plot,
        width = 10,  height = 6,  dpi = 300)

message(sprintf("Plot of EM's FO GovDebt over time saved in %s",
                file.path(getwd(), FIG_PATH, filename) ))

# 1. Plot and save Annual FO in GDebt as  share of GDP ---------------------

PlotData = GDebt_shares
PlotData$Year = as.Date(paste0(PlotData$Year, "-01-01"))

GDebt_Share_plot <- ggplot(data = PlotData, aes(x = Year, y = Total, group = 1)) +
  geom_line(color = "forestgreen", linewidth = 1.25) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(y = "Share of GDP", x = NULL) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text.y = element_text(angle = 90)
  )

GDebt_Share_plot

filename = "FOInGDebt_share_2004-24.png"
ggsave(file.path(FIG_PATH, filename), plot = GDebt_Share_plot,
       width = 10,  height = 6,  dpi = 300)

message(sprintf("Plot of EM's FO GovDebt as share of GDP over time saved in %s",
                file.path(getwd(), FIG_PATH, filename) ))


rm(PlotData, filename)