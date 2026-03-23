# PURPOSE:
#   1. Plot annual total foreign-owned government debt
#
# INPUT:  GDebt_FO_EM
#
# OUTPUT: <FIG_PATH>/EM_FO_GDebt_2004-24.png
#
# CALLED BY: MXNBnd_Replicate.R

# 1. Plot and save the annual EME total ----------------------------------------

PlotData = GDebt_FO_EM
PlotData$Year = as.Date(paste0(PlotData$Year, "-01-01"))
PlotData$Total = PlotData$Total/1000000 #although raw data says units in millions, number seems too large

GDebt_FO_EM_plot <- ggplot(data = PlotData, aes(x = Year, y = Total, group = 1)) +
  geom_line(color = "darkblue", linewidth = 1.25) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(y = "Trillions of $", x = NULL) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text.y = element_text(angle = 90)
  )

GDebt_FO_EM_plot

filename = "EM_FO_GDebt_2004-24.png"
ggsave(file.path(FIG_PATH, filename),
  plot = EM_plot,
  width = 10,  height = 6,  dpi = 300)

message(sprintf("Plot of EM's FO GovDebt over time saved in %s",
                file.path(getwd(), FIG_PATH, filename) ))

rm(PlotData, filename)