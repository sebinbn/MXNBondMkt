# PURPOSE:
#   1. Plot annual total foreign-owned government debt
#   2. Plot barchart of top EMEs by amount of total foreign-owned govt debt in 2024
#   3. Plot annual foreign-owned government debt as share of GDP
#   4. Plot barchart of top EMEs by foreign-owned govt debt as share of GDP in 2024

#
# INPUT:  GDebt_FO_EM, GDebt_shares
#
# OUTPUT: <FIG_PATH>/FOInGDebt_2004-24.png
#         <FIG_PATH>/FOInGDebt_2024.png
#         <FIG_PATH>/FOInGDebt_share_2004-24.png
#         <FIG_PATH>/FOInGDebt_share_2024.png
#
# CALLED BY: MXNBnd_Replicate.R

# Helpers used ---------------------------------------

# Setting lineplot theme
lineplot_theme <- theme_minimal() + theme(
  axis.text.y = element_text(size = 17,angle = 90),
  axis.title = element_text(size = 20),
  axis.text.x = element_text(size = 15) )

# Setting barchart theme controls
bar_theme = theme_minimal() + theme(
  axis.text.x = element_text(angle=45, vjust = 1,hjust = 1,size = 14),
  axis.text = element_text(size=15),
  axis.title = element_text(size=17) )

# Function to save plot

save_FO_Plot = function(plotname,filename){
  
  ggsave(file.path(FIG_PATH, filename),  plot = plotname,
         width = 9,  height = 6,  dpi = 300)
  
  message(sprintf("%s saved in %s",
                  deparse(substitute(plotname)),
                  file.path(getwd(), FIG_PATH, filename) ))
  
}

# function to extract 2024 data and arrange in descending order
Extract_2024 = function(df){
  df_2024 = df %>% filter(Year == 2024)
  cols_filter = !names(df_2024) %in% c("Year", "Total")
  df_2024 = data.frame( Country = names(df_2024)[cols_filter],
                        Value = as.numeric(df_2024[1, cols_filter])    )
  df_2024$Country[which(
    df_2024$Country %in% 
      c('Poland, Republic of','Russian Federation', 'Türkiye, Republic of') )] = 
    c("Poland","Russia", "Turkey") #this method needs countries to be alphabetical, fix later
  
  df_2024 = na.omit(df_2024) %>% arrange(desc(Value)) %>%
    mutate(Country = factor(Country, levels = Country))
  df_2024
}

# 1. Plot and save the annual EME total ----------------------------------------

PlotData = GDebt_FO_EM
PlotData$Year = as.Date(paste0(PlotData$Year, "-01-01"))

GDebt_FO_plot <- ggplot(data = PlotData, aes(x = Year, y = Total, group = 1)) +
  geom_line(color = "darkblue", linewidth = 1.25) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(y = "Billions of $", x = NULL) +
  lineplot_theme

save_FO_Plot(GDebt_FO_plot,"FOInGDebt_2004-24.png")


# 2. Plot barchart of top EMEs by amount ------------------------------------

df_2024 = Extract_2024(GDebt_FO_EM)

GDebt_FO_bar = ggplot(df_2024, aes(x = Country, y = Value)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL,y = "Billions of USD") +
  bar_theme

save_FO_Plot(GDebt_FO_bar,"FOInGDebt_2024.png")  

# 3. Plot and save Annual FO in GDebt as  share of GDP ---------------------

PlotData = GDebt_shares
PlotData$Year = as.Date(paste0(PlotData$Year, "-01-01"))

GDebt_Share_plot <- ggplot(data = PlotData, aes(x = Year, y = Total, group = 1)) +
  geom_line(color = "forestgreen", linewidth = 1.25) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(
    breaks = seq(0.06, max(PlotData$Total, na.rm = TRUE), by = 0.02)
  ) +
  labs(y = "Share of GDP", x = NULL) +
  lineplot_theme

save_FO_Plot(GDebt_Share_plot, "FOInGDebt_share_2004-24.png")

# 4. Plot barchart of top EMEs by share of GDP ------------------------------------

df_2024 = Extract_2024(GDebt_shares)

GDebt_share_bar = ggplot(df_2024, aes(x = Country, y = Value)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL,y = "Share of GDP") +
  bar_theme

save_FO_Plot(GDebt_share_bar,"FOInGDebt_share_2024.png")  


rm(PlotData, df_2024, lineplot_theme, bar_theme,Extract_2024, save_FO_Plot)