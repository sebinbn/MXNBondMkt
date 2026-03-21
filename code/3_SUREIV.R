# PURPOSE:
#   1. Plot SURE-IV result using SUREIV_tab generated in 2_2_SUREIV.R
#   
#
# INPUT:  SUREIV_tab
# OUTPUT: SUREIV_fig  (ggplot object)
#         
# CALLED BY: MXNBnd_Replicate.R


# 1. Load Data ---------------------------------------------------------------
Y_FO_Dat = SUREIV_tab

Y_FO_Dat$Var[5:nrow(Y_FO_Dat)] = paste(substr(Y_FO_Dat$Var[5:nrow(Y_FO_Dat)],4,5),"Yr")
Y_FO_Dat$Var[1:4] = paste(substr(Y_FO_Dat$Var[1:4],4,5),"Mo")
Y_FO_Dat$Var = factor(Y_FO_Dat$Var,levels = Y_FO_Dat$Var) 

# 2. Plot -----------------------------------------------------------------
SUREIV_plot = ggplot(Y_FO_Dat, aes(x =Var)) +
  geom_point(aes(y = F_Own_coef), size = 2) +
  geom_errorbar(aes(ymin = F_Own_coef - tFse, ymax = F_Own_coef + tFse), width = 0.2, color = "blue") +  # tF Error bars
  #geom_errorbar(aes(ymin = F_Own_coef - se, ymax = F_Own_coef + se), width = 0.2, color = "blue") +  # Error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) + 
  scale_x_discrete(breaks = Y_FO_Dat$Var[seq(1, length(Y_FO_Dat$Var), by = 2)])+
  labs(x = 'Maturity', y = 'Basis points')+
  theme_minimal()+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16 ),
        legend.title = element_blank(), legend.position = c(0.2, 0.9))

# 5. Save -----------------------------------------------------------------

filename = "Y_FO_SUREIV.png"
ggsave(file.path(FIG_PATH,filename), plot = Mat_plot,
       width = 10, height = 6, dpi = 300)

message(sprintf("Plot of SURE-IV results saved as %s",
                paste(getwd(),FIG_PATH,filename,sep = "/")
) )

rm(Y_FO_Dat, filename)
