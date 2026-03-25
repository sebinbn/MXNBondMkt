

rm(list = ls())


# Paths
DATA_RAW  <- "data/raw"
DATA_CLEAN <- "data/clean"
FIG_PATH  <- "output/figures"
TAB_PATH  <- "output/tables"

# The following controls if bootstrapped confidence intervals for TV-IRF are to
# be estimated. This process takes ~20 mins and can be skipped in reruns of this 
# file by changing to F. The result is saved the first time and reruns will use 
# saved bootstrap result.
run_TVVAR_boot = T 


source("code/0_1_setup.R")

# Cleaning Data
source("code/1_1_BBYieldClean.R")
source("code/1_2_BondOwnrshpClean.R")
source("code/1_3_EFFR_IIP_TIIE_xrateClean.R")

source("code/1_4_DailyDataCreate.R")
source("code/1_5_MonthlyDataCreate.R")

source("code/1_6_IMF-PIP-EMDataCreate.R")

# Data Analysis
source("code/2_1_SummStats.R")
source("code/2_2_ADFTest.R")
source("code/2_3_SUREIV.R")
source("code/2_4_TVVAR.R")
source("code/TVVARmods.R")

# Generating Figures

source("code/3_Mex_RvF_mat.R")
source("code/3_SUREIV.R")
source("code/3_TVVAR.R")

source("code/3_EME-FO.R")
