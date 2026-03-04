

rm(list = ls())


# Paths
DATA_RAW  <- "data/raw/"
DATA_CLEAN <- "data/clean/"
FIG_PATH  <- "output/figures/"
TAB_PATH  <- "output/tables/"



source("code/0_1_setup.R")

# Cleaning Data
source("code/1_1_BBYieldClean.R")
source("code/1_2_BondOwnrshpClean.R")
source("code/1_3_EFFR_IIP_TIIE_xrateClean.R")

source("code/1_4_DailyDataCreate.R")
source("code/1_5_MonthlyDataCreate.R")

# Data Analysis
source("code/2_1_ADFTest.R")
source("code/2_2_SUREIV.R")
source("code/TVVARmods.R")

# Generating Figures

source("code/3_Mex_RvF_mat.R")