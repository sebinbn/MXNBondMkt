

rm(list = ls())


# Paths
DATA_RAW  <- "data/raw/"
DATA_CLEAN <- "data/clean/"
FIG_PATH  <- "output/figures/"
TAB_PATH  <- "output/tables/"



source("code/0_1_setup.R")


source("code/1_1_BBYieldClean.R")
source("code/1_2_BondOwnrshpClean.R")
