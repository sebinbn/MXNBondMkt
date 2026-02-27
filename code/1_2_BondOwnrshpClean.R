# PURPOSE:
#   1. Clean Banxico data on government bond ownership by investor category.
#        NA is replaced with 0 before calculating proportions.
#   2. Compute Banxico's consolidated holding 
#   3. Calculate ownership proportions for every group.
#   
#
# INPUT:  DATA_RAW/Mex_GD_ownership.xlsx  downloaded from Banxico website; all values in millions of pesos.
# OUTPUT: Own_Data (data frame)            -- ownership data with proportions. 
#         Own_Data_startdate (data frame)  -- first non-NA date per variable.


# 1. Import Ownership Data ----------------------------------------------------

# Rows 1-10 contain metadata and descriptions; row 11 contains column headers.
# skip = 10 lands directly on the header row; data follows from row 12 onward.
Own_Data <- read_xlsx(
  path = file.path(DATA_RAW, "Mex_GD_ownership.xlsx"),
  skip = 17
)

Own_Data$Date  <- as.Date(Own_Data$Date)
Own_Data[, -1] <- lapply(Own_Data[, -1], as.numeric)

# 2. Identify Start Date for Each Ownership Variable --------------------------
# Records when each series begins; used downstream for sample selection.
# Also imports human-readable variable descriptions from row 10 for reference.
# These map Banxico codes to plain-language labels.

first_row      <- apply(!is.na(Own_Data[, -1]), 2, function(x) which(x)[1])
Own_Data_startdate <- data.frame(
  Code   = names(first_row),
  Description = names(read_xlsx(file.path(DATA_RAW, "Mex_GD_ownership.xlsx"),
                                range = cell_rows(10)))[-1],
  Start_Date = Own_Data$Date[first_row]
)
# Note: SF235837 and SF65212 have the most missing values early in the sample
# but are subsumed into the consolidated Banxico holding calculated below.

rm(first_row)

# 2. Calculate Total Banxico Holding ----------------------------------------------

# Banxico's total holding is the sum of its three sub-components. See Description
# in Own_Data_startdate to identify this.
# NAs are set to 0 before summing so they do not propagate into the aggregate.

Own_Data[is.na(Own_Data)] <- 0
Own_Data$Banxico <- rowSums(Own_Data[, c("SF235837", "SF65210", "SF65212")])

# 3.a. Validate Component Sum Against Official Total ----------------------------
# Banxico's reported total (SF65219) occasionally differs slightly from the
# sum of components, likely due to rounding. Using the component sum as the
# denominator avoids shares systematically exceeding 1.

Own_Data$Sum <- rowSums(Own_Data[,
                                 c("SF235837", "SF65210", "SF65211", "SF65212",
                               "SF65213",  "SF65214", "SF65215", "SF65216", 
                               "SF65218")]
                        )

Diff_pct <- (Own_Data$Sum/ Own_Data$SF65219 - 1) * 100

n_diff <- sum(Diff_pct >= 0.01, na.rm = TRUE)
message(sprintf("Observations where component sum exceeds official total by >= 0.01%%: %d of %d",
                n_diff, nrow(Own_Data)))

# 3.b. Calculate Ownership Proportions ------------------------------------------
# Each category's proportion = category value / sum of components.

prop_cols <- setdiff(colnames(Own_Data)[-1], c("SF65219","Sum"))    # columns whose proportions are calculated
prop_names <- paste0(prop_cols, "_p")

Own_Data[, prop_names] <- lapply(prop_cols, function(col) {
  Own_Data[[col]] / Own_Data$Sum
})

# 3.c. Saving Cleaned Data ------------------------------------------
names(Own_Data)[names(Own_Data) == "SF65218"] <- "F_Own"
names(Own_Data)[names(Own_Data) == "SF65218_p"] <- "F_Own_p"

save(Own_Data, file = file.path(DATA_CLEAN, "Own_Data.RData" ))

# removing variables no longer necessary
rm(Diff_pct, n_diff)
message(sprintf("Data on ownership of bonds by various categories cleaned.\n
                Cleaned data saved in %s", paste(getwd(),DATA_CLEAN, sep = "/")
                ) )