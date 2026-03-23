# PURPOSE:
#   1. Import IMF annual data on foreign liabilities in government debt from DATA_RAW.
#   2. Extract data for 20 EMEs (as defined by IMF) from 2004 to 2024.
#   3. Save a cleaned country-year table and sum of EME liability to DATA_CLEAN.
#
# INPUT:  <DATA_RAW>/IIP_lblty_GG_vOct2025.csv (in Mns USD)
#         
# OUTPUT: <DATA_CLEAN>/IMF_GDebt_FO_EM_2004_2024.csv (in Bns USD)
#         
# CALLED BY: MXNBnd_Replicate.R

# 1. Import IMF Data  ----------------------------------------------

required_cols = c("COUNTRY", "TIME_PERIOD", "OBS_VALUE")

imf_raw = read.csv(file.path(DATA_RAW, "IIP_lblty_GG_vOct2025.csv" ),
                   check.names = FALSE)
imf_raw = imf_raw[,c("COUNTRY", "TIME_PERIOD", "OBS_VALUE")]
imf_raw$TIME_PERIOD = as.integer(imf_raw$TIME_PERIOD)
imf_raw$OBS_VALUE = as.numeric(imf_raw$OBS_VALUE)

imf_subset = imf_raw[imf_raw$TIME_PERIOD >= 2004 & imf_raw$TIME_PERIOD <= 2024,]

# 2. Match EMEs to country names in the IMF file ---------------------------

# List of 20 IMF EME countries sourced from 
# https://www.imf.org/external/pubs/ft/fandd/2021/06/the-future-of-emerging-markets-duttagupta-and-pazarbasioglu.htm
# Names have been added as they appear in the IMF dataset after manually exploring the dataset..
eme_countries =  c(
  "Argentina", "Brazil", "Chile", "China, People's Republic of", "Colombia",
  "Egypt, Arab Republic of", "Hungary", "India", "Indonesia", "Iran",
  "Malaysia", "Mexico", "Philippines", "Poland, Republic of", "Russian Federation",
  "Saudi Arabia", "South Africa", "Thailand", "Türkiye, Republic of", "United Arab Emirates"
)

eme_year_grid = expand.grid(Country = eme_countries, Year = 2004:2024,
                            stringsAsFactors = FALSE)

# Merging to pick EME data from imf_subset. all.x = TRUE keeps EMEs that don't have data as well
GDebt_FO_EM = merge(eme_year_grid,imf_subset,
                    by.x = c("Country", "Year"),  by.y = c("COUNTRY", "TIME_PERIOD"),
                    all.x = TRUE)
GDebt_FO_EM$OBS_VALUE = GDebt_FO_EM$OBS_VALUE/1000  #converting to Bns of USD

message(sprintf(
  "Note: EMEs that dont have data in IMF PIP dataset for any year between 2004 and 2024 are: \n%s",
  paste(GDebt_FO_EM %>%
          group_by(Country) %>%
          summarise(all_na = all(is.na(OBS_VALUE)), .groups = "drop")%>%
          filter(all_na)%>%
          pull(Country),
        collapse = "\n")
))

GDebt_FO_EM = reshape(GDebt_FO_EM, timevar = "Country",idvar = "Year",
                      direction = "wide")
names(GDebt_FO_EM)[-1] = substr(names(GDebt_FO_EM)[-1],
                                nchar("OBS_VALUE")+2,
                                nchar(names(GDebt_FO_EM)[-1]) )


# 3. Calculate EME total --------------------------------------------

GDebt_FO_EM$Total = rowSums(GDebt_FO_EM[,names(GDebt_FO_EM) != "Year"], na.rm = T)


# 4. Save data, remove intermediates ------------------------------------------

write.csv(GDebt_FO_EM, file = file.path(DATA_CLEAN, "IMF_EM_GDebt_2004-24.csv"),
    row.names = FALSE )

message(sprintf(
  "Table with EME's Foreign Owned Gov debt saved in %s",
  file.path(getwd(), DATA_CLEAN, "IMF_EME_GovDebt_2004_2024.csv")
))

rm(required_cols, imf_raw, imf_subset, eme_countries, eme_year_grid)