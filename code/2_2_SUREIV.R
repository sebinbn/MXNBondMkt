

## First stage regression -------------------------------------------------

Stage1 = lm(F_Own ~ 0 + EFFR + d_ln_IIP, data = Mex_m_diff)
summary(Stage1)

# For weak instrument, tF se = se * tf correction factor, where the correction
# factor depends on the 1st stage F. 
# Below, I use Table 3A from Lee et.al (2022) in AER. This table shows correction
# factor for few Stage 1 Fs, and the values in between can be interpolated.
# From the Table,
# Stage 1 F - tF correction
# 7.940     - 2.025
# 8.196     - 1.980
# 8.473     - 1.935
# 8.773     - 1.892
# Since Stage 1 F-stat falls between 8.196 and 8.473, the precise correction 
# factor is interpolated in tFCorr 
Stg1_F = summary(Stage1)$fstatistic["value"] 
tFCorr = 1.935 + (8.473 - Stg1_F)/(8.473 - 8.196)* (1.98-1.935)

## Second stage SURE -------------------------------------------------