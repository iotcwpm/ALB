# indicators.R - DESC
# ioalbmse/data-raw/indicators.R

# Copyright European Union, 2015-2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

indicators <- list(
  # 1. S1
  S1 = list(~yearMeans(SB/SB0), name = "SB/SB[0]",
    desc = "Mean spawner biomass relative to unfished"),
  # 2. S2
  S2 = list(~apply(SB/SB0, c(1, 3:6), min), name = "min(SB/SB[0]", 
    desc = "Minimum spawner biomass relative to unfished"),
  # 3. S3
  S3 = list(~yearMeans(SB/SBMSY), name = "SB/SB[MSY]",
    desc = "Mean spawnwer biomass relative to SBMSY"),
  # 4. S4
  S4 = list(~yearMeans(F/Ftarget), name = "F/F[target]",
    desc = "Mean fishing mortality relative to target"),
  # 5. S5
  S5 = list(~yearMeans(F/FMSY), name = "F/F[MSY]",
    desc = "Mean fishing mortality relative to FMSY"),
  # 6. S6
  S6 = list(~yearSums(FLQuant((SB / SBMSY) > 1 & (F / FMSY) < 1)) / dim(SB)[2],
    name = "P(Green)", desc = "Probability of being in Kobe green quadrant"),
  # 7. S7
  S7 = list(~yearSums(FLQuant((SB / SBMSY) < 1 & (F / FMSY) > 1)) / dim(SB)[2],
    name = "P(Red)", desc = "Probability of being in Kobe red quadrant"),
  # 8. S8
  S8 = list(~iterProb(yearMeans(SB / SBMSY) >= 1), name = "P(SB >= SB[MSY])",
    desc = "Probability of SB greater/equal than SBMSY"),
  
  # 9. F1
  F1 = list(~yearSums((SB / (0.2 * SB0)) > 1) / dim(SB)[2],
    name = "P(SB > 0.20 %*% SB[0])",
    desc = "Probability of spawner biomass being above 20 SB[0]"),
  # 10. F2
  F2 = list(~yearSums((SB / SBlim) > 1) / dim(SB)[2], name = "P(SB > SB[lim])", 
    desc = "Probability of spawner biomass being above SBlim"),
  
  # 11. Y1
  Y1 = list(~yearMeans(C) / 1000, name = "hat(C), 1000 t",
    desc = "Mean catch over years (1000 t)"),
  # 12. Y3
  Y3 = list(~yearMeans(C/MSY), name = "C/MSY", 
    desc = "Mean proportion of MSY"),
  
  # 13. T1
  T1 = list(~yearMeans(C[, -1]/C[, -dims(C)$year]), name = "var(C)",
    desc = "Mean absolute proportional change in catch"),
  # 14. T2
  T2 = list(~sqrt(yearVars(C))/yearMeans(C), name = "CV(C)",
    desc = "Catch variability"),
  # 15. T3
  T3 = list(~yearVars(F), name = "var(F)",
    desc = "Variance in fishing mortality"),
  # 16. T4
  T4 = list(~yearSums(C < 0.1 * MSY) / dim(C)[2], name = "P(catch < 0.1 %*% MSY)", 
    desc = "Probability of fishery shutdown")
  )

save(indicators, file="../data/iotcindicators.RData", compress="xz")
