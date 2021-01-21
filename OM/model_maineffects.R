# model_maineffects.R - DESC
# OM/model_maineffects.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(doParallel)
registerDoParallel(3)

# --- RUN grid

# SETUP main effects run

basecase  <- list(M=0.30, sigmaR=0.6, steepness=0.8, cpues=14, lfreq=1, llq=1)

meffs <- mapply(function(x, y) x[ac(x) != ac(y)], full, basecase)

grid <- rbindlist(lapply(rep(list(basecase),
  sum(unlist(lapply(meffs, length)))), as.data.table))

grid[, col := rep(names(meffs), unlist(lapply(meffs, length)))]

for(i in names(grid))
  grid[col == i, i] <- meffs[[i]]

grid <- ss3om::nameGrid(grid, from=1)

grid <- setioalbgrid(grid, dir = "model/maineffects",
  base = "model/base", name = "abt", write = TRUE)

save(grid, file = "model/maineffects/grid.RData")

# RUN models
# ls | parallel -j8 --bar --progress '(cd {}; ss_3.30.15)'


# --- LOAD results

system("ln -s ../base/ model/maineffects/14-M0.3_sigmaR0.6_steepness0.8_cpues12_lfreq1.000_llq1.00_colbase_iter14")

load("model/maineffects/grid.RData")

grid <- rbind(grid, data.table(M=0.3, sigmaR=0.6, steepness=0.8, cpues=14,
  lfreq=1, llq=1.00, col="base", iter=14,
  id="14-M0.3_sigmaR0.6_steepness0.8_cpues12_lfreq1.000_llq1.00_colbase_iter14"))

res <- loadRES(dir = "model/maineffects", grid = grid, repfile="Report.sso.gz", compfile="CompReport.sso.gz", covarfile="covar.sso.gz")


# LOAD stocks

stk <- loadFLS(dir="model/maineffects")

save(res, stk, file="model/maineffects.RData", compress="xz")
