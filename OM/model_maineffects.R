# model_maineffects.R - DESC
# OM/model_maineffects.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(icesTAF)
library(ioalbmse)

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

system("ln -s ../base/ model/maineffects/12-M0.3_sigmaR0.6_steepness0.8_cpues14_lfreq1.000_llq1.00_colbase_iter12")

load("model/maineffects/grid.RData")

grid <- rbind(grid, data.table(M=0.3, sigmaR=0.6, steepness=0.8, cpues=14,
  lfreq=1, llq=1.00, col="base", iter=12,
  id="12-M0.3_sigmaR0.6_steepness0.8_cpues14_lfreq1.000_llq1.00_colbase_iter12"))

maineffects <- loadOMS(dir="model/maineffects", grid=grid, combine=FALSE)

save(maineffects, file="model/maineffects/load.RData", compress="xz")


ggplot(res, aes(x=factor(M), y=ssb1988)) + geom_point() +
  facet_grid(sigmaR + steepness ~ cpues + lfreq + llq,
    labeller = labeller(.cols=label_both, .rows = label_both))




# PLOT compare SSB corners & maineffects
dat <- rbindlist(list(main=as.data.frame(FLQuants(lapply(maineffects$output, extractSSB))),
  corners=as.data.frame(FLQuants(lapply(corners$output[-c(3, 7)], extractSSB)))), idcol="set")

ggplot(dat, aes(x=year, y=data, color=set, group=qname)) +
  geom_line() + facet_wrap(~set)


res <- oms$res

stk <- oms$stock

stks <- lapply(stk, simplify)

srr <- oms$sr

plot(stk, metrics=list(Rec=rec, SSB=ssb)) + facet_grid(qname~stock, scales="free")
