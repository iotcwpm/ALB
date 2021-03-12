# model.R - DESC
# /model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ioalbmse)
library(mse)

load_all('../mse/')



# LOAD OM and OEM

load('../OM/model/om.RData')

# MSE arguments

mseargs <- list(y0=1950, iy=2017, fy=2039, nsqy=3, seed = 1234, data_lag=1)

# INDICATORS

indicators <- list(
  # P(SB >= SB_MSY)
  S8 = list(~iterProb((yearMeans(SB) / SBMSY) >= 1),
    name = "P(SB >= SB[MSY])",  desc = "Probability of SB greater/equal than SBMSY"),
  # P(Green)
  S6 = list(~yearSums(FLQuant((SB / SBMSY) > 1 & (F / FMSY) < 1)) / dim(SB)[2],
    name = "P(Green)", desc = "Probability of being in Kobe green quadrant"))

# --- PERFECT

method(oem) <- perfect.oem

# control

control <- mpCtrl(list(
  hcr = mseCtrl(method=catchSSB.hcr,
    args=list(dtarget=0.10, dlimit=0.10, lambda=2, MSY=c(refpts(om)$MSY), ssb_lag=2)),
  est = mseCtrl(method=perfect.sa)))

# TEST run

run <- mp(om=om, oem=oem, ctrl=control, args=mseargs)

# TUNE for P(F=FMSY) = 0.5

system.time(
per_05SBMSY <- tunebisect(om, oem=oem, control=control, args=mseargs,
  metrics=list(SB=ssb), indicator=indicators["S8"], pyears=2025:2039,
  tune=list(dtarget=c(0.10, 0.90)), prob=0.5, tol=0.1, maxit=12)
)


control <- mpCtrl(list(
  hcr = mseCtrl(method=ices.hcr,
    args=list(fmin=0.05, ftrg=0.40, blim=50000, bsafe=250000)),
  est = mseCtrl(method=perfect.sa)))

system.time(
per_05SBMSY <- tunebisect(om, oem=oem, control=control, args=mseargs,
  metrics=list(SB=ssb), indicator=indicators["S8"], pyears=2025:2039,
  tune=list(bsafe=c(100000, 500000)), prob=0.5, tol=0.1, maxit=12)
)


plot(om, per_05SBMSY$min, per_05SBMSY$max)


# GRID of catchSSB params

# TO FIND

library(doParallel)
registerDoParallel(50)

grs <- expand.grid(list(dtarget=seq(0.20, 0.60, length=5),
  dlimit=seq(0.05, 0.20, length=5), lambda=seq(0.5, 5, length=5)))

gruns <- foreach(i=seq(dim(grs)[1])) %dopar% {

  # CHANGE control
  con <- control
  con$MSY <- 30000
  con$hcr@args[names(grs)] <- grs[i,]

  run <- mp(om, oem=oem, ctrl=con, args=mseargs, parallel=FALSE)
  per <- performance(metrics(stock(run), metrics=list(SB=ssb)),
    indicator=indicators["S8"], years=2025:2039, refpts=om@refpts,
    probs=NULL)
  cat("--- [", i, "]\n")
  list(run=run, perf=per)
}

save(grs, gruns, file="gruns.RData")

load("perf.RData")

per <- rbindlist(lapply(gruns, "[[", "perf"), idcol="run")
perf <- per[, .(prob=mean(data)), by=run]

res <- cbind(perf, grs)

res[prob == min(prob),]


# TEST run

run <- mp(om=om, oem=oem, ctrl=control, args=mseargs)


control <- mpCtrl(list(
  hcr = mseCtrl(method=catchSSB.hcr,
    args=list(dtarget=0.20, dlimit=0.05, lambda=5, MSY=c(refpts(om)$MSY), ssb_lag=2)),
  est = mseCtrl(method=perfect.sa)))

# TUNE for P(F=FMSY) = 0.5

system.time(
per_gridtun <- tunebisect(om, oem=oem, control=control, args=mseargs,
  metrics=list(SB=ssb), indicator=indicators["S8"], pyears=2025:2039,
  tune=list(dtarget=c(0.15, 0.25)), prob=0.5, tol=0.1, maxit=12)
)


ggplot(perf, aes(run, prob)) + geom_point()


# --- TUNE for P(F=FMSY) = 0.5

# --- TUNE

# --- OUTPUT

# list

# performance

# SAVE
