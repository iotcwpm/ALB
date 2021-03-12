# perfect.R - DESC
# /perfect.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(mse)
library(mseviz)

# R

source('../../R/pikeperch/mps.R')
source('../../R/indicators.R')
source('../../R/pikeperch/metrics.R')
source('../../R/pikeperch/mps.R')
source('../../R/mplot.R')

# DATA

load("../../om/pikeperch/om.RData")

# ARGUMENTS

mseargs <- list(y0=1993, iy=2019, fy=2030, nsqy=3, seed = 1234,
  data_lag=0)

# PERFECT oem

oem <- FLoem(method=perfect.oem,
  observations=oem@observations,
  deviances=oem@deviances)

# --- FMSY

fwmsy <- fwd(stock(om), sr=sr(om), deviances=residuals(sr(om)),
  fbar=window(stock(stock(om)), start=2020) %=% refpts(om)$FMSY)

plot(fwmsy)


# --- F=0

fw0 <- fwd(stock(om), sr=sr(om), deviances=residuals(sr(om)),
  fbar=window(stock(stock(om)), start=2020) %=% 0.001)

plot(fw0)


# --- perfect sa

control <- mpCtrl(list(
	est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=psacatch4010.hcr, args=list(MY=200)),
  isys = mseCtrl(method=birds.is,
    args=list(bcatch=mean(oem@observations$birds$catch[,ac(2014:2018)])))))

psa <- tunebisect(om, oem=oem, control=control, metrics=metrics, args=mseargs,
  indicator=indicators[c("PFMSY")], tune=list(MY=c(50, 500)),
  prob=0.5, tol=0.02, maxit=15, pyears=list(2026:2028),
  refpts=refpts(om))

plot(om, PSA=psa)

# --- perfect sa 2017

mseargs <- list(y0=1993, iy=2017, fy=2030, nsqy=3, seed = 1234,
  data_lag=0)

psa17 <- tunebisect(om, oem=oem, control=control, metrics=metrics, args=mseargs,
  indicator=indicators[c("PFMSY")], tune=list(MY=c(50, 500)),
  prob=0.5, tol=0.02, maxit=15, pyears=list(2026:2028),
  refpts=refpts(om))

plot(om, PSA17=psa17)

# --- perfect saoem

mseargs <- list(y0=1993, iy=2019, fy=2030, nsqy=3, seed = 1234,
  data_lag=0)

oem <- FLoem(method=sampling.oem,
  observations=oem@observations,
  deviances=oem@deviances)

control <- mpCtrl(list(
	est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=psacatch4010.hcr, args=list(MY=200)),
  isys = mseCtrl(method=birds.is, args=list(bcatch=20))
))

psaoem <- tunebisect(om, oem=oem, control=control, metrics=metrics, args=mseargs,
  indicator=indicators[c("PFMSY")], tune=list(MY=c(50, 500)),
  prob=0.5, tol=0.02, maxit=15, pyears=list(2026:2028),
  refpts=refpts(om))

plot(om, PSA=psa, PSAOEM=psaoem)


# --- SAVE

save(fwmsy, fw0, psa, psaoem, psa17, file="perfect/perfect.RData",
  compress="xz")


# --- PLOTS

# psa
png(file=file.path("perfect", "psa.png"),
  type="cairo", units="in", width=6, height=5, pointsize=9, res=144)
plot(om, psa)
dev.off()

# fw0
png(file=file.path("perfect", "fw0.png"),
  type="cairo", units="in", width=6, height=5, pointsize=9, res=144)
plot(fw0)
dev.off()

# fwmsy
png(file=file.path("perfect", "fwmsy.png"),
  type="cairo", units="in", width=6, height=5, pointsize=9, res=144)
plot(fwmsy)
dev.off()

# FMSY
mpng(file="perfect/fwmsy.png",
  plot(fwmsy)
)

# F0
mpng(file="perfect/fw0.png",
  plot(fw0)
)

# PSA
mpng(file="perfect/psa.png",#
  plot(om, psa)
)
     
# PSA on refpts
rps <- refpts(psa)[c("FMSY", "Blim")]

mpng(file="perfect/psa_refpots.png",
plot(FLQuants(`F/F[MSY]`=fbar(stock(psa)) / refpts(psa)$FMSY,
  `SSB/B[lim]`=ssb(stock(psa)) / refpts(psa)$Blim)) +
  geom_hline(yintercept=1, linetype=2) +
  facet_grid(qname~., labeller = label_parsed)
)


