# cpue.R - DESC
# /cpue.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(FLa4a)
library(mse)
library(mseviz)

# R

source("../../R/indicators.R")
source("../../R/pikeperch/metrics.R")
source("../../R/pikeperch/mps.R")
source("../../R/lbspr/oemLn.R")
source("../../R/pikeperch/cpue.R")
source("../../R/mplot.R")

# DATA

load("../../om/pikeperch/om.RData")

# ARGUMENTS

mseargs <- list(y0=1993, iy=2019, fy=2030, nsqy=3, seed = 1234,
  data_lag=0)
target <- mean(quantSums(index(observations(oem)$idx[[1]])[1:2,
  ac(1995:2010)] * stock.wt(stock(om))[1:2, ac(1995:2010)]))

# OEM

oem <- FLoem(method=fyma.oem,
  observations=oem@observations,
  deviances=oem@deviances)


# --- TEST RUN

control <- mpCtrl(list(
  hcr = mseCtrl(method=cpue.hcr,
    args=list(k1=0.1, k2=0.1, k3=0.1, k4=0.1,
      dtaclow=0.80, dtacupp=1.20, target=target, start=2020)),
  est = mseCtrl(method=cpue.ind, args=list(nyears=5)),
  isys = mseCtrl(method=birds.is,
    args=list(bcatch=mean(oem@observations$birds$catch[,ac(2014:2018)])))))

test <- mp(om=om, oem=oem, ctrl=control, args=mseargs,
   tracking=c("cpue.mean", "cpue.slope"))

# --- TUNE for P(F=FMSY) = 0.5

range <- round(c(0.1, 4) * target)

tun05 <- tunebisect(om, oem=oem, control=control, metrics=metrics, args=mseargs,
  indicator=indicators[c("PFMSY")], tune=list(target=range), 
  prob=0.5, tol=0.02, maxit=15, pyears=list(2026:2028),
  refpts=refpts(om), tracking=c("cpue.mean", "cpue.slope"))

plot(om, TUN05=tun05)


# --- TUNE for P(B<Blim) = 0.05

tunblim <- tunebisect(om, oem=oem, control=control, metrics=metrics, 
  args=mseargs, indicator=indicators[c("PBlim")], tune=list(target=range), 
  prob=0.05, tol=0.002, maxit=15, pyears=list(2030:2040),
  refpts=refpts(om), tracking=c("cpue.mean", "cpue.slope"))

plot(om, FMSY=tun05, Blim=tunblim)

tuns <- list(FMSY=tun05, Blim=tunblim)


# --- TUNE w/ no TAC limits

nlcontrol <- mpCtrl(list(
  hcr = mseCtrl(method=cpue.hcr,
    args=list(k1=0.1, k2=0.1, k3=0.1, k4=0.1,
      dtaclow=0.01, dtacupp=10, target=target)),
  est = mseCtrl(method=cpue.ind, args=list(nyears=5)),
  isys = mseCtrl(method=birds.is, args=list(bcatch=21))))

tunnl <- tunebisect(om, oem=oem, control=nlcontrol, metrics=metrics, 
  args=mseargs, indicator=indicators[c("PFMSY")], tune=list(target=range), 
  prob=0.5, tol=0.02, maxit=15, pyears=list(2026:2028),
  refpts=refpts(om), tracking=c("cpue.mean", "cpue.slope"))


# --- SET min.cpue, min.catch

mincontrol <- mpCtrl(list(
  hcr = mseCtrl(method=cpue.hcr,
    args=list(k1=0.1, k2=0.1, k3=0.1, k4=0.1,
      dtaclow=0.75, dtacupp=1.20, target=12,
      min.mcpue=0.20, min.catch=5)),
  est = mseCtrl(method=cpue.ind, args=list(nyears=5)),
  isys = mseCtrl(method=birds.is, args=list(bcatch=21))))

tunmin <- tunebisect(om, oem=oem, control=mincontrol, metrics=metrics,
  args=mseargs, indicator=indicators[c("PFMSY")], tune=list(target=range), 
  prob=0.5, tol=0.02, maxit=15, pyears=list(2026:2028),
  refpts=refpts(om), tracking=c("cpue.mean", "cpue.slope"))


# --- TM INCREASE mesh size

origsel <- harvest(stock(om))[, ac(2020:2040)]
nsel <- FLQuant(c(0.2, 0.7, 0.8, 0.9, rep(1, 3), 0.9, 0.8, 0.7, 0.6),
  dimnames=list(age=0:10, year=2020:2040))
harvest(stock(om))[, ac(2020:2040)] <- nsel

runsel <- mp(om=om, oem=oem, ctrl=tun05@control, args=mseargs,
   tracking=c("cpue.mean", "cpue.slope"))

harvest(stock(om))[, ac(2020:2040)] <- origsel

# --- TUNE from 2017

mseargs17 <- list(y0=1993, iy=2017, fy=2030, nsqy=3, seed = 1234,
  data_lag=0)

tun2017 <- tunebisect(om, oem=oem, control=control, metrics=metrics,
  args=mseargs17, indicator=indicators[c("PFMSY")],
  tune=list(target=c(0.1, 40)), 
  prob=0.5, tol=0.02, maxit=15, pyears=list(2026:2028),
  refpts=refpts(om), tracking=c("cpue.mean", "cpue.slope"))

plot(om, tun2017)

# --- OUTPUT

catch(stock(om))[,ac(1993:2018)]  <- catch(stock(om))[,ac(1993:2018)] -
  oem@observations$birds$catch[,ac(1993:2018)]

# performance
perf <- performance(FLStocks(lapply(tuns, stock)), indicators=indicators,
  refpts=refpts(om), metrics=metrics, years=list(2026:2028, 2020:2030),
  mp="cpue")

# performance by year 
yperf <- performance(FLStocks(lapply(tuns, stock)), indicators=indicators[-3],
  refpts=refpts(om), metrics=metrics, years=2020:2030, mp="cpue")


# --- SAVE

save(tuns, tunblim=tunblim, tunfmsy=tun05, tunnl=tunnl,
  tunmin=tunmin, tun2017=tun2017, runsel=runsel, perf, yperf,
  file="cpue/cpue.RData", compress="xz")


# --- PLOTS

# tuns
mpng("cpue/runs.png",
plot(om, FMSY=tuns[[1]], Blim=tuns[[2]], probs=c(.10, .25, .50, .75, .90))
)

pe <- perf[year == 2028 & run %in% c("FMSY", "Blim"),]

# plotBPS
mpng("cpue/bps.png",
plotBPs(pe, target=c(FMSY=1, BVBi=1), limit=c(Blim=1),
  indicators=c("FMSY", "Blim", "BVBi", "BWML", "C"))
)

# plotTOs
mpng("cpue/tos.png",
plotTOs(pe, x="C", y=c("FMSY", "Blim", "BVBi", "BWML"))
)

# kobeMPs
mpng("cpue/kobe.png",
kobeMPs(pe, x="SBMSY", y="FMSY") + ylim(c(0, 2))
)

# FMSY
mpng("cpue/fmsy.png",
plot(FLQuants(lapply(tuns, function(x) fbar(stock(x)) / refpts(x)$FMSY))) +
  geom_hline(yintercept=1, linetype=2) + ylim(c(0, 4)) +
  facet_grid(qname~.,
    labeller = as_labeller(c(FMSY="P(F/FMSY)=50%", Blim="P(B<Blim)=5%"))) +
  ylab("F/FMSY")
)

# Blim
mpng("cpue/blim.png",
plot(FLQuants(lapply(tuns, function(x) stock(stock(x)) / refpts(x)$Blim))) +
  geom_hline(yintercept=1, linetype=2) + ylim(c(0, 4)) +
  facet_grid(qname~.,
    labeller = as_labeller(c(FMSY="P(F/FMSY)=50%", Blim="P(B<Blim)=5%"))) +
  ylab("B/Blim")
)


# tunsel

mpng("cpue/runsel.png",
plot(om, Current=tun05, New=runsel)
)

mpng("cpue/selex.png",
ggplot(FLQuants(Current=origsel, New=nsel), aes(age, data)) +
  geom_line(aes(group=qname, colour=qname)) +
  xlab("Age") + ylab("Selectivity") +
  theme(legend.position="none")
)
