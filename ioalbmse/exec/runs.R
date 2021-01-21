# runs.R - DESC
# /runs.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ioalbmse)

library(doParallel)
registerDoParallel(4)

# --- SETUP

# data
data(indicators)
data(oms)

# PREPARE cpue
cpue <- propagate(window(ocpue$index, end=2040), 200)
# BUG Order of age, fix in condition.R
cpuesel <- ocpue$sel.pattern[c(1,7:14,2:6),'2014']

oemparams <- FLPar(sd=c(sqrt(yearVars(ocpue$index.res))),
  b=c(apply(osr$residuals, c(1,3:6), function(x) acf(x, plot=FALSE)$acf[2])))
oemparams <- FLPar(sd=0.3, b=0)

# GENERATE SR residuals
resd <- c(sqrt(yearVars(osr$residuals)))
rerho <- c(apply(osr$residuals, c(1,3:6), function(x) acf(x, plot=FALSE)$acf[2]))

sres <- FLQuant(0, dimnames=dimnames(window(stock(omp), start=2014)))
for(i in seq(dim(sres)[6]))
  sres[,,,,,i] <- rlnoise(1, sres[,,,,,i], sd=resd[1], b=rerho[i])
osr@.Data[[1]] <- sres

# ARGUMENTS
years <- seq(2014, 2034, by=2)
outyears <- years[1] + c(1, 5, 10, 20)

# --- LONG TERM projection at F_bar~0

Rltom <- fwd(omp, sr=osr, residuals=sres,
  control=fwdControl(quant='f', year=2015:2040, value=1e-5))

# --- CONSTANT catch

Rconc <- fwd(omp, sr=osr, residuals=sres,
  control=fwdControl(list(quant='catch', year=2015:2040, value=rep(c(rpts$MSY), each=26))))

# --- CONSTANT F

Rconf <- fwd(omp, sr=osr, residuals=sres,
  control=fwdControl(list(quant='f', year=2015:2040, value=rep(c(rpts$FMSY), each=26))))

plot(Rltom, Rconc, Rconf)

save(Rltom, Rconc, Rconf, file="base_runs.RData", compress="xz")


# --- msePT

# Initial test run: 7.2 min w/SA, 1.1 min w/o

system.time(P0 <- msePT(omp, osr, rpts, cpue=cpue, cpuesel=cpuesel, years=years,
  hcrparams=FLPar(lambda=0.5, dltac=0.25, dhtac=0.15, Dlimit=0.15, Dtarget=0.50), 
  oemparams=oemparams, sa=FALSE))

plot(P0$om) + geom_vline(aes(xintercept=an(ISOdate(2015,1,1))))

# Perturbation trial, F_2013 = F_2012 * 3 + MP

P0pt <- fwd(omp, sr=osr,
  control=fwdControl(list(year=2013, quant="f", value=c(fbar(omp)[,'2012'] * 2))))

P0pt <- msePT(P0pt, osr, cpue=cpue, cpuesel=cpuesel, years=years,
  hcrparams=FLPar(lambda=0.5, dltac=0.25, dhtac=0.15, Dlimit=0.15, Dtarget=0.50), 
  oemparams=FLPar(sd=0.2, b=0))

plot(window(om, end=2012), P0pt$om) + geom_vline(aes(xintercept=an(ISOdate(2013,1,1)))) +
  geom_vline(aes(xintercept=an(ISOdate(2015,1,1))))

save(P0, P0pt, file="pt/msePT_runs.RData", compress="xz")


# TUNING

# Run 1st grid: 5000 runs

grid <- list(lambda=seq(0.5, 1.50, length=5),
  dltac=seq(0.10, 0.30, length=3), dhtac=seq(0.10, 0.30, length=3),
  Dlimit=seq(0.10, 0.20, length=3), Dtarget=seq(0.30, 0.50, length=3))

# 200 cores, 6819 = 1h 53m
system.time(rgPT <- doRuns(msePT, grid=grid, omp=omp, sr=osr,
  cpue=cpue, cpuesel=cpuesel, years=years, oemparams=FLPar(sd=0.3, b=0)))

save(rgPT, file="pt/grid_msePT_5000.RData", compress="xz")

# COMPUTE P(SB_2015-2034 > SBMSY)
tun1 <- unlist(lapply(rgPT,
  function(x) sum(c(ssb(x)[,ac(2015:2034)]) > c(rpts$SBMSY)) / (length(ssb(x)[,ac(2015:2034)]))))

# SUBSET runs with 0.48 > P < 0.25
idx1 <- names(tun1)[tun1 < 0.501 & tun1 > 0.499]
rg1pt <- rgPT[sample(idx1, 4)]

prg1pt <- performance(rg1pt, indicators, rpts, years=2034, grid=grid, mp="PT")
qrg1pt <- performance(rg1pt, indicators, rpts, years=2034,
  probs=c(0.1, 0.25, 0.50, 0.75, 0.90), grid=grid, mp="PT")

save(rg1pt, prg1pt, qrg1pt, file="pt/rg1_msePT.RData", compress="xz")

# COMPUTE P(SB_2015-2034 > SBMSY & F_2015-2034 < FMSY)
tun2 <- unlist(lapply(rgPT, function(x)
  sum(c(ssb(x)[,ac(2015:2034)]) > c(rpts$SBMSY) & c(fbar(x)[,ac(2015:2034)]) < c(rpts$FMSY)) /
    (length(ssb(x)[,ac(2015:2034)]))))

# SUBSET runs with 0.72 > P < 0.78
idx2 <- names(tun2)[tun2 < 0.751 & tun2 > 0.749]
rg2pt <- rgPT[sample(idx2, 4)]

prg2pt <- performance(rg2pt, indicators, rpts, years=outyears, grid=grid, mp="PT")
qrg2pt <- performance(rg2pt, indicators, rpts, years=outyears,
  probs=c(0.1, 0.25, 0.50, 0.75, 0.90), grid=grid, mp="PT")

save(rg2pt, prg2pt, qrg2pt, file="pt/rg2_msePT.RData", compress="xz")


# ERROR levels

# RECRUITMENT failure


# --- mseIndex

# Initial test run

system.time(I0 <- mseIndex(omp=omp, sr=osr, cpue=cpue, cpuesel=cpuesel,
  hcrparams=FLPar(lambda=1.05, ny=5, dltac=0.20, dhtac=0.10),
  years=years, oemparams=FLPar(b=0.3, sd=0), imparams=NA, verbose=TRUE))

plot(I0$om) + geom_vline(aes(xintercept=an(ISOdate(2015,1,1))))

# Perturbation trial, F_2013 = F_2012 * 3 + MP

I0pt <- fwd(omp, sr=osr,
  control=fwdControl(list(year=2013, quant="f", value=c(fbar(omp)[,'2012'] * 2))))

I0pt <- mseIndex(I0pt, sr=osr, cpue=cpue, cpuesel=cpuesel,
  hcrparams=FLPar(lambda=1.05, ny=5, dltac=0.20, dhtac=0.10),
  years=years, oemparams=FLPar(b=0.3, sd=0), imparams=NA, verbose=TRUE)
 
plot(window(om, end=2012), I0pt$om) + geom_vline(aes(xintercept=an(ISOdate(2013,1,1)))) +
  geom_vline(aes(xintercept=an(ISOdate(2015,1,1))))

save(I0, I0pt, file="ind/mseIND_runs.RData", compress="xz")

# TUNING

# Run grid: 270 runs

grid <- list(lambda=seq(0.2, 2, length=20), ny=seq(3, 5),
  dltac=seq(0.10, 0.30, length=3), dhtac=seq(0.10, 0.30, length=3))

system.time(rgIND <- doRuns(mseIndex, grid=grid, omp=omp, sr=osr,
  cpue=cpue, cpuesel=cpuesel, years=years, oemparams=FLPar(sd=0.3, b=0)))

save(rgIND, file="grid_mseIndex.RData", compress="xz")

# COMPUTE P(SB_2015-2034 > SBMSY)
tun1 <- unlist(lapply(rgIND,
  function(x) sum(c(ssb(x[,ac(2020:2034)]) > c(rpts$SBMSY)) / (length(ssb(x)[,ac(2020:2034)]))))

# SUBSET runs with 0.48 > P < 0.25
idx1 <- names(sort(abs(tun1 - 0.50))[1:4])
rg1ind <- rgIND[idx1]

prg1ind <- performance(rg1ind, indicators, rpts, years=outyears, grid=grid, mp="IND")
qrg1ind <- performance(rg1ind, indicators, rpts, years=outyears,
  probs=c(0.1, 0.25, 0.50, 0.75, 0.90), grid=grid, mp="IND")

save(rg1ind, prg1ind, qrg1ind, file="ind/tun1_mseIndex.RData", compress="xz")

# COMPUTE P(SB_2015-2034 > SBMSY & F_2015-2034 < FMSY)

t2 <- lapply(rgIND, function(x)
  FLQuant(sum((ssb(x)[,ac(2020:2034)] / rpts$SBMSY) > 1 & (fbar(x)[,ac(2020:2034)] / rpts$FMSY) < 1) / length(ssb(x)[,ac(2020:2034)])))

tun2 <- unlist(lapply(rgIND, function(x)
  sum(c(ssb(x)[,ac(2020:2034)]) > c(rpts$SBMSY) & c(fbar(x)[,ac(2020:2034)]) < c(rpts$FMSY)) /
    (length(ssb(x)[,ac(2020:2034)]))))

# SUBSET runs with 0.72 > P < 0.78
idx2 <- names(sort(abs(tun2 - 0.75))[1:4])
rg2ind <- rgIND[idx2]

prg2ind <- performance(rg2ind, indicators, rpts, years=outyears, grid=grid, mp="IND")
qrg2ind <- performance(rg2ind, indicators, rpts, years=outyears,
  probs=c(0.1, 0.25, 0.50, 0.75, 0.90), grid=grid, mp="IND")

save(rg2ind, prg2ind, qrg2ind, file="ind/tun2_mseIndex.RData", compress="xz")












# COMBINE datasets

# TUNING 1

names(rg1ind) <- paste("IND_", names(rg1ind))
names(rg1pt) <- paste("PT_", names(rg1pt))

runs <- FLStocks(c(rg1ind, rg1pt))
perf <- cbind(prg1ind, prg1pt)
qperf <- cbind(qrg1ind, qrg1pt)

save(runs, perf, qperf, file="tun1.RData", compress="xz")

