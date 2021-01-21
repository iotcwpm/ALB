# tune.R - DESC
# /tune.R

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
cpuesel <- ocpue$sel.pattern[-1,'2014']
oemparams <- FLPar(sd=c(sqrt(yearVars(ocpue$index.res))),
  b=c(apply(osr$residuals, c(1,3:6), function(x) acf(x, plot=FALSE)$acf[2])))

oemparams <- FLPar(sd=0.3, b=0)

# GENERATE SR residuals

sres <- rlnoise(200, len=FLQuant(0, dimnames=dimnames(cpue)), sd=oemparams$sd, b=oemparams$b, seed=2017) 

resd <- c(sqrt(yearVars(osr$residuals)))
rerho <- c(apply(osr$residuals, c(1,3:6), function(x) acf(x, plot=FALSE)$acf[2]))

sres <- FLQuant(0, dimnames=dimnames(window(stock(omp), start=2014)))
for(i in seq(dim(sres)[6]))
  sres[,,,,,i] <- rlnoise(1, sres[,,,,,i], sd=resd[1], b=rerho[i])
osr@.Data[[1]] <- sres

# ARGUMENTS
years <- seq(2014, 2034, by=2)
outyears <- years[1] + c(1, 5, 10, 20)


# --- TUNE 1: P(B > B_MSY) = 0.5, 20 y mean

grid <- list(lambda=seq(0.5, 1.50, length=5),
  dltac=seq(0.10, 0.30, length=3), dhtac=seq(0.10, 0.30, length=3),
  Dlimit=seq(0.10, 0.20, length=2), Dtarget=seq(0.30, 0.50, length=3))

system.time(rgPT <- doRuns(msePT, grid=grid, omp=omp, sr=osr, sa=FALSE,
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
  probs=c(0.1, 0.25, 0.50, 0.75, 0.90), grid=grid,mp="PT")

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




# TUNE 2: P(kobe=green) = 0.75, 20 y mean








# COMBINE datasets

# TUNING 1

load("pt/tun1_msePT.RData")
load("ind/tun1_mseIndex.RData")

names(rg1pt) <- paste("PT_", names(rg1pt))
names(rg1ind) <- paste("IND_", names(rg1ind))

runs <- FLStocks(c(rg1ind, rg1pt))
perf <- rbind(prg1ind[,1:7], prg1pt[,1:7])
qperf <- rbind(qrg1ind[,1:10], qrg1pt[,1:10])

save(runs, perf, qperf, file="tun1.RData", compress="xz")

# TUNING 2

load("pt/tun2_msePT.RData")
load("ind/tun2_mseIndex.RData")

names(rg2pt) <- paste("PT_", names(rg2pt))
names(rg2ind) <- paste("IND_", names(rg2ind))

runs <- FLStocks(c(rg2ind, rg2pt))
perf <- rbind(prg2ind[,1:7], prg2pt[,1:7])
qperf <- rbind(qrg2ind[,1:10], qrg2pt[,1:10])

save(runs, perf, qperf, file="tun2.RData", compress="xz")


