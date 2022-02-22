# om.R - DESC
# /om.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)
library(ss3om)
library(FLasher)

load("model/full.Rdata")

range(stock, c("minfbar", "maxfbar")) <- c(1, 12)


ss3z30 <- function(zatage, dmns) {

  zaa <- zatage[Yr %in% dmns$year, -1]
  setnames(zaa, c("Sex", "Yr"), c("unit", "year"))

  # CONVERT class of last age column
  zaa[[dim(zaa)[2]]] <- as.numeric(NA)

  zatage <- data.table::melt(zaa, id.vars=c("unit", "year"),
    measure.vars=dmns$age, variable.name="age", value.name = "data")
  
  z <- as.FLQuant(zatage, units="z")
  dimnames(z) <- dmns[-4]

  z[dim(z)[1],] <- z[dim(z)[1] - 1,]

  return(z)
}


# --- FWD 2018-19

nc <- c(`2018`=41615, `2019`=39426)

fctrl <- fwdControl(list(year=2018:2019, quant="catch", value=nc),
    list(year=2018:2019, quant="fbar", relYear=2017:2018, min=0, max=2))

stock <- fwd(stock, sr=sr, control=fctrl)


# --- RESAMPLE with weighting

# GENERATE weights

results[, weight:=pvalue]

# GENERATE resamples

set.seed(47)

its <- 500

samps <- sample.int(dim(results)[1], size=its, prob=results[,weight],
  replace=TRUE)

# RESAMPLE runs based on weights

stock <- iter(stock, samps)
dimnames(stock) <- list(iter=seq(its))

sr <- iter(sr, samps)
dimnames(sr) <- list(iter=seq(its))

refpts <- iter(refpts, samps)
dimnames(refpts)$iter <- seq(its)

results <- results[samps, ]
results[, orig:=iter]
results[, iter:=seq(its)]

indices <- FLIndices(cpue1=iter(indices$LLCPUE1, samps),
  cpue3=iter(indices$LLCPUE3, samps))

indices <- lapply(indices, function(x) {
  dimnames(x) <- list(iter=seq(its))
  return(x)
  }
)

# FIX harvest

load("model/full/load.Rdata")

zs <- Reduce(combine, 
  lapply(full$output[samps], function(x)
    ss3z30(data.table(x$Z_at_age), dimnames(window(stock, end=2017))[1:5]))
)

harvest(stock)[, ac(1950:2017)] <- zs - m(stock)[,ac(1950:2017)]


# --- DEVIANCES

iy <- 2017
fy <- 2040

# rho: fishlife:albacore rho(rec)c = 0.47

rho <- 0.45

# PAST deviances

lastdevs <- residuals(sr)[, ac(2013:2015)]

alldevs <- residuals(sr)[, ac(1975:2015)]

# 1. Autocorrelated from 2010:2015

devsrho <- Reduce(combine, lapply(seq(its), function(x)
    ar1rlnorm(rho=rho, years=seq(2016, fy), iters=1,
    sdlog=results[x, Recr_sigma]) %*% yearMeans(lastdevs[,,,,,x])))

dimnames(devsrho) <- list(age=1, iter=results$iter)

# 2. Autocorrelated moving to N(0,1)

devsmov <- window(lastdevs, end=fy)

var <- rnorm(its, devsmov %=% 0, results[, Recr_sigma]) 

for(i in seq(2015, fy))
  devsmov[, ac(i)] <- rho * devsmov[, ac(i-1)] + var[, ac(i)] * sqrt(1 - rho^2)

# 3. GENERATE lnorm(0, sigma_i) deviances

devs0 <- Reduce(combine, lapply(results[, Recr_sigma], function(x)
  rlnorm(1, FLQuant(0, dimnames=list(year=seq(2016, fy), age=1)), x)))

# SRR deviances

deviances <- FLQuants(N=append(exp(lastdevs), devs0),
  RHO=exp(append(lastdevs, devsrho)), MOV=exp(devsmov))

# deviances <- lapply(deviances, expand, unit=c('F', 'M'))

ggsave("report/output/deviances.png",
plot(deviances) + geom_vline(xintercept=c(2015, 2018), linetype=3) +
  geom_hline(yintercept=1, linetype=2, size=0.3)
)

deviances <- lapply(deviances, expand, unit=c('F', 'M'), fill=TRUE)

# DEFAULT to moving deviances with rho=0.47

residuals(sr) <- deviances$N

# --- OM

tmp <- stf(window(stock, end=2017), end=fy)
stock <- window(stock, end=fy)
stock[, ac(2020:fy)] <- tmp[, ac(2020:fy)]

om <- FLom(stock=stock, sr=sr, refpts=refpts,
  projection=mseCtrl(method=fwd.om))

# ADD target and limit

refpts(om)$Ftarget <- refpts(om)$FMSY 
refpts(om)$SBlim <- refpts(om)$SBMSY * 0.20

# --- OEM: observations (stk, idx), deviances(sr, idx, stk)

stk <- nounit(stock)

# index.q


fits <- lapply(full$output, function(x) ss3om:::ss3index.fit(DT(x$cpue),
  setNames(nm=names(full$indices)[c(1,3)])))

fits <- Reduce(combine, lapply(fits, function(x) lapply(x, function(y) y[,,,1])))
dimnames(fits[[1]]) <- list(unit='all')
dimnames(fits[[2]]) <- list(unit='all')

fits[[1]] <- iter(fits[[1]], results$orig)
fits[[2]] <- iter(fits[[2]], results$orig)

nqs <- computeQ(indices, window(stk, start=1970), fits)
dimnames(nqs[[1]]) <- list(season='all')
dimnames(nqs[[2]]) <- list(season='all')

index.q(indices[[1]]) <- nqs[[1]]
index.q(indices[[2]]) <- nqs[[2]]

indices <- lapply(indices, fwdWindow, end=2040)

oem.obs <- list(stk=stk, idx=lapply(indices, fwdWindow, end=fy))

stk.devs <- FLQuants(
  catch.n=rlnorm(500, catch.n(stk) %=% 0, 0.2)
)

idx.devs <- lapply(indices, function(x) rlnorm(500, index.q(x) %=% 0, 0.3))

oem <- FLoem(observations=oem.obs, deviances=list(stk=stk.devs, idx=idx.devs),
  method=sampling.oem)

# SAVE

save(om, oem, results, deviances, file="output/om_full.Rdata", compress="xz")
