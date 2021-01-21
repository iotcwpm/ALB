# condition.R - DESC
# ioalbmse/exec/condition.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ioalbmse)
library(FLRP)

library(doParallel)
registerDoParallel(220)

# -- SETUP scenarios {{{
sce <- list(
	# M
	M=c('0202', '0303', '0404', '0403', '0402'),
	# sigmaR
	sigmaR=c(0.4, 0.6),
	# steepness
	steepness=c(0.7, 0.8, 0.9),
	# CPUE CV
	cpuecv=c(0.2, 0.3, 0.4, 0.5),
	# ESS
	ess=c(20, 50, 100),
	# CPUE LL q
	# Yearly multiplier for cpuecv
	llq=c('1.0000', '1.0025'),
	# CPUE LL select
	# Logarithmic or DoubleNormal
	llsel=c("Log", "DoNorm")
) # }}}

# -- SETUP grid/SS3 folders

dir <- "grid"

grid <- setioalbgrid(sce, dir=dir, base="sa", name="abt", write=TRUE)

# -- RUN SS3 grid
# $ parallel --joblog log --jobs 215 --progress 'cd {} && ss3' ::: *             
# tail -f log
runss3grid(grid, options="", dir=dir, pack=TRUE)

# -- LOAD results

# res
res <- loadres(dir=dir, repfile = "Report.sso",
  covarfile = "covar.sso", grid=grid)

# rpts: MSY, SB_MSY, F_MSY, SB0
rpts <- FLPar(MSY=res$TotYield_MSY, SBMSY=2 * res$SSB_MSY, FMSY=res$Fstd_MSY,
  SB0=2 * res$SPB_1950, Ftarget=res$Fstd_MSY, SBlim=2 * 0.40*res$SSB_MSY)

# omfull
omf <- loadomDT(dir=dir, repfile = "Report.sso", covarfile = "covar.sso",
  compfile="CompReport.sso")

# SET range of ages fully selected
range(omf, c("minfbar", "maxfbar")) <- c(5,12)

# SET name and desc(ription)
name(omf) <- "ALB"
desc(omf) <- paste("IOTC ALB SS3", dir)

save(omf, res, file="out/omfull.RData", compress="xz")

# fqsfull
fqs <- metrics(omf)

save(fqs, res, file="out/fqsfull.RData", compress="xz")


# --- INSPECT and SUBSET runs

# CHECK convergence
id1 <- res$Convergence_Level <= 0.001
id1 <- which(res$Convergence_Level <= 0.001)

# data.frame of K (t) vs. habitat size (km2) for all ALB stocks
prK <- data.frame(
	habitat=c(6073, 244, 3752, 7547, 3779, 7426),
	K=c(474828, NA, 3.576e+5, 3.982e5, 3.5e5, 307830))
dimnames(prK)[[1]] <- c("IO","MED","NAT","NPA","SAT","SPA")

# lm K ~ 0 + habitat
mod <- lm(K ~ 0 + habitat, prK)

# 99.9% CI for t/km2 ratio * IO ALB area (Arrizabalaga et al, 2015, doi: 10.1016/j.dsr2.2014.07.001)
cis <- confint(mod, level = 0.999) * 6073

# idx
id2 <- which(res$TotBio_Unfished <= cis[2])

# DROP if VB_2014_1 < 2 * C_2014
vb2014 <- unitSums(quantSums(catch.sel(omf)[,'2014',,1] *
  stock.n(omf)[,'2014',,1] * stock.wt(omf)[,'2014',,1]))
id3 <- which(vb2014 > 80000)

# iters TO KEEP
idx <- Reduce(intersect, list(id1, id2, id3))

# res
res <- res[idx,]

# SUBSET by idx
om <- FLCore::iter(omf, idx)

# MERGE areas
om <- simplify(om, c("season", "unit"))

# DROP age 0
om <- om[-1,]

range(om, "plusgroup") <- 14

# rpts: MSY, SB_MSY, F_MSY, SB0
orpts <- with(res, FLPar(MSY=TotYield_MSY, SBMSY=2 * SSB_MSY, FMSY=Fstd_MSY,
  SB0=2 * SPB_Virgin, Ftarget=Fstd_MSY, SBlim=2 * 0.40 * SSB_MSY,
  units=c("t", "t", "f", "t", "f", "t")))

# sr
# BUG Only 353 being returned [21.03.2018]
resid <- loadquants(subdirs=file.path("grid", grid$id[idx]), object="resid")

osr <- predictModel(model="bevholtss3",
  params=with(res, FLPar(s=steepness, R0=exp(`SR_LN(R0)`), v=SPB_Virgin,
    units=c("", "1000", "t"))), FLQuants(residuals=resid[, ac(1975:2012)]))

save(om, orpts, osr, res, file='out/om.RData', compress='xz')

# --- SETUP omp

# omp
orp <- FLBRP(om)
omp <- fwdWindow(om, orp, end=2045)

# bridge gap years
library(tunadata)

resd <- c(sqrt(yearVars(osr$residuals)))
rerho <- c(apply(osr$residuals, c(1,3:6), function(x) acf(x, plot=FALSE)$acf[2]))

sres <- rlnoise(length(resd), FLQuant(0, dimnames=dimnames(window(stock(omp),
  start=2014))), sd=resd, b=rerho, seed=reseed)


omb <- fwd(omp, sr=osr,
  fwdControl(quant="catch", year=c(2015, 2016), value=c(34184, 35939)),
  residuals=)












# ---

# index: cpue, sd, b
cpue <- foreach(i=seq(length(grid$id))) %dopar% {
  print(i)
  readFLIBss3(dir=grid$id[i], fleets=c(LLCPUE3=3))
}

index <- cpue[[1]]@index
sel.pattern <- Reduce('combine', lapply(cpue, function(x) x@sel.pattern))
index.res <- Reduce('combine', lapply(cpue, function(x) x@index.var))
index.q <- Reduce('combine', lapply(cpue, function(x) x@index.q))

save(cpue, index, sel.pattern, index.res, index.q, file='cpue.RData', compress='xz')

# cpue list
# TODO REVIEW deSeason, deGender
ocpue <- list(index=seasonMeans(index),
  sel.pattern=seasonMeans(unitMeans(sel.pattern[-1,])),
  index.q=seasonMeans(index.q),
  index.res=seasonMeans(index.res),
  catch.n=seasonSums(catch.n[-1,])
) 

# --- OMF
# om orp rpts osr res ind
save(om, rpts, osr, res, ocpue, file='out/omf.RData', compress='xz')

# --- TRIM down om and res

# --- OM SUBSET, deGender and deSeason
# om orp rpts osr res ind

om <- FLCore::iter(om, idx)
om <- simplify(om, c("unit", "season"))

desc(om) <- paste0("IO ALB MSE SS3 grid of 20170306, ", length(idx), " selected runs")

orp <- FLBRP(om)
res <- res[idx, ]
orpts <- FLPar(MSY=res$TotYield_MSY, SBMSY=2 * res$SSB_MSY, FMSY=res$Fstd_MSY,
  SB0=2 * res$SPB_1950, Ftarget=res$Fstd_MSY, SBlim=2 * 0.40 * res$SSB_MSY)
# bevholtss3
osr <- predictModel(residuals=FLCore::iter(osr$residuals,idx)[, ac(1975:2012)],
  params=FLPar(s=res$steepness, R0=exp(res$`SR_LN(R0)`), v=res$SPB_1950,
  iter=dim(res)[1]),
  model=rec ~ (4 * s * R0 * ssb) / (v * (1 - s) + ssb * (5 * s - 1)))
ocpue <- list(index=propagate(ocpue$index, 1440)[,,,,,idx],
  sel.pattern=ocpue$sel.pattern[,,,,,idx],
  index.q=ocpue$index.q[,,,,,idx],
  index.res=ocpue$index.res[,,,,,idx],
  catch.n=ocpue$catch.n[,,,,,idx]
  ) 

omp <- fwdWindow(om, orp, end=2044)

save(om, omp, res, orpts, osr, ocpue, file='out/om.RData', compress='xz')

# --- OMS - For TESTING

set.seed(2018)
idx <- sample(dimnames(m(om))$iter, 200)

om <- FLCore::iter(om, idx)

desc(om) <- "IO ALB MSE SS3 grid of 20170117, 200 randomly selected runs"

orp <- FLBRP(om)
omp <- fwdWindow(om, orp, end=2044)
res <- res[idx, ]
orpts <- FLPar(MSY=res$TotYield_MSY, SBMSY=2 * res$SSB_MSY, FMSY=res$Fstd_MSY,
  SB0=2 * res$SPB_1950, Ftarget=res$Fstd_MSY, SBlim=2 * 0.40 * res$SSB_MSY)
osr <- predictModel(residuals=FLCore::iter(osr$residuals,idx)[, ac(1975:2012)],
  params=FLPar(s=res$steepness, R0=exp(res$`SR_LN(R0)`), v=res$SPB_1950, iter=dim(res)[1]),
  model=rec ~ (4 * s * R0 * ssb) / (v * (1 - s) + ssb * (5 * s - 1)))
ocpue <- list(index=ocpue$index[,,,,,idx],
  sel.pattern=ocpue$sel.pattern[,,,,,idx],
  index.q=ocpue$index.q[,,,,,idx],
  index.res=ocpue$index.res[,,,,,idx],
  catch.n=ocpue$catch.n[,,,,,idx]
) 

save(om, omp, res, orpts, osr, ocpue, file='out/oms.RData', compress='xz')

# --- OM - SUBSET based on analysis
