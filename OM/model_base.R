# model_sa.R - DESC
# ALBMSE/OM/model_sa.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ss3om)
library(ss3diags)

# SET base case = PSLFwt/CPUE_SouthWest

mkdir("model/base")

cp("data/PSLFwt/CPUE_SouthWest/*", "model/base/")

# LOAD output

out <- SS_output("model/base", verbose=FALSE, hidewarn=FALSE, warn=TRUE,
  printstats=FALSE, forecast=FALSE, covar=TRUE,
  repfile="Report.sso.gz", compfile="CompReport.sso.gz",
  covarfile="covar.sso.gz")

outsum <- SSsummarize(list(base=out))


# --- RETRO

start <- 1
end <- 10

# SET base dir, retro_0

bdir <- "model/base"

# CREATE dirs

mkdir("model/retro")
mkdir("output/retro")

# READ starter

sta <- SS_readstarter(file.path("model", "base", "starter.ss_new"),
  verbose=FALSE)

# CREATE retro_ folders with updated starter.ss

for(i in seq(start, end)) {
  rpath <- paste0("model/retro/retro_", sprintf("%02d", i))
  mkdir(rpath)
  sta$retro_yr <- -1 * i
  SS_writestarter(sta, dir=rpath, verbose=FALSE)
  cp(file.path(bdir, "control.ss_new"), file.path(rpath, "abt.ctl"))
  cp(file.path(bdir, "data.ss_new"), file.path(rpath, "abt.dat"))
  cp(file.path(bdir, "forecast.ss"), file.path(rpath, "forecast.ss"))
}

system("cp -rf model/base/ model/retro/retro_00")

# RUN retro

system("cd model/retro; ls | parallel -j5 --progress '(cd {}; ss_3.30.15 -nox)'")

# LOAD results

retro <- lapply(setNames(nm = list.dirs("model/retro")[-1]),
  readOutputss3)

# SUMMARIZE 5 peels only

retrosumm <- SSsummarize(retro[1:6])

# FLStocks retro

retrofls <- FLStocks(lapply(retro, buildFLSss330, range=c(minfbar=1, maxfbar=12)))

# DEBUG ---
sretfls <- lapply(retrofls, simplify)
plot(sretfls)

save(out, retro, retrosumm, file="model/base.RData", compress="xz")


# --- DIAGNOSTICS

load("model/base.RData")

# - fit & convergence

convergencelevel("model/base")

# - catch likelihood > 1e-5

retro[[1]]$likelihoods_used["Catch", "values"] > 1e-5

# - Mohn's rho

SSmohnsrho(retrosumm)

SSplotRetro(retrosumm, xmin=2005)

# - runs test

# CPUEs
sspar(mfrow=c(2, 2), plot.cex = 0.7)
cpue_rtes <- SSplotRunstest(out, add=T, subplots="cpue", indexselect=1:4)

# LN
sspar(mfrow=c(2, 3), plot.cex = 0.7)
len_rtes <- SSplotRunstest(out, add=T, subplots="len")

# - MASE XVAL

# --- STATUS

sspar(labs = T)

# PLOT MVLN uncertainty in Kobe
status <- SSdeltaMVLN(out, run="MVLN")

# ADD corners
# DEBUG Fstd_MSY
points(res$SSB_endyr / res$SSB_MSY, res$F_endyr / res$F)

sspar(mfrow = c(2, 2), plot.cex = 0.8)
SSplotEnsemble(status$kb, add = TRUE, legend = FALSE)

# - ASPM (?)

# McMC

# ss_3.30.15 -mcmc 5000500 -mcsave 500 -mcseed 91438
# ss_3.30.15 -mceval

# --- ss3om TESTS

out <- readOutputss3("model/base")
stk <- buildFLSss330(out, range = c(minfbar = 1, maxfbar = 12))
idx <- readFLIBss3("model/base")

extractFbar(out)
unitSums(seasonSums(fbar(stk)))

extractRec(out)
rec(stk)

extractSSB(out)
ssb(stk)


# --- CHOICE of F ranges

# TODO COPY base to frange
# TODO CHANGE starter.ss, 1 12 #_min and max age over which average F will be calculated
# TODO RUN frange

ouf <- readOutputss3("model/frange")
