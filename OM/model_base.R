# model_base.R - Runs and diagnostics for the base case SS3 model
# ALBMSE/OM/model_base.R

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

base <- readOutputss3("model/base")

res_base <- readRESss3("model/base")

stk_base <- readFLSss3("model/base")

# --- DIAGNOSTICS

# 2. Convergence level

res_base$Convergence_Level > 1e-4


# --- RETRO

# CREATE dir
mkdir("model/retro")

# CREATE retros
prepareRetro("model/base", rpath="model/base/retro", start=1, end=10)

system("cp -rf model/base/ model/retro/retro_00")

# RUN retro

system("cd model/retro; ls | parallel -j5 --progress '(cd {}; ss_3.30.15 -nox)'")

# LOAD results

dirs <- setNames(c("model/base", list.dirs("model/base/retro",
  recursive=FALSE)), nm=seq(0, 10))

retro <- lapply(dirs, readOutputss3)

retrostk <- loadFLS("model/retro")

# SUMMARIZE 5 peels only
retrosumm <- SSsummarize(retro[1:6])

# FLStocks retro
retrofls <- FLStocks(lapply(retro, buildFLSss330, range=c(minfbar=1, maxfbar=12)))

# SAVE
save(base, retro, retrosumm, file="model/base.RData", compress="xz")


# --- DIAGNOSTICS

load("model/base.RData")

# - fit & convergence

convergencelevel("model/base")

# - catch likelihood > 1e-5

base$likelihoods_used["Catch", "values"] > 1e-5

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

# - MASE HCXVAL

SShcbias(retrosumm)

sspar(mfrow=c(2, 2))
SSplotHCxval(retrosumm, Season=1)
SSplotHCxval(retrosumm, Season=3, plot=FALSE)
SSplotHCxval(retrosumm, Season=4)


SSplotHCxval(retrosumm, Season=2)


# Yearly MASE per CPUE

mases <- lapply(1:4, function(x) SSmase(retrosumm, Season=x, verbose=FALSE))
mase <- rbindlist(mases)
mase[!is.na(MASE), .(mase=mean((MAE.PR * n.eval) / (MAE.base * n.eval), na.rm=TRUE)),
  by=Index]


# MASE for season 3

SSmase(retrosumm, Season=3, quants="cpue", verbose=FALSE)

#




# --- STATUS

sspar(labs = T)

# PLOT MVLN uncertainty in Kobe
status <- SSdeltaMVLN(base, run="MVLN")

# TODO ADD corners

sspar(mfrow = c(2, 2), plot.cex = 0.8)
SSplotEnsemble(status$kb, add = TRUE, legend = FALSE)

# - ASPM (?)

# McMC

# ss_3.30.15 -mcmc 5000500 -mcsave 500 -mcseed 91438
# ss_3.30.15 -mceval

# ADnuts

mcbase <- sample_nuts(model="~/home/mosqu003/Bin/jjms", path="model/base_mcmc",
  iter=2000, warmup=iter / 4, chains=3, cores=chains, admb_args="",
  control=list(metric='mle', max_treedepth=12, refresh=1))

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
