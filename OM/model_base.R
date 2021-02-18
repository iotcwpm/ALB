# model_base.R - Runs and diagnostics for the base case SS3 model
# ALBMSE/OM/model_base.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
library(ss3om)
library(ss3diags)

mkdir("model/base")

# --- RUN models

# SET base case = PSLFwt/CPUE_SouthWest

# M=0.30, sigmaR=0.6, steepness=0.8, cpues=14, lfreq=1, llq=1

cp("data/PSLFwt/CPUE_SouthWest/*", "model/base/")

# RETRO base case

mkdir("model/base/retro")

# CREATE retro folders

prepareRetro("model/base", rpath="model/base/retro", start=1, end=10)

# RUN retro

system("cd model/retro; ls | parallel -j5 --progress '(cd {}; ss_3.30.15 -nox)'")


# --- LOAD output

base <- readOMSss3("model/base", range=c(minfbar=1, maxfbar=12))

# LOAD retro

dirs <- setNames(c("model/base", list.dirs("model/base/retro",
  recursive=FALSE)), nm=seq(0, 10))

retro <- lapply(dirs, readOutputss3)

# SUMMARIZE 5 peels only
retrosumm <- SSsummarize(retro[1:6])

# FLStocks retro
retrostocks <- FLStocks(lapply(retro, buildFLSss330, range=c(minfbar=1, maxfbar=12)))

# SAVE
save(base, retrosumm, retrostocks, file="model/base.RData", compress="xz")


# --- DIAGNOSTICS

load("model/base.RData")

results <- base$results

# 2. Convergence level

results$Convergence_Level > 1e-4

# - catch likelihood > 1e-5

results$Catch > 1e-5

# - Mohn's rho: output.R

# - runs test: output.R

# - MASE HCXVAL

ssmase <- rbindlist(lapply(1:4, function(y) SSmase(retrosumm, Season=y, verbose=FALSE)))
mase <- ssmase[!is.na(MASE), .(mase=mean((MAE.PR * n.eval) / (MAE.base * n.eval),
  na.rm=TRUE)), by=.(Index, Season)]

hcbias <- SShcbias(retrosumm)

SSplotHCxval(retrosumm, Season=1)
SSplotHCxval(retrosumm, Season=2)
SSplotHCxval(retrosumm, Season=3)
SSplotHCxval(retrosumm, Season=4)


# --- STATUS

out <- SS_output("model/base", repfile = "Report.sso.gz",
  compfile = "CompReport.sso.gz", covarfile = "covar.sso.gz")

# PLOT MVLN uncertainty in Kobe
status <- SSdeltaMVLN(out, run="MVLN")

# TODO ADD corners

sspar(mfrow = c(3, 2), plot.cex = 0.8)
SSplotEnsemble(status$kb, add = TRUE, legend = FALSE)

# - ASPM (?)

# McMC

# ss_3.30.15 -mcmc 5000500 -mcsave 500 -mcseed 91438
# ss_3.30.15 -mceval

# ADnuts

mcbase <- sample_nuts(model="~/home/mosqu003/Bin/jjms", path="model/base_mcmc",
  iter=2000, warmup=iter / 4, chains=3, cores=chains, admb_args="",
  control=list(metric='mle', max_treedepth=12, refresh=1))
