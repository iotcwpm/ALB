# model_base.R - Runs and diagnostics for the base case SS3 model
# ALBMSE/OM/model_base.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ss3om)
library(ss3diags)

dir.create("model/base")


# --- RUN models

# SET base case = PSLFwt/CPUE_SouthWest

# M=0.30, sigmaR=0.6, steepness=0.8, cpues=14, lfreq=1, llq=1

cp("data/PSLFwt/CPUE_SouthWest/*", "model/base")

# ss_3.30.16

# RETRO base case

dir.create("model/base/retro")

# CREATE retro folders

prepareRetro("model/base")

# RUN retro

# ls | parallel -j21 --bar --progress '(cd {}; ss_3.30.16 && packss3run; cd retro; for d in ./*/ ; do (cd "$d" && ss_3.30.16 && packss3run); done)'


# --- LOAD output

base <- readOMSss3("model/base", range=c(minfbar=1, maxfbar=12))

# LOAD retro

dirs <- setNames(c("model/base", list.dirs("model/base/retro", recursive=FALSE)),
  nm=seq(0, 5))

retro <- lapply(dirs, readOutputss3)

# summarize 5 + base peels only

retrosumm <- SSsummarize(retro[1:6])

# SAVE

save(base, retrosumm, file="model/base.RData", compress="xz")

