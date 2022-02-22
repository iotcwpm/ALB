# model_full_load.R - DESC
# /model_full_load.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ss3om)

library(doParallel)
registerDoParallel(48)

load("model/full/grid.Rdata")

dirs <- list.dirs("model/full", recursive=FALSE)


# --- CHECKS

# MODELS have run: Report.sso.gz exists

all(file.exists(file.path(dirs, "Report.sso.gz")))

# RETROS have run

retfs <- unlist(lapply(file.path(dirs, "retro"), file.path,
  paste0("retro_0", 1:5), "Report.sso.gz"))

all(file.exists(retfs))

# LOAD OMS: stock, indices, results, output

full <- loadOMS(subdirs=dirs, grid=grid, range=c(minfbar=1, maxfbar=12),
  combine=TRUE)

save(full, file="model/full/load.Rdata", compress="xz")

# LOAD retros

retros <- foreach(x=setNames(dirs, nm=seq(length(dirs))),
  .errorhandling = "pass") %dopar% {
  
  rdirs <- setNames(c(x, as.list(list.dirs(file.path(x, "retro"),
    recursive=FALSE))), nm=seq(0, 5))

  cat("[", x, "]\n")

  rretro <- lapply(rdirs, readOutputss3)

  return(SSsummarize(rretro, verbose=FALSE))
}

# SAVE

save(full, retros, file="model/full/load.Rdata", compress="xz")
