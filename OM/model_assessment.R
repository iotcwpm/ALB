# model_assessment.R - DESC
# /model_assessment.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ss3om)
library(ss3diags)

mkdir("model/assessment")

# COPY boot/data/

# RUN PSLFwt models (1, 2)

# LOAD

paths <- file.path("model/assessment", "PSLFwt",
  c("CPUE_NorthWest", "CPUE_SouthWest"))

models <- lapply(setNames(paths, nm=c("NW", "SW")),
  readOMSss3, range=c(minfbar=1, maxfbar=12))

assessment <- list(
  # stock
  stock=FLStocks(lapply(models, function(x) simplify(x$stock))),
  # results
  results=rbindlist(lapply(models, "[[", "results"), idcol="model"),
  # indices
  indices=list(NW=models[["NW"]]$indices, SW=models[["SW"]]$indices),
  out=list(NW=models[["NW"]]$out, SW=models[["SW"]]$out))

# SAVE

save(assessment, file="model/assessment/load.RData", compress="xz")
