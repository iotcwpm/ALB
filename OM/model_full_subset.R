# model_full_subset.R - DESC
# ALB/OM/model_full_subset.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


load("model/full/load.Rdata")
load("model/full/rate.Rdata")

# --- SUBSET

sel <- results$sel

results <- results[(sel),]

sr <- iter(full$sr, sel)

refpts <- full$refpts[, sel]

# 2 SEX, no seasons

stock <- stf(noseason(iter(full$stock, sel)), end=2040)

sr <- FLPar(c(params(sr)[,1,]), dimnames=dimnames(params(sr(om)))[-2])

# indices: index. index.q (index.var), unit=1, seasons 1-4
indices <- FLIndices(full$indices[c("LLCPUE1", "LLCPUE3")])

indices <- lapply(indices, function(x) {
  FLIndexBiomass(index=index(x)[,,,1,,sel], index.var=index.var(x)[,,,1,,sel],
    sel.pattern=unitMeans(sel.pattern(x)[,,,1,,sel]), index.q=index.q(x)[,,,1,,sel],
    range=c(startf=0, endf=0.25, min=1, max=12))
  })

# FIX dimanmes$unit
dimnames(indices[[1]]) <- list(season='all')
dimnames(indices[[2]]) <- list(season='all')

# -- tests.R: TEST 2-sex model

save(stock, sr, refpts, results, indices, file="model/full.Rdata",
  compress="xz")
