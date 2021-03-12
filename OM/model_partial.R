# model_partial.R - DESC
# /model_partial.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(ioalbmse)
load_all('../ioalbmse')

library(r4ss)
library(ss3diags)
library(AlgDesign)

source("utilities.R")

library(doParallel)
registerDoParallel(3)


# SET full grid

full <- list(
  M = seq(0.20, 0.35, length = 4),
  sigmaR = seq(0.4, 0.8, length = 3),
  steepness = seq(0.7, 0.9, length = 3),
  cpues = c(14, 12),
  lfreq = c(1e-2, 0.1, 1),
  llq = c(1, 1.01)
)

fullgrid <- expand.grid(lapply(full, factor))

# --- EVALUATE nTrials

res <- lapply(setNames(nm=seq(50, 250)), optFederov, frml= ~ ., data=fullgrid)

des <- lapply(res, "[[", "design")
eva <- lapply(des, function(x)
  eval.design(frml= ~ ., x, confounding=TRUE, X=fullgrid))

evd <- rbindlist(lapply(lapply(eva, "[", 2:8), as.data.frame), idcol="nTrials")

ggplot(evd, aes(x=as.numeric(nTrials), y=Geff)) + geom_point() +
  ylim(c(0.80, NA))

# GET design for 84 nTrials

des <- optFederov( ~ ., data=fullgrid, nTrials = 84)

eva <- eval.design( ~ ., des$design, confounding=TRUE, X=fullgrid)

lapply(des$design, table)

# 

design <- cbind(as.data.frame(lapply(des$design, as.numeric)), iter=seq(1, 84),
  row=as.numeric(rownames(des$design)))

grid <- setioalbgrid(design, dir = "model/partial",
  base = "data/PSLFwt/CPUE_SouthWest", name = "abt", write=TRUE)

lapply(file.path("model/partial", grid$id), prepareRetro)

# ls | parallel -j21 --bar --progress '(cd {}; ss_3.30.15 && packss3run; cd retro; for d in ./*/ ; do (cd "$d" && ss_3.30.15 && packss3run); done)'

dirs <- list.dirs("model/partial", recursive=FALSE)


partial<- loadOMS(subdirs=dirs, grid=grid)

save(partial, file="model/partial/partial.RData", compress="xz")




stock <- simplify(partial$stock)[-1,]

results <- partial$results


