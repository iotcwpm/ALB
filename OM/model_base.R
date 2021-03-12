# model_base.R - Runs and diagnostics for the base case SS3 model
# ALBMSE/OM/model_base.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ss3om)
library(ss3diags)

dir.create("model/base/run")


# --- RUN models

# SET base case = PSLFwt/CPUE_SouthWest

# M=0.30, sigmaR=0.6, steepness=0.8, cpues=14, lfreq=1, llq=1

cp("alb/PSLFwt/CPUE_SouthWest/*", "base/run/")

# RETRO base case

dir.create("base/run/retro")

# CREATE retro folders

prepareRetro("base/run", rpath="base/retro", start=1, end=10)

# RUN retro

system("cd base/retro; ls | parallel -j5 --progress '(cd {}; ss_3.30.15 -nox)'")


# --- LOAD output

base <- readOMSss3("model/base", range=c(minfbar=1, maxfbar=12))

# LOAD retro

dirs <- setnames(c("model/base", list.dirs("model/base/retro", recursive=false)),
  nm=seq(0, 10))

retro <- lapply(dirs, readoutputss3)

# summarize 5 + base peels only
retrosumm <- sssummarize(retro[1:6])

# SAVE
save(base, retrosumm, file="model/base.RData", compress="xz")

# --- CHECK SRR

out <- base$out
stk <- simplify(base$stock)
srr <- base$sr
rps <- base$refpts


# Does SSB match? YES

extractSSB(out)
ssb(stk) / 2

# Compare predictions and observations

ggplot(FLQuants(pred=predict(srr, ssb=ssb(fut0)), obs=rec(fut0)),
  aes(x=year, y=data, group=qname)) + geom_point(aes(colour=qname)) +
  ylim(c(0,NA))

fut <- stf(stk, end=2040)

fut0 <- fwd(fut, sr=srr,
  control=fwdControl(year=2018:2040, quant="fbar", value=rps$FMSY))

rec(fut0[-1,])[, ac(2016:2018)]

plot(fut0) + geom_vline(xintercept=2017)


# ---- NoLF

# retro

prepareRetro("model/noLF", years=5)

dirs <- setNames(c("model/noLF", list.dirs("model/noLF/retro", recursive=FALSE)),
  nm=seq(0, 5))

retro <- lapply(dirs, readOutputss3)

# summarize 5 + base peels only
nolfret <- SSsummarize(retro)





# LOAD

nolf <- readFLSss3('model/noLF')
molfout <- readOutputss3('model/noLF')
nolfres <- readRESss3('model/noLF')
nolfrps <- readFLRPss3('model/noLF')

# COMPARE to base

plot(FLStocks(SA=simplify(base$stock), NoLF=simplify(nolf)))

# TODO metrics(FLStocks)
metrics(FLStocks(SA=simplify(base$stock), NoLF=simplify(nolf)))


plot(simplify(base$stock), metrics=list(SSB=ssb, F=fbar)) 
+
  geom_flpar(data=FLPars(SSB=base$refpts$SBMSY, F=base$refpts$FMSY),
    x=ISOdate(1955,1,1))





