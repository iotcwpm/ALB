# sa.R - DESC
# /sa.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ss3om)

# sa

sa <- readFLSss3('run', birthseas=4, repfile="Report.sso.gz",
  compfile="CompReport.sso.gz", covarfile="covar.sso.gz")

srsa <- readFLSRss3('run', repfile="Report.sso.gz",
  compfile="CompReport.sso.gz", covarfile="covar.sso.gz")

sa <- simplify(sa, c("unit", "season"), spwn.season=4)[-1,]

range(sa, c("minfbar", "maxfbar", "plusgroup")) <- c(5, 12, 14)

# res

resa <- readRESss3("run", repfile="Report.sso.gz", compfile="CompReport.sso.gz")

# CORRECT SSB_MSY
resa$SSB_endyr <- resa$SSB_endyr * 2 
resa$SSB_Virgin <- resa$SSB_Virgin * 2 
resa$SSB_MSY <- resa$SSB_MSY * 2 

sra <- predictModel(model="bevholtss3",
  params=FLPar(s=0.8, R0=exp(resa[,'SR_LN.R0.']), v=resa[,'SSB_Virgin']/2,
    sratio=0.47, units=c("", "1000", "t")))

save(sa, resa, sra, file="../data/sa.RData", compress="xz")
