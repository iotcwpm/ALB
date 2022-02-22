# model_jabba.R - DESC
# /model_jabba.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


source("model.R")

library(doParallel)
registerDoParallel(25)

# --- TEST HCRs

# perfect.sa + fixedF.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr,
    args=list(ftrg=mean(refpts(om)$FMSY)))))

perSA_fixHCR <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, perSA_fixHCR)

# perfect.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.85, nyears=5, metric=stock))))

perSA_treHCR <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, perSA_treHCR)


# perfect.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=11000, trigger=55000, target=30000, min=500,
    metric='ssb', output='catch'))))

perSA_hstHCR <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, perSA_hstHCR)
plot_hockeystick.hcr(control$hcr) + xlab("SSB(t)")+ ylab("catch (t)")

# runs

hcrs <- list(fixf=perSA_fixHCR, trend=perSA_treHCR, hockey=perSA_hstHCR)


# --- TEST jabba.sa

# jabba.sa + fixedF.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=fixedF.hcr,
    args=list(ftrg=mean(refpts(om)$FMSY)))))

jabSA_fixHCR <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, jabSA_fixHCR)


# jabba.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.85, nyears=5, metric=stock))))

jabSA_treHCR <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, jabSA_treHCR)


# jabba.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=11000, trigger=55000, target=30000, min=500,
    metric='ssb', output='catch'))))

jabSA_hstHCR <- mp(om, oem, ctrl=control, args=mseargs)

# runs

jabs <- list(fixf=jabSA_fixHCR, trend=jabSA_treHCR, hockey=jabSA_hstHCR)

# SAVE

save(hcrs, jabs, file="model/tests.Rdata", compress="xz")
