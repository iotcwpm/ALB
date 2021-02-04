# model.R - DESC
# ALBMSE/OM/model.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
library(ss3om)

library(doParallel)
registerDoParallel(3)

# DESIGN base grid

full <- list(
  M = seq(0.20, 0.35, length = 4),
  sigmaR = seq(0.4, 0.8, length = 3),
  steepness = seq(0.7, 0.9, length = 3),
  cpues = c(14, 12),
  lfreq = c(1e-2, 0.1, 1),
  llq = c(1, 1.01)
)

nsam <- prod(unlist(lapply(full, length)))

# model_base


# model_corners

sourceTAF("model_corners.R")


# ALTERNATIVE OMs

# cpues = 1
# llsel



# TEST partial factorial design

library(AlgDesign)

levels.design = unlist(lapply(lapply(full, seq), length))

f.design <- gen.factorial(levels.design)

fract.design <- optFederov(data=f.design, nTrials=50,
  approximate=FALSE)


# EXPAND parameters bounds
