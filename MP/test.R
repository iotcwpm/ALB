# test.R - DESC
# /test.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# XX {{{
# }}}

# ---

plot(om, tune_jabSA_stHCR)

tracking(tune_jabSA_stHCR)[c('hcr', 'C.om', 'SB.om'),]

plot(tracking(tune_jabSA_stHCR)[c('hcr', 'C.om', 'SB.om'),]) +
  facet_grid(metric~iter)

plot(unitSums(catch(tune_jabSA_stHCR))) + facet_wrap(~iter) + ylim(c(0,NA))
plot(unitMeans(fbar(tune_jabSA_stHCR))) + facet_wrap(~iter) + ylim(c(0,NA))
plot(unitSums(vb(stock(tune_jabSA_stHCR)))) + facet_wrap(~iter) + ylim(c(0,NA))
plot(unitSums(catch(tune_jabSA_stHCR)) / unitSums(vb(stock(tune_jabSA_stHCR)))) + facet_wrap(~iter) + ylim(c(0,NA))


# LOOK into iters where fwd returns NA
# - 
