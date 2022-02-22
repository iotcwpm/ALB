# test_output.R - DESC
# /test_output.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# XX {{{
# }}}



plot(observations(oem)$idx)

survey(stock(om), observations(oem)$idx[[1]])

index(observations(oem)$idx[[1]])
