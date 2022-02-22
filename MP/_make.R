# make.R - DESC
# /make.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)

# test_spict

make('test_spict.R', 'data/om.RData', "test/test_spict.RData")

# report

make('report.R', "test/test_spict.RData", "report/test_spict/*.png")

