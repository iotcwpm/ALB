# boot.R - DESC
# /boot.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(remotes)

# INSTALL latest versions from github

install_github(c("flr/FLCore", "flr/ggplotFL", "flr/FLFishery",
  "flr/FLasher", "flr/ss3om", "flr/mse", "r4ss/r4ss"))

# forks of ss3diags & jabba

install_github(c("iagomosqueira/ss3diags"))
install_github(c("iagomosqueira/jabba"))

# CRAN

install.packages(c("forecast", "doParallel"))


