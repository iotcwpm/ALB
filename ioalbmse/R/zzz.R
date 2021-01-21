# zzz.R - DESC
# ioalbmse/R/zzz.R

# Copyright European Union, 2015-2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

.onLoad <- function(libname, pkgname) {
	if (.Platform$OS.type == "unix") {
    Sys.chmod(system.file('bin/linux/ss3', package=pkgname),
      mode = "0755", use_umask = TRUE)
  }
}
