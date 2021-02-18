# grid.R - DESC
# ioalbmse/R/grid.R

# Copyright European Union, 2015-2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# setioalbgrid {{{

#' Sets a grid of SS3 model runs for Indian Ocean albacore
#'
#' @param sce [TODO:description]
#' @param dir=paste0 [TODO:description]
#' @param grid_ [TODO:description]
#' @param format(Sys.time [TODO:description]
#' @param Y%m%d [TODO:description]
#' @param base=system.file [TODO:description]
#' @param ext-data/sa/2016 [TODO:description]
#' @param package [TODO:description]
#' @param name [TODO:description]
#' @param from=1 [TODO:description]
#' @param write=TRUE [TODO:description]
#' @param delete=TRUE [TODO:description]
#'
#' @return [TODO:description]
#' @export
#'
#' @examples

setioalbgrid <- function(sce, dir=paste0('grid_', format(Sys.time(), "%Y%m%d")),
  base=system.file("ext-data/sa/2016", package="ioalbmse"), name='abt', from=1,
  write=TRUE, delete=TRUE) {

  # EXPAND grid from sce if list
  if(is(sce, "data.frame"))
    grid <- sce
  else
  	grid <- nameGrid(expand.grid(sce, stringsAsFactors=FALSE), from=from)

  if(!write)
    return(grid)

  # SET ctl, dat full paths
  ctlf <- paste0(base, "/", name, ".ctl")
  datf <- paste0(base, "/", name, ".dat")
 	
  # READ source files
  dats <- SS_readdat_3.30(datf, verbose=FALSE)
  ctls <- SS_readctl_3.30(file=ctlf, use_datlist=T, datlist=dats,
    verbose=FALSE)

  # NAMES in grid
  pars <- names(grid)[!names(grid) %in% c("iter", "id")]

  # CREATE dir
  if(dir.exists(dir))
    if(delete) {
      unlink(dir, recursive = TRUE, force = TRUE)
      dir.create(dir)
    } else  
      stop(paste("folder", dir, "already exists. Delete first."))
	else
    dir.create(dir)

	# SETUP grid
  foreach (i=grid$iter, .errorhandling = "remove") %dopar% {

    dat <- dats
    ctl <- ctls

    row <- which(grid$iter == i)
 
    # M, ctl$MG_parms[c("NatM_p_1_*_GP_1",]
    if("M" %in% pars) {
      ctl$MG_parms[c("NatM_p_1_Fem_GP_1", "NatM_p_1_Mal_GP_1"), "INIT"] <-
        grid[row, "M"]
    }

    # sigmaR, ctl$SR_parms["SR_sigmaR",]
    if("sigmaR" %in% pars) {
      ctl$SR_parms["SR_sigmaR", "INIT"] <- grid[row, "sigmaR"]
    }

		# steepness, ctl$SR_parms["SR_BH_steep",]
    if("steepness" %in% pars) {
      ctl$SR_parms["SR_BH_steep", "INIT"] <- grid[row, "steepness"]
    }

    # LF ~ CPUE lkhdf lambdas
    if("lfreq" %in% pars) {

      extla <- data.frame(
        like_comp = 4,
        fleet = c(1,2,3,4,7),
        phase = 1,
        value = grid[row,]$lfreq,
        sizefreq_method =  1)
      rownames(extla) <- c(
        "length_F1_LL1_sizefreq_method_1_Phz1",
        "length_F2_LL2_sizefreq_method_1_Phz1",
        "length_F3_LL3_sizefreq_method_1_Phz1",
        "length_F4_LL4_sizefreq_method_1_Phz1",
        "length_F7_PS1_sizefreq_method_1_Phz1")
      ctl$lambdas <- rbind(ctl$lambdas, extla)

      ctl$N_lambdas  <-  dim(ctl$lambdas)[1]
    }

    # cpues
    if("cpues" %in% pars) {
      ctl$lambdas[ctl$lambdas$like_comp == 1, "value"] <- 0
      ctl$lambdas[ctl$lambdas$like_comp == 1 &
        ctl$lambdas$fleet == grid[row, ]$cpues, "value"] <- 1
    }

    # llsel TODO
    if("llsel" %in% pars) {
      if(grid[row, "llsel"] == "DoNorm") {
        # CHANGE fleet 3 (S) 
        ctl$size_selex_parms[7:12, "INIT"] <- c(80, -2.2, 3, 4.3, -999, -5)
      }
      if(grid[row, "llsel"] == "Log") {
        # CHANGE fleet 3 (S) 
        ctl$size_selex_parms[7:12, "INIT"] <- c(90, -0.50, 5.00, 6.00, -999, 5)
        ctl$size_selex_parms[12, "PHASE"] <- c(-5)
      }
    }

    # CPUE Q increases
    if("llq" %in% pars) {
      dat$CPUE$obs <- data.table(dat$CPUE)[, obs:=obs / as.numeric(grid[row, ]$llq) ^
        seq(0, length(obs) - 1), by=index]$obs
    }

    # TODO RELAX bounds

		# CREATE dir
    dirname <- file.path(dir, grid[row, "id"])
		dir.create(dirname)

		# COPY unaltered files
		# starter.ss
		file.copy(file.path(base, "starter.ss"),
			file.path(dirname, "starter.ss"))
		
		# forecast.ss
		file.copy(file.path(base, "forecast.ss"),
			file.path(dirname, "forecast.ss"))

		# WRITE modified files
		# ctl
    SS_writectl_3.30(ctl, file.path(dirname, paste0(name, ".ctl")), verbose=FALSE)
		
    # dat
    SS_writedat_3.30(dat, outfile=file.path(dirname, paste0(name, ".dat")))
		}

	invisible(data.table(grid, key="iter"))
} # }}}
