# mps.R - DESC
# /mps.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

birds.is <- function(stk, bcatch, ctrl, args, tracking, ...) {

  # ADD bird catch to target
	ctrl$value <- ctrl$value + bcatch

	# return
	list(ctrl = ctrl, tracking = tracking)
}

# catch4010.phcr

catch4010.phcr   <-function (stk, msy, args, tracking, ...) {

  if (is(msy, "FLPar")) 
    MSYpar <- msy
  else
    MSYpar <- FLPar(msy = msy)
 
  list(MSYpars = MSYpar, tracking = tracking)
}


# catch4010.hcr {{{

catch4010.hcr <- function(stk, dtarget=0.40, dlimit=0.10, MY, 
  dtaclow=0.80, dtacupp=1.20, args, tracking, ...) {

  # COMPUTE depletion
  dep <- tracking["SPR", ac(args$ay)]

  # RULE
  ca <- ifelse(is.na(dep), MY/2, ifelse(dep <= dlimit, 0,
    ifelse(dep < dtarget, MY / (dtarget - dlimit) * (dep - dlimit), MY)))

  # CONTROL
    ctrl <- getCtrl(c(ca), "catch", args$ay + 1, length(c(ca)))
    
	return(list(ctrl=ctrl, tracking=tracking))

} # }}}

indicator.hcr <- function (stk, itrg, args, tracking) {
    ay <- args$ay
    dy <- args$dy
    
    mlag <- args$management_lag
    
    if(!is(itrg, "FLQuant"))
      itrg <- FLQuant(itrg, dimnames=list(iter=dimnames(stk@catch)$iter))
    
    mult <- stk@indicator[,ac(dy)]/itrg
    
    ctrl <- getCtrl(mult, "catch", ay + mlag, dim(itrg)[6])

    list(ctrl = ctrl, tracking = tracking)
}

# psacatch4010.hcr {{{

psacatch4010.hcr <- function(stk, MY, dtarget=1, dlimit=0.20,
  dtaclow=0.80, dtacupp=1.20, args, tracking, ...) {

  # COMPUTE depletion
  dep <- ssb(stk)[, ac(args$ay)] / refpts(om)$SBMSY

  # RULE
  ca <- ifelse(is.na(dep), MY/2, ifelse(dep <= dlimit, 0,
    ifelse(dep < dtarget, MY / (dtarget - dlimit) * (dep - dlimit), MY)))

  # CONTROL
    ctrl <- getCtrl(c(ca), "catch", args$ay + 1, length(c(ca)))
    
	return(list(ctrl=ctrl, tracking=tracking))

} # }}}
