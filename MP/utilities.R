# utilities.R - DESC
# /utilities.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

catchHockey.hcr <- function(stk, args, tracking, Btrigger, Ctarget,
  Cmin=1, Blim=Btrigger * 0.20) {
  
  # GET SSB
  sb <- lapply(FLStocks(window(stk, start=args$dy, end=args$dy)), ssb)

  # RULE: F1, 2, 4 - B1; F3 - B2
	
  gradient <- (Ctarget - Cmin) / (Btrigger - Blim)

  cout <- mapply(function(x, blim, btrigger, ctarget, cmin, grad) {

    # SB <= Blim, C = Cmin
    x[x <= blim] <- cmin
    
    # SB > Btrigger, C = Ctarget
    x[x > btrigger] <- ctarget

    # Blim < SB <= Btrigger, C = (SB - Blim) * gradient + Cmin
    x[(x <= btrigger) & (x > blim)] <- (x[(x <= btrigger) & (x > blim)] - blim) *
      grad + cmin

    return(x)

    }, x=sb, blim=Blim, btrigger=Btrigger, ctarget=Ctarget, cmin=Cmin,
      grad=gradient, SIMPLIFY=FALSE)

  # ALLOCATION
  aloc <- lapply(FLStocks(stk), function(x) {
    catch(x)[, ac(args$dy)] %/% areaSums(catch(x)[, ac(args$dy)])
      })

  cout <- mapply("%*%", cout, aloc, SIMPLIFY=FALSE)

  # FLQuants to fwdControl
  ctrl <- cjmfwc(cout, quant="catch", nstocks=length(stk))

  return(list(ctrl=ctrl, tracking=tracking))
} # }}}



