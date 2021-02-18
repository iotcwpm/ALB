---
title:
author: 
tags:
---

OM
├── bootstrap
├── data
│   ├── data.R: LOAD base case SS3 runs
│   └── data.RData: runs(`FLSs`), indices(`FLIs`), srr(`FLSR`), rps(`FLPar`),
│         out(`SS_output`)
├── model
│   ├── *model_base.R*:
│   │   └── base.RData:
│   ├── *model_corners.R*: full (df)
│   │   └── model/corners/corners.RData: res, stk, stks, srr
│   ├── *model_interact.R*:
│   ├── *model_maineffects.R*:
│   └── *model_om.R*:
├── output
│   └── *output.R*:
└── report
    └── *report.R*:

# data.R

# model.R

## model_base.R

- SETUP model/base (PSLFwt/CPUE_SouthWest)
- RUN & LOAD model/base
- SETUP retro
- RUN & LOAD retro
- SUMMARIZE 5-peels retro
- COMPUTE diagnostics on base case run
  - Residuals
  - Retrospective
  - Prediction skill MASE
  - Runs test
- OUTPUT: base, retro, retrosumm (model/base.RData)

## model_maineffects.R

## model_corners.R

## model_interact.R

## model_om.R




- RUN base(s) + diagnostics (retro, runstest, hcxval)
- RUN corners + diagnostics
- RUN main effects + diagnostics
- RUN interactions
- COMPARE base MVLN ~ corners + main effects
