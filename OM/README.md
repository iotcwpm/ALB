---
title:
author: 
tags:
---


# File STRUCTURE


OM
├── bootstrap
├── data
│   ├── **data.R**: LOAD base case SS3 runs
│   └── **data.RData**: runs(FLSs), indices(FLIs), srr(FLSR), rps(FLPar),
│         out(SS_output)
├── model
│   ├── **model_base.R**:
│   │   └── **base.RData**:
│   ├── **model_corners.R**:
│   ├── **model_maineffects.R**:
│   └── **model_om.R**:
├── output
└── report


- RUN base case SA models
- COMPUTE diagnostics on base case runs
  - Residuals
  - Retrospective
  - Prediction skill MASE
  - Runs test
- ASSEMBLE initial grid
- RUN main effects
- RUN corners

