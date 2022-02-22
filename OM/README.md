---
title:
author: 
tags:
---


# WORKFLOW`


OM
├── data
│   └── *data.R* - GET base case SS3 runs.
│       └── **data/data.Rdata**: runs, indices, srr, rps, out
├── model
│   ├── *model_assessment.R* - RUN all WPTmT model runs.
│   ├── *model_base.R* - RUN base case SS3 run.
│   │   └── **model/base.Rdata**: base (stock, sr, indices, refpts, results, out), retrosumm
│   ├── *model_corners.R* - SETUP & RUN grid corners.
│   │   └── **model/corners/corners.Rdata**: res, stk, stks, srr
│   ├── *model_maineffects.R* -
│   ├── *model_full_grid.R* -
│   ├── *model_full_load.R* -
│   ├── *model_full_rate.R* -
│   ├── *model_full_subset.R* -
│   ├── *model_partial.R* -
│   │   ├── *model_partial_load.R* -
│   │   └── *model_partial_results.R* -
│   └── *model_om.R* -
├── output
│   └── *output.R* -
├── report
│   └── *report.R* -
└── test
    └── test.R* -

