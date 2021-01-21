
# ioalbmse

Management Strategy Evaluation for the Indian Ocean Tuna Commission Albacore Tuna Stock

# PACKAGE MAP
.
├── data
│   ├── base.RData: Base SS3 run (base), results df (resb)
│   ├── indicators.RData: List of IOTC indicators (indicators), om RPs (refpts)
│   ├── om.RData: IOTC SS3 OM (om), SR (sro), results (res), FLBRP (br), hessians (hess)
│   └── tuning.RData
├── exec
│   ├── makeOM.R
│   ├── Rplot.png
│   ├── runs.R
│   └── tune.R
├── inst
│   ├── bin
│   ├── sa
│   │   └── base
│   └── shiny-apps
│       └── mseapp
├── R
│   ├── BRule.R
│   ├── data.R
│   ├── fwdControl.R
│   ├── grid.R
│   ├── IRate.R
│   ├── load.R
│   ├── performance.R
│   ├── plots.R
│   ├── readSS3.R
│   ├── shiny.R
│   ├── tune.R
│   └── utilities.R
├── tests
│   ├── test-BRule.R
│   ├── test-IRate.R
│   └── test-tune.R
└── vignettes
    ├── ioalbmse.Rmd
    └── mpruns.Rmd
