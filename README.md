# Shiny Sap Flow Analyser  <img src="www/shiny_sapfluxr.png" align="right" width=139 height=139 alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Interactive web application for processing and visualising heat pulse velocity data from ICT SFM1x sensors.

## Overview

This Shiny application provides an easy-to-use interface for students and researchers to:
- Load heat pulse data from ICT sensors (JSON, CSV, or legacy formats), with optional clock-drift and weather/VPD import
- Configure probe and wood properties (YAML or manual entry)
- Calculate heat pulse velocity using multiple methods, with quality flagging
- Apply spacing and wound corrections
- Calibrate secondary methods and switch on the Péclet number (sDMA)
- Convert to sap flux density and integrate to tree water use
- Aggregate to daily totals and visualise results interactively
- Generate a fully reproducible R script of the whole session

Built on the [sapfluxr](../sapfluxr) R package. The app mirrors the package pipeline; for the
underlying functions see the sapfluxr [Get Started](../sapfluxr/vignettes/sapfluxr.Rmd) vignette
and the per-stage guides.

## Installation

### 1. Install sapfluxr

`sapfluxr` contains compiled C++ code (Rcpp). Ensure a C++ toolchain is present first:

| Platform | Requirement |
|---|---|
| **Windows** | [Rtools](https://cran.r-project.org/bin/windows/Rtools/) — match your R version |
| **macOS** | `xcode-select --install` |
| **Linux** | `gcc`/`g++` via your package manager |

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("neez777/sapfluxr")
```

### 2. Install Shiny dependencies

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs",
  "fresh", "plotly", "DT", "waiter", "leaflet", "webshot2",
  "dplyr", "tidyr", "purrr", "yaml", "lubridate", "ggplot2",
  "progressr", "R6"
))
```

### 3. Run the app

Clone or download this repository, then launch from R:

```r
shiny::runApp("path/to/shiny-sapfluxr")
```

Or, from inside the `shiny-sapfluxr` directory:

```r
shiny::runApp()
```

## Features

### Data Import
- Automatic format detection (ICT JSON, CSV, legacy formats)
- Data validation with quality checks
- Optional clock drift correction

### Configuration
- **YAML Mode**: Select pre-defined probe and wood property configurations
- **Manual Mode**: Enter custom parameters with real-time derived value calculation
- Configurations include:
  - Probe geometry and sensor positions
  - Wood thermal and physical properties
  - Tree measurements (DBH, sapwood depth)

### Heat Pulse Velocity Methods
- **HRM** (Heat Ratio Method) — low/reverse flows
- **MHR** (Maximum Heat Ratio) — moderate to high flows
- **Tmax_Coh** / **Tmax_Klu** (T-max, Cohen & Kluitenberg) — high flows

### Corrections, Calibration & sDMA
- **Spacing correction**: zero-flow identification (PELT, dual-stable, or VPD changepoints) with
  segment/gradient offset models and Burgess/linear correction maths (Burgess et al. 2001)
- **Wound correction**: linear or polynomial scaling with temporal wound tracking
- **Method calibration**: align secondary methods to the corrected HRM scale
- **sDMA**: Péclet-based method switching for full diurnal coverage
- Visual diagnostics: zero-flow period identification, before/after comparison, Burgess
  coefficient lookup, temperature trace analysis, and symmetry checks

### Flux Density & Aggregation
- Convert velocity to sap flux density ($J_v = Z \cdot V_h$)
- Radial integration over the sapwood (linear-decay or constant-velocity) to tree water use
- Daily / hourly aggregation with completeness tracking

### Interactive Visualisation
- **Time Series Plot**: Interactive plotly chart with:
  - Multiple method comparison
  - Toggle methods on/off
  - Range slider for date selection
  - Quality flag indicators
  - Hover tooltips

- **Pulse Trace Viewer**: Click on time series to view:
  - Individual pulse temperature traces
  - All four thermistor readings (do, di, uo, ui)
  - Pulse diagnostics and metadata

## Workflow

The app presents the pipeline as a sequence of tabs (the Tools section adds YAML builders and
the reproducible-script generator):

1. **Data Upload** (Tab 1) → load heat pulse data; optional clock-drift and weather/VPD
2. **Configuration** (Tab 2) → select or enter probe and wood properties
3. **Calculations** (Tab 3) → run HPV methods and apply quality flags
4. **Visualise Raw HPV** (Tab 4) → inspect uncorrected data, identify outliers
5. **Spacing Correction** (Tab 5) → identify zero-flow anchors and correct baseline drift
6. **Wound Correction** (Tab 5b) → scale for probe-insertion damage
7. **Calibration & sDMA** (Tabs 6a–7a) → align secondary methods and switch on Péclet
8. **Flux Density** (Tab 8) → convert to flux density and integrate to tree water use
9. **Aggregation** (Tab 9) → daily totals and summaries

## Project Structure

```
shiny-sapfluxr/
├── app.R                          # Main application entry point
├── DESCRIPTION                    # Package dependencies
├── modules/                       # Shiny modules
│   ├── mod_1_data_upload.R       # Data import
│   ├── mod_util_clock_drift.R    # Clock-drift correction
│   ├── mod_util_weather_upload.R # Weather / VPD import
│   ├── mod_2_config.R            # Probe & wood configuration
│   ├── mod_3_methods.R           # HPV calculation & quality flags
│   ├── mod_4_plot_timeseries.R   # Time series visualisation
│   ├── mod_4_pulse_trace.R       # Pulse trace viewer
│   ├── mod_5a_corrections_spacing.R # Spacing correction
│   ├── mod_5b_corrections_wound.R   # Wound correction
│   ├── mod_6a_calibration.R      # Method calibration
│   ├── mod_7_sdma.R              # sDMA switching
│   ├── mod_8_flux_density.R      # Flux density & integration
│   ├── mod_9_aggregation.R       # Temporal aggregation
│   ├── mod_util_code_generation.R   # Reproducible R script
│   └── utils.R                   # Helper functions
├── www/                           # Web assets
└── *.mmd                          # Workflow diagrams
```

## Development

This app is under active development.

### Current Status
- ✅ Data upload, clock-drift, and weather/VPD import
- ✅ Probe & wood configuration
- ✅ HPV calculation & quality flagging
- ✅ Time series plotting and pulse trace viewer
- ✅ Spacing and wound corrections
- ✅ Method calibration & sDMA switching
- ✅ Flux density & sapwood integration
- ✅ Temporal aggregation
- ✅ Reproducible R-script generation

## License

GPL-3

## Authors

- Grant Joyce (Developer)

## Issues & Feedback

Please report issues at: https://github.com/neez777/sapfluxr/issues
