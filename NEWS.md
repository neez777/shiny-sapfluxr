# shiny-sapfluxr 0.6.0

## Breaking Changes

* **R 4.4.0 is now the minimum.** `DESCRIPTION` previously declared `R (>= 4.0.0)`,
  but the app relies throughout on the null-coalescing operator `%||%`, which only
  entered base R in 4.4.0. On R 4.3.3 the app started, loaded data and ran most
  pages normally, then stalled on the zero-flow changepoint page with nothing in
  the R console — the worst possible failure mode. `app.R` now checks the R version
  before anything else and refuses to start with an explicit message.

  Upgrading R resolves it. There is no workaround on 4.3.

## Behaviour Changes

* **Quality control now actually runs.** `flag_vh_quality()` was called with five
  arguments taken from `input$qc_*` values that the Methods panel never creates —
  `qc_detect_missing_pulses`, `qc_check_cross_sensor`, `qc_cross_sensor_threshold`,
  `qc_add_rows_for_missing` and `qc_max_gap_to_fill_hours`. All arrived as `NULL`,
  so the call failed with "argument is of length zero", the surrounding handler
  caught it, and the **unflagged** results were returned instead. Quality control
  was therefore skipped on every run, on every machine, in every prior version.

  Those five arguments are now omitted so the documented `sapfluxr` defaults apply
  (`detect_missing_pulses = TRUE`, `check_cross_sensor = FALSE`,
  `cross_sensor_threshold = 3`, `add_rows_for_missing = TRUE`,
  `max_gap_to_fill_hours = 24`), and a failure now raises a notification in the app
  rather than only a console message.

  **Results will differ from earlier versions.** Runs now carry real quality flags
  where previously everything came through unflagged, so flag counts and anything
  filtered on them will not match previously generated output. Re-run affected
  analyses rather than comparing across versions.

## Bug Fixes

* **Plots no longer freeze the app on current Shiny.** Four modules removed plotly
  traces by index without establishing that those indices existed. Deleting a
  non-existent trace raises `indices must be valid indices for gd.data` in the
  browser; because the error is raised client-side it cannot be caught in R, so the
  `tryCatch(error = function(e) {})` wrapped around each call had no effect and
  nothing appeared in the R console.

  In Shiny 1.14 that exception escapes into the client action queue, which then
  stops processing server messages for the remainder of the session. Every later
  message is dropped, including `waiter`'s request to hide its overlay — so the
  zero-flow changepoint page dimmed and never recovered, while R completed the
  analysis normally. Shiny 1.13 handled each message independently and tolerated
  the errors, which is why the same code worked on some machines and not others.

  - `mod_4_plot_timeseries.R` deleted trace `n_base` one hundred times in a loop,
    relying on the delete failing to know when to stop, emitting up to a hundred
    client errors per trigger. It now tracks how many overlay traces it added and
    at which offset, and removes exactly those.
  - `mod_6b_calibration_validation.R` did the same over twenty iterations, and is
    fixed the same way.
  - `mod_5a_corrections_spacing.R` and `mod_5b_corrections_wound.R` added and
    removed overlays at fixed indices on plots rendered with fewer traces than
    that. Both now render their overlays up-front, hidden, and toggle visibility
    with `restyle`. Both also guard the toggle against renders that produce no
    overlay traces, which happens when the selected sensor has no data.

* **Missing requirements are reported at startup.** `app.R` now checks every
  required package in one pass and names all missing ones in a single
  `install.packages()` line, instead of surfacing them one failed launch at a time.

* **The app icon no longer disappears on Linux.** `www/sapfluxR.png` was tracked
  with a capital R while `app.R` requests `sapfluxr.png`. Case-insensitive
  filesystems resolve both to the same file; Linux does not, so a fresh clone
  served a 404 and the header icon was missing. Renamed in the index; content
  unchanged.

* **Dependency declarations corrected.** `htmlwidgets` was used in `modules/utils.R`
  but declared nowhere, arriving only transitively via plotly. `lutz` and `zip` were
  called unguarded with `::` but only listed under `Suggests`. All three move to
  `Imports`. `webshot2` is removed, having no usages anywhere in the app.

## Diagnostics

* Added `inst/diagnostics/repro_dual_stable.R`, which runs the same
  `find_dual_stable_periods()` call as the zero-flow changepoint page without
  Shiny, waiter, plotly or a browser, against the example data shipped with
  `sapfluxr`. Output is comparable across machines, so it separates a fault in the
  analysis from a fault in the Shiny layer.

---

# shiny-sapfluxr 0.5.1

## Behaviour Changes

* **Calibration breakpoints are now seeded** (Tab 6a, Method Calibration). Requires
  `sapfluxr (>= 0.9.0)` (the DESCRIPTION pins `>= 0.9.1`).

  The calibration regression previously let `sapfluxr::compare_methods_segmented()` start its
  breakpoint search from the median HRM velocity. On a high-frequency record that median sits in
  the low-flow cloud, so the search could settle on a local optimum well below the true
  method-handover point. `sapfluxr` 0.9.0 seeds the starting value from an R^2 threshold sweep
  instead.

  No app code changed — the module already calls `compare_methods_segmented()` without an explicit
  `initial_breakpoint`, so it inherits the corrected behaviour. **The package must be reinstalled**
  for this to take effect; `devtools::load_all()` is not sufficient, because the module calls
  `sapfluxr::` qualified. The auto-detected threshold shown in Threshold Settings, and the value
  committed by "Apply Calibration", both follow from the seeded breakpoint.

  See the `sapfluxr` 0.9.0 NEWS entry for the outstanding field-data validation on this change.

* **Heat-capacity temperature selector reworked** (Tab 8, Sap Flux Density). The control previously
  offered two options under a "Sap-flux Temperature (Becker & Edwards)" heading, with the
  attribution attached to the fixed-temperature option. It now offers all three conversion methods
  under a neutral "Heat-Capacity Temperature" heading, with Becker & Edwards naming the per-pulse
  method:

  | Option | `temperature_mode` | Behaviour |
  |---|---|---|
  | Constant (no temperature dependence) | `"constant"` | Fixed conversion factor from the wood properties |
  | Static (single fixed temperature) | `"static"` | Evaluated at one wood temperature |
  | Becker & Edwards (per-pulse pre-pulse temperature) | `"dynamic"` | Evaluated per pulse from `prepulse_temp_c` |

  **Constant was previously unreachable** from the interface — the selector offered only the two
  temperature-dependent methods and defaulted to Static, so the app could not reproduce a
  fixed-factor conversion, which is the `sapfluxr` default and the behaviour of analyses predating
  version 0.8.0. The handling code was already present but had no way to be selected. The default
  remains Static, so existing results are unaffected.

* The generated reproducibility script no longer labels every conversion as Becker & Edwards. Its
  comment and step description now name the method actually used.

## Bug Fixes

* **Becker & Edwards flux now works for sDMA methods** (Tab 8, Sap Flux Density). Selecting the
  Becker & Edwards (per-pulse) temperature produced a result for HRM but not for `sDMA:MHR`: the
  sDMA rows lacked the per-pulse temperature, so the conversion returned all-NA flux for them.
  Fixed in `sapfluxr (>= 0.9.1)`, which the DESCRIPTION now pins — the app itself is unchanged.
  **The sDMA step (Tab 7) must be re-run** against the rebuilt package for existing analyses to
  gain the temperature; results computed before the fix will still NA-out under Becker & Edwards.

* **Tree Water Use — Hourly plot no longer breaks with multiple methods** (Page 9, Radial
  Integration). With more than one method displayed, the Hourly plot rendered a bare "Error:" with
  empty axes while the Daily plot worked. Root cause: the plot downsamples above 30,000 points using
  `seq(1, n, length.out = ...)`, which returns **doubles**; the tree-water-use data is a tibble, and
  tibble row-subsetting rejects non-integer indices ("Can't convert from `i` <double> to <integer>
  due to loss of precision"). It only triggered with two methods because that is when the row count
  crossed 30,000. Fixed by coercing the sample indices to integer. The same
  `seq(length.out=)`-into-tibble-subset pattern was corrected in the sDMA plot
  (`mod_7_sdma.R`), the spacing-correction and working-reference plots (`mod_5a_*`), and the
  wound-correction plots (`mod_5b_corrections_wound.R`), all of which would fail the same way on
  large (e.g. 3 Hz) datasets.

  The Hourly renderer was also hardened so a caught error is never shown as an empty "Error:" again:
  Shiny silent/`req()` conditions are re-raised (leaving the output blank instead of an error title),
  genuine errors show their actual message, the `method_label` column and a NULL `show_points` input
  are guarded, and the intentional placeholder plots use `plotly::plotly_empty()` so they no longer
  emit plotly's "No trace type specified" console warning.

* Wood Properties tool: the Representative Wood Temperature help text described itself as seeding
  "the Becker & Edwards sap-flux conversion". It seeds the Static temperature, which under the
  above naming is a different method.

---

# shiny-sapfluxr 0.5.0

## New Features

* **Automatic VPD on weather upload** (Tab 1): vapour pressure deficit and daily VPD minima are now
  calculated automatically as soon as weather data is loaded — the manual "Calculate VPD" button has
  been removed. When heat pulse data is present, weather is auto-trimmed to its date range; the
  trim controls remain as an optional override and recompute VPD on change.
* **Becker & Edwards sap-flux temperature** control (Tab 8, Sap Flux Density): choose Static (a fixed
  temperature, defaulting to the wood-properties temperature) or Dynamic (each pulse's pre-pulse
  temperature) for the `calc_sap_flux_density()` conversion. Placed at the conversion step where it
  actually acts, rather than at velocity calculation.
* **Representative Wood Temperature** field reinstated on the Wood Properties tool (Wood Constant
  tab), stored in the YAML `wood_property$temperature` and used as the Static default above.
* **Interactive pulse-trace viewer** (Tab 4): the Pulse Trace Controls now let you vary the pre-pulse
  baseline method and see the trace and a per-method live heat-pulse-velocity readout update in real
  time.

## Bug Fixes

* **Spacing correction gradient anchors**: removing a confirmed zero-flow anchor now also removes its
  purple anchor marker from the plot and excludes it from the gradient correction. Previously the
  marker persisted and the removed anchor was still used in the calculation.

---

# shiny-sapfluxr 0.4.0

## Breaking / Behaviour Changes

* **Removed "Recalculate Péclet" button** from Tab 7 (sDMA). Péclet numbers are now computed
  automatically inside `apply_sdma_processing()` when the user clicks "Apply sDMA Switching".
  No separate Step 1 action is required.

## New Features

* **Example data loading** (Tab 2): a "Load Example Data" radio option and Tools button allow
  users to instantly load the bundled 10-day thesis dataset (heat pulse, weather, and wood
  configuration) without uploading files — ideal for exploring the app for the first time.

## Improvements

* sDMA module (`mod_7_sdma.R`): `apply_sdma_processing()` now receives `probe_config` and
  `wood_properties` directly; the manual construction of a `vh_combined` tibble (with
  `peclet_number` pre-populated) has been removed. The validation sub-module still receives
  `rv$vh_with_peclet` (HRM rows with `Pe_corrected` column) derived from the sDMA result.
* Reproducible R script updated: the generated code no longer includes a separate
  `recalculate_peclet()` call; `apply_sdma_processing(..., probe_config, wood_properties)` is
  the single sDMA entry point.
* "About sDMA" info box updated to reflect the two-step workflow (configure → apply).

---

# shiny-sapfluxr 0.3.0

## New Modules

* **Flux Density & Integration** (Tab 8, `mod_8_flux_density.R`)
  - `calc_sap_flux_density()` with the Z factor from wood properties
  - Two radial integration models: `linear_decay` and `constant_velocity`
  - Tree dimension inputs (DBH, sapwood thickness, bark thickness)
  - Downloadable flux density data

* **Aggregation** (Tab 9, `mod_9_aggregation.R`)
  - Hourly and daily aggregation of tree water use
  - Completeness tracking (fraction of hours with data per day)
  - Downloadable daily totals

## UI Changes

* Expanded from 7 to 9 tabs to accommodate the new flux density and aggregation stages.
* Tab 7 split into calibration (Tab 6a) and sDMA (Tab 7a) sub-steps for clarity.
* Calibration tab (`mod_6a_calibration.R`) separated from the sDMA module for independent
  access — users can calibrate without applying sDMA switching.

## Bug Fixes

* Fixed reactive dependency chain between the wound correction and calibration modules that
  caused stale data to persist after re-uploading.
* Corrected VPD overlay rendering on the Raw HPV plot when weather data is uploaded after
  initial HPV calculation.

---

# shiny-sapfluxr 0.2.0

## New Modules

* **Wound Correction Module** (`mod_wound_correction.R`)
  - Support for multiple probe reinstallation dates
  - Temporal tracking of wound diameter expansion
  - Linear interpolation between reinstallation events
  - Interactive UI for adding/removing reinstallation dates
  - Visual feedback with wound diameter plots
  - Temporal tracking of wound diameter expansion
  - Linear interpolation between reinstallation events
  - Interactive UI for adding/removing reinstallation dates
  - Visual feedback with wound diameter plots

* **Calibration & sDMA Module** (`mod_calibration_sdma.R`)
  - Method calibration using linear regression
  - Quality metrics display (R², RMSE)
  - sDMA (Selectable Dual Method Approach) for automatic method switching
  - Interactive comparison of methods before and after calibration
  - sDMA selection frequency visualisation
  - Support for both early and late calibration workflows

* **Code Generation Module** (`mod_code_generation.R`)
  - R6-based code tracking system
  - Generates fully reproducible R scripts
  - Tracks all user actions and parameters
  - Copy to clipboard and download functionality
  - Includes session info for reproducibility

## UI Improvements

* Restructured workflow into 7 logical tabs:
  1. Data Upload
  2. Configuration
  3. Calculations
  4. Visualise Raw HPV
  5. Corrections (Spacing + Wound)
  6. Calibration & sDMA ← NEW
  7. Visualise Corrected

* Added VPD overlay to Raw HPV visualisation
  - Shows daily minimum VPD on secondary y-axis
  - Compatible with Peclet number display
  - Conditional UI based on weather data availability

* Enhanced interactive visualisations
  - Method comparison plots (before/after calibration)
  - sDMA selection frequency bar charts
  - Calibration quality summaries

* Improved error handling and user notifications
  - Validation checks for missing methods
  - Clear error messages for configuration issues
  - Progress indicators for long-running operations

## Documentation

* **Technical Documentation**
  - `CALIBRATION_SDMA_INTEGRATION.md` - Detailed calibration integration guide
  - `QUICK_START_CALIBRATION_SDMA.md` - Quick reference for calibration
  - `CALIBRATION_SDMA_SUMMARY.md` - Comprehensive overview
  - `CALIBRATION_SDMA_APP_INTEGRATION.patch` - Step-by-step integration

* **Workflow Diagrams**
  - Created 4 Mermaid diagram files for different views:
    * `WORKFLOW_DIAGRAM.mmd` - Complete detailed workflow
    * `WORKFLOW_SIMPLE.mmd` - Simplified tab-by-tab flow
    * `MODULE_INTERACTIONS.mmd` - Module dependencies
    * `DATA_TRANSFORMATIONS.mmd` - Data structure evolution
  - `MERMAID_DIAGRAMS_README.md` - Guide to viewing and using diagrams

* **Documentation Organisation**
  - Cleaned up root directory (52-73% file reduction)
  - Created `docs_archive/` for old documentation
  - Created `DOCUMENTATION_INDEX.md` master index
  - Added archive README files explaining archived content

## Performance

* Optimised plot rendering for large datasets
  - Automatic data sampling for >5,000 points in comparison plots
  - Efficient reactive data flow between modules
  - Reduced memory usage in calibration module

## Dependencies

* Added `R6` for code generation CodeTracker class
* Added `shinyjs` for enhanced UI interactions
* Added `fresh` for consistent theming (already in use)

## Bug Fixes

* Fixed data flow between correction modules
* Corrected reactive dependency chain for final visualisation
* Fixed VPD plot conditional panel logic

## Internal Changes

* Standardised module naming conventions
* Improved code organisation and documentation
* Added module template for future development
* Enhanced reactive data handling

---

# shiny-sapfluxr 0.1.0

## Initial Release

* Interactive web application for sap flow analysis
* Data upload and validation
* Probe and wood property configuration
* Heat pulse velocity calculations (multiple methods)
* Interactive visualisation of raw HPV data
* Spacing correction with multiple methods (PELT, Manual, VPD, Heartwood)
* Basic plotting and data export
* Integration with sapfluxr package

## Features

* Tab-based workflow structure
* Real-time data validation
* Interactive plots with Plotly
* Configuration file upload and validation
* Weather data integration (optional)
* Clock drift correction
* Comprehensive error handling
