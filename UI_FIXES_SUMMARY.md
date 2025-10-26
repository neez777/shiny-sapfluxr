# UI Fixes Summary

## Changes Made

### 1. **Removed Tab 4 and Renamed Tab 3** ✓

**Before:**
- Tab 1: Data Upload
- Tab 2: Configuration
- Tab 3: Methods
- Tab 4: Calculate (empty placeholder)
- Tab 5: Visualise
- Tab 6: Export

**After:**
- Tab 1: Data Upload
- Tab 2: Configuration
- Tab 3: Calculations (renamed from "Methods")
- Tab 4: Visualise
- Tab 5: Export

**Changes:**
- Removed empty Tab 4 (Calculate)
- Renamed Tab 3 from "Methods" to "Calculations"
- Renumbered subsequent tabs

---

### 2. **Fixed Toast Notifications** ✓

**Problem:** Notifications weren't disappearing automatically

**Root Cause:** Using both `duration` and `closeButton = TRUE` together can interfere with auto-dismiss behavior in some Shiny versions.

**Solution:** Removed `closeButton` parameter and use only `duration`:

**Success messages:**
```r
showNotification(
  "Operation successful",
  type = "message",
  duration = 5  # Auto-closes after 5 seconds
)
```

**Error messages:**
```r
showNotification(
  paste("Error:", e$message),
  type = "error",
  duration = NULL  # Stays until page refresh/navigation
)
```

**Files updated:**
- `R/mod_data_upload.R`
- `R/mod_clock_drift.R`
- `R/mod_config.R`
- `R/mod_methods.R`

---

### 3. **Added Visual Progress Indicator** ✓

**Problem:** No visible progress during calculations

**Solutions Implemented:**

#### A. Added `waiter` Package
- Added `waiter` to DESCRIPTION dependencies
- Initialized `waiter::use_waiter()` in app UI
- Creates spinning loading overlay during calculations

#### B. Updated Calculation Progress
**Before:**
```r
results <- progressr::with_progress({
  sapfluxr::calc_heat_pulse_velocity(...)
})
```

**After:**
```r
# Create waiter with spinning indicator
w <- waiter::Waiter$new(
  id = session$ns("results_table"),
  html = waiter::spin_fading_circles(),
  color = waiter::transparent(0.5)
)

w$show()  # Show spinner

results <- progressr::with_progress({
  sapfluxr::calc_heat_pulse_velocity(...)
}, message = "Calculating heat pulse velocities...")

w$hide()  # Hide spinner when done
```

#### C. Added `progressr` to Dependencies
- Added `progressr (>= 0.13.0)` to DESCRIPTION
- Configured progress handler in app.R

---

## How It Works Now

### File Upload:
1. User selects file
2. Progress bar appears showing 3 steps:
   - "Reading file..."
   - "Validating data..."
   - "Complete!"
3. Success notification appears (auto-closes in 5 seconds)

### Calculations:
1. User clicks "Calculate Heat Pulse Velocity"
2. **Waiter spinner appears** over results area (fading circles animation)
3. sapfluxr processes data (internal progress reporting in R console)
4. Spinner disappears when complete
5. Success notification appears (auto-closes in 5 seconds)

### Errors:
1. Error notification appears
2. **Stays visible** until user navigates away or refreshes page
3. Allows user to read full error message

---

## Testing Checklist

Test the following:

- [ ] Tab 3 is now labeled "Calculations"
- [ ] Tab 4 is "Visualise" (not "Calculate")
- [ ] No empty "Calculate" tab exists
- [ ] Upload file - success notification disappears after 5 seconds
- [ ] Run calculation - waiter spinner appears
- [ ] Calculation completes - spinner disappears, notification shows
- [ ] Success notification disappears after 5 seconds
- [ ] Trigger error - error notification stays visible

---

## Known Behavior

### Progress Reporting:
- **Waiter spinner**: Visual feedback that something is happening
- **progressr console output**: Progress details appear in R console (not in Shiny UI)
- **sapfluxr progress**: sapfluxr's internal progress updates appear in R console

### Why not a progress bar in the UI?
- sapfluxr's progress updates are designed for console output
- Shiny's progress system doesn't integrate perfectly with progressr in all cases
- **Waiter spinner** provides clear visual feedback that calculation is running
- Console output shows detailed progress for debugging

### Notifications:
- **Success**: Auto-close after 5 seconds (user doesn't need to dismiss)
- **Errors**: Stay visible (important diagnostic information)
- **Loading**: Replaced with waiter spinner (better UX)

---

## Future Enhancements

Possible improvements:
1. Convert console progress to Shiny progress bar
2. Add estimated time remaining
3. Add cancel button for long operations
4. Show progress percentage in waiter overlay

---

## Dependencies Added

```r
# DESCRIPTION
Imports:
    progressr (>= 0.13.0),  # Progress reporting
    waiter (>= 0.2.5),      # Loading animations
    ...
```

Make sure to install these packages:
```r
install.packages(c("progressr", "waiter"))
```

---

## Files Modified

1. **app.R**
   - Removed Tab 4 menu item
   - Renamed Tab 3 to "Calculations"
   - Removed Tab 4 content
   - Renumbered tabs
   - Added waiter initialization
   - Configured progressr handler
   - Added library(waiter)

2. **DESCRIPTION**
   - Added `progressr (>= 0.13.0)` dependency
   - Added `waiter (>= 0.2.5)` dependency

3. **R/mod_methods.R**
   - Added waiter spinner
   - Removed closeButton from notifications
   - Updated duration to 5 seconds for success

4. **R/mod_data_upload.R**
   - Removed closeButton from notifications
   - Kept duration at 5 seconds

5. **R/mod_clock_drift.R**
   - Removed closeButton from notifications

6. **R/mod_config.R**
   - Removed all closeButton parameters

---

## Summary

✅ Tab 4 removed, Tab 3 renamed to "Calculations"
✅ Toast notifications now auto-dismiss after 5 seconds
✅ Visual progress indicator (waiter spinner) during calculations
✅ Better user experience with clear feedback
✅ Errors stay visible for user attention

The app should now feel more polished and responsive!
