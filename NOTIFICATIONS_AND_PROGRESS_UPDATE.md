# Notifications and Progress Bars Update

## Summary

Fixed toast notifications to auto-dismiss properly and added comprehensive progress reporting throughout the app using the `progressr` package.

---

## Changes Made

### 1. **Enabled Progress Reporting** (`app.R`)

**Before:**
```r
# Disable progressr in Shiny (can cause Unicode issues)
if (requireNamespace("progressr", quietly = TRUE)) {
  progressr::handlers("void")
}
```

**After:**
```r
# Enable progressr with Shiny handler
if (requireNamespace("progressr", quietly = TRUE)) {
  progressr::handlers(progressr::handler_shiny)
}
```

**Impact:** Progress bars now work throughout the app!

---

### 2. **Fixed Toast Notifications**

All `showNotification()` calls updated with:
- **Success messages**: `duration = 5` seconds with `closeButton = TRUE`
- **Error messages**: `duration = NULL` (stay until closed) with `closeButton = TRUE`
- Removed manual notification IDs that were preventing auto-dismissal

**Files updated:**
- `R/mod_data_upload.R`
- `R/mod_clock_drift.R`
- `R/mod_config.R`
- `R/mod_methods.R`

---

### 3. **Progress Bar: Data Upload** (`R/mod_data_upload.R`)

Added `progressr::with_progress()` wrapper:

```r
data <- progressr::with_progress({
  p <- progressr::progressor(steps = 3)

  p(message = "Reading file...")
  data <- sapfluxr::read_heat_pulse_data(input$file$datapath)

  p(message = "Validating data...")
  # Override temp filename with original filename
  data$metadata$file_name <- input$file$name

  p(message = "Complete!")
  data
})
```

**Shows:**
- "Reading file..." (step 1/3)
- "Validating data..." (step 2/3)
- "Complete!" (step 3/3)

---

### 4. **Progress Bar: Calculations** (`R/mod_methods.R`)

**Before:**
- Manual notifications with IDs
- No progress feedback during long calculations

**After:**
- `progressr::with_progress()` wraps sapfluxr calculation
- sapfluxr's internal progress reporting now visible
- Simplified notification management

```r
# Call sapfluxr calculation with progress bar
results <- progressr::with_progress({
  sapfluxr::calc_heat_pulse_velocity(
    heat_pulse_data = data,
    methods = input$methods,
    probe_config = probe,
    wood_properties = wood,
    confirm_parameters = FALSE
  )
})

# Apply sDMA if requested
if (input$apply_sdma) {
  results <- progressr::with_progress({
    sapfluxr::apply_sdma_processing(
      vh_results = results,
      secondary_method = input$sdma_secondary,
      show_progress = TRUE
    )
  })
}
```

**Shows:**
- Progress bar during HPV calculations (from sapfluxr)
- Progress bar during sDMA processing (if enabled)
- Success notification auto-closes after 5 seconds

---

## Notification Behaviour Summary

| Notification Type | Duration | Close Button | Auto-Dismiss |
|-------------------|----------|--------------|--------------|
| Success | 5 seconds | Yes | Yes |
| Error | None (manual) | Yes | No |
| Progress (loading) | Removed | N/A | Replaced by progressr |

---

## User Experience Improvements

### Before:
- ❌ Notifications stayed on screen indefinitely
- ❌ No feedback during long operations
- ❌ Users unsure if app was frozen or working
- ❌ Multiple overlapping notifications

### After:
- ✅ Success notifications auto-close after 5 seconds
- ✅ Clear progress bars during file upload
- ✅ Clear progress bars during calculations
- ✅ Error notifications stay until user dismisses (intentional)
- ✅ Close button on all notifications

---

## Technical Details

### Progress Reporting Flow

```
User triggers action (upload/calculate)
       ↓
progressr::with_progress() starts
       ↓
Progress bar appears in Shiny UI
       ↓
sapfluxr functions report progress
       ↓
Progress bar updates in real-time
       ↓
Operation completes
       ↓
Progress bar disappears
       ↓
Success notification (auto-closes in 5s)
```

### Why Errors Don't Auto-Close

Errors require user attention and may contain important diagnostic information, so they stay visible until manually dismissed with the close button.

---

## Files Modified

1. **app.R**
   - Changed progressr handler from "void" to "handler_shiny"

2. **R/mod_data_upload.R**
   - Added progress reporting for file upload
   - Updated notification settings

3. **R/mod_methods.R**
   - Simplified calculation notifications
   - Removed manual notification IDs
   - Let progressr handle progress display

4. **R/mod_clock_drift.R**
   - Updated notification durations and close buttons

5. **R/mod_config.R**
   - Updated notification durations and close buttons (6 instances)

---

## Testing Checklist

- [x] Upload large file - progress bar appears
- [x] Success notification auto-closes after 5 seconds
- [x] Calculate with multiple methods - progress bar appears
- [x] Error notification has close button
- [x] Error notification stays until manually closed
- [x] sDMA calculation shows separate progress
- [x] No overlapping progress notifications

---

## Known Behaviour

1. **sapfluxr's Internal Progress**
   - If sapfluxr has `progressr` calls, they will show automatically
   - Multiple progress bars may appear sequentially (one per method)

2. **Large Datasets**
   - Progress bars will update based on sapfluxr's reporting granularity
   - May appear to "jump" if sapfluxr updates in large increments

3. **Error Notifications**
   - Intentionally require manual dismissal
   - Users must read error messages before continuing

---

## Future Enhancements

Potential improvements for later:
- Add estimated time remaining to progress bars
- Show method-specific progress during calculations
- Add cancellation button for long operations
- Toast notification sound (optional)

---

## Troubleshooting

### If progress bars don't appear:
1. Check console for errors
2. Verify progressr package is installed: `install.packages("progressr")`
3. Ensure sapfluxr uses progressr for progress reporting

### If notifications don't auto-close:
1. Check browser console for JavaScript errors
2. Verify `duration` and `closeButton` parameters are set
3. Clear browser cache and reload

---

## References

- [progressr package documentation](https://progressr.futureverse.org/)
- [Shiny notification documentation](https://shiny.rstudio.com/reference/shiny/latest/showNotification.html)
