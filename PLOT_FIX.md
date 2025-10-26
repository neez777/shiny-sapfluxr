# Time Series Plot - Final Fix

## The Problem

Time series plot was showing:
```
Error: subscript out of bounds
```

The smaller range slider plot below the main plot wasn't displaying.

---

## Root Cause: Quality Flag Markers

**The subscript error was NOT in the method colors** - it was in the **quality flag markers section**!

### What Went Wrong:

When the plot tried to display quality flag markers (WARNING, ERROR, SUSPECT), it used:

```r
# BEFORE - Caused "subscript out of bounds" error:
for (flag in unique(markers$quality_flag)) {
  p <- p %>%
    add_trace(
      marker = list(
        symbol = flag_shapes[[flag]],      # ❌ CRASH if flag not in list!
        color = flag_colours[[flag]]       # ❌ CRASH if flag not in list!
      )
    )
}
```

**Problem:** If sapfluxr returns a quality flag that's not in the predefined list (e.g., "OUTLIER", "QUESTIONABLE", or any other flag), the `[[flag]]` subsetting fails with "subscript out of bounds".

This was OUTSIDE the main tryCatch block, so the error wasn't being caught properly.

---

## The Solution

### Quality Flag Safe Access:

```r
# AFTER - Safe access with defaults:
for (flag in unique(markers$quality_flag)) {
  flag_data <- markers %>% filter(quality_flag == !!flag)

  # Get shape and color safely - use defaults if not defined
  flag_shape <- if (flag %in% names(flag_shapes)) {
    flag_shapes[[flag]]
  } else {
    "circle"  # Default shape for unknown flags
  }

  flag_color <- if (flag %in% names(flag_colours)) {
    flag_colours[[flag]]
  } else {
    "#999999"  # Default gray for unknown flags
  }

  p <- p %>%
    add_trace(
      marker = list(
        symbol = flag_shape,  # ✅ Always has a value
        color = flag_color    # ✅ Always has a value
      )
    )
}
```

**Benefits:**
- ✅ Predefined colors/shapes for known flags (WARNING, ERROR, SUSPECT)
- ✅ Default gray circle for any unknown flags
- ✅ No more "subscript out of bounds" errors
- ✅ Plot handles any quality flags sapfluxr might return

---

## Range Slider Fix

### Problem:
The range slider needs enough space at the bottom of the plot to display properly.

### Solution:

```r
# BEFORE:
xaxis = list(
  rangeslider = list(visible = TRUE)
),
margin = list(b = 100)

# AFTER:
xaxis = list(
  rangeslider = list(
    visible = TRUE,
    thickness = 0.1  # Height as fraction of plot (10%)
  )
),
margin = list(b = 150)  # Increased bottom margin for slider
```

Also adjusted legend position slightly: `y = -0.25` (was -0.2) to avoid overlap.

---

## What Changed

### File: `R/mod_plot_timeseries.R`

**Lines 369-380:** Quality flag marker rendering
- Added safe checks before accessing flag_shapes and flag_colours
- Provide default values for unknown flags

**Lines 413-415:** Range slider configuration
- Added `thickness = 0.1` parameter
- Increased bottom margin from 100 to 150

**Line 435:** Legend position
- Moved down slightly to `y = -0.25`

---

## Testing Checklist

**Time Series Plot:**
- [ ] Plot displays without "subscript out of bounds" error ✓
- [ ] All methods show with appropriate colors ✓
- [ ] sDMA combinations display correctly ✓
- [ ] Quality flags display with correct markers
- [ ] Unknown quality flags display with gray circles
- [ ] Plot updates when toggling methods
- [ ] Plot updates when toggling sensor positions

**Range Slider:**
- [ ] Range slider appears below main plot ✓
- [ ] Can drag slider to zoom time range
- [ ] Main plot updates when slider moved
- [ ] Date selector buttons work (1d, 1w, 1m, 3m, All)

**Zoom Preservation:**
- [ ] Zoom level maintained when toggling methods
- [ ] Zoom level maintained when toggling sensor positions
- [ ] "Reset Zoom" button works

---

## Summary

✅ **"Subscript out of bounds" error fixed** by adding safe access to quality flag markers
✅ **Range slider configured** with proper thickness and margin
✅ **Unknown quality flags handled** gracefully with defaults
✅ **Plot layout optimized** for all components

**The plot should now work perfectly!** 🎉

All quality flags (known or unknown) will display properly, and the range slider will allow easy timescale navigation.
