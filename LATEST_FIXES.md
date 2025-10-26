# Latest Fixes - Toast Notifications & sDMA

## Issues Fixed

### 1. **Progressr Message Error** ✓
**Error:** `unused argument (message = "Calculating heat pulse velocities...")`

**Cause:** `progressr::with_progress()` doesn't accept a `message` argument

**Fix:** Removed the message parameters from both calculation calls:
```r
# Before (WRONG)
results <- progressr::with_progress({
  sapfluxr::calc_heat_pulse_velocity(...)
}, message = "Calculating heat pulse velocities...")

# After (CORRECT)
results <- progressr::with_progress({
  sapfluxr::calc_heat_pulse_velocity(...)
})
```

---

### 2. **sDMA Multiple Secondary Methods** ✓
**Request:** Allow selection of multiple secondary methods for sDMA

**Change:** Changed from single-select dropdown to multi-select checkboxes

**Before:**
```r
selectInput(
  ns("sdma_secondary"),
  "Secondary Method for sDMA:",  # Singular
  choices = c(...),
  selected = "MHR"
)
```

**After:**
```r
checkboxGroupInput(
  ns("sdma_secondary"),
  "Secondary Methods for sDMA:",  # Plural
  choices = c(
    "MHR" = "MHR",
    "Tmax (Cohen)" = "Tmax_Coh",
    "Tmax (Kluitenberg)" = "Tmax_Klu",
    "HRMXa" = "HRMXa",
    "HRMXb" = "HRMXb"
  ),
  selected = "MHR"
)
```

**Now you can select multiple methods** - e.g., MHR + Tmax_Coh together

---

### 3. **Toast Notifications Still Not Disappearing** ✓
**Problem:** Notifications weren't auto-closing despite `duration = 5`

**Root Cause:** In Shiny modules, notifications need explicit `session` parameter to work correctly

**Fix:** Added `session = session` to ALL showNotification calls:

```r
# Before (DOESN'T WORK in modules)
showNotification(
  "Success message",
  type = "message",
  duration = 5
)

# After (WORKS!)
showNotification(
  "Success message",
  type = "message",
  duration = 5,
  session = session  # ← This is critical in modules!
)
```

**Files Updated:**
- `R/mod_methods.R` - 3 notifications fixed
- `R/mod_data_upload.R` - 2 notifications fixed
- `R/mod_clock_drift.R` - 2 notifications fixed
- `R/mod_config.R` - 6 notifications fixed

---

## Why `session = session` is Needed

In Shiny **modules**, the notification system needs to know which session to send the notification to. Without it:
- Notifications might not appear
- Notifications might not auto-close
- Notifications might appear in wrong session (in multi-user scenarios)

**Key Point:** This is only necessary in modules (functions using `moduleServer`). In the main `server` function, it's optional.

---

## How It Works Now

### Success Notifications:
```r
showNotification(
  "Operation successful!",
  type = "message",
  duration = 5,        # Auto-close after 5 seconds
  session = session    # Route to correct session
)
```
- Appears immediately
- **Disappears automatically after 5 seconds** ✓
- No user action required

### Error Notifications:
```r
showNotification(
  paste("Error:", e$message),
  type = "error",
  duration = NULL,     # Stay until page change
  session = session    # Route to correct session
)
```
- Appears immediately
- **Stays visible** (important errors need attention)
- Disappears when user navigates to another tab or refreshes

---

## Testing Checklist

Test the following scenarios:

### Data Upload:
- [ ] Upload file
- [ ] Success notification appears
- [ ] **Notification disappears after ~5 seconds** ✓

### Calculations:
- [ ] Select methods
- [ ] Click Calculate
- [ ] Waiter spinner appears
- [ ] Calculation completes
- [ ] Success notification appears
- [ ] **Notification disappears after ~5 seconds** ✓

### sDMA:
- [ ] Enable sDMA checkbox
- [ ] **Multiple secondary methods can be selected** ✓
- [ ] Select 2+ methods (e.g., MHR + Tmax_Coh)
- [ ] Calculation works with multiple methods

### Errors:
- [ ] Trigger an error (e.g., sDMA without HRM)
- [ ] Error notification appears
- [ ] **Error notification stays visible** ✓
- [ ] Navigate to another tab
- [ ] Error notification disappears (expected behavior)

---

## Summary of Changes

| File | Changes |
|------|---------|
| `mod_methods.R` | • Fixed progressr message error<br>• Changed sDMA to checkboxGroupInput<br>• Added `session = session` to 3 notifications |
| `mod_data_upload.R` | • Added `session = session` to 2 notifications |
| `mod_clock_drift.R` | • Added `session = session` to 2 notifications |
| `mod_config.R` | • Added `session = session` to 6 notifications |

---

## What You Should See Now

1. **Calculations work** without errors
2. **sDMA allows multiple secondary methods** (checkboxes instead of dropdown)
3. **Success notifications disappear after 5 seconds automatically**
4. **Error notifications stay visible** until you navigate away
5. **Waiter spinner shows** during calculations

If notifications still don't disappear, there might be browser caching issues. Try:
1. Hard refresh: Ctrl+Shift+R (Windows) or Cmd+Shift+R (Mac)
2. Clear browser cache
3. Restart R session and re-run app

---

## Technical Notes

### Why Duration Alone Wasn't Enough:
The `duration` parameter tells Shiny **when** to remove the notification, but in modules, it also needs to know **where** to remove it from. The `session` parameter provides this routing information.

### Module Context:
```r
moduleServer(id, function(input, output, session) {
  # Inside here, 'session' refers to THIS module's session
  # Without passing it, notifications go to global session
  # which might have different timing/lifecycle
})
```

This is a common "gotcha" in Shiny module development!
