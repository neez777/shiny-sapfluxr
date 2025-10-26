# Unicode Regex Error Fix

## Error Message
```
invalid regular expression '[🌀-🧿]', reason 'Invalid character range'
```

## The Problem

This error occurs when there's a Unicode emoji character range in a regex pattern. Windows R has issues with Unicode regex ranges.

## Where the Error Comes From

The error is in **sapfluxr**, not the Shiny app. It was supposed to be fixed in line 27-28 of `sapfluxr/R/01a_data_import.R`.

## Solutions (Try in Order)

### Solution 1: Reload sapfluxr Package

**In R Console (not Shiny):**
```r
# Navigate to sapfluxr directory
setwd("E:/R/project/sapfluxr")

# Reload the package
devtools::load_all()

# Reinstall
devtools::install()
```

**Then restart the Shiny app.**

---

### Solution 2: Check sapfluxr Version

The fix might not be in your installed version.

**In R Console:**
```r
# Check what version is loaded
packageVersion("sapfluxr")

# Check where it's installed from
find.package("sapfluxr")

# Detach and reload
detach("package:sapfluxr", unload = TRUE)
library(sapfluxr)
```

---

### Solution 3: Force Reinstall sapfluxr

**In R Console:**
```r
# Remove old installation
remove.packages("sapfluxr")

# Reinstall from source
setwd("E:/R/project/sapfluxr")
devtools::install()
```

---

### Solution 4: Restart R Session

Sometimes R caches old package versions.

1. Close RStudio completely
2. Reopen RStudio
3. Run:
```r
setwd("E:/R/project/sapfluxr")
devtools::load_all()
```
4. Run Shiny app

---

### Solution 5: Check if Fix is Present

**Verify the fix exists in sapfluxr:**

```r
# Read the file
readLines("E:/R/project/sapfluxr/R/01a_data_import.R")[27:28]
```

**Should show:**
```r
[1] "      # Use iconv instead of regex to avoid Windows Unicode issues"
[2] "      clean_msg <- iconv(msg, to = \"ASCII//TRANSLIT\", sub = \"\")"
```

**If it doesn't show this, the fix isn't there!**

---

### Solution 6: Manual Fix (if needed)

If the fix is missing from `sapfluxr/R/01a_data_import.R`, edit line ~27:

**Replace:**
```r
clean_msg <- gsub("[\\U0001F300-\\U0001F9FF]", "", msg)
```

**With:**
```r
# Use iconv instead of regex to avoid Windows Unicode issues
clean_msg <- iconv(msg, to = "ASCII//TRANSLIT", sub = "")
clean_msg <- trimws(clean_msg)
```

Then:
```r
devtools::load_all()
```

---

## Why This Happens

1. sapfluxr uses emoji in console messages (📊, 🌲, etc.)
2. Shiny notifications can't display emoji properly
3. Old code used regex to remove emoji: `gsub("[🌀-🧿]", "", msg)`
4. Windows R doesn't support Unicode ranges in regex
5. **Fix:** Use `iconv()` instead to strip non-ASCII characters

---

## Testing the Fix

**In R Console:**
```r
# Load sapfluxr
library(sapfluxr)

# Test with a data file
data <- read_heat_pulse_data("path/to/your/data.txt")

# If this works without error, sapfluxr is fixed
```

**In Shiny:**
1. Run the app
2. Upload your data file
3. Should now work without the Unicode error

---

## If Error Persists

The error might be coming from a different place. Check the R console output when the error occurs - it will show the full call stack telling you exactly which function threw the error.

Look for:
- Which file is being read
- Which function in sapfluxr failed
- The exact line number

Then we can fix that specific location.

---

## Prevention

To avoid this in future:
1. Always use `devtools::load_all()` after editing sapfluxr
2. Use `devtools::install()` to install changes
3. Restart R session if changes don't appear
4. Check `packageVersion("sapfluxr")` to ensure correct version is loaded

---

## Still Getting the Error?

If the error persists after trying all solutions:

1. **Check R console output** when uploading - it will show the call stack
2. **Copy the full error message** including stack trace
3. **Note which data file** causes the error
4. We may need to find other places in sapfluxr using the problematic regex

The call stack will tell us exactly where to fix it.
