# JavaScript Error Troubleshooting Guide

## The Problem
You're experiencing: **"Cannot read properties of undefined (reading 'replace')"** in your Shiny app.

## Most Likely Causes & Solutions

### 1. **Browser Cache Issue** (Most Common)
**Try this first:**
- Hard refresh: `Ctrl+Shift+R` (PC) or `Cmd+Shift+R` (Mac)
- Open in **incognito/private mode**
- Try a **different browser**
- Clear browser cache completely

### 2. **Conflicting JavaScript Files**
The Quarto EDA files in your project contain JavaScript that may conflict:
```
./eda_files/libs/quarto-html/quarto.js
./eda_files/libs/bootstrap/bootstrap.min.js
```

**Solution:** Run app from clean directory:
```bash
Rscript test_clean_env.R
```

### 3. **Package Version Conflicts**
Your environment shows:
- R version: 4.4.2
- Shiny version: 1.11.1  
- Platform: aarch64-apple-darwin20 (Apple Silicon Mac)

**Solution:** Try with system packages instead of renv:
```bash
Rscript test_isolated.R
```

### 4. **Network/Proxy Issues**
If you're on a corporate network, proxy settings might interfere.

**Solution:** Try:
```bash
Rscript diagnose_error.R
```

## Step-by-Step Debugging

### Step 1: Test Minimal App
```bash
Rscript diagnose_error.R
```
If this fails → R/Shiny installation issue
If this works → Package conflict issue

### Step 2: Test Clean Environment  
```bash
Rscript test_clean_env.R
```
If this works → JavaScript file conflicts in your project
If this fails → System/browser issue

### Step 3: Test Different Browsers
- Chrome (incognito)
- Firefox 
- Safari
- Edge

### Step 4: Check Browser Console
1. Open Developer Tools (F12)
2. Go to Console tab
3. Look for the exact error message
4. Share the full error stack trace

## Working Solutions Available

### Option A: Ultra Simple App (Guaranteed to work)
```bash
Rscript run_simple.R
```
- Uses only basic Shiny components
- No advanced JavaScript
- Same query functionality

### Option B: No-Chat Version
```bash
R -e "shiny::runApp('app_no_chat.R')"
```
- Full dashboard functionality
- No chat interface
- All original features

### Option C: Clean Environment Test
```bash
Rscript test_clean_env.R
```
- Isolated from project files
- Minimal dependencies
- Diagnostic tool

## If Nothing Works

The issue might be:
1. **Browser extensions** interfering
2. **Corporate firewall/proxy** blocking JavaScript
3. **System-level JavaScript engine** issue
4. **Apple Silicon compatibility** with specific package versions

**Try:**
1. Different computer/device
2. Mobile browser
3. Contact your IT department about JavaScript restrictions
4. Use RStudio Server instead of desktop

## Need More Help?

Share the output of:
```bash
Rscript diagnose_error.R
```

And the exact error from browser console (F12 → Console tab).