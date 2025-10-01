#!/bin/bash

echo "=== MediGuard Bulletproof Launch ==="
echo "This version uses ZERO JavaScript - guaranteed to work"
echo "Features: Overview, Query Assistant, Data Explorer"
echo ""
echo "Starting bulletproof app..."
echo "URL: http://localhost:3838"
echo "Press Ctrl+C to stop"
echo ""

Rscript -e "
cat('Loading bulletproof MediGuard app...\n')
source('app_bulletproof.R')
"