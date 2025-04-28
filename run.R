
# ------------------------------------------------------------------------------------
# EXTRA-EU IMPORTS Nowcasting
#
# This script initializes the environment, prepares the data, fits nowcasting models,
# and generates forecast outputs for international trade (imports) across multiple countries.
#
# Key Outputs:
# - Nowcasted import values (monthly) using multivariate and univariate TRAMO models.
# - Trade-weighted indicators prepared for modeling (HICP, Exchange Rates, IPI).
# - Final forecast results exported in structured formats (Excel, JSON).
#
# Estimated Runtime:
# - ~3 minutes if real-time data download (current nowcast period).
# - ~10 seconds if pre-saved datasets are used (historical or future nowcast periods).
#
# Usage Instructions:
# 1. Ensure the R environment is restored using `renv::restore()`.
# 2. Run this script to execute the full nowcasting workflow.
#
# Notes:
# - The modeling is configured dynamically based on the `nowcast_period` setting.
# - Updates to trade weights are necessary for forecasts beyond 2025.
#
# Created by: TURKSTAT-DATG Team
# ------------------------------------------------------------------------------------

renv::restore()
source("scripts/main.R")

# get_nowcast values of each country. It depends on period of nowcast period.
get_nowcast(nowcast_period = "2025-03")
