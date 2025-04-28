# EXTRA-EU IMPORTS

**Nowcasting EXTRA-EU IMPORTS using Trade-Weighted Indicators and TRAMO Models**

------------------------------------------------------------------------

## 🚀 About the Project

**EXTRA-EU IMPORTS** is a modular, scalable, and reproducible nowcasting pipeline built in R.\
It leverages trade-weighted macroeconomic indicators and applies advanced time series modeling (TRAMO framework) to produce timely and accurate forecasts for international trade flows.

The system is designed to be: - 🔄 **Fully Reproducible** (with `renv` environment management) - 🛠 **Modular and Configurable** (country-specific model settings via control tables) - ⚡ **Efficient and Scalable** (fast execution even on large datasets) - 🌍 **Open and Transparent** (based entirely on public data from Eurostat and OECD)

------------------------------------------------------------------------

## 🧩 Architecture Overview

The pipeline is divided into modular components: - **Environment Setup:** Clean and reproducible R session with all dependencies. - **Data Import:** Retrieve or load macroeconomic indicators (IPI, HICP, EXC_RATE). - **Data Preparation:** Rebase indices, impute missing values, and compute trade-weighted aggregates. - **Modeling:** Fit univariate and multivariate TRAMO models using RJDemetra. - **Forecasting:** Generate and export nowcasts in structured formats.

For a full architectural diagram, please refer to the documentation or `/docs/Architecture.pdf`.

------------------------------------------------------------------------

## 📦 Libraries Used

All libraries used in this project are open-source and actively maintained: - [`tidyverse`](https://cran.r-project.org/package=tidyverse) - [`RJDemetra`](https://cran.r-project.org/package=RJDemetra) - [`lubridate`](https://cran.r-project.org/package=lubridate) - [`openxlsx`](https://cran.r-project.org/package=openxlsx) - [`jsonlite`](https://cran.r-project.org/package=jsonlite) - [`zoo`](https://cran.r-project.org/package=zoo) - [`eurostat`](https://cran.r-project.org/package=eurostat) - [`rsdmx`](https://cran.r-project.org/package=rsdmx)

Environment management is handled via [`renv`](https://cran.r-project.org/package=renv).

------------------------------------------------------------------------

## 🔒 License

This project is licensed under the [MIT License](https://opensource.org/licenses/MIT).\
See the [LICENSE](LICENSE) file for more details.\
You are free to use, modify, and distribute this code, provided that the original authors are credited.

------------------------------------------------------------------------

## ✨ How to Run

Clone the repository and set up the environment:

``` bash
git clone https://github.com/yourusername/EXTRA-IMPORT.git
cd EXTRA-IMPORT
```

Open R and run:

``` r
install.packages("renv")  # Only if renv is not installed yet
renv::restore()           # Restores the project environment
source("run.R")           # Executes the complete nowcasting pipeline
```

------------------------------------------------------------------------

## 🤝 Contributions

Contributions, issues, and feature requests are welcome!\
Feel free to open an issue or submit a pull request.

------------------------------------------------------------------------

> Made with ❤️ by **TURKSTAT-DATG**.

------------------------------------------------------------------------

# 📂 Directory Structure

```         
EXTRA-IMPORT/
├── Data/
├── Results/
├── Scripts/
│   ├── global.R
│   ├── utilities.R
│   ├── data_import.R
│   ├── data_preparation.R
│   ├── tramo_model_mv1.R
│   ├── tramo_model_uv.R
│   ├── tramo_model_average.R
│   ├── tramo_model_mv2.R
│   └── export_results.R
├── renv.lock
├── run.R
├── README.md
└── LICENSE
```

------------------------------------------------------------------------

## ✅ Notes

-   2025 forecasts use trade weights based on the latest available data.
-   Future forecasts will require updating the trade weights accordingly.
