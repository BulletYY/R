# R Projects: Macroeconomics, Financial Markets and Interactive Analytics

This repository contains three complementary projects developed in **R**. Together, they present a complete analytical workflow: from exploratory analysis of macroeconomic data, through financial-market technical analysis, to an interactive dashboard for data exploration and statistical testing.

The projects focus on practical applications of data preparation, visualization, time-series analysis and interactive reporting in economics and finance.

## Projects at a Glance

| Project | Main objective | Main file |
|---|---|---|
| **US Macroeconomic Exploratory Data Analysis** | Examine how selected economic and financial indicators behaved between 2000 and 2024, with particular attention to crisis periods. | `EDA_R.html` |
| **Apple Technical Analysis** | Analyze AAPL market data using popular technical indicators and moving-average crossover signals. | `r_quant.html` |
| **CPI and Nasdaq Shiny Dashboard** | Interactively explore the relationship between US inflation and monthly Nasdaq returns. | `SHINY APP.R` |

---

## 1. US Macroeconomic Exploratory Data Analysis

The first project is a broad exploratory analysis of the US economy and financial markets between **2000 and 2024**. It investigates how major indicators changed over time and compares their behavior across different economic environments.

The analysis covers:

- the US labor market, including unemployment and Nonfarm Payrolls,
- the Federal Funds Rate,
- CPI inflation,
- oil and wheat prices,
- Nasdaq monthly returns,
- Nasdaq trading volume,
- differences between crisis and relatively stable periods.

Several historical periods are used as analytical reference points, including the Dot-com crisis, the Global Financial Crisis, the COVID-19 pandemic and the period following the Russian invasion of Ukraine. The project emphasizes that economic variables do not react identically during every crisis and that their interpretation depends on the wider macroeconomic context.

The final report contains data preparation steps, visualizations, descriptive analysis, hypothesis-based sections and written conclusions.

### Report

Open the following file in a web browser:

```text
EDA_R.html
```

### Required input data

The report expects these files in the working directory:

```text
US_DATA.csv
CPIUS.csv
```

The analysis uses the following R packages:

```r
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(visdat)
```

---

## 2. Apple Technical Analysis

The second project applies technical-analysis methods to daily market data for **Apple Inc. (`AAPL`)**. Price data is downloaded from Yahoo Finance with the `quantmod` package, starting from January 2022.

The project calculates and visualizes:

- 20-session and 50-session Simple Moving Averages,
- 14-session Relative Strength Index,
- MACD based on 12, 26 and 9 sessions,
- daily trading volume,
- descriptive market statistics,
- automatically detected BUY and SELL signals.

Trading signals are generated from moving-average crossovers:

- a **BUY** signal appears when the 20-session average crosses above the 50-session average,
- a **SELL** signal appears when the 20-session average crosses below the 50-session average.

The rendered report combines candlestick charts, indicator panels, signal markers and summary tables, providing a compact overview of the selected asset's trend and momentum characteristics.

### Report

Open the following file in a web browser:

```text
r_quant.html
```

The analysis uses:

```r
library(quantmod)
library(TTR)
library(dplyr)
```

Because the market data is downloaded dynamically, rerunning the source analysis may produce results that differ from the rendered report.

---

## 3. CPI and Nasdaq Shiny Dashboard

The third project is an interactive **R Shiny** application designed to explore the relationship between US CPI inflation and monthly Nasdaq returns.

The application allows users to:

- filter observations by CPI values,
- filter observations by Nasdaq return values,
- select a range of years or individual years,
- browse filtered records in an interactive table,
- create line charts, scatter plots and boxplots,
- add a linear regression line,
- divide charts into yearly panels,
- inspect variable distributions with histograms,
- calculate descriptive statistics,
- measure Pearson correlation,
- perform Shapiro-Wilk and Jarque-Bera normality tests,
- export filtered data to a CSV file,
- reset all filters from the interface.

The dashboard complements the static macroeconomic report by allowing users to inspect selected relationships dynamically and test how results change after applying different filters.

### Running the application

Before launching the app, an object named `dane_makroekonomiczne` must be available in the R environment. It must include at least these columns:

| Column | Description |
|---|---|
| `DATE` | Observation date |
| `CPI_US` | US CPI inflation measure |
| `monthly_return_NQ` | Monthly Nasdaq return |
| `Year` | Calendar year |

Run the application with:

```r
source("SHINY APP.R")
```

In RStudio, the file can also be opened and launched using the **Run App** button after the required dataset has been loaded.

The application uses:

```r
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(ggplot2)
library(scales)
library(tseries)
```

---

## Repository Structure

```text
R-main/
├── EDA_R.html        # Rendered US macroeconomic EDA report
├── r_quant.html      # Rendered AAPL technical-analysis report
├── SHINY APP.R       # Interactive CPI and Nasdaq dashboard
├── README.md         # Repository documentation
└── LICENSE           # MIT License
```

## Installation

Install R and, optionally, RStudio. The packages used across all projects can be installed with:

```r
install.packages(c(
  "dplyr",
  "DT",
  "ggplot2",
  "lubridate",
  "quantmod",
  "scales",
  "shiny",
  "shinythemes",
  "tidyr",
  "tseries",
  "TTR",
  "visdat"
))
```

The two HTML reports are already rendered and can be viewed without installing R. R and the listed packages are required only to rerun the analyses or launch the Shiny application.

## License

This repository is distributed under the **MIT License**. See the [`LICENSE`](LICENSE) file for details.
