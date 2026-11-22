# Determinants of Firm Sales in Kuwait

Regression, variable selection and path analysis of the 2025 World Bank Enterprise Survey (WBES) for Kuwait.

Course project for *Statistics & Forecasting*, MSc Business Analytics, University of Sharjah (Fall 2026–2027).

## Research questions

1. Which firm characteristics are significantly associated with annual sales once the others are controlled?
2. To what extent do those associations operate indirectly through firm size?

## Data

- **Source:** World Bank Enterprise Survey, Kuwait 2025 (stratified random sample; 150 establishments, 146 after removing missing values).
- **Outcome:** annual sales in KWD (item D2), log-transformed.
- **Predictors:** 13 candidates, including employees (L1), firm age (B5), manager experience (B7), quality certification (B8), external audit (K21), R&D spending (H8) and sector (A4A). Full coding is in Table A1 of the report.
- **The raw data is not included in this repository.** Download the Kuwait 2025 microdata (reference ID KWT_2025_WBES_v01_M) from the World Bank Microdata Library: https://microdata.worldbank.org/catalog/8151 (use the "Get Microdata" tab and check the terms of use there). It is also listed on the Enterprise Surveys site: https://www.enterprisesurveys.org/en/data. Save the file as `data/Project_Data.csv`, then run the script.

## Methods

1. Descriptive statistics and Pearson correlations
2. OLS of ln(sales) on all 13 candidates with HC3 robust standard errors
3. Backward elimination by AIC (7 variables retained) and BIC (4 retained)
4. Regression diagnostics (VIF, Breusch–Pagan, Shapiro–Wilk, Cook's distance, curvature tests)
5. Path analysis in `lavaan` with firm size as mediator (2,000 bootstrap resamples)

## Key findings

- Firm size is the strongest correlate of sales: elasticity of about 1.18 (a 10% increase in employees is associated with roughly 12% higher sales).
- Holding other factors constant, certified firms report about 84% higher sales, audited firms about 105% higher and R&D firms about 53% higher. Manufacturers report about 48% lower sales than service firms.
- Most of the certification and R&D association with sales runs through firm size (55% and 71% of the total effect). The audit association is almost entirely direct.
- The final model explains 79.8% of the variance in log sales (adjusted R² = 0.787).
- Results are correlational: the data are cross-sectional and the path ordering (characteristics → size → sales) is assumed, not tested.

## Repository structure

```
.
├── README.md
├── analysis.R          # full analysis: cleaning, models, diagnostics, path analysis
├── report.pdf          # written report with tables and figures
├── data/               # place Project_Data.csv here (not tracked)
└── outputs/            # figures written by analysis.R
```

## How to run

Requires R with these packages: `dplyr`, `tidyr`, `lmtest`, `sandwich`, `car`, `lavaan`.

```r
install.packages(c("dplyr", "tidyr", "lmtest", "sandwich", "car", "lavaan"))
source("analysis.R")
```

Run it from the repository root so the relative paths resolve. A seed is set (`set.seed(123)`) so the bootstrap results are reproducible.

## Limitations

Cross-sectional design (reverse causality cannot be ruled out), self-reported sales, binary measures of certification, audit and R&D that do not capture type or intensity, and no sampling weights.

## Report

The full write-up, including literature review, tables and figures, is in [`report.pdf`](report.pdf).
