# Food Security and Global Health Region Analysis

This repository contains the datasets and R code necessary to analyze the relationship between food security publications and various global health indicators across different WHO regions and income groups.

## Repository Structure

The project is divided into two main directories:

### 1. data/ (Datasets)
Contains the raw and processed Excel files for each step of the analysis:

* `data.xlsx`: The original raw database containing bibliometric variables and all global indicators (health, economy, agriculture, etc.) at the publication/country level.
* `step_1_region_global_health.xlsx`: Aggregated results for Step 1. Contains the output of simple regression models evaluating the associations between publications and indicators across different WHO regions.
* `step_2_region_global_health.xlsx`: Aggregated results for Step 2. Contains the output of Mixed-Effects Models, where "region" is used as a random effect to capture regional variance.
* `step_3_region_global_health.xlsx`: Aggregated results for Step 3. Includes interactions and moderation analyses (e.g., simple slopes at the 25th and 75th percentiles) to understand how the effect of food security publications changes based on certain covariates.

### 2. src/ (Source Code)
Contains the R scripts that execute data cleaning, statistical analysis, and visualization:

* `Main Analysis.R`: The core analytical engine.
  * Data Cleaning: Depurates the raw database (`data.xlsx`), converting text to numeric values, removing commas, parsing percentages into proportions (0-1), and handling currency symbols.
  * Aggregation: Groups data at the country-year level and calculates population-weighted averages (`Population in year`), stratifying the analysis by Income Groups and WHO Regions.
  * Pre-analysis: Categorizes dozens of indicators into thematic clusters (e.g., Health System & Financing, Agricultural Inputs, Dietary Patterns) and establishes the role of each indicator (dependent vs. independent variable).
  * Statistical Modeling: Implements an automated model selection framework. It determines whether a variable requires a count model (Poisson, Negative Binomial), a proportion model (Quasi-binomial, GLMM Beta), or a continuous model (Linear Gaussian, LMM), and executes the regressions for steps 1, 2, and 3.

* `R Plots.R`: The visualization and post-processing script.
  * Takes the processed output from the regressions (`step_1`, `step_2`, `step_3`) and applies a standardized mathematical scaling factor (see the Scaling Methodology section below).
  * Generates Figure 1 (Heatmap depicting the direction, magnitude, and significance of regional associations).
  * Generates Figure 2 (Forest Plot depicting coefficients, IRRs, and ORs with their respective confidence intervals under mixed-effects models).
  * Exports high-resolution visualizations (`.png`, `.pdf`, `.svg`).

---

## Scaling Methodology

To ensure reproducibility and facilitate the interpretation of results in the manuscript, all effect sizes in `R Plots.R` are scaled to represent the effect of an increase of **+100 publications**.

This scaling is implemented as follows:

1. Main Effects (Linear Models):
   The raw $\beta$ coefficient and its 95% Confidence Intervals (CI) are multiplied by 100.
   `estimate_scaled = estimate_raw * 100`

2. Ratio Models (IRR, OR):
   Since the raw estimates are generated on a log-link or logit scale, they are multiplied by 100 before exponentiation.
   `estimate_scaled = exp(estimate_raw * 100)`

3. Interactions (Step 3):
   Simple slopes (the effect of +100 publications at low and high levels of a moderator) and contrast ratios are calculated by propagating the `X_SCALE <- 100` factor exactly to the logarithmic estimates and their confidence bounds.

---

## Usage

1. Clone this repository to your local machine.
2. Ensure you have installed the required R packages (`dplyr`, `ggplot2`, `readxl`, `lme4`, `glmmTMB`, `MASS`, `lmtest`, `sandwich`, etc.).
3. Execute `src/Main Analysis.R` to regenerate all regression models from the raw data.
4. Execute `src/R Plots.R` to apply the +100 publications scaling and generate publication-ready visualizations.
