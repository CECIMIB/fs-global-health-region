# Food Security and Global Health Region Analysis

This repository contains the data and R scripts used to analyze the relationship between food security publications and various global health indicators across different WHO regions.

## Repository Structure

- `data/`: Contains the processed datasets (`step_1_region_global_health.xlsx`, `step_2_region_global_health.xlsx`, and `step_3_region_global_health.xlsx`) used for the analyses.
- `src/`: Contains the R scripts. The primary script `R Plots.R` handles data loading, preprocessing, scaling, and the generation of publication-ready visualizations (Figure 1: Heatmap and Figure 2: Forest Plot).

## Methodology & Scaling

The R script performs a scaling transformation to standardize the reporting of effects to **per +100 publications**. This ensures interpretability and replicability.

- **For linear models ($\beta$)**: The raw coefficient and 95% Confidence Intervals are multiplied by 100.
- **For ratio-based models (IRR, OR)**: The raw log-scale estimates and their confidence intervals are scaled by 100 before exponentiating: `exp(100 * estimate_raw)`.
- **Interactions (Step 3)**: Scaled simple slopes, interaction ratios, and contrast estimates are computed using the same 100-publication scaling factor.

## Visualizations

The script outputs high-quality figures in `.png`, `.pdf`, and `.svg` formats:
- **Figure 1**: A heatmap showing regional associations with Holm-adjusted significance.
- **Figure 2**: A multi-panel forest plot displaying coefficient ($\beta$), IRR, and OR estimates with 95% CIs.
