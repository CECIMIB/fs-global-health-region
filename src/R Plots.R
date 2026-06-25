# Food Security and Global Health Indicators
# =============================================================================

# --- Load libraries ---
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)
library(grid)
library(gridExtra)

# ==========================================================================
# 1. READ DATA
# ==========================================================================

# Use read_excel for .xlsx files
step_1 <- read_excel("data/step_1_region_global_health.xlsx")
step_2 <- read_excel("data/step_2_region_global_health.xlsx")
step_3 <- read_excel("data/step_3_region_global_health.xlsx")

# ==========================================================================
# 2. HELPER FUNCTIONS FOR CLEANING
# ==========================================================================

clean_indicator_names <- function(data) {
  data %>%
    mutate(
      indicator_clean = case_when(
        indicator == "CURRENT HEALTH EXPENDITURE (% OF GDP)" ~ "Health expenditure (% GDP)",
        indicator == "PHYSICIANS (PER 1,000 PEOPLE)" ~ "Physicians (per 1,000)",
        indicator == "NURSES AND MIDWIVES (PER 1,000 PEOPLE)" ~ "Nurses & midwives (per 1,000)",
        indicator == "NUMBER OF DALYS" ~ "Number of DALYs",
        indicator == "NUMBER OF DEATHS" ~ "Number of deaths",
        indicator == "DEATH RATE" ~ "Death rate",
        indicator == "HEALTHCARE ACCESS AND QUALITY" ~ "Healthcare access & quality",
        indicator == "OUT-OF-POCKED EXPENDITURE ON HEALTH" ~ "Out-of-pocket expenditure",
        indicator == "Population,ages 65+" ~ "Population ages 65+",
        indicator == "Child mortality rate" ~ "Child mortality rate",
        indicator == "Life expectancy at birth" ~ "Life expectancy at birth",
        indicator == "Sex ratio" ~ "Sex ratio",
        indicator == "Sex gap in life expectancy" ~ "Sex gap in life expectancy",
        indicator == "Healthy life expectancy" ~ "Healthy life expectancy",
        indicator == "Lifespan Inequality in wome" ~ "Lifespan inequality (women)",
        indicator == "Lifespan Inequality in man" ~ "Lifespan inequality (men)",
        TRUE ~ indicator
      )
    )
}

clean_region_names <- function(data) {
  if("region" %in% names(data)) {
    data <- data %>%
      mutate(
        region_clean = case_when(
          region == "Eastern Mediterranean" ~ "Eastern\nMediterranean",
          region == "South-East Asia" ~ "South-East\nAsia",
          region == "Western Pacific" ~ "Western\nPacific",
          TRUE ~ region
        )
      )
  }
  return(data)
}

clean_model_abbrev <- function(data) {
  data %>%
    mutate(
      model_abbrev = case_when(
        model_type == "Linear Gaussian" ~ "LG",
        model_type == "Negative binomial (log link)" ~ "NB",
        model_type == "Quasi-binomial (logit link)" ~ "QB",
        model_type == "Poisson (log link)" ~ "P",
        model_type == "LMM (Gaussian)" ~ "LMM",
        model_type == "GLMM (nbinom2, log link)" ~ "GLMM-NB",
        model_type == "GLMM (beta, logit link)" ~ "GLMM-Beta",
        TRUE ~ model_type
      )
    )
}

# ==========================================================================
# 3. SCALING FUNCTIONS (+100 Publications)
# ==========================================================================
X_SCALE <- 100

apply_main_effect_scaling <- function(data) {
  # Scales the main effects (raw estimates) to +100 publications
  # Applies to step_1, step_2, and step_3
  data %>%
    mutate(
      is_or_irr_model = effect_unit %in% c("OR", "IRR"),
      is_beta_model = effect_unit == "\u03b2",
      
      # Scale estimate
      estimate_scaled = case_when(
        is_or_irr_model & is.finite(estimate_raw) ~ exp(X_SCALE * estimate_raw),
        is_beta_model & is.finite(estimate_raw) ~ estimate_raw * X_SCALE,
        TRUE ~ estimate_raw # Fallback
      ),
      
      # Scale CI low
      ci_low_scaled = case_when(
        is_or_irr_model & is.finite(ci_low_raw) ~ exp(X_SCALE * ci_low_raw),
        is_beta_model & is.finite(ci_low_raw) ~ ci_low_raw * X_SCALE,
        TRUE ~ ci_low_raw # Fallback
      ),
      
      # Scale CI high
      ci_high_scaled = case_when(
        is_or_irr_model & is.finite(ci_high_raw) ~ exp(X_SCALE * ci_high_raw),
        is_beta_model & is.finite(ci_high_raw) ~ ci_high_raw * X_SCALE,
        TRUE ~ ci_high_raw # Fallback
      )
    )
}

apply_interaction_scaling <- function(data) {
  # Applies interaction scaling specifically for step_3
  if (!all(c("x_slope_p25_raw", "x_slope_p75_raw") %in% names(data))) {
    return(data) # Skip if interaction columns are missing
  }
  
  data %>%
    mutate(
      is_or_model = !is.na(effect_unit) & effect_unit %in% c("OR", "IRR"), # Includes IRR since logic is the same for ratios
      is_beta_model = !is.na(effect_unit) & effect_unit == "\u03b2",
      
      # --- A) Scaled interaction effect (per +100 units in X) ---
      interaction_scaled_per100 = case_when(
        is_or_model & is.finite(estimate_raw) ~ exp(X_SCALE * estimate_raw),
        is_beta_model & is.finite(estimate_raw) ~ estimate_raw * X_SCALE,
        TRUE ~ NA_real_
      ),
      interaction_scaled_per100_ci_low = case_when(
        is_or_model & is.finite(ci_low_raw) ~ exp(X_SCALE * ci_low_raw),
        is_beta_model & is.finite(ci_low_raw) ~ ci_low_raw * X_SCALE,
        TRUE ~ NA_real_
      ),
      interaction_scaled_per100_ci_high = case_when(
        is_or_model & is.finite(ci_high_raw) ~ exp(X_SCALE * ci_high_raw),
        is_beta_model & is.finite(ci_high_raw) ~ ci_high_raw * X_SCALE,
        TRUE ~ NA_real_
      ),
      
      # --- B) Simple slopes: effect of +100 X at low vs high Z ---
      slope_X_per100_at_Zp25 = case_when(
        is_or_model & is.finite(x_slope_p25_raw) ~ exp(X_SCALE * x_slope_p25_raw),
        is_beta_model & is.finite(x_slope_p25_raw) ~ x_slope_p25_raw * X_SCALE,
        TRUE ~ NA_real_
      ),
      slope_X_per100_at_Zp25_ci_low = case_when(
        is_or_model & is.finite(x_slope_p25_ci_low_raw) ~ exp(X_SCALE * x_slope_p25_ci_low_raw),
        is_beta_model & is.finite(x_slope_p25_ci_low_raw) ~ x_slope_p25_ci_low_raw * X_SCALE,
        TRUE ~ NA_real_
      ),
      slope_X_per100_at_Zp25_ci_high = case_when(
        is_or_model & is.finite(x_slope_p25_ci_high_raw) ~ exp(X_SCALE * x_slope_p25_ci_high_raw),
        is_beta_model & is.finite(x_slope_p25_ci_high_raw) ~ x_slope_p25_ci_high_raw * X_SCALE,
        TRUE ~ NA_real_
      ),
      
      slope_X_per100_at_Zp75 = case_when(
        is_or_model & is.finite(x_slope_p75_raw) ~ exp(X_SCALE * x_slope_p75_raw),
        is_beta_model & is.finite(x_slope_p75_raw) ~ x_slope_p75_raw * X_SCALE,
        TRUE ~ NA_real_
      ),
      slope_X_per100_at_Zp75_ci_low = case_when(
        is_or_model & is.finite(x_slope_p75_ci_low_raw) ~ exp(X_SCALE * x_slope_p75_ci_low_raw),
        is_beta_model & is.finite(x_slope_p75_ci_low_raw) ~ x_slope_p75_ci_low_raw * X_SCALE,
        TRUE ~ NA_real_
      ),
      slope_X_per100_at_Zp75_ci_high = case_when(
        is_or_model & is.finite(x_slope_p75_ci_high_raw) ~ exp(X_SCALE * x_slope_p75_ci_high_raw),
        is_beta_model & is.finite(x_slope_p75_ci_high_raw) ~ x_slope_p75_ci_high_raw * X_SCALE,
        TRUE ~ NA_real_
      ),
      
      # --- C) Contrast: high vs low moderator ---
      slope_diff_raw_p75_minus_p25 = if_else(
        is.finite(x_slope_p75_raw) & is.finite(x_slope_p25_raw),
        x_slope_p75_raw - x_slope_p25_raw,
        NA_real_
      ),
      ratio_or_diff_X_per100_high_vs_lowZ = case_when(
        is_or_model & is.finite(slope_diff_raw_p75_minus_p25) ~ exp(X_SCALE * slope_diff_raw_p75_minus_p25),
        is_beta_model & is.finite(slope_diff_raw_p75_minus_p25) ~ slope_diff_raw_p75_minus_p25 * X_SCALE,
        TRUE ~ NA_real_
      ),
      
      # --- Optional: ready-to-paste strings for reporting ---
      slope_X_per100_at_Zp25_text = case_when(
        is.finite(slope_X_per100_at_Zp25) ~ sprintf("Effect(+%d X) @ Zp25: %.4f [%.4f\u2013%.4f]",
                X_SCALE, slope_X_per100_at_Zp25, slope_X_per100_at_Zp25_ci_low, slope_X_per100_at_Zp25_ci_high),
        TRUE ~ NA_character_
      ),
      slope_X_per100_at_Zp75_text = case_when(
        is.finite(slope_X_per100_at_Zp75) ~ sprintf("Effect(+%d X) @ Zp75: %.4f [%.4f\u2013%.4f]",
                X_SCALE, slope_X_per100_at_Zp75, slope_X_per100_at_Zp75_ci_low, slope_X_per100_at_Zp75_ci_high),
        TRUE ~ NA_character_
      ),
      ratio_or_diff_X_per100_text = case_when(
        is.finite(ratio_or_diff_X_per100_high_vs_lowZ) ~ sprintf("Effect Contrast (+%d X): %.4f (Zp75 vs Zp25)",
                X_SCALE, ratio_or_diff_X_per100_high_vs_lowZ),
        TRUE ~ NA_character_
      )
    )
}

# ==========================================================================
# 4. PREPROCESS DATASETS
# ==========================================================================

# -- Process step 1 --
step_1 <- step_1 %>%
  mutate(
    indicator = case_when(
      dependent_var == "total_publications" ~ independent_var,
      TRUE ~ dependent_var
    ),
    indicator_role = case_when(
      dependent_var == "total_publications" ~ "IV",
      TRUE ~ "DV"
    )
  ) %>%
  clean_indicator_names() %>%
  mutate(indicator_label = paste0(indicator_clean, "  [", indicator_role, "]")) %>%
  clean_region_names() %>%
  clean_model_abbrev() %>%
  apply_main_effect_scaling()

# -- Process step 2 --
step_2 <- step_2 %>% filter(!is.na(estimate_raw)) %>%
  mutate(
    indicator = case_when(
      dependent_var == "total_publications" ~ independent_var,
      TRUE ~ dependent_var
    ),
    indicator_role = case_when(
      role_of_indicator == "independent" ~ "IV",
      TRUE ~ "DV"
    )
  ) %>%
  clean_indicator_names() %>%
  mutate(indicator_label = paste0(indicator_clean, "  [", indicator_role, "]")) %>%
  clean_region_names() %>%
  clean_model_abbrev() %>%
  mutate(
    category_label = case_when(
      category == "health_system_and_financing" ~ "Health System & Financing",
      category == "health_outcomes_and_demography" ~ "Health Outcomes & Demography",
      TRUE ~ category
    )
  ) %>%
  apply_main_effect_scaling()

# -- Process step 3 --
step_3 <- step_3 %>%
  mutate(
    indicator = case_when(
      dependent_var == "total_publications" ~ independent_var,
      TRUE ~ dependent_var
    ),
    indicator_role = case_when(
      dependent_var == "total_publications" ~ "IV",
      TRUE ~ "DV"
    )
  ) %>%
  clean_indicator_names() %>%
  mutate(indicator_label = paste0(indicator_clean, "  [", indicator_role, "]")) %>%
  clean_region_names() %>%
  clean_model_abbrev() %>%
  apply_main_effect_scaling() %>%
  apply_interaction_scaling()

# ==========================================================================
# FACTOR ORDERING (Shared)
# ==========================================================================

indicator_order_system <- c(
  "Health expenditure (% GDP)  [DV]",
  "Out-of-pocket expenditure  [DV]",
  "Healthcare access & quality  [DV]",
  "Physicians (per 1,000)  [IV]",
  "Nurses & midwives (per 1,000)  [IV]"
)

indicator_order_outcomes <- c(
  "Number of DALYs  [DV]",
  "Number of deaths  [DV]",
  "Death rate  [DV]",
  "Child mortality rate  [DV]",
  "Life expectancy at birth  [DV]",
  "Healthy life expectancy  [DV]",
  "Sex gap in life expectancy  [DV]",
  "Sex ratio  [IV]",
  "Population ages 65+  [IV]",
  "Lifespan inequality (women)  [DV]",
  "Lifespan inequality (men)  [DV]"
)

indicator_order <- c(indicator_order_system, indicator_order_outcomes)

region_order <- c("Africa", "Americas", "Eastern\nMediterranean", "Europe",
                  "South-East\nAsia", "Western\nPacific")

n_system   <- length(indicator_order_system)
n_outcomes <- length(indicator_order_outcomes)
sep_y      <- n_outcomes + 0.5


# =============================================================================
# Figure 1: Heatmap — Regional Associations (Step 1)
# =============================================================================

# Add plot-specific columns
step_1 <- step_1 %>%
  mutate(
    effect_direction = case_when(
      effect_unit == "\u03b2" & estimate_scaled > 0 ~ "Positive",
      effect_unit == "\u03b2" & estimate_scaled < 0 ~ "Negative",
      effect_unit %in% c("IRR", "OR") & estimate_scaled > 1 ~ "Positive",
      effect_unit %in% c("IRR", "OR") & estimate_scaled < 1 ~ "Negative",
      TRUE ~ "Null"
    ),
    sig_label = case_when(
      p_value_adj_holm < 0.001 ~ "***",
      p_value_adj_holm < 0.01  ~ "**",
      p_value_adj_holm < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# Normalize effect magnitude
step_1 <- step_1 %>%
  mutate(
    effect_magnitude_raw = case_when(
      effect_unit == "\u03b2"  ~ abs(estimate_raw), # Rank logic preserves order on raw
      effect_unit %in% c("IRR", "OR") ~ abs(estimate_raw),
      TRUE ~ 0
    )
  ) %>%
  group_by(effect_unit) %>%
  mutate(
    norm_magnitude = percent_rank(effect_magnitude_raw),
    norm_magnitude = ifelse(is.na(norm_magnitude), 0, norm_magnitude)
  ) %>%
  ungroup() %>%
  mutate(
    signed_norm = case_when(
      effect_direction == "Positive" ~ norm_magnitude,
      effect_direction == "Negative" ~ -norm_magnitude,
      TRUE ~ 0
    )
  )

step_1 <- step_1 %>%
  mutate(
    indicator_label = factor(indicator_label, levels = rev(indicator_order)),
    region_clean = factor(region_clean, levels = region_order)
  )

theme_pub <- theme_minimal(base_size = 11) +
  theme(
    text = element_text(family = "sans", color = "grey10"),
    plot.title = element_text(size = 13, face = "bold", hjust = 0,
                              margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9.5, color = "grey30", hjust = 0,
                                 margin = margin(b = 10)),
    plot.caption = element_text(size = 7.5, color = "grey50", hjust = 0,
                                lineheight = 1.3, margin = margin(t = 12)),
    axis.text.x = element_text(size = 9, face = "bold", color = "grey20",
                               lineheight = 0.9),
    axis.text.y = element_text(size = 9, color = "grey20"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.4),
    legend.position = "bottom",
    legend.title = element_text(size = 8.5, face = "bold"),
    legend.text = element_text(size = 7.5),
    legend.key.height = unit(0.35, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.margin = margin(t = 5),
    plot.margin = margin(t = 10, r = 120, b = 10, l = 10)
  )

p1 <- ggplot(step_1, aes(x = region_clean, y = indicator_label)) +
  geom_tile(aes(fill = signed_norm), color = "white", linewidth = 0.6) +
  geom_text(aes(label = sig_label), size = 4, fontface = "bold",
            color = "grey10", vjust = 0.1) +
  geom_text(aes(label = model_abbrev), size = 2, color = "grey45",
            vjust = 1.9, fontface = "italic") +
  scale_fill_gradientn(
    colors = c("#a50f15", "#cb181d", "#ef6548", "#fc9272", "#fee0d2",
               "#f7f7f7",
               "#deebf7", "#9ecae1", "#6baed6", "#3182bd", "#08519c"),
    values = rescale(c(-1, -0.8, -0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5, 0.8, 1)),
    limits = c(-1, 1),
    na.value = "grey90",
    name = "Effect direction & relative magnitude",
    breaks = c(-0.75, -0.25, 0, 0.25, 0.75),
    labels = c("Higher\nnegative", "Lower\nnegative", "Near\nzero", "Lower\npositive", "Higher\npositive"),
    guide = guide_colorbar(
      title.position = "top", title.hjust = 0.5, barwidth = 14, barheight = 0.5,
      frame.colour = "grey50", ticks.colour = "grey50"
    )
  ) +
  geom_hline(yintercept = sep_y, linewidth = 1.2, color = "grey30") +
  annotate("segment", x = 7.4, xend = 7.4, y = n_outcomes + 1 - 0.4, yend = n_outcomes + n_system + 0.4, color = "grey30", linewidth = 0.6) +
  annotate("segment", x = 7.2, xend = 7.4, y = n_outcomes + 1 - 0.4, yend = n_outcomes + 1 - 0.4, color = "grey30", linewidth = 0.6) +
  annotate("segment", x = 7.2, xend = 7.4, y = n_outcomes + n_system + 0.4, yend = n_outcomes + n_system + 0.4, color = "grey30", linewidth = 0.6) +
  annotate("text", x = 7.65, y = n_outcomes + n_system / 2 + 0.5, label = "Health System\n& Financing", size = 2.8, fontface = "bold.italic", color = "grey25", hjust = 0.5, lineheight = 0.9, angle = 270) +
  annotate("segment", x = 7.4, xend = 7.4, y = 0.6, yend = n_outcomes + 0.4, color = "grey30", linewidth = 0.6) +
  annotate("segment", x = 7.2, xend = 7.4, y = 0.6, yend = 0.6, color = "grey30", linewidth = 0.6) +
  annotate("segment", x = 7.2, xend = 7.4, y = n_outcomes + 0.4, yend = n_outcomes + 0.4, color = "grey30", linewidth = 0.6) +
  annotate("text", x = 7.65, y = n_outcomes / 2 + 0.5, label = "Health Outcomes\n& Demography", size = 2.8, fontface = "bold.italic", color = "grey25", hjust = 0.5, lineheight = 0.9, angle = 270) +
  scale_x_discrete(expand = c(0, 0), position = "top") +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = "off", xlim = c(0.5, 6.5)) +
  labs(
    title = "Regional Associations Between Food Security Publications and Global Health Indicators",
    subtitle = "Step 1 regression analysis across WHO regions | Scaled per +100 publications",
    caption = paste0(
      "[DV] = health indicator as dependent variable (Publications \u2192 Indicator)  |  ",
      "[IV] = health indicator as independent variable (Indicator \u2192 Publications)\n",
      "Significance: *** p < 0.001;  ** p < 0.01;  * p < 0.05 (Holm-adjusted p-values only)\n",
      "Model types: LG = Linear Gaussian;  NB = Negative binomial;  QB = Quasi-binomial;  P = Poisson\n",
      "Color: Blue = positive association, Red = negative association  |  ",
      "Intensity: rank-normalized relative magnitude within each effect-unit type (\u03b2, IRR, OR)"
    )
  ) +
  theme_pub

ggsave("Figure_1_heatmap_regional.png", p1, width = 10, height = 10, dpi = 600, bg = "white")
ggsave("Figure_1_heatmap_regional.pdf", p1, width = 10, height = 10, bg = "white")
ggsave("Figure_1_heatmap_regional.svg", p1, width = 10, height = 10, bg = "white")

cat("Figure 1 (Heatmap) saved successfully.\n")

# =============================================================================
# Figure 2: Forest Plot — Mixed-Effects Model Results (Step 2)
# =============================================================================

step_2 <- step_2 %>%
  mutate(
    sig_label = case_when(
      p_adj_holm < 0.001 ~ "***",
      p_adj_holm < 0.01  ~ "**",
      p_adj_holm < 0.05  ~ "*",
      TRUE ~ ""
    ),
    is_significant = p_adj_holm < 0.05,
    
    # We now use the scaled variants
    plot_estimate = estimate_scaled,
    plot_ci_low = ci_low_scaled,
    plot_ci_high = ci_high_scaled,
    
    ref_line = case_when(
      effect_unit == "\u03b2" ~ 0,
      TRUE ~ 1
    )
  )

cat_colors <- c("Health System & Financing" = "#d95f02",
                "Health Outcomes & Demography" = "#7570b3")

theme_forest <- theme_minimal(base_size = 11) +
  theme(
    text = element_text(family = "sans", color = "grey10"),
    axis.text.y = element_text(size = 9, color = "grey20"),
    axis.text.x = element_text(size = 8.5, color = "grey30"),
    axis.title.x = element_text(size = 9.5, color = "grey30", margin = margin(t = 6)),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.major.x = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey75", fill = NA, linewidth = 0.4),
    plot.title = element_text(size = 11, face = "bold", color = "grey15", margin = margin(b = 6)),
    legend.position = "none",
    plot.margin = margin(t = 5, r = 35, b = 5, l = 5)
  )

build_panel <- function(data, panel_title, x_label, scale_type = "linear") {
  present <- intersect(indicator_order, unique(data$indicator_label))
  data <- data %>%
    mutate(indicator_label = factor(indicator_label, levels = rev(present)))
  
  ref <- unique(data$ref_line)
  
  p <- ggplot(data, aes(x = plot_estimate, y = indicator_label)) +
    geom_vline(xintercept = ref, linetype = "dashed", color = "grey55", linewidth = 0.45) +
    geom_errorbarh(aes(xmin = plot_ci_low, xmax = plot_ci_high), color = "grey40", height = 0.3, linewidth = 0.55) +
    geom_point(aes(fill = category_label, alpha = is_significant), shape = 21, size = 3.5, stroke = 0.8, color = "grey30") +
    geom_text(aes(label = model_abbrev), x = -Inf, size = 2.3, color = "grey50", hjust = -0.1, vjust = -1.0, fontface = "italic") +
    scale_fill_manual(values = cat_colors, guide = "none") +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.45), guide = "none") +
    labs(title = panel_title, x = x_label)
  
  if (scale_type == "pseudo_log") {
    # We may need to adjust sigma because beta is now scaled * 100
    p <- p + scale_x_continuous(trans = pseudo_log_trans(sigma = 10000), labels = scales::comma)
  } else if (scale_type == "log10") {
    p <- p + scale_x_log10(labels = scales::number_format(accuracy = 0.01))
  }
  
  if (scale_type == "log10") {
    data <- data %>% mutate(sig_x_pos = plot_ci_high * 1.08)
  } else {
    x_range <- max(data$plot_ci_high, na.rm = TRUE) - min(data$plot_ci_low, na.rm = TRUE)
    data <- data %>% mutate(sig_x_pos = plot_ci_high + x_range * 0.03)
  }
  
  p <- p +
    geom_text(data = data, aes(x = sig_x_pos, label = sig_label), size = 4.5, fontface = "bold", color = "#b2182b", hjust = 0, vjust = 0.35) +
    coord_cartesian(clip = "off") +
    theme_forest
  
  return(p)
}

df_beta <- step_2 %>% filter(effect_unit == "\u03b2")
df_irr  <- step_2 %>% filter(effect_unit == "IRR")
df_or   <- step_2 %>% filter(effect_unit == "OR")

p_beta <- build_panel(df_beta, "Coefficient (\u03b2)", "\u03b2 (95% CI, scaled +100 pubs)", scale_type = "pseudo_log")
p_irr  <- build_panel(df_irr,  "Incidence Rate Ratio (IRR)", "IRR (95% CI, scaled +100 pubs)", scale_type = "log10")
p_or   <- build_panel(df_or,   "Odds Ratio (OR)", "OR (95% CI, scaled +100 pubs)", scale_type = "log10")

legend_plot <- ggplot() +
  annotate("point", x = c(1, 3.8), y = c(1, 1), shape = 21, size = 4, stroke = 0.8, color = "grey30", fill = c("#d95f02", "#7570b3")) +
  annotate("text", x = c(1.35, 4.15), y = c(1, 1), label = c("Health System & Financing", "Health Outcomes & Demography"), size = 2.8, hjust = 0, color = "grey20") +
  annotate("point", x = c(8, 9.8), y = c(1, 1), shape = 21, size = 4, stroke = 0.8, color = "grey30", fill = "grey55", alpha = c(1, 0.4)) +
  annotate("text", x = c(8.35, 10.15), y = c(1, 1), label = c("p < 0.05 (Holm)", "Not significant"), size = 2.8, hjust = 0, color = "grey20") +
  annotate("text", x = c(0.6, 7.6), y = c(1.55, 1.55), label = c("Domain", "Holm-adjusted significance"), size = 3, hjust = 0, fontface = "bold", color = "grey15") +
  scale_x_continuous(limits = c(0, 13)) +
  scale_y_continuous(limits = c(0.4, 1.8)) +
  theme_void()

title_grob <- textGrob("Mixed-Effects Model Results: Food Security Publications and Global Health Indicators", gp = gpar(fontsize = 13, fontface = "bold", col = "grey10"), hjust = 0, x = unit(0.02, "npc"))
subtitle_grob <- textGrob("Step 2 regression analysis with WHO region as random effect | Scaled per +100 publications", gp = gpar(fontsize = 9.5, col = "grey35"), hjust = 0, x = unit(0.02, "npc"))

caption_text <- paste0(
  "[DV] = health indicator as dependent variable (Publications -> Indicator)  |  ",
  "[IV] = health indicator as independent variable (Indicator -> Publications)\n",
  "Significance: *** p < 0.001;  ** p < 0.01;  * p < 0.05 (Holm-adjusted p-values only)\n",
  "Models: LMM = Linear mixed model;  GLMM-NB = Generalized LMM (neg. binomial);  GLMM-Beta = Generalized LMM (beta regression)\n",
  "Estimates and CIs are scaled to represent the effect of +100 publications.  |  Dashed line = null effect\n",
  "Non-converged models (Lifespan Inequality) excluded  |  Opacity: lighter = not significant"
)
caption_grob <- textGrob(caption_text, gp = gpar(fontsize = 7.5, col = "grey50", lineheight = 1.3), hjust = 0, x = unit(0.02, "npc"), just = "left")

h_beta <- nrow(df_beta)
h_irr  <- nrow(df_irr)
h_or   <- nrow(df_or)

final_plot <- arrangeGrob(
  title_grob, subtitle_grob, p_beta, p_irr, p_or, legend_plot, caption_grob,
  ncol = 1, heights = unit(c(0.7, 0.5, h_beta, h_irr, h_or, 1.8, 2.0), c("cm", "cm", "null", "null", "null", "cm", "cm"))
)

ggsave("Figure_2_forest_regional.png", final_plot, width = 10, height = 12, dpi = 600, bg = "white")
ggsave("Figure_2_forest_regional.pdf", final_plot, width = 10, height = 12, bg = "white")
ggsave("Figure_2_forest_regional.svg", final_plot, width = 10, height = 12, bg = "white")

cat("Figure 2 (Forest Plot) saved successfully.\n")

cat("\n=== SUMMARY OF PROCESSING ===\n")
cat("Datasets loaded: step_1, step_2, step_3\n")
cat("Scaling per +100 publications applied to all data frames (estimates and CIs).\n")
cat("Step 3 Interactions scaled successfully.\n")
