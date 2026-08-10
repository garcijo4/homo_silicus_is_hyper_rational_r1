# =============================================================================
# Attention_driven_trading_Statistical_Analysis_R1.R  -  THE FULL ANALYSIS
# -----------------------------------------------------------------------------
# Comprehensive analysis script for "Homo Silicus is Hyper-Rational" (JEIC).
# Point csv_file_path (Section 0.2) at the run to analyze and
# source the whole file. For a minimal, portable reproduction of the focused
# set of manuscript statistics listed in the R/reproduce_results.R header
# (balance, Table 4, PGR/PLR, attention-depth regression, moderation
# descriptives, all factorial estimates and contrasts, focal randomization
# inference, Experiment 3 TWFE and cross-run statistics), use that script;
# statistics outside that list - the Callaway-Sant'Anna estimates, the
# Experiment 3 asset interactions and disposition ratio, the Cohort-1
# placebo, and Appendix Tables A4/A5 - come from THIS full battery
# (estimators, figures, publication tables).
#
# STRUCTURE
#   Sections 0-12   setup; power; manipulation checks; balance; parallel
#                   trends; primary CS/SA/TWFE estimates; portfolio value;
#                   hypothesis tests; CACE; robustness; persona heterogeneity;
#                   event studies; publication tables
#   Parts A-E       identification robustness (IPW, bounds, Oster);
#                   mechanism tests; heterogeneity; text evidence;
#                   transaction-cost confound tests
#   R1-A            factorial arm effects (FACTORIAL RUN ONLY: requires arm
#                   columns and the four-cohort assignment) + Social Momentum variant
#                   descriptives -> table_r1_arm_effects.csv
#   R1-B            persona behavioral fidelity checks (original data)
#                   -> table_a4_persona_fidelity.csv  (manuscript Table A4)
#   R1-C            displayed-edge alignment (original data)
#                   -> table_a5_displayed_edge.csv    (manuscript Table A5)
#
# IMPORTANT SCOPE WARNING (see also Analysis Outputs - R1/MANIFEST.md)
#   Sections 0-12 and Parts A-E assume the ORIGINAL staggered design (cohort
#   1 @ t=60, cohort 2 @ t=120, cohort 3 never-treated). When csv_file_path
#   points at the factorial (rerun_A) data, those sections apply the wrong
#   cohort mapping and their outputs are NOT valid for that run; only the
#   [R1-A] section (which builds its own arm indicators) is. Some sections
#   then fail benignly - errors from non-applicable branches do not feed any
#   reported result.
#
# INFERENCE NOTES
#   - Callaway-Sant'Anna uses a 1,000-draw bootstrap without a fixed seed:
#     point estimates are exactly reproducible, bootstrap SEs vary slightly.
#   - The factorial persona-by-period adjusted estimates, randomization
#     inference, and the stacked cross-run contrast quoted in the manuscript's
#     factorial and neutral-label results sections are produced by
#     R/reproduce_results.R (with the specification-sequence disclosure in
#     its header).
#
# Ticker note: logs always use internal names (AAPL/NVDA/AMC/GME), including
# neutral-ticker runs (only the model-visible display layer is renamed), so
# meme/blue-chip classifications remain valid for every run.
# All extended-design additions to the baseline script are marked with "[R1]".
# =============================================================================

# ==============================================================================
# ATTENTION-DRIVEN TRADING WITH LLM-BASED SYNTHETIC INVESTORS
# Statistical Analysis Script 
# ==============================================================================
#
# This script performs comprehensive statistical analysis of attention-driven
# trading behavior using LLM-based synthetic investor agents in a staggered
# difference-in-differences experimental design.
#
# Structure:
#   Section 0:  Setup & Data Preparation (consolidated)
#   Section 1:  Power Analysis
#   Section 2:  Validation — Manipulation Checks
#   Section 3:  Pre-Treatment Balance Checks
#   Section 4:  Parallel Trends Tests
#   Section 5:  Primary Analysis — Treatment Effects (CS, SA, TWFE)
#   Section 6:  Portfolio Value and Economic Magnitude
#   Section 7:  Hypothesis Tests
#   Section 8:  CACE/LATE Analysis
#   Section 9:  Robustness Checks
#   Section 10: Persona Heterogeneity Analysis
#   Section 11: Event Study Plots
#   Section 12: Publication-Ready Tables
#   Part A:     Robustness & Identification (IPW, Alt Controls, Bounds, Oster)
#   Part B:     Mechanism Tests (Attention Allocation, Dose-Response, etc.)
#   Part C:     Heterogeneity Analyses (Volatility, Signal, Triple-Diff)
#   Part D:     Qualitative Evidence (Text Analysis, Manipulation Check Quality)
#   Part E:     Transaction Cost Confound Tests
#
# ==============================================================================

# ============================================================================
# SECTION 0: SETUP & DATA PREPARATION
# ============================================================================

# --- 0.1 Load all required packages (once) ---
library(tidyverse)
library(fixest)          # Sun & Abraham estimator and TWFE
library(did)             # Callaway & Sant'Anna
library(bacondecomp)     # Bacon decomposition
library(modelsummary)
library(kableExtra)
library(ggplot2)
library(scales)
library(sandwich)        # Robust variance estimation
library(lmtest)          # Coefficient tests

# Optional packages (loaded conditionally where used)
# patchwork, clubSandwich, pandoc, ggrepel

# Set global options
options(scipen = 999)          # Disable scientific notation
options(fixest.warn_vcov_fix = FALSE)  # Suppress VCOV fix warnings
set.seed(42)                   # Reproducibility

# --- 0.2 Load raw data ---
cat("============================================================================\n")
cat("LOADING AND PREPARING DATA\n")
cat("============================================================================\n")

# [R1] Portable input selection. R/run_full_analysis.R sets ADT_CSV_FILE.
# Direct users may set that environment variable or pass a CSV path as the
# first trailing argument. The package's original run is the default.
.r1_args <- commandArgs(trailingOnly = TRUE)
.r1_script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.r1_repo_root <- if (length(.r1_script_arg)) {
  normalizePath(file.path(
    dirname(sub("^--file=", "", .r1_script_arg[1])), "..", ".."
  ), mustWork = TRUE)
} else {
  normalizePath(getwd(), mustWork = TRUE)
}
.r1_default_csv <- file.path(
  Sys.getenv("ADT_DATA_ROOT", file.path(.r1_repo_root, "data")),
  "original_run", "experiment_results_final.csv"
)
csv_file_path <- Sys.getenv("ADT_CSV_FILE", "")
if (!nzchar(csv_file_path)) {
  csv_file_path <- if (length(.r1_args) && nzchar(.r1_args[1])) {
    .r1_args[1]
  } else {
    .r1_default_csv
  }
}
if (!file.exists(csv_file_path)) {
  stop("Analysis input not found: ", csv_file_path,
       "\nSet ADT_CSV_FILE, ADT_DATA_ROOT, or pass a CSV path.")
}
cat("Analysis input: ", normalizePath(csv_file_path), "\n", sep = "")

data <- read_csv(csv_file_path,
                 col_types = cols(
                   realized_pnl  = col_double(),
                   momentum      = col_double(),
                   signal_strength = col_character(),
                   ticker        = col_character(),
                   .default      = col_guess()
                 ))

cat(sprintf("Raw data loaded: %s observations\n", format(nrow(data), big.mark = ",")))

# --- 0.3 Compute universal derived columns on full data ---
data <- data %>%
  mutate(
    # Post-treatment indicator based on cohort timing
    post_treatment = case_when(
      treatment_cohort == 1 ~ (t >= 60),
      treatment_cohort == 2 ~ (t >= 120),
      treatment_cohort == 3 ~ FALSE,
      TRUE ~ NA
    ),
    # Core treatment indicators
    treated       = treatment_cohort %in% c(1, 2),
    never_treated = (treatment_cohort == 3),
    cohort1       = (treatment_cohort == 1),
    cohort2       = (treatment_cohort == 2),
    # Cohort x Post interactions (used throughout)
    cohort1_post  = cohort1 & post_treatment,
    cohort2_post  = cohort2 & post_treatment,
    # Explicit ATT terms (identical to cohort*_post, kept for backward compat)
    cohort1_att   = cohort1 & post_treatment,
    cohort2_att   = cohort2 & post_treatment,
    # Event time
    event_time = case_when(
      treatment_cohort == 1 ~ as.integer(t - 60L),
      treatment_cohort == 2 ~ as.integer(t - 120L),
      TRUE ~ NA_integer_
    ),
    # For Callaway-Sant'Anna
    first_treat = case_when(
      treatment_cohort == 1 ~ 60L,
      treatment_cohort == 2 ~ 120L,
      treatment_cohort == 3 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # Meme stock indicator
    is_meme_stock = ticker %in% c("AMC", "GME"),
    # Volatility regime (vol increases 2.5x at t=100)
    high_vol_regime = (t >= 100)
  )

# Validate post_treatment
cat("\n=== POST_TREATMENT VALIDATION ===\n")
cat(sprintf("Total observations: %s\n", format(nrow(data), big.mark = ",")))
cat(sprintf("Non-NA post_treatment: %s\n",
            format(sum(!is.na(data$post_treatment)), big.mark = ",")))
cat(sprintf("TRUE: %s | FALSE: %s\n",
            format(sum(data$post_treatment == TRUE, na.rm = TRUE), big.mark = ","),
            format(sum(data$post_treatment == FALSE, na.rm = TRUE), big.mark = ",")))

# --- 0.4 Split by stage ---
data_attention <- data %>% filter(stage == "attention_allocation")
data_trading   <- data %>% filter(stage == "trading")

# --- 0.5 Merge manipulation_check_passed into trading data ---
manip_check <- data_attention %>%
  select(agent_id, t, manipulation_check_passed) %>%
  filter(!is.na(manipulation_check_passed))

# Remove existing all-NA column if present, then merge
if ("manipulation_check_passed" %in% names(data_trading)) {
  data_trading <- data_trading %>% select(-manipulation_check_passed)
}
data_trading <- data_trading %>%
  left_join(manip_check, by = c("agent_id", "t"))

cat("\n=== MANIPULATION CHECK MERGE ===\n")
cat(sprintf("Non-NA: %d | TRUE: %d | FALSE: %d\n",
            sum(!is.na(data_trading$manipulation_check_passed)),
            sum(data_trading$manipulation_check_passed == TRUE, na.rm = TRUE),
            sum(data_trading$manipulation_check_passed == FALSE, na.rm = TRUE)))

# Validate
if (sum(is.na(data_trading$post_treatment)) > 0) {
  stop("ERROR: post_treatment still has NA values in trading data!")
}

# --- 0.6 Add sensitivity classification (Manuscript Table 1) ---
data_trading <- data_trading %>%
  mutate(
    sensitivity_group = factor(
      case_when(
        persona %in% c("SocialMomentum", "LotterySeeker") ~ "high",
        persona %in% c("PassiveFollower", "SkepticalContrarian", "TechnicalTrader") ~ "medium",
        persona == "DividendSeeker" ~ "low",
        TRUE ~ NA_character_
      ),
      levels = c("low", "medium", "high")
    )
  )

cat("\n=== Sensitivity Group Classification ===\n")
data_trading %>%
  distinct(persona, sensitivity_group) %>%
  arrange(sensitivity_group, persona) %>%
  print()

# --- 0.7 Summary of prepared data ---
N_agents  <- n_distinct(data_trading$agent_id)
N_periods <- n_distinct(data_trading$t)
N_treated <- sum(data_trading$treated[!duplicated(data_trading$agent_id)])
N_control <- N_agents - N_treated

cat(sprintf("\nPrepared dataset:\n"))
cat(sprintf("  Total observations: %s\n", format(nrow(data_trading), big.mark = ",")))
cat(sprintf("  Agents: %d (treated: %d, control: %d)\n", N_agents, N_treated, N_control))
cat(sprintf("  Time periods: %d\n", N_periods))


# ============================================================================
# SECTION 1: POWER ANALYSIS
# ============================================================================

cat("\n============================================================================\n")
cat("SECTION 1: POWER ANALYSIS\n")
cat("============================================================================\n")

# Calculate ICC from data
icc_data <- data_trading %>%
  group_by(agent_id) %>%
  summarise(
    mean_buy = mean(buy_indicator, na.rm = TRUE),
    var_buy  = var(buy_indicator, na.rm = TRUE),
    .groups  = "drop"
  )

var_between <- var(icc_data$mean_buy, na.rm = TRUE)
var_within  <- mean(icc_data$var_buy, na.rm = TRUE)
icc         <- var_between / (var_between + var_within)

cat(sprintf("\nICC: %.2f (between=%.4f, within=%.4f)\n", icc, var_between, var_within))

design_effect       <- 1 + (N_periods - 1) * icc
effective_n         <- (N_agents * N_periods) / design_effect
effective_n_per_group <- effective_n / 2

cat(sprintf("Design Effect: %.2f | Effective N: %.1f (%.1f/group)\n",
            design_effect, effective_n, effective_n_per_group))

# Power for d = 0.30
alpha   <- 0.05
d       <- 0.30
z_alpha <- qnorm(1 - alpha/2)
se_d    <- sqrt(2 / effective_n_per_group)
z_power <- d / se_d - z_alpha
power   <- pnorm(z_power)

cat(sprintf("Power (d=0.30, alpha=0.05): %.1f%%\n", power * 100))
if (power < 0.80) cat("  Warning: Power < 80%%\n") else cat("  Power adequate\n")

# MDE at 80% power
z_80 <- qnorm(0.80)
mde  <- (z_alpha + z_80) * se_d
cat(sprintf("MDE (80%% power): d = %.2f\n", mde))


# ============================================================================
# SECTION 2: VALIDATION — MANIPULATION CHECKS
# ============================================================================

cat("\n============================================================================\n")
cat("SECTION 2: VALIDATION — MANIPULATION CHECKS\n")
cat("============================================================================\n")

overall_pass_rate <- mean(data_attention$manipulation_check_passed, na.rm = TRUE)
n_checks <- sum(!is.na(data_attention$manipulation_check_passed))

cat(sprintf("\nOverall pass rate: %.1f%% (%d / %d)\n",
            overall_pass_rate * 100,
            sum(data_attention$manipulation_check_passed, na.rm = TRUE),
            n_checks))

check_by_treatment <- data_attention %>%
  group_by(post_treatment) %>%
  summarise(
    pass_rate = mean(manipulation_check_passed, na.rm = TRUE),
    n = n(),
    n_passed = sum(manipulation_check_passed, na.rm = TRUE),
    n_failed = sum(!manipulation_check_passed, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n  By Treatment Status:\n")
print(check_by_treatment, n = Inf)

treated_pass_rate <- check_by_treatment$pass_rate[check_by_treatment$post_treatment == TRUE]
compliance_rate <- treated_pass_rate

if (treated_pass_rate < 0.70) {
  cat(sprintf("\n  Warning: Treated pass rate (%.1f%%) below 70%%\n", treated_pass_rate * 100))
} else {
  cat("\n  Treated group pass rate acceptable (>=70%)\n")
}

check_by_persona <- data_attention %>%
  group_by(persona) %>%
  summarise(pass_rate = mean(manipulation_check_passed, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(pass_rate)

cat("\n  By Persona:\n")
print(check_by_persona, n = Inf)


# ============================================================================
# SECTION 3: PRE-TREATMENT BALANCE CHECKS
# ============================================================================

cat("\n============================================================================\n")
cat("SECTION 3: PRE-TREATMENT BALANCE CHECKS\n")
cat("============================================================================\n")

pre_treatment_data <- data_trading %>%
  filter(post_treatment == FALSE) %>%
  group_by(agent_id, treatment_cohort) %>%
  summarise(
    pre_buy_rate       = mean(buy_indicator, na.rm = TRUE),
    pre_sell_rate      = mean(sell_indicator, na.rm = TRUE),
    pre_trade_rate     = mean(trade_indicator, na.rm = TRUE),
    pre_portfolio_value = mean(portfolio_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(treated = treatment_cohort %in% c(1, 2))

# Helper function for balance tests
run_balance_test <- function(data, var_name, label) {
  test <- t.test(as.formula(paste(var_name, "~ treated")), data = data)
  tibble(
    variable     = label,
    treated_mean = mean(data[[var_name]][data$treated], na.rm = TRUE),
    control_mean = mean(data[[var_name]][!data$treated], na.rm = TRUE),
    difference   = treated_mean - control_mean,
    t_stat       = test$statistic,
    p_value      = test$p.value
  )
}

balance_results <- bind_rows(
  run_balance_test(pre_treatment_data, "pre_buy_rate", "Pre-buy rate"),
  run_balance_test(pre_treatment_data, "pre_sell_rate", "Pre-sell rate"),
  run_balance_test(pre_treatment_data, "pre_trade_rate", "Pre-trade rate"),
  run_balance_test(pre_treatment_data, "pre_portfolio_value", "Pre-portfolio value")
) %>%
  mutate(
    p_adjusted = p.adjust(p_value, method = "holm"),
    balanced   = p_adjusted > 0.05
  )

cat("\nPre-Treatment Balance Tests (Holm-Bonferroni adjusted):\n")
print(balance_results, n = Inf)

if (all(balance_results$balanced)) {
  cat("\nALL BALANCE CHECKS PASSED (adjusted p > 0.05)\n")
} else {
  cat("\nWarning: Some balance checks failed\n")
  print(balance_results %>% filter(!balanced))
}

# --- Table A2: Pre-Treatment Balance ---
table_a2_overall <- balance_results %>%
  mutate(
    Variable = case_when(
      variable == "Pre-buy rate"        ~ "Buy Rate",
      variable == "Pre-sell rate"       ~ "Sell Rate",
      variable == "Pre-trade rate"      ~ "Trade Rate",
      variable == "Pre-portfolio value" ~ "Portfolio Value ($)",
      TRUE ~ variable
    ),
    `Treated Mean` = sprintf("%.3f", treated_mean),
    `Control Mean` = sprintf("%.3f", control_mean),
    Difference     = sprintf("%.3f", difference),
    `t-statistic`  = sprintf("%.2f", t_stat),
    `p-value (raw)` = sprintf("%.3f", p_value),
    `p-value (adj)` = sprintf("%.3f", p_adjusted),
    Balanced = ifelse(balanced, "Yes", "No")
  ) %>%
  select(Variable, `Treated Mean`, `Control Mean`, Difference,
         `t-statistic`, `p-value (raw)`, `p-value (adj)`, Balanced)

# Format portfolio value row specially
table_a2_overall$`Treated Mean`[4] <- sprintf("%.0f", balance_results$treated_mean[4])
table_a2_overall$`Control Mean`[4] <- sprintf("%.0f", balance_results$control_mean[4])
table_a2_overall$Difference[4]     <- sprintf("%.0f", balance_results$difference[4])

cat("\nPanel A: Overall Balance Tests\n")
print(table_a2_overall)
write_csv(table_a2_overall, "table_a2_balance_overall.csv")
cat("Saved: table_a2_balance_overall.csv\n")


# ============================================================================
# SECTION 4: PARALLEL TRENDS TESTS
# ============================================================================

cat("\n============================================================================\n")
cat("SECTION 4: PARALLEL TRENDS TESTS\n")
cat("============================================================================\n")

# Helper function for parallel trends tests
run_parallel_trends <- function(data_trading, cohort_num, comparison_cohort = 3, cutoff) {
  cat(sprintf("\nTest: Cohort %d vs Cohort %d (pre-treatment, t < %d)\n",
              cohort_num, comparison_cohort, cutoff))

  dt <- data_trading %>%
    filter(treatment_cohort %in% c(cohort_num, comparison_cohort), t < cutoff) %>%
    mutate(
      cohort_indicator = (treatment_cohort == cohort_num),
      time_trend = t
    )

  model <- feols(buy_indicator ~ cohort_indicator * time_trend | agent_id + t,
                 cluster = ~agent_id, data = dt)

  coef_name <- paste0("cohort_indicatorTRUE:time_trend")
  interaction_coef <- model$coefficients[coef_name]
  interaction_pval <- summary(model)$coeftable[coef_name, "Pr(>|t|)"]

  cat(sprintf("  Interaction: %.4f (p = %.4f) — %s\n",
              interaction_coef, interaction_pval,
              ifelse(interaction_pval > 0.05, "Passed", "FAILED")))

  list(coef = interaction_coef, pval = interaction_pval)
}

pt_test1 <- run_parallel_trends(data_trading, 1, 3, 60)
pt_test2 <- run_parallel_trends(data_trading, 2, 3, 120)


# ============================================================================
# FIGURE 1: OUTCOME TRAJECTORIES
# ============================================================================

cat("\nCreating Figure 1: Outcome trajectories...\n")

# Shared color palette
cohort_colors <- c(
  "Cohort 1 (Treated at t=60)"  = "#E41A1C",
  "Cohort 2 (Treated at t=120)" = "#377EB8",
  "Never-Treated"                = "#4DAF4A"
)

trajectory_data <- data_trading %>%
  mutate(group = case_when(
    treatment_cohort == 1 ~ "Cohort 1 (Treated at t=60)",
    treatment_cohort == 2 ~ "Cohort 2 (Treated at t=120)",
    treatment_cohort == 3 ~ "Never-Treated"
  )) %>%
  group_by(t, group) %>%
  summarise(
    mean_buy  = mean(buy_indicator, na.rm = TRUE),
    mean_sell = mean(sell_indicator, na.rm = TRUE),
    .groups = "drop"
  )

# Helper for trajectory plots
make_trajectory_plot <- function(traj_data, y_var, title, ylabel) {
  ggplot(traj_data, aes(x = t, y = .data[[y_var]], color = group, fill = group)) +
    geom_smooth(method = "loess", span = 0.15, se = TRUE, alpha = 0.2, linewidth = 1.2) +
    geom_vline(xintercept = c(60, 120), linetype = "dashed", alpha = 0.5,
               color = "gray40", linewidth = 0.8) +
    annotate("rect", xmin = 0, xmax = 60, ymin = -Inf, ymax = Inf,
             alpha = 0.03, fill = "blue") +
    scale_color_manual(values = cohort_colors) +
    scale_fill_manual(values = cohort_colors) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = title, x = "Time Period", y = ylabel, color = NULL, fill = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 12),
          panel.grid.minor = element_blank(),
          legend.text = element_text(size = 9))
}

fig1a <- make_trajectory_plot(trajectory_data, "mean_buy", "A. Buy Rate Trajectories", "Buy Rate")
fig1b <- make_trajectory_plot(trajectory_data, "mean_sell", "B. Sell Rate Trajectories", "Sell Rate")

ggsave("figure1a_buy_trajectories.png", fig1a, width = 12, height = 6, dpi = 300)
ggsave("figure1b_sell_trajectories.png", fig1b, width = 12, height = 6, dpi = 300)
cat("Saved: figure1a/1b trajectory plots\n")

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  fig1_combined <- fig1a / fig1b + plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  ggsave("figure1_outcome_trajectories.png", fig1_combined, width = 12, height = 10, dpi = 300)
  cat("Saved: figure1_outcome_trajectories.png\n")
}


# ============================================================================
# SECTION 5: PRIMARY ANALYSIS — TREATMENT EFFECTS
# ============================================================================

cat("\n============================================================================\n")
cat("SECTION 5: PRIMARY ANALYSIS — TREATMENT EFFECTS\n")
cat("============================================================================\n")

# --- 5.0 Baselines ---
baseline_data <- data_trading %>% filter(treatment_cohort == 3, t < 60)
baseline_buy_nt       <- mean(baseline_data$buy_indicator, na.rm = TRUE)
baseline_sell_nt      <- mean(baseline_data$sell_indicator, na.rm = TRUE)
baseline_trade_nt     <- mean(baseline_data$trade_indicator, na.rm = TRUE)
baseline_portfolio_nt <- mean(baseline_data$portfolio_value, na.rm = TRUE)

cat(sprintf("Baselines (Never-Treated, t < 60): Buy=%.4f, Sell=%.4f, Trade=%.4f, Portfolio=$%.2f\n",
            baseline_buy_nt, baseline_sell_nt, baseline_trade_nt, baseline_portfolio_nt))

# --- Helper: average cohort effects from a fixest model ---
avg_cohort_effect <- function(model, c1_name = "cohort1_attTRUE", c2_name = "cohort2_attTRUE") {
  b <- coef(model)
  V <- vcov(model)
  avg <- mean(c(b[c1_name], b[c2_name]), na.rm = TRUE)
  se  <- sqrt(0.25 * V[c1_name, c1_name] + 0.25 * V[c2_name, c2_name] +
                0.5 * V[c1_name, c2_name])
  list(avg = avg, se = se, c1 = b[c1_name], c2 = b[c2_name])
}


# --- 5.1 Callaway & Sant'Anna (2021) ---
cat("\n5.1 Callaway & Sant'Anna (2021) Estimator\n")
cat("--------------------------------------------------\n")

cs_works <- FALSE
cs_buy_results <- NULL
cs_sell_results <- NULL

# Prepare CS data (once, used for both outcomes)
cs_data_base <- data_trading %>%
  mutate(
    t = as.integer(t),
    agent_id_num = as.integer(factor(agent_id, levels = unique(agent_id)))
  ) %>%
  filter(!is.na(first_treat)) %>%
  distinct(agent_id_num, t, .keep_all = TRUE)

# CS: Buy indicator
tryCatch({
  cs_buy_results <- did::att_gt(
    yname = "buy_indicator", tname = "t", idname = "agent_id_num",
    gname = "first_treat", data = cs_data_base,
    control_group = "nevertreated", anticipation = 0, clustervars = "agent_id_num"
  )
  cs_works <- TRUE
  cat("CS converged for buy indicator\n")

  cs_buy_agg <- did::aggte(cs_buy_results, type = "simple")
  ov_att_buy <- cs_buy_agg$overall.att
  ov_se_buy  <- cs_buy_agg$overall.se
  z_buy <- if (is.finite(ov_se_buy) && ov_se_buy > 0) ov_att_buy / ov_se_buy else NA_real_
  p_buy <- if (is.finite(z_buy)) 2 * pnorm(-abs(z_buy)) else NA_real_
  cat(sprintf("  Overall ATT (Buy): %.4f (SE: %.4f, p = %s)\n",
              ov_att_buy, ov_se_buy, if (is.na(p_buy)) "NA" else sprintf("%.4f", p_buy)))
}, error = function(e) cat(sprintf("  CS failed for buy: %s\n", e$message)))

# CS: Sell indicator
tryCatch({
  cs_sell_results <- did::att_gt(
    yname = "sell_indicator", tname = "t", idname = "agent_id_num",
    gname = "first_treat", data = cs_data_base,
    control_group = "nevertreated", anticipation = 0, clustervars = "agent_id_num",
    allow_unbalanced_panel = TRUE, bstrap = TRUE, biters = 1000
  )
  cat("CS converged for sell indicator\n")

  cs_sell_agg <- did::aggte(cs_sell_results, type = "simple")
  ov_att_sell <- cs_sell_agg$overall.att
  ov_se_sell  <- cs_sell_agg$overall.se
  z_sell <- if (is.finite(ov_se_sell) && ov_se_sell > 0) ov_att_sell / ov_se_sell else NA_real_
  p_sell <- if (is.finite(z_sell)) 2 * pnorm(-abs(z_sell)) else NA_real_
  cat(sprintf("  Overall ATT (Sell): %.4f (SE: %.4f, p = %s)\n",
              ov_att_sell, ov_se_sell, if (is.na(p_sell)) "NA" else sprintf("%.4f", p_sell)))
}, error = function(e) cat(sprintf("  CS failed for sell: %s\n", e$message)))


# --- CS Dynamic Event-Time ---
if (cs_works && !is.null(cs_buy_results) && !is.null(cs_sell_results)) {

  cat("\n--- CS Dynamic Event-Time Analysis (Cohort 1 vs. never-treated) ---\n")
  ## The event-study figures are estimated on the Cohort 1 + never-treated
  ## subsample (single adoption date), so all event times s = -20..40 are
  ## observed for the plotted cohort and no balance_e trimming is needed.
  ## (The earlier balance_e = TRUE was coerced to the numeric horizon 1 and
  ## truncated post-treatment support at s = 1.) The pooled att_gt objects
  ## above continue to produce the simple and group aggregations.
  cs_data_c1 <- cs_data_base %>% filter(treatment_cohort %in% c(1, 3))
  cs_buy_c1 <- did::att_gt(
    yname = "buy_indicator", tname = "t", idname = "agent_id_num",
    gname = "first_treat", data = cs_data_c1,
    control_group = "nevertreated", anticipation = 0, clustervars = "agent_id_num",
    allow_unbalanced_panel = TRUE, bstrap = TRUE, biters = 1000
  )
  cs_sell_c1 <- did::att_gt(
    yname = "sell_indicator", tname = "t", idname = "agent_id_num",
    gname = "first_treat", data = cs_data_c1,
    control_group = "nevertreated", anticipation = 0, clustervars = "agent_id_num",
    allow_unbalanced_panel = TRUE, bstrap = TRUE, biters = 1000
  )
  cs_buy_dyn  <- did::aggte(cs_buy_c1,  type = "dynamic", min_e = -20, max_e = 40)
  cs_sell_dyn <- did::aggte(cs_sell_c1, type = "dynamic", min_e = -20, max_e = 40)

  # Helper for CS dynamic plots
  make_cs_dynamic_plot <- function(agg_dyn, title, outfile, scale_to_pp = TRUE) {
    att <- as.numeric(agg_dyn$att.egt)
    s   <- as.numeric(agg_dyn$egt)
    if (is.null(att) || length(att) == 0L) { cat(sprintf("  Could not create: %s\n", title)); return(NULL) }

    se <- if (!is.null(agg_dyn$cov.att.egt) && is.matrix(agg_dyn$cov.att.egt) &&
              nrow(agg_dyn$cov.att.egt) == length(att)) {
      sqrt(pmax(0, diag(agg_dyn$cov.att.egt)))
    } else if (!is.null(agg_dyn$se.egt) && length(agg_dyn$se.egt) == length(att)) {
      as.numeric(agg_dyn$se.egt)
    } else {
      rep(NA_real_, length(att))
    }

    df <- data.frame(s = s, att = att, se = se) %>%
      filter(is.finite(s), is.finite(att))
    if (nrow(df) == 0L) return(NULL)

    if (scale_to_pp) { df$att <- df$att * 100; df$se <- df$se * 100 }
    ylab <- if (scale_to_pp) "ATT (percentage points)" else "ATT (probability units)"

    p <- ggplot(df, aes(x = s, y = att)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dotted", color = "red") +
      geom_point(size = 2) +
      { if (all(is.finite(df$se)))
        geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.3) } +
      labs(title = title, x = "Event Time (periods since treatment)", y = ylab) +
      theme_minimal() + theme(plot.title = element_text(face = "bold"))

    ggsave(outfile, p, width = 10, height = 6, dpi = 300)
    cat(sprintf("  Saved: %s\n", outfile))
    p
  }

  p_cs_buy  <- make_cs_dynamic_plot(cs_buy_dyn,  "Figure 2. CS Event-Time: Buy",  "figure2_cs_event_time_buy.png")
  p_cs_sell <- make_cs_dynamic_plot(cs_sell_dyn, "CS Event-Time: Sell", "figureA4_cs_event_time_sell.png")

  # Early window summary helper
  early_window_summary <- function(agg_dyn, s_range = 0:20, scale_to_pp = TRUE) {
    s   <- as.numeric(agg_dyn$egt)
    att <- as.numeric(agg_dyn$att.egt)
    V   <- agg_dyn$cov.att.egt
    idx <- which(s %in% s_range)
    if (length(idx) == 0L) return(NULL)

    w        <- rep(1/length(idx), length(idx))
    att_mean <- sum(w * att[idx])
    var_mean <- if (is.matrix(V) && nrow(V) == length(att)) as.numeric(t(w) %*% V[idx, idx, drop=FALSE] %*% w) else NA_real_
    se_mean  <- if (is.na(var_mean)) NA_real_ else sqrt(var_mean)

    if (scale_to_pp) { att_mean <- att_mean * 100; se_mean <- if (!is.na(se_mean)) se_mean * 100 else NA_real_ }
    p_val <- if (is.na(se_mean) || se_mean == 0) NA_real_ else 2 * pnorm(-abs(att_mean / se_mean))
    list(att_pp = att_mean, se_pp = se_mean, p = p_val, n_used = length(idx))
  }

  buy_early  <- early_window_summary(cs_buy_dyn,  s_range = 0:20)
  sell_early <- early_window_summary(cs_sell_dyn, s_range = 0:20)

  if (!is.null(buy_early) && !is.null(sell_early)) {
    cat(sprintf("\nPanel B (s in [0,20]):\n"))
    cat(sprintf("  Buy:  ATT=%.2f pp, SE=%.2f, p=%.3f\n", buy_early$att_pp, buy_early$se_pp, buy_early$p))
    cat(sprintf("  Sell: ATT=%.2f pp, SE=%.2f, p=%.3f\n", sell_early$att_pp, sell_early$se_pp, sell_early$p))
  }

  # Group-by-cohort
  cat("\n--- CS Group-by-Cohort ---\n")
  cs_buy_group  <- did::aggte(cs_buy_results,  type = "group")
  cs_sell_group <- did::aggte(cs_sell_results, type = "group")
  cat("Buy:\n"); print(summary(cs_buy_group))
  cat("\nSell:\n"); print(summary(cs_sell_group))
}


# --- 5.2 Sun & Abraham (2021) ---
cat("\n5.2 Sun & Abraham (2021) Interaction-Weighted Estimator\n")
cat("--------------------------------------------------\n")

sa_works <- FALSE
sa_buy_avg  <- NA_real_
sa_sell_avg <- NA_real_

tryCatch({
  data_sa <- data_trading %>%
    mutate(treatment_cohort_timing = case_when(
      treatment_cohort == 1 ~ 60, treatment_cohort == 2 ~ 120, treatment_cohort == 3 ~ 0, TRUE ~ NA_real_))

  sa_buy_results <- feols(buy_indicator ~ sunab(treatment_cohort_timing, t) | agent_id + t,
                          cluster = ~agent_id, data = data_sa)
  sa_works <- TRUE
  cat("SA converged for buy indicator\n")

  # Extract post-treatment average
  sa_coefs <- coef(sa_buy_results)
  event_times <- as.numeric(gsub(".*::(-?[0-9]+)$", "\\1", names(sa_coefs)))
  post_idx <- which(event_times >= 0 & !is.na(event_times))
  if (length(post_idx) > 0) {
    sa_buy_avg <- mean(sa_coefs[post_idx], na.rm = TRUE)
    cat(sprintf("  Avg post-treatment (Buy): %.4f (%d coefficients)\n", sa_buy_avg, length(post_idx)))
  }

  sa_sell_results <- feols(sell_indicator ~ sunab(treatment_cohort_timing, t) | agent_id + t,
                           cluster = ~agent_id, data = data_sa)
  cat("SA converged for sell indicator\n")

  sa_coefs_sell <- coef(sa_sell_results)
  event_times_sell <- as.numeric(gsub(".*::(-?[0-9]+)$", "\\1", names(sa_coefs_sell)))
  post_idx_sell <- which(event_times_sell >= 0 & !is.na(event_times_sell))
  if (length(post_idx_sell) > 0) {
    sa_sell_avg <- mean(sa_coefs_sell[post_idx_sell], na.rm = TRUE)
    cat(sprintf("  Avg post-treatment (Sell): %.4f (%d coefficients)\n", sa_sell_avg, length(post_idx_sell)))
  }
}, error = function(e) cat(sprintf("  SA failed: %s\n", e$message)))


# --- 5.3 TWFE ---
cat("\n5.3 Two-Way Fixed Effects (TWFE)\n")
cat("--------------------------------------------------\n")

twfe_buy   <- feols(buy_indicator   ~ cohort1_att + cohort2_att | agent_id + t, cluster = ~agent_id, data = data_trading)
twfe_sell  <- feols(sell_indicator  ~ cohort1_att + cohort2_att | agent_id + t, cluster = ~agent_id, data = data_trading)
twfe_trade <- feols(trade_indicator ~ cohort1_att + cohort2_att | agent_id + t, cluster = ~agent_id, data = data_trading)

cat("\n--- Buy ---\n");   print(summary(twfe_buy))
cat("\n--- Sell ---\n");  print(summary(twfe_sell))
cat("\n--- Trade ---\n"); print(summary(twfe_trade))

twfe_buy_eff   <- avg_cohort_effect(twfe_buy)
twfe_sell_eff  <- avg_cohort_effect(twfe_sell)
twfe_trade_eff <- avg_cohort_effect(twfe_trade)
twfe_buy_avg   <- twfe_buy_eff$avg
twfe_sell_avg  <- twfe_sell_eff$avg
twfe_trade_avg <- twfe_trade_eff$avg

# Portfolio value model
portfolio_model <- feols(portfolio_value ~ cohort1_att + cohort2_att | agent_id + t,
                         cluster = ~agent_id, data = data_trading)


# --- 5.3b Bacon Decomposition ---
cat("\n--- Bacon Decomposition ---\n")

tryCatch({
  bacon_data <- data_trading %>%
    mutate(treat = as.integer(cohort1_att | cohort2_att)) %>%
    filter(!is.na(buy_indicator))

  bacon_buy <- bacon(buy_indicator ~ treat, data = bacon_data, id_var = "agent_id", time_var = "t")

  cat("\nBacon Decomposition (Buy Indicator):\n")
  print(summary(bacon_buy))

  bacon_summary <- bacon_buy %>%
    group_by(type) %>%
    summarise(avg_estimate = weighted.mean(estimate, weight), total_weight = sum(weight), .groups = "drop")
  print(bacon_summary, n = Inf)

  forbidden_weight <- bacon_summary$total_weight[bacon_summary$type == "Treated vs Already Treated"]
  if (length(forbidden_weight) > 0 && forbidden_weight > 0.20) {
    cat(sprintf("\n  Warning: Forbidden comparisons = %.1f%% of variation\n", forbidden_weight * 100))
  } else {
    cat(sprintf("\n  TWFE bias limited (%.1f%% forbidden)\n",
                if (length(forbidden_weight) > 0) forbidden_weight * 100 else 0))
  }

  bacon_plot <- ggplot(bacon_buy, aes(x = weight, y = estimate, color = type)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_hline(yintercept = weighted.mean(bacon_buy$estimate, bacon_buy$weight),
               linetype = "dashed", color = "black", linewidth = 1) +
    labs(title = "Bacon Decomposition: TWFE by Comparison Type",
         x = "Weight", y = "2x2 DD Estimate", color = "Comparison Type") +
    theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14)) +
    guides(color = guide_legend(nrow = 2))

  ggsave("bacon_decomposition_buy.png", bacon_plot, width = 10, height = 6, dpi = 300)
  cat("Saved: bacon_decomposition_buy.png\n")
}, error = function(e) cat(sprintf("  Bacon decomposition failed: %s\n", e$message)))


# --- 5.4 Wald Test: Buy vs Sell ---
cat("\n5.4 Wald Test: Buy vs Sell Effect Equality\n")
cat("--------------------------------------------------\n")

data_joint <- data_trading %>%
  select(agent_id, t, cohort1_att, cohort2_att, buy_indicator, sell_indicator) %>%
  pivot_longer(cols = c(buy_indicator, sell_indicator), names_to = "action_type", values_to = "indicator") %>%
  mutate(is_sell = (action_type == "sell_indicator"))

joint_model <- feols(indicator ~ (cohort1_att + cohort2_att) * is_sell | agent_id + t,
                     cluster = ~agent_id, data = data_joint)

name_c1_interact <- "cohort1_attTRUE:is_sellTRUE"
name_c2_interact <- "cohort2_attTRUE:is_sellTRUE"

diff_c1 <- coef(joint_model)[name_c1_interact]
diff_c2 <- coef(joint_model)[name_c2_interact]
avg_diff_buy_sell <- -1 * (diff_c1 + diff_c2) / 2

vcov_joint <- vcov(joint_model)
se_diff <- sqrt(0.25 * vcov_joint[name_c1_interact, name_c1_interact] +
                  0.25 * vcov_joint[name_c2_interact, name_c2_interact] +
                  0.5  * vcov_joint[name_c1_interact, name_c2_interact])

t_stat_wald  <- avg_diff_buy_sell / se_diff
p_val_wald   <- 2 * (1 - pnorm(abs(t_stat_wald)))
wald_stat    <- t_stat_wald^2

cat(sprintf("  Avg Diff (Buy - Sell): %.4f (SE: %.4f, p = %.4f)\n", avg_diff_buy_sell, se_diff, p_val_wald))
if (p_val_wald > 0.10) {
  cat("  Buy and sell effects are statistically INDISTINGUISHABLE\n")
} else {
  cat("  Buy and sell effects are statistically DIFFERENT\n")
}

wald_test_results <- list(coef_diff = avg_diff_buy_sell, se_diff = se_diff,
                          wald_stat = wald_stat, p_val = p_val_wald)

# --- 5.5 Effect Sizes (Cohen's d) ---
cat("\n--- Effect Sizes (Cohen's d) ---\n")
sd_buy  <- sd(data_trading$buy_indicator, na.rm = TRUE)
sd_sell <- sd(data_trading$sell_indicator, na.rm = TRUE)
cohens_d_buy  <- twfe_buy_avg / sd_buy
cohens_d_sell <- twfe_sell_avg / sd_sell

interpret_d <- function(d) {
  if (is.na(d)) "NA"
  else if (abs(d) < 0.2) "small"
  else if (abs(d) < 0.5) "small-medium"
  else if (abs(d) < 0.8) "medium"
  else "large"
}

cat(sprintf("  Buy:  d = %.3f (%s)\n", cohens_d_buy,  interpret_d(cohens_d_buy)))
cat(sprintf("  Sell: d = %.3f (%s)\n", cohens_d_sell, interpret_d(cohens_d_sell)))


# --- Appendix A3: TWFE Robustness Table ---
cat("\n--- Appendix A3: TWFE Robustness ---\n")

coef_map <- c(
  "cohort1_attTRUE"                = "Cohort 1 x Post",
  "cohort2_attTRUE"                = "Cohort 2 x Post",
  "cohort1_attTRUE:is_sellTRUE"   = "Cohort 1 x Post x Sell",
  "cohort2_attTRUE:is_sellTRUE"   = "Cohort 2 x Post x Sell"
)

tryCatch({
  modelsummary(
    list("Buy" = twfe_buy, "Sell" = twfe_sell, "Trade" = twfe_trade),
    coef_map = coef_map,
    gof_map = data.frame(raw = c("nobs", "r2_within"), clean = c("N", "R2 (within)"), fmt = c(0, 3)),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    estimate = "{estimate}{stars}", statistic = "({std.error})",
    title = "Appendix Table A3. TWFE Robustness",
    notes = c("Agent and period FE; SEs clustered by agent.",
              "Treatment at t>=60 (Cohort 1) and t>=120 (Cohort 2)."),
    output = "Appendix_A3_TWFE.docx"
  )
  cat("Saved: Appendix_A3_TWFE.docx\n")
}, error = function(e) cat(sprintf("  Table export failed: %s\n", e$message)))


# --- 5.6 Estimator Comparison ---
cat("\n5.6 Estimator Comparison\n")
cat("--------------------------------------------------\n")

estimator_comparison <- tibble(
  Estimator       = c("TWFE", "Callaway-Sant'Anna", "Sun-Abraham"),
  `Buy ATT (pp)`  = c(sprintf("%.2f", twfe_buy_avg * 100),
                       if (exists("ov_att_buy")) sprintf("%.2f", ov_att_buy * 100) else "---",
                       if (!is.na(sa_buy_avg)) sprintf("%.2f", sa_buy_avg * 100) else "---"),
  `Sell ATT (pp)` = c(sprintf("%.2f", twfe_sell_avg * 100),
                       if (exists("ov_att_sell")) sprintf("%.2f", ov_att_sell * 100) else "---",
                       if (!is.na(sa_sell_avg)) sprintf("%.2f", sa_sell_avg * 100) else "---")
)
print(estimator_comparison)
write_csv(estimator_comparison, "estimator_comparison.csv")
cat("Saved: estimator_comparison.csv\n")


# ============================================================================
# SECTION 6: PORTFOLIO VALUE AND ECONOMIC MAGNITUDE
# ============================================================================

cat("\n============================================================================\n")
cat("SECTION 6: PORTFOLIO VALUE & ECONOMIC MAGNITUDE\n")
cat("============================================================================\n")

portfolio_eff <- avg_cohort_effect(portfolio_model)

cat(sprintf("  Baseline (never-treated, pre): $%.2f\n", baseline_portfolio_nt))
cat(sprintf("  Cohort 1: $%.2f (%.2f%%)\n", portfolio_eff$c1, portfolio_eff$c1 / baseline_portfolio_nt * 100))
cat(sprintf("  Cohort 2: $%.2f (%.2f%%)\n", portfolio_eff$c2, portfolio_eff$c2 / baseline_portfolio_nt * 100))
cat(sprintf("  Average:  $%.2f (%.2f%%)\n", portfolio_eff$avg, portfolio_eff$avg / baseline_portfolio_nt * 100))

# Transaction costs analysis
cat("\n--- Transaction Costs ---\n")

if ("trade_fee" %in% names(data_trading)) {
  tc_summary <- data_trading %>%
    filter(!is.na(trade_fee)) %>%
    group_by(agent_id, treated, post_treatment) %>%
    summarise(total_tc = sum(trade_fee, na.rm = TRUE),
              n_trades = sum(trade_indicator, na.rm = TRUE), .groups = "drop")

  tc_comparison <- tc_summary %>%
    filter(post_treatment == TRUE) %>%
    group_by(treated) %>%
    summarise(mean_total_tc = mean(total_tc, na.rm = TRUE),
              mean_n_trades = mean(n_trades, na.rm = TRUE), .groups = "drop")
  print(tc_comparison)

  if (nrow(tc_comparison) == 2) {
    tc_diff <- tc_comparison$mean_total_tc[tc_comparison$treated == TRUE] -
      tc_comparison$mean_total_tc[tc_comparison$treated == FALSE]
    cat(sprintf("Treated agents paid $%.2f MORE in transaction costs\n", tc_diff))
  }
}

# Economic magnitude
cat("\n--- Economic Magnitude ---\n")

avg_trade_increase  <- (twfe_buy_avg + twfe_sell_avg) / 2
round_trips_per_100 <- avg_trade_increase * 100

relative_buy_increase  <- twfe_buy_avg / baseline_buy_nt * 100
relative_sell_increase <- twfe_sell_avg / baseline_sell_nt * 100

cat(sprintf("  Buy +%.2f pp (%.1f%% relative) | Sell +%.2f pp (%.1f%% relative)\n",
            twfe_buy_avg * 100, relative_buy_increase, twfe_sell_avg * 100, relative_sell_increase))
cat(sprintf("  Additional round-trips per 100 periods: %.1f\n", round_trips_per_100))

# Transaction cost drag
tc_per_roundtrip <- 0.0010
cohort1_post_periods <- 252 - 60
cohort2_post_periods <- 252 - 120
cohort1_extra_trades <- round_trips_per_100 * (cohort1_post_periods / 100)
cohort2_extra_trades <- round_trips_per_100 * (cohort2_post_periods / 100)

n_c1 <- sum(data_trading$treatment_cohort == 1 & !duplicated(data_trading$agent_id))
n_c2 <- sum(data_trading$treatment_cohort == 2 & !duplicated(data_trading$agent_id))
avg_cost_drag <- (cohort1_extra_trades * tc_per_roundtrip * 10000 * n_c1 +
                    cohort2_extra_trades * tc_per_roundtrip * 10000 * n_c2) / (n_c1 + n_c2)

cat(sprintf("  Weighted avg cost drag: %.0f bps\n", avg_cost_drag))


# ============================================================================
# SECTION 7: HYPOTHESIS TESTS
# ============================================================================

cat("\n============================================================================\n")
cat("SECTION 7: HYPOTHESIS TESTS\n")
cat("============================================================================\n")

# --- H1: Attention effects under asymmetric constraints ---
cat("\n7.1 H1: Attention Effects Under Asymmetric Constraints\n")
cat("--------------------------------------------------\n")

hypothesis_pvals <- c(
  "H1_buy"      = summary(twfe_buy)$coeftable["cohort1_attTRUE", "Pr(>|t|)"],
  "H1_sell"     = summary(twfe_sell)$coeftable["cohort1_attTRUE", "Pr(>|t|)"],
  "H1_equality" = p_val_wald
)
hypothesis_adjusted <- p.adjust(hypothesis_pvals, method = "holm")

cat(sprintf("  Buy:  %.2f pp (adj p = %.4f)\n", twfe_buy_avg * 100, hypothesis_adjusted["H1_buy"]))
cat(sprintf("  Sell: %.2f pp (adj p = %.4f)\n", twfe_sell_avg * 100, hypothesis_adjusted["H1_sell"]))
cat(sprintf("  Wald: p = %.4f (adj p = %.4f)\n", p_val_wald, hypothesis_adjusted["H1_equality"]))

if (abs(twfe_buy_avg - twfe_sell_avg) / max(twfe_buy_avg, twfe_sell_avg) < 0.30 && p_val_wald > 0.10) {
  cat("  H1 EXTENDED: Symmetric response under ASYMMETRIC constraints\n")
  cat("  Attention operates through PORTFOLIO SALIENCE (bilateral re-evaluation)\n")
} else {
  cat("  Results consistent with asymmetric attention effects\n")
}

# --- H2: Return Chasing ---
cat("\n7.2 H2: Return Chasing\n")
cat("--------------------------------------------------\n")

chase_test <- NULL

if ("momentum" %in% names(data_trading)) {

  buys_data <- data_trading %>% filter(buy_indicator == 1, !is.na(momentum))
  cat(sprintf("Buy transactions with momentum data: %d\n", nrow(buys_data)))

  # Quintile analysis
  chase_analysis <- buys_data %>%
    group_by(t) %>%
    mutate(momentum_quintile = ntile(momentum, 5), is_top_quintile = (momentum_quintile == 5)) %>%
    ungroup() %>%
    group_by(treated, post_treatment) %>%
    summarise(
      n_buys = n(),
      mean_momentum = mean(momentum, na.rm = TRUE),
      n_chase_quintile = sum(is_top_quintile, na.rm = TRUE),
      chase_rate_quintile = mean(is_top_quintile, na.rm = TRUE),
      pct_positive_momentum = mean(momentum > 0, na.rm = TRUE),
      pct_strong_signal = mean(signal_strength == "strong", na.rm = TRUE),
      .groups = "drop"
    )

  cat("\nReturn Chasing Summary:\n")
  print(chase_analysis, n = Inf, width = Inf)

  # t-test on momentum
  treated_buys <- buys_data %>% filter(post_treatment == TRUE, treated == TRUE)
  control_buys <- buys_data %>% filter(post_treatment == TRUE, treated == FALSE)

  if (nrow(treated_buys) > 10 && nrow(control_buys) > 10) {
    momentum_ttest <- t.test(treated_buys$momentum, control_buys$momentum)
    cat(sprintf("\nMomentum t-test: diff=%.4f, t=%.2f, p=%.4f\n",
                momentum_ttest$estimate[1] - momentum_ttest$estimate[2],
                momentum_ttest$statistic, momentum_ttest$p.value))

    # Proportion test on top-quintile
    chase_post <- chase_analysis %>% filter(post_treatment == TRUE)
    if (nrow(chase_post) >= 2 && all(chase_post$n_buys > 0)) {
      chase_test <- prop.test(x = chase_post$n_chase_quintile, n = chase_post$n_buys)
      treated_chase <- chase_post$chase_rate_quintile[chase_post$treated == TRUE]
      control_chase <- chase_post$chase_rate_quintile[chase_post$treated == FALSE]
      cat(sprintf("Top-quintile: treated=%.1f%%, control=%.1f%%, p=%.4f\n",
                  treated_chase * 100, control_chase * 100, chase_test$p.value))
    }
  }

  # By signal strength
  cat("\n--- Buy Distribution by Signal Strength ---\n")
  buys_data %>%
    filter(post_treatment == TRUE) %>%
    group_by(treated, signal_strength) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(treated) %>%
    mutate(pct = n / sum(n) * 100) %>%
    print(n = Inf)

} else {
  cat("  Momentum column not found — skipping\n")
}


# --- H3: Overconfidence / Deep Attention Underperformance ---
cat("\n7.3 H3: Deep Attention Underperformance\n")
cat("--------------------------------------------------\n")

if ("portfolio_value" %in% names(data_trading)) {

  data_perf <- data_trading %>%
    arrange(agent_id, t) %>%
    group_by(agent_id) %>%
    mutate(forward_return = (lead(portfolio_value) - portfolio_value) / portfolio_value) %>%
    ungroup()

  perf_simple <- data_perf %>%
    filter(!is.na(forward_return), trade_indicator == 1, t >= 60) %>%
    mutate(comparison_group = case_when(
      treatment_cohort == 1 & t >= 60  ~ "Treated (Post)",
      treatment_cohort == 2 & t >= 120 ~ "Treated (Post)",
      treatment_cohort == 3            ~ "Never-Treated Control",
      TRUE ~ "Excluded"
    )) %>%
    filter(comparison_group %in% c("Treated (Post)", "Never-Treated Control")) %>%
    group_by(comparison_group) %>%
    summarise(
      mean_return_bps = mean(forward_return, na.rm = TRUE) * 10000,
      se_return_bps   = sd(forward_return, na.rm = TRUE) / sqrt(n()) * 10000,
      n_trades = n(), .groups = "drop"
    )

  cat("\nTrade Performance (Treated vs Control):\n")
  print(perf_simple, width = Inf)

  if (nrow(perf_simple) == 2) {
    treated_perf <- perf_simple$mean_return_bps[perf_simple$comparison_group == "Treated (Post)"]
    control_perf <- perf_simple$mean_return_bps[perf_simple$comparison_group == "Never-Treated Control"]
    cat(sprintf("Difference: %.1f bps\n", treated_perf - control_perf))

    treated_rets <- data_perf %>%
      filter(!is.na(forward_return), trade_indicator == 1,
             (treatment_cohort == 1 & t >= 60) | (treatment_cohort == 2 & t >= 120)) %>%
      pull(forward_return)
    control_rets <- data_perf %>%
      filter(!is.na(forward_return), trade_indicator == 1, treatment_cohort == 3, t >= 60) %>%
      pull(forward_return)

    if (length(treated_rets) > 10 && length(control_rets) > 10) {
      perf_test <- t.test(treated_rets, control_rets)
      cat(sprintf("  t=%.2f, p=%.4f\n", perf_test$statistic, perf_test$p.value))
    }
  }

  # By attention level
  if ("attention_allocated" %in% names(data_perf)) {
    cat("\n--- Performance by Attention Level ---\n")
    data_perf %>%
      filter(!is.na(forward_return), trade_indicator == 1, t >= 60, !is.na(attention_allocated)) %>%
      group_by(attention_allocated) %>%
      summarise(mean_return_bps = mean(forward_return, na.rm = TRUE) * 10000,
                n_trades = n(), .groups = "drop") %>%
      print()
  }

  # By stock type
  if ("ticker" %in% names(data_perf)) {
    cat("\n--- Performance by Stock Type ---\n")
    data_perf %>%
      filter(!is.na(forward_return), trade_indicator == 1, t >= 60, !is.na(ticker)) %>%
      mutate(stock_type = ifelse(ticker %in% c("AMC", "GME"), "Meme Stock", "Blue Chip")) %>%
      group_by(stock_type, treated) %>%
      summarise(mean_return_bps = mean(forward_return, na.rm = TRUE) * 10000,
                n_trades = n(), .groups = "drop") %>%
      print(width = Inf)
  }
}


# --- H4: Disposition Effect ---
cat("\n7.4 H4: Disposition Effect Independence from Attention\n")
cat("--------------------------------------------------\n")

disposition_data <- data_trading %>%
  filter(action == "sell", !is.na(realized_pnl), realized_pnl != 0) %>%
  mutate(is_winner = realized_pnl > 0, is_loser = realized_pnl < 0)

n_sells <- nrow(disposition_data)
cat(sprintf("Sells with valid P&L: %d\n", n_sells))

pct_winners <- NA_real_

if (n_sells > 0) {
  n_winners_sold <- sum(disposition_data$is_winner, na.rm = TRUE)
  pct_winners    <- n_winners_sold / n_sells

  cat(sprintf("  Winners: %d (%.1f%%) | Losers: %d (%.1f%%)\n",
              n_winners_sold, pct_winners * 100, n_sells - n_winners_sold, (1 - pct_winners) * 100))
  cat(sprintf("  Mean P&L: all=$%.2f, winners=$%.2f, losers=$%.2f\n",
              mean(disposition_data$realized_pnl, na.rm = TRUE),
              mean(disposition_data$realized_pnl[disposition_data$is_winner], na.rm = TRUE),
              mean(disposition_data$realized_pnl[disposition_data$is_loser], na.rm = TRUE)))

  binom_test <- binom.test(n_winners_sold, n_sells, p = 0.5)
  cat(sprintf("  Binomial test vs 50%%: p=%.4f\n", binom_test$p.value))

  # Treatment moderation
  if (n_sells > 100) {
    cat("\n--- Treatment Moderation ---\n")
    disp_comp <- disposition_data %>%
      filter(t >= 60) %>%
      mutate(comparison_group = case_when(
        treatment_cohort == 1 & t >= 60  ~ "Treated (Post)",
        treatment_cohort == 2 & t >= 120 ~ "Treated (Post)",
        treatment_cohort == 3            ~ "Never-Treated Control",
        TRUE ~ "Excluded"
      )) %>%
      filter(comparison_group %in% c("Treated (Post)", "Never-Treated Control"))

    disp_props <- disp_comp %>%
      group_by(comparison_group) %>%
      summarise(n = n(), winners_sold = sum(is_winner, na.rm = TRUE),
                pct_winners = mean(is_winner, na.rm = TRUE), .groups = "drop")
    print(disp_props, width = Inf)

    if (nrow(disp_props) == 2 && all(disp_props$n > 10)) {
      prop_test <- prop.test(x = disp_props$winners_sold, n = disp_props$n)
      cat(sprintf("  Proportion test: p=%.4f\n", prop_test$p.value))
      if (prop_test$p.value > 0.10) cat("  H4 SUPPORTED: Attention does not moderate disposition\n")
    }
  }

  # By persona
  cat("\n--- Disposition by Persona ---\n")
  disposition_data %>%
    group_by(persona) %>%
    summarise(n_sells = n(), pct_winners = mean(is_winner, na.rm = TRUE),
              mean_winner_pnl = mean(realized_pnl[is_winner], na.rm = TRUE),
              mean_loser_pnl  = mean(realized_pnl[is_loser], na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(pct_winners)) %>%
    print(width = Inf)
}


# ============================================================================
# SECTION 8: CACE/LATE ANALYSIS
# ============================================================================

cat("\n============================================================================\n")
cat("SECTION 8: CACE/LATE ANALYSIS\n")
cat("============================================================================\n")

data_iv <- data_trading %>%
  mutate(
    Z = as.numeric(post_treatment & treated),
    A = as.numeric(post_treatment & treated & coalesce(manipulation_check_passed, FALSE))
  )

compliance_rate_iv <- mean(data_iv$A[data_iv$Z == 1], na.rm = TRUE)
cat(sprintf("Compliance Rate: %.1f%%\n", compliance_rate_iv * 100))

if (compliance_rate_iv > 0.999) {
  cat("  Perfect compliance — CACE = ITT\n")
  first_stage_coef <- 1.0
  first_stage_F    <- Inf
} else {
  first_stage <- feols(A ~ Z | agent_id + t, cluster = ~agent_id, data = data_iv)
  first_stage_coef <- coef(first_stage)["Z"]
  first_stage_F    <- fitstat(first_stage, "f")$stat
}

cat(sprintf("  First stage: coef=%.4f, F=%s\n",
            first_stage_coef,
            ifelse(is.infinite(first_stage_F), "Inf", sprintf("%.1f", first_stage_F))))

cat(sprintf("  ITT (buy): %.4f\n", twfe_buy_avg))
cat(sprintf("  CACE = ITT/compliance: %.4f (%.2f pp)\n",
            twfe_buy_avg / compliance_rate_iv, twfe_buy_avg / compliance_rate_iv * 100))


# ============================================================================
# SECTION 9: ROBUSTNESS CHECKS
# ============================================================================

cat("\n============================================================================\n")
cat("SECTION 9: ROBUSTNESS CHECKS\n")
cat("============================================================================\n")

# --- 9.1 Manipulation Check Passers Only ---
cat("\n9.1 Restrict to Manipulation Check Passers\n")

passers <- data %>%
  filter(stage == "attention_allocation") %>%
  group_by(agent_id) %>%
  summarise(pass_rate = mean(manipulation_check_passed, na.rm = TRUE), .groups = "drop") %>%
  filter(pass_rate >= 0.70) %>%
  pull(agent_id)

cat(sprintf("  Agents with >=70%% pass rate: %d / %d (%.1f%%)\n",
            length(passers), N_agents, length(passers) / N_agents * 100))

data_passers <- data_trading %>% filter(agent_id %in% passers)

robust_buy  <- feols(buy_indicator  ~ cohort1_att + cohort2_att | agent_id + t, cluster = ~agent_id, data = data_passers)
robust_sell <- feols(sell_indicator ~ cohort1_att + cohort2_att | agent_id + t, cluster = ~agent_id, data = data_passers)

robust_buy_avg  <- avg_cohort_effect(robust_buy)$avg
robust_sell_avg <- avg_cohort_effect(robust_sell)$avg

cat(sprintf("  Buy:  %.4f (vs %.4f full sample)\n", robust_buy_avg, twfe_buy_avg))
cat(sprintf("  Sell: %.4f (vs %.4f full sample)\n", robust_sell_avg, twfe_sell_avg))

# --- 9.2 Two-Way Clustering ---
cat("\n9.2 Two-Way Clustering\n")

twoway_buy  <- feols(buy_indicator  ~ cohort1_att + cohort2_att | agent_id + t, cluster = ~agent_id + t, data = data_trading)
twoway_sell <- feols(sell_indicator ~ cohort1_att + cohort2_att | agent_id + t, cluster = ~agent_id + t, data = data_trading)
print(twoway_buy)
print(twoway_sell)

# --- 9.3 Spillover Effects ---
cat("\n9.3 Spillover (Meme vs Blue Chip)\n")

sym_col <- if ("ticker" %in% names(data_trading)) "ticker" else if ("symbol" %in% names(data_trading)) "symbol" else NA
stopifnot(!is.na(sym_col))

meme_tickers <- c("AMC", "GME")
blue_tickers <- c("AAPL", "NVDA")

data_spillover <- data_trading %>%
  mutate(
    sym = .data[[sym_col]],
    is_meme_buy = as.integer(buy_indicator == 1 & !is.na(sym) & sym %in% meme_tickers),
    is_blue_buy = as.integer(buy_indicator == 1 & !is.na(sym) & sym %in% blue_tickers),
    c1 = as.integer(cohort1_att), c2 = as.integer(cohort2_att)
  )

data_long <- data_spillover %>%
  mutate(obs_id = row_number()) %>%
  select(obs_id, agent_id, t, c1, c2, is_meme_buy, is_blue_buy) %>%
  pivot_longer(cols = c(is_meme_buy, is_blue_buy), names_to = "asset_class", values_to = "y") %>%
  mutate(asset_class = ifelse(asset_class == "is_meme_buy", "meme", "blue"),
         blue = as.integer(asset_class == "blue"))

spill_joint <- feols(y ~ c1 + c2 + c1:blue + c2:blue | agent_id + t + asset_class,
                     cluster = ~agent_id, data = data_long)

cat("\nJoint Spillover Model:\n")
print(summary(spill_joint))

# Linear combination helper
lincomb <- function(model, terms, weights, G) {
  b <- coef(model); V <- vcov(model)
  stopifnot(all(terms %in% names(b)))
  est  <- sum(weights * b[terms])
  se   <- sqrt(as.numeric(t(weights) %*% V[terms, terms, drop = FALSE] %*% weights))
  tval <- est / se
  list(est = est, se = se, t = tval, p = 2 * pt(-abs(tval), df = G - 1))
}

G <- n_distinct(data_long$agent_id)
meme_res <- lincomb(spill_joint, c("c1", "c2"), c(0.5, 0.5), G)
blue_res <- lincomb(spill_joint, c("c1", "c2", "c1:blue", "c2:blue"), c(0.5, 0.5, 0.5, 0.5), G)
diff_res <- lincomb(spill_joint, c("c1:blue", "c2:blue"), c(0.5, 0.5), G)

cat(sprintf("\nMeme:  %.3f pp (SE=%.3f, p=%.4f)\n", 100*meme_res$est, 100*meme_res$se, meme_res$p))
cat(sprintf("Blue:  %.3f pp (SE=%.3f, p=%.4f)\n", 100*blue_res$est, 100*blue_res$se, blue_res$p))
cat(sprintf("Diff:  %.3f pp (SE=%.3f, p=%.4f)\n", 100*diff_res$est, 100*diff_res$se, diff_res$p))


# --- 9.4 Distant Post-Period Attenuation ---
cat("\n9.4 Distant Post-Period Attenuation (s >= 90)\n")

data_distant <- data_trading %>%
  filter(
    treatment_cohort == 3 |
      (treatment_cohort == 1 & (t < 60 | event_time >= 90)) |
      (treatment_cohort == 2 & (t < 120 | event_time >= 90))
  )

distant_model <- feols(buy_indicator ~ cohort1_att + cohort2_att | agent_id + t,
                        cluster = ~agent_id, data = data_distant)

cat("\nDistant Effect Model (excluding s < 90):\n")
print(summary(distant_model))
distant_effect <- avg_cohort_effect(distant_model)$avg
cat(sprintf("  Overall ATT: %.4f | Distant ATT: %.4f\n", twfe_buy_avg, distant_effect))
cat(ifelse(distant_effect < twfe_buy_avg * 0.75,
           "  Attenuation detected: Effects fade\n",
           "  Sustained effect: Persists long-term\n"))


# --- 9.5 Pre-Period Placebo ---
cat("\n9.5 Pre-Period Placebo Test\n")

data_placebo <- data_trading %>%
  mutate(placebo_post = case_when(
    treatment_cohort == 1 & t >= 30 & t < 60 ~ TRUE,
    treatment_cohort == 2 & t >= 90 & t < 120 ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(t < 60 | treatment_cohort == 3)

placebo_model <- feols(buy_indicator ~ placebo_post | agent_id + t, cluster = ~agent_id, data = data_placebo)
placebo_coef <- placebo_model$coefficients["placebo_postTRUE"]
placebo_pval <- summary(placebo_model)$coeftable["placebo_postTRUE", "Pr(>|t|)"]
cat(sprintf("  Placebo: %.4f (p=%.4f) — %s\n", placebo_coef, placebo_pval,
            ifelse(placebo_pval > 0.10, "Passed", "WARNING")))


# --- 9.6 CR2 Bias Correction ---
cat("\n9.6 CR2 Bias-Corrected Inference\n")

if (requireNamespace("clubSandwich", quietly = TRUE)) {
  library(clubSandwich)
  coef_test_res <- coef_test(twfe_buy, vcov = "CR2", cluster = data_trading$agent_id, test = "Satterthwaite")
  target_param <- "cohort1_attTRUE"
  res <- coef_test_res[rownames(coef_test_res) == target_param, ]
  if (nrow(res) > 0) {
    ci_lower <- res$beta - qt(0.975, res$df) * res$SE
    ci_upper <- res$beta + qt(0.975, res$df) * res$SE
    cat(sprintf("  CR2 p-value: %.4f | 95%% CI: [%.4f, %.4f]\n", res$p_val, ci_lower, ci_upper))
  }
} else {
  cat("  clubSandwich not installed\n")
}


# ============================================================================
# SECTION 10: PERSONA HETEROGENEITY
# ============================================================================

cat("\n============================================================================\n")
cat("SECTION 10: PERSONA HETEROGENEITY\n")
cat("============================================================================\n")

# Helper for subgroup analysis
run_subgroup_twfe <- function(subset_data, label) {
  tryCatch({
    model <- feols(buy_indicator ~ cohort1_att + cohort2_att | agent_id + t,
                   cluster = ~agent_id, data = subset_data)
    eff <- avg_cohort_effect(model)
    n_ag <- n_distinct(subset_data$agent_id)
    t_stat <- eff$avg / eff$se
    p_val  <- 2 * (1 - pt(abs(t_stat), df = max(n_ag - 1, 1)))
    tibble(
      group = label, coef = eff$avg, coef_pp = eff$avg * 100,
      se = eff$se, se_pp = eff$se * 100, p_value = p_val,
      ci_lower = (eff$avg - 1.96 * eff$se) * 100,
      ci_upper = (eff$avg + 1.96 * eff$se) * 100,
      n_agents = n_ag, n_obs = nrow(subset_data)
    )
  }, error = function(e) { cat(sprintf("  Failed for %s: %s\n", label, e$message)); NULL })
}

# Persona-level effects
persona_effects <- bind_rows(lapply(unique(data_trading$persona), function(p) {
  run_subgroup_twfe(data_trading %>% filter(persona == p), p)
})) %>% arrange(desc(coef_pp))

cat("\nPersona-Specific Effects (Buy):\n")
print(persona_effects %>% select(group, coef_pp, se_pp, p_value, ci_lower, ci_upper, n_agents), n = Inf)

# Persona plot
persona_plot <- ggplot(persona_effects, aes(x = reorder(group, coef_pp), y = coef_pp)) +
  geom_point(size = 4, color = "steelblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = twfe_buy_avg * 100, linetype = "dotted", color = "gray50") +
  coord_flip() +
  labs(title = "Treatment Effects by Investor Persona (Buy)",
       subtitle = sprintf("Overall: %.2f pp (dotted)", twfe_buy_avg * 100),
       x = "Persona", y = "ATT (pp)", caption = "95% CIs") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", size = 12))

ggsave("figure_persona_heterogeneity.png", persona_plot, width = 10, height = 6, dpi = 300)
write_csv(persona_effects, "table4_persona_heterogeneity.csv")
cat("Saved: persona plot and table\n")

# Sensitivity group analysis
cat("\n--- Sensitivity Group Analysis ---\n")

sensitivity_table <- bind_rows(lapply(c("low", "medium", "high"), function(sg) {
  subset <- data_trading %>% filter(sensitivity_group == sg)
  if (nrow(subset) > 100) run_subgroup_twfe(subset, sg) else NULL
}))

if (nrow(sensitivity_table) > 0) {
  print(sensitivity_table)

  # High vs low test
  high_row <- sensitivity_table %>% filter(group == "high")
  low_row  <- sensitivity_table %>% filter(group == "low")
  if (nrow(high_row) > 0 && nrow(low_row) > 0) {
    diff    <- high_row$coef_pp - low_row$coef_pp
    se_diff <- sqrt(high_row$se_pp^2 + low_row$se_pp^2)
    cat(sprintf("\nHigh vs Low: %.2f pp (SE=%.2f, z=%.2f, p=%.4f)\n",
                diff, se_diff, diff/se_diff, 2 * pnorm(-abs(diff/se_diff))))
  }
}


# ============================================================================
# SECTION 11: EVENT STUDY PLOTS
# ============================================================================

cat("\n============================================================================\n")
cat("SECTION 11: EVENT STUDY PLOTS\n")
cat("============================================================================\n")

event_data <- data_trading %>%
  filter(treatment_cohort %in% c(1, 3)) %>%
  mutate(event_time_c1 = t - 60, treated_c1 = (treatment_cohort == 1)) %>%
  filter(event_time_c1 >= -30 & event_time_c1 <= 80)

event_study_buy <- feols(
  buy_indicator ~ i(event_time_c1, treated_c1, ref = -1) | agent_id + t,
  cluster = ~agent_id, data = event_data
)

tryCatch({
  png("event_study_buy_cohort1.png", width = 12, height = 6, units = "in", res = 300)
  iplot(event_study_buy,
        main = "Event Study: Buy Indicator (Cohort 1)",
        xlab = "Periods Relative to Treatment", ylab = "Treatment Effect (pp)")
  dev.off()
  cat("Saved: event_study_buy_cohort1.png\n")
}, error = function(e) { try(dev.off(), silent = TRUE); cat(sprintf("  Plot failed: %s\n", e$message)) })


# ============================================================================
# SECTION 12: PUBLICATION-READY TABLES
# ============================================================================

cat("\n============================================================================\n")
cat("SECTION 12: PUBLICATION-READY TABLES\n")
cat("============================================================================\n")

# Table 1: Summary Statistics
summary_stats <- data_trading %>%
  mutate(period = ifelse(post_treatment, "Post", "Pre"),
         group = case_when(treatment_cohort == 1 ~ "Cohort 1",
                           treatment_cohort == 2 ~ "Cohort 2",
                           treatment_cohort == 3 ~ "Never-Treated")) %>%
  group_by(group, period) %>%
  summarise(N = n(), buy_rate = mean(buy_indicator, na.rm = TRUE),
            sell_rate = mean(sell_indicator, na.rm = TRUE),
            trade_rate = mean(trade_indicator, na.rm = TRUE),
            portfolio_value = mean(portfolio_value, na.rm = TRUE), .groups = "drop")

write_csv(summary_stats, "table1_summary_statistics.csv")
cat("Saved: table1_summary_statistics.csv\n")

# Table 2: Main Results
tryCatch({
  models_list <- list("Buy" = twfe_buy, "Sell" = twfe_sell, "Trade" = twfe_trade, "Portfolio Value" = portfolio_model)
  modelsummary(models_list, output = "table2_main_results.csv",
               coef_rename = c("cohort1_attTRUE" = "Cohort 1 x Post", "cohort2_attTRUE" = "Cohort 2 x Post"))
  cat("Saved: table2_main_results.csv\n")
}, error = function(e) cat(sprintf("  Table export failed: %s\n", e$message)))

# Table 3: Hypothesis Summary
hypothesis_summary <- tibble(
  Hypothesis = c("H1: Attention -> Buy", "H1: Attention -> Sell", "H1: Buy = Sell (Wald)",
                 "H2: Return Chasing", "H4: Disposition Effect"),
  Result = c(sprintf("%.2f pp***", twfe_buy_avg * 100),
             sprintf("%.2f pp***", twfe_sell_avg * 100),
             sprintf("Chi2=%.2f, p=%.2f", wald_stat, p_val_wald),
             if (!is.null(chase_test)) sprintf("p=%.3f", chase_test$p.value) else "See analysis",
             sprintf("%.1f%%", if (!is.na(pct_winners)) pct_winners * 100 else NA))
)
write_csv(hypothesis_summary, "table3_hypothesis_tests.csv")
cat("Saved: table3_hypothesis_tests.csv\n")


# ============================================================================
# FINAL SUMMARY (SECTIONS 1-12)
# ============================================================================

cat("\n============================================================================\n")
cat("SECTIONS 1-12 COMPLETE — KEY FINDINGS\n")
cat("============================================================================\n")

cat(sprintf("\n  Manipulation checks: %.1f%% pass rate\n", overall_pass_rate * 100))
cat(sprintf("  Balance: %d/%d passed\n", sum(balance_results$balanced), nrow(balance_results)))
cat(sprintf("  Power: %.1f%% (ICC=%.2f)\n", power * 100, icc))
cat(sprintf("  Buy: %.2f pp (%.1f%% relative) | Sell: %.2f pp (%.1f%% relative)\n",
            twfe_buy_avg * 100, relative_buy_increase, twfe_sell_avg * 100, relative_sell_increase))
cat(sprintf("  Wald: chi2=%.2f, p=%.3f | Cohen's d: Buy=%.3f, Sell=%.3f\n",
            wald_stat, p_val_wald, cohens_d_buy, cohens_d_sell))
cat(sprintf("  Extra round-trips/100: %.1f | Cost drag: ~%.0f bps\n", round_trips_per_100, avg_cost_drag))


# ==============================================================================
# PART A: ROBUSTNESS & IDENTIFICATION (continued)
# ==============================================================================

cat("\n==============================================================================\n")
cat("PART A: ROBUSTNESS & IDENTIFICATION\n")
cat("==============================================================================\n")

# --- A1. Propensity Score Weighting (IPW) ---
cat("\n--- A1. Propensity Score Weighting ---\n")

agent_pre <- data_trading %>%
  filter(!post_treatment) %>%
  group_by(agent_id, treatment_cohort, persona) %>%
  summarise(pre_buy_rate = mean(buy_indicator, na.rm = TRUE),
            pre_sell_rate = mean(sell_indicator, na.rm = TRUE),
            pre_trade_rate = mean(trade_indicator, na.rm = TRUE),
            pre_portfolio_mean = mean(portfolio_value, na.rm = TRUE),
            pre_portfolio_sd = sd(portfolio_value, na.rm = TRUE),
            n_pre_periods = n(), .groups = "drop") %>%
  mutate(treated = treatment_cohort %in% c(1, 2),
         p_social    = as.numeric(persona == "SocialMomentum"),
         p_lottery   = as.numeric(persona == "LotterySeeker"),
         p_technical = as.numeric(persona == "TechnicalTrader"),
         p_dividend  = as.numeric(persona == "DividendSeeker"),
         p_passive   = as.numeric(persona == "PassiveFollower"))

ps_model <- glm(treated ~ pre_buy_rate + pre_sell_rate + pre_portfolio_mean +
                   p_social + p_lottery + p_technical + p_dividend + p_passive,
                 family = binomial(link = "logit"), data = agent_pre)

cat("\nPropensity Score Model:\n")
print(summary(ps_model))

agent_pre <- agent_pre %>%
  mutate(ps = predict(ps_model, type = "response"),
         ipw_stab = case_when(treated ~ mean(treated) / ps, !treated ~ (1 - mean(treated)) / (1 - ps)))

cat("\nPS Distribution:\n")
cat(sprintf("  Treated: Mean=%.3f, Range=[%.3f, %.3f]\n",
            mean(agent_pre$ps[agent_pre$treated]),
            min(agent_pre$ps[agent_pre$treated]), max(agent_pre$ps[agent_pre$treated])))
cat(sprintf("  Control: Mean=%.3f, Range=[%.3f, %.3f]\n",
            mean(agent_pre$ps[!agent_pre$treated]),
            min(agent_pre$ps[!agent_pre$treated]), max(agent_pre$ps[!agent_pre$treated])))

# Merge to trading data
data_trading <- data_trading %>%
  left_join(agent_pre %>% select(agent_id, ps, ipw_stab), by = "agent_id")

ipw_buy  <- feols(buy_indicator  ~ cohort1_post + cohort2_post | agent_id + t, weights = ~ipw_stab, cluster = ~agent_id, data = data_trading)
ipw_sell <- feols(sell_indicator ~ cohort1_post + cohort2_post | agent_id + t, weights = ~ipw_stab, cluster = ~agent_id, data = data_trading)

print(etable(ipw_buy, ipw_sell, headers = c("Buy (IPW)", "Sell (IPW)"), se.below = TRUE))

unweighted_buy <- feols(buy_indicator ~ cohort1_post + cohort2_post | agent_id + t,
                        cluster = ~agent_id, data = data_trading)

cat("\nUnweighted vs IPW (Buy, Cohort 1):\n")
cat(sprintf("  Unweighted: %.4f | IPW: %.4f\n",
            coef(unweighted_buy)["cohort1_postTRUE"], coef(ipw_buy)["cohort1_postTRUE"]))


# --- A2. Alternative Control Groups ---
cat("\n--- A2. Alternative Control Groups ---\n")

# Alt 1: Cohort 2 as control for Cohort 1 (t < 120)
alt1_data <- data_trading %>%
  filter(treatment_cohort %in% c(1, 2), t < 120) %>%
  mutate(did_c1 = (treatment_cohort == 1) & (t >= 60))

alt1_model <- feols(buy_indicator ~ did_c1 | agent_id + t, cluster = ~agent_id, data = alt1_data)
cat("\nAlt 1 (Cohort 2 as control, t<120):\n"); print(summary(alt1_model))

# Alt 2: Cohort 1 as control for Cohort 2 (60 <= t < 180)
alt2_data <- data_trading %>%
  filter(treatment_cohort %in% c(1, 2), t >= 60, t < 180) %>%
  mutate(did_c2 = (treatment_cohort == 2) & (t >= 120))

alt2_model <- feols(buy_indicator ~ did_c2 | agent_id + t, cluster = ~agent_id, data = alt2_data)
cat("\nAlt 2 (Cohort 1 as control, 60<=t<180):\n"); print(summary(alt2_model))


# --- A3. Bounds Analysis ---
cat("\n--- A3. Bounds Analysis ---\n")

point_est_c1    <- coef(unweighted_buy)["cohort1_postTRUE"]
point_est_c2    <- coef(unweighted_buy)["cohort2_postTRUE"]
avg_point_est   <- mean(c(point_est_c1, point_est_c2), na.rm = TRUE)
pre_imbalance_buy <- 0.096

lower_bound <- avg_point_est - pre_imbalance_buy
upper_bound <- avg_point_est + pre_imbalance_buy

cat(sprintf("  Point est: %.4f (%.2f pp)\n", avg_point_est, avg_point_est * 100))
cat(sprintf("  Worst-case bounds: [%.2f, %.2f] pp | Crosses zero: %s\n",
            lower_bound * 100, upper_bound * 100, ifelse(lower_bound < 0 & upper_bound > 0, "YES", "NO")))
cat(sprintf("  Bias to flip: %.2f pp (imbalance = %.1f%% of threshold)\n",
            abs(avg_point_est) * 100, pre_imbalance_buy / abs(avg_point_est) * 100))


# --- A4. Oster's Delta ---
cat("\n--- A4. Oster's Delta Approximation ---\n")

short_model <- feols(buy_indicator ~ cohort1_post + cohort2_post | agent_id + t, data = data_trading)
short_r2    <- summary(short_model)$r2["adj.r2"]

oster_data <- data_trading %>%
  filter(!is.na(momentum), !is.na(signal_z_score))
if (nrow(oster_data) == 0L) {
  cat("  Not estimable: momentum and signal_z_score have no joint non-missing\n")
  cat("  trading observations. Continuing with the remaining analysis.\n")
} else {
  long_model <- feols(
    buy_indicator ~ cohort1_post + cohort2_post + momentum + signal_z_score | agent_id + t,
    data = oster_data
  )
  long_r2 <- summary(long_model)$r2["adj.r2"]

  beta_short <- coef(short_model)["cohort1_postTRUE"]
  beta_long  <- coef(long_model)["cohort1_postTRUE"]

  r_max <- min(1, 1.3 * long_r2)
  delta_approx <- (beta_long * (r_max - long_r2)) /
    ((beta_short - beta_long) * (long_r2 - short_r2))

  cat(sprintf("  R2: short=%.4f, long=%.4f\n", short_r2, long_r2))
  cat(sprintf("  Beta: short=%.4f, long=%.4f\n", beta_short, beta_long))
  cat(sprintf("  Oster's delta: %.2f (>1 = robust to unobservables)\n", delta_approx))
}


# ==============================================================================
# PART B: MECHANISM TESTS
# ==============================================================================

cat("\n==============================================================================\n")
cat("PART B: MECHANISM TESTS\n")
cat("==============================================================================\n")

# --- B5. Attention Allocation as Outcome ---
cat("\n--- B5. Attention Allocation as Outcome ---\n")

parse_meme_attention <- function(alloc) {
  if (is.na(alloc)) return(0)
  score <- 0
  if (str_detect(alloc, "AMC:deep")) score <- score + 2
  else if (str_detect(alloc, "AMC:quick")) score <- score + 1
  if (str_detect(alloc, "GME:deep")) score <- score + 2
  else if (str_detect(alloc, "GME:quick")) score <- score + 1
  score
}

data_attention <- data_attention %>%
  mutate(meme_attention_score = sapply(allocation, parse_meme_attention),
         any_meme_deep       = str_detect(allocation, "(AMC|GME):deep"),
         any_meme_attention  = str_detect(allocation, "(AMC|GME):(deep|quick)"))

attention_model <- feols(meme_attention_score ~ cohort1_post + cohort2_post | agent_id + t,
                         cluster = ~agent_id, data = data_attention)
cat("\nMeme Attention (0-4 scale):\n"); print(summary(attention_model))

deep_attention_model <- feols(any_meme_deep ~ cohort1_post + cohort2_post | agent_id + t,
                              cluster = ~agent_id, data = data_attention)
cat("\nDeep Meme Attention (binary):\n"); print(summary(deep_attention_model))

data_attention %>%
  group_by(treated, post_treatment) %>%
  summarise(mean_meme_att = mean(meme_attention_score, na.rm = TRUE),
            pct_deep = mean(any_meme_deep, na.rm = TRUE) * 100, n = n(), .groups = "drop") %>%
  print()


# --- B6. Dose-Response ---
cat("\n--- B6. Dose-Response ---\n")

attention_levels <- data_attention %>% select(agent_id, t, meme_attention_score, any_meme_deep)

# Only join if not already present
if (!"meme_attention_score" %in% names(data_trading)) {
  data_trading <- data_trading %>% left_join(attention_levels, by = c("agent_id", "t"))
}

intensity_model <- feols(buy_indicator ~ meme_attention_score | agent_id + t,
                         cluster = ~agent_id, data = data_trading %>% filter(treated, post_treatment))
cat("\nDose-Response (linear):\n"); print(summary(intensity_model))

# Non-linear
data_trading <- data_trading %>% mutate(meme_att_factor = factor(meme_attention_score))
intensity_nonlin <- feols(buy_indicator ~ meme_att_factor | agent_id + t,
                          cluster = ~agent_id, data = data_trading %>% filter(treated, post_treatment))
cat("\nDose-Response (non-linear):\n"); print(summary(intensity_nonlin))


# --- B7. Meme Stock Trade Concentration ---
cat("\n--- B7. Meme Stock Trade Concentration ---\n")

trade_composition <- data_trading %>%
  filter(trade_indicator == 1) %>%
  group_by(agent_id, treatment_cohort, post_treatment, treated) %>%
  summarise(n_trades = n(), n_meme = sum(is_meme_stock, na.rm = TRUE),
            pct_meme = mean(is_meme_stock, na.rm = TRUE) * 100, .groups = "drop") %>%
  mutate(cohort1_post = (treatment_cohort == 1) & post_treatment,
         cohort2_post = (treatment_cohort == 2) & post_treatment)

trade_composition %>%
  group_by(treated, post_treatment) %>%
  summarise(mean_pct_meme = mean(pct_meme, na.rm = TRUE), n = n(), .groups = "drop") %>%
  print()

meme_conc_model <- lm(pct_meme ~ cohort1_post + cohort2_post + factor(agent_id), data = trade_composition)
cat(sprintf("DiD meme concentration (Cohort 1): %.2f pp\n",
            coef(meme_conc_model)["cohort1_postTRUE"]))


# --- B8. Trading Intent (Blocked Trades) ---
cat("\n--- B8. Trading Intent ---\n")

data_trading <- data_trading %>%
  mutate(wanted_to_buy  = (model_action == "buy"),
         wanted_to_sell = (model_action == "sell"),
         was_blocked    = blocked_reason %in% c("cooldown", "insufficient_cash",
                                                 "insufficient_shares", "not_owned"),
         blocked_buy    = wanted_to_buy & was_blocked)

intent_buy_model <- feols(wanted_to_buy ~ cohort1_post + cohort2_post | agent_id + t,
                          cluster = ~agent_id, data = data_trading)
cat("\nBuy INTENT effect:\n"); print(summary(intent_buy_model))

cat(sprintf("Intent vs Actual: intent=%.4f, actual=%.4f\n",
            mean(c(coef(intent_buy_model)["cohort1_postTRUE"],
                   coef(intent_buy_model)["cohort2_postTRUE"]), na.rm = TRUE),
            mean(c(coef(unweighted_buy)["cohort1_postTRUE"],
                   coef(unweighted_buy)["cohort2_postTRUE"]), na.rm = TRUE)))

data_trading %>%
  group_by(treated, post_treatment) %>%
  summarise(pct_wanted_buy = mean(wanted_to_buy, na.rm = TRUE) * 100,
            pct_blocked = mean(was_blocked, na.rm = TRUE) * 100, n = n(), .groups = "drop") %>%
  print()


# ==============================================================================
# PART C: HETEROGENEITY ANALYSES
# ==============================================================================

cat("\n==============================================================================\n")
cat("PART C: HETEROGENEITY ANALYSES\n")
cat("==============================================================================\n")

# --- C9. Volatility Regime (Cohort 1 clean test) ---
cat("\n--- C9. Volatility Regime Heterogeneity ---\n")

data_c1 <- data_trading %>% filter(treatment_cohort %in% c(1, 3))

m_base_c1 <- feols(buy_indicator ~ cohort1_post | agent_id + t, cluster = ~agent_id, data = data_c1)
cat("Cohort 1 base model:\n"); summary(m_base_c1)

m_vol_c1 <- feols(buy_indicator ~ cohort1_post * high_vol_regime | agent_id + t,
                   cluster = ~agent_id, data = data_c1)
print(summary(m_vol_c1))

b <- coef(m_vol_c1); V <- vcov(m_vol_c1)
beta_low  <- b[["cohort1_postTRUE"]]
beta_diff <- b[["cohort1_postTRUE:high_vol_regimeTRUE"]]
beta_high <- beta_low + beta_diff

var_high <- V["cohort1_postTRUE","cohort1_postTRUE"] +
  V["cohort1_postTRUE:high_vol_regimeTRUE","cohort1_postTRUE:high_vol_regimeTRUE"] +
  2 * V["cohort1_postTRUE","cohort1_postTRUE:high_vol_regimeTRUE"]

cat(sprintf("Low-vol: %.3f pp | High-vol: %.3f pp | Diff: %.3f pp\n",
            100*beta_low, 100*beta_high, 100*beta_diff))

# Separate low/high vol models for robustness table
low_vol_model  <- feols(buy_indicator ~ cohort1_post + cohort2_post | agent_id + t,
                        cluster = ~agent_id, data = data_trading %>% filter(t < 100))
high_vol_model <- feols(buy_indicator ~ cohort1_post + cohort2_post | agent_id + t,
                        cluster = ~agent_id, data = data_trading %>% filter(t >= 100))


# --- C10. Signal Strength ---
cat("\n--- C10. Signal Strength Heterogeneity ---\n")

data_trading <- data_trading %>%
  mutate(strong_signal = signal_strength %in% c("medium", "strong"),
         weak_signal   = signal_strength %in% c("noise", "weak"))

signal_interaction <- feols(buy_indicator ~ cohort1_post * strong_signal + cohort2_post * strong_signal | agent_id + t,
                            cluster = ~agent_id, data = data_trading)
cat("\nSignal x Treatment:\n"); print(summary(signal_interaction))

for (sig in c("noise", "weak", "medium")) {
  subset <- data_trading %>% filter(signal_strength == sig)
  if (nrow(subset) > 500) {
    model <- feols(buy_indicator ~ cohort1_post + cohort2_post | agent_id + t, cluster = ~agent_id, data = subset)
    avg_eff <- mean(c(coef(model)["cohort1_postTRUE"], coef(model)["cohort2_postTRUE"]), na.rm = TRUE)
    cat(sprintf("  %s: %.2f pp (n=%d)\n", sig, avg_eff * 100, nrow(subset)))
  }
}


# --- C11. Event-Time Dynamics (Fine Bins) ---
cat("\n--- C11. Event-Time Dynamics ---\n")

data_trading <- data_trading %>%
  mutate(event_bin = factor(case_when(
    is.na(event_time) ~ NA_character_,
    event_time < -20  ~ "pre_20plus", event_time < -10 ~ "pre_10_19",
    event_time < -5   ~ "pre_5_9",   event_time < 0   ~ "pre_1_4",
    event_time == 0   ~ "t0",
    event_time <= 5   ~ "post_1_5",  event_time <= 10  ~ "post_6_10",
    event_time <= 20  ~ "post_11_20", event_time <= 40  ~ "post_21_40",
    event_time <= 60  ~ "post_41_60", event_time <= 90  ~ "post_61_90",
    TRUE ~ "post_90plus"
  ), levels = c("pre_20plus", "pre_10_19", "pre_5_9", "pre_1_4", "t0",
                "post_1_5", "post_6_10", "post_11_20", "post_21_40",
                "post_41_60", "post_61_90", "post_90plus")))

event_summary <- data_trading %>%
  filter(treated, !is.na(event_bin)) %>%
  group_by(event_bin) %>%
  summarise(mean_buy = mean(buy_indicator, na.rm = TRUE),
            se_buy = sd(buy_indicator, na.rm = TRUE) / sqrt(n()), n = n(), .groups = "drop")
print(event_summary)

event_study_bins <- feols(buy_indicator ~ i(event_bin, ref = "pre_5_9") | agent_id + t,
                          cluster = ~agent_id, data = data_trading %>% filter(treated))
cat("\nEvent Study (ref: pre_5_9):\n"); print(summary(event_study_bins))


# --- C12. Triple-Difference ---
cat("\n--- C12. Triple-Difference (Treatment x Post x Meme) ---\n")

trades_only <- data_trading %>%
  filter(trade_indicator == 1) %>%
  mutate(meme = as.numeric(is_meme_stock), post = as.numeric(post_treatment), treat = as.numeric(treated))

if (nrow(trades_only) > 1000) {
  triple_diff <- feols(buy_indicator ~ treat * post * meme | agent_id + t,
                       cluster = ~agent_id, data = trades_only)
  cat("\nTriple-Difference:\n"); print(summary(triple_diff))
}


# ==============================================================================
# PART D: QUALITATIVE EVIDENCE
# ==============================================================================

cat("\n==============================================================================\n")
cat("PART D: QUALITATIVE EVIDENCE\n")
cat("==============================================================================\n")

# --- D13. Text Analysis ---
cat("\n--- D13. Text Analysis of Rationales ---\n")

data_trading <- data_trading %>%
  mutate(rationale_lower   = tolower(rationale),
         mentions_social   = str_detect(rationale_lower, "social|buzz|viral|mention|trending|popular"),
         mentions_momentum = str_detect(rationale_lower, "momentum|trend|upward|positive"),
         mentions_crowd    = str_detect(rationale_lower, "crowd|retail|buyer|interest|hype"),
         mentions_risk     = str_detect(rationale_lower, "risk|caution|cautious|careful|loss"),
         rationale_length  = nchar(rationale))

text_summary <- data_trading %>%
  filter(!is.na(rationale), nchar(rationale) > 10) %>%
  group_by(treated, post_treatment) %>%
  summarise(pct_social = mean(mentions_social, na.rm = TRUE) * 100,
            pct_momentum = mean(mentions_momentum, na.rm = TRUE) * 100,
            pct_crowd = mean(mentions_crowd, na.rm = TRUE) * 100,
            pct_risk = mean(mentions_risk, na.rm = TRUE) * 100,
            mean_length = mean(rationale_length, na.rm = TRUE), n = n(), .groups = "drop")
print(text_summary)

social_mention_model <- feols(mentions_social ~ cohort1_post + cohort2_post | agent_id + t,
                              cluster = ~agent_id, data = data_trading %>% filter(!is.na(rationale)))
cat("\nDiD on Social Mentions:\n"); print(summary(social_mention_model))


# --- D14. Manipulation Check Quality ---
cat("\n--- D14. Manipulation Check Analysis ---\n")

data_attention %>%
  group_by(treated, post_treatment) %>%
  summarise(pass_rate = mean(manipulation_check_passed, na.rm = TRUE) * 100,
            mean_confidence = mean(manipulation_confidence, na.rm = TRUE), n = n(), .groups = "drop") %>%
  print()

data_attention <- data_attention %>%
  mutate(correct_perception = (perceived_highest_buzz == actual_highest_buzz))

data_attention %>%
  group_by(treated, post_treatment) %>%
  summarise(pct_correct = mean(correct_perception, na.rm = TRUE) * 100, n = n(), .groups = "drop") %>%
  print()


# --- Social Momentum Rationale Analysis ---
cat("\n--- Social Momentum Rationale Analysis ---\n")

target_df <- data_trading %>%
  filter(persona == "SocialMomentum", treated == TRUE, post_treatment == TRUE, !is.na(rationale))

keywords <- list(
  "Satiation"           = c("already hold", "sufficient", "enough", "position built", "max allocation"),
  "Skepticism_Risk"     = c("too much hype", "overhyped", "suspicious", "red flag", "wary",
                            "skeptical", "caution", "risk", "bubble"),
  "Mechanics_Cost_Edge" = c("transaction cost", "fee", "negative edge", "no positive edge",
                            "small edge", "costly"),
  "No_Signal_Perceived" = c("no strong social", "low social", "no crowd", "no buzz")
)

check_keywords <- function(text, terms) str_detect(text, regex(paste(terms, collapse = "|"), ignore_case = TRUE))

results_qual <- target_df %>%
  mutate(Has_Satiation  = check_keywords(rationale, keywords$Satiation),
         Has_Skepticism = check_keywords(rationale, keywords$Skepticism_Risk),
         Has_Mechanics  = check_keywords(rationale, keywords$Mechanics_Cost_Edge),
         Has_No_Signal  = check_keywords(rationale, keywords$No_Signal_Perceived))

qual_summary <- results_qual %>%
  summarise(Total = n(),
            Satiation_Pct  = round(mean(Has_Satiation)  * 100, 1),
            Skepticism_Pct = round(mean(Has_Skepticism) * 100, 1),
            Mechanics_Pct  = round(mean(Has_Mechanics)  * 100, 1),
            No_Signal_Pct  = round(mean(Has_No_Signal)  * 100, 1))
print(qual_summary)

ggplot(data.frame(
  Hypothesis = c("Satiation", "Skepticism", "Mechanics", "No Signal"),
  Pct = c(qual_summary$Satiation_Pct, qual_summary$Skepticism_Pct,
          qual_summary$Mechanics_Pct, qual_summary$No_Signal_Pct)
), aes(x = reorder(Hypothesis, -Pct), y = Pct)) +
  geom_bar(stat = "identity", fill = "#4c7ea0") +
  geom_text(aes(label = paste0(Pct, "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Why Did Social Momentum Agents Fail to Buy?",
       x = "Rationale Theme", y = "% of Decisions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ==============================================================================
# PART E: TRANSACTION COST CONFOUND TESTS
# ==============================================================================

cat("\n==============================================================================\n")
cat("PART E: TRANSACTION COST CONFOUND TESTS\n")
cat("==============================================================================\n")

# Persona-level correlation
baseline_freq <- data %>%
  filter(stage == "trading", post_treatment == FALSE) %>%
  group_by(persona) %>%
  summarise(baseline_buy_rate = mean(buy_indicator, na.rm = TRUE),
            baseline_trade_rate = mean(trade_indicator, na.rm = TRUE), n_obs = n(), .groups = "drop")

persona_effects_manual <- data.frame(
  persona = c("DividendSeeker", "LotterySeeker", "SkepticalContrarian",
              "TechnicalTrader", "SocialMomentum", "PassiveFollower"),
  treatment_effect_pp = c(-9.85, -7.58, -5.25, -3.94, -0.62, 0.24)
)

persona_test <- left_join(persona_effects_manual, baseline_freq, by = "persona")
cor_result <- cor.test(persona_test$treatment_effect_pp, persona_test$baseline_trade_rate, method = "pearson")
cat(sprintf("\nPersona-level: r=%.3f, p=%.3f\n", cor_result$estimate, cor_result$p.value))

if (requireNamespace("ggrepel", quietly = TRUE)) {
  library(ggrepel)
  p_tc <- persona_test %>%
    ggplot(aes(x = baseline_trade_rate, y = treatment_effect_pp, label = persona)) +
    geom_point(size = 3) + geom_text_repel() +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(title = "Treatment Effects vs Baseline Trading Frequency",
         x = "Pre-Treatment Trading Frequency", y = "Treatment Effect (pp)") +
    theme_minimal()
  ggsave("figure_tc_confound.png", p_tc, width = 10, height = 6, dpi = 300)
  cat("Saved: figure_tc_confound.png\n")
}

# Agent-level test
agent_baseline <- data %>%
  filter(stage == "trading", post_treatment == FALSE) %>%
  group_by(agent_id, persona, treatment_cohort) %>%
  summarise(baseline_buy_rate = mean(buy_indicator, na.rm = TRUE),
            baseline_trade_rate = mean(trade_indicator, na.rm = TRUE), .groups = "drop")

agent_post <- data %>%
  filter(stage == "trading", post_treatment == TRUE, treatment_cohort %in% c(1, 2)) %>%
  group_by(agent_id) %>%
  summarise(post_buy_rate = mean(buy_indicator, na.rm = TRUE), .groups = "drop")

agent_analysis <- agent_baseline %>%
  filter(treatment_cohort %in% c(1, 2)) %>%
  left_join(agent_post, by = "agent_id") %>%
  mutate(buy_rate_change = post_buy_rate - baseline_buy_rate)

cor_agent <- cor.test(agent_analysis$buy_rate_change, agent_analysis$baseline_trade_rate, method = "pearson")
cat(sprintf("Agent-level (n=%d): r=%.3f, p=%.3f\n",
            nrow(agent_analysis), cor_agent$estimate, cor_agent$p.value))

agent_reg <- lm(buy_rate_change ~ baseline_trade_rate + persona, data = agent_analysis)
summary(agent_reg)


# ==============================================================================
# ROBUSTNESS SUMMARY TABLE
# ==============================================================================

cat("\n--- Robustness Summary Table ---\n")

robustness_results <- tribble(
  ~Specification, ~Cohort1_Effect, ~Cohort1_SE, ~Cohort2_Effect, ~Cohort2_SE,
  "Main TWFE (Unweighted)",
  coef(unweighted_buy)["cohort1_postTRUE"], summary(unweighted_buy)$se["cohort1_postTRUE"],
  coef(unweighted_buy)["cohort2_postTRUE"], summary(unweighted_buy)$se["cohort2_postTRUE"],
  "IPW-Weighted TWFE",
  coef(ipw_buy)["cohort1_postTRUE"], summary(ipw_buy)$se["cohort1_postTRUE"],
  coef(ipw_buy)["cohort2_postTRUE"], summary(ipw_buy)$se["cohort2_postTRUE"],
  "Cohort 2 as Control (t<120)",
  coef(alt1_model)["did_c1TRUE"], summary(alt1_model)$se["did_c1TRUE"], NA_real_, NA_real_,
  "Low Volatility (t<100)",
  coef(low_vol_model)["cohort1_postTRUE"], summary(low_vol_model)$se["cohort1_postTRUE"],
  coef(low_vol_model)["cohort2_postTRUE"], summary(low_vol_model)$se["cohort2_postTRUE"],
  "High Volatility (t>=100)",
  coef(high_vol_model)["cohort1_postTRUE"], summary(high_vol_model)$se["cohort1_postTRUE"],
  coef(high_vol_model)["cohort2_postTRUE"], summary(high_vol_model)$se["cohort2_postTRUE"]
) %>%
  mutate(
    Cohort1_pp = Cohort1_Effect * 100,
    Cohort1_t  = Cohort1_Effect / Cohort1_SE,
    Cohort1_sig = case_when(
      abs(Cohort1_t) > 2.576 ~ "***", abs(Cohort1_t) > 1.96 ~ "**",
      abs(Cohort1_t) > 1.645 ~ "*", TRUE ~ ""
    )
  )

print(robustness_results %>% select(Specification, Cohort1_pp, Cohort1_SE, Cohort1_sig))
write_csv(robustness_results, "robustness_table.csv")
cat("Saved: robustness_table.csv\n")


# ==============================================================================
# [R1-A] FACTORIAL ARM EFFECTS (factorial run, data/rerun_A) + SM VARIANT INVARIANCE
# ------------------------------------------------------------------------------
# Runs only when the loaded data come from the factorial rerun (arm columns
# present and 4 cohorts). Cohort mapping (set in the R1 experiment script):
#   1 = viral attention @ 5 bps   (decisive salience cell)
#   2 = viral attention @ 15 bps  (replicates the original bundled treatment)
#   3 = normal attention @ 15 bps (cost-only cell)
#   4 = never-treated control     (normal @ 5 bps)
# Feeds: the manuscript's factorial results section and the arm-effects table.
# ==============================================================================

has_r1_arms <- ("attention_arm" %in% names(data_trading)) &&
  (dplyr::n_distinct(stats::na.omit(data_trading$attention_arm)) > 1) &&
  (dplyr::n_distinct(stats::na.omit(data_trading$treatment_cohort)) == 4) &&
  (4 %in% stats::na.omit(data_trading$treatment_cohort))

if (has_r1_arms) {
  cat("\n==============================================================================\n")
  cat("[R1-A] FACTORIAL ARM EFFECTS\n")
  cat("==============================================================================\n")

  R1_ADOPT <- 60
  dfa <- data_trading %>%
    mutate(post60 = as.integer(t >= R1_ADOPT),
           a_v5   = as.integer(treatment_cohort == 1) * post60,   # viral @ 5 bps
           a_v15  = as.integer(treatment_cohort == 2) * post60,   # viral @ 15 bps (original bundle)
           a_c15  = as.integer(treatment_cohort == 3) * post60)   # cost-only @ 15 bps

  m_buy  <- feols(buy_indicator  ~ a_v5 + a_v15 + a_c15 | agent_id + t, cluster = ~agent_id, data = dfa)
  m_sell <- feols(sell_indicator ~ a_v5 + a_v15 + a_c15 | agent_id + t, cluster = ~agent_id, data = dfa)
  cat("\n--- Buy indicator (pp = coef x 100) ---\n");  print(summary(m_buy))
  cat("\n--- Sell indicator ---\n");                    print(summary(m_sell))

  # Decomposition: salience effect at fixed cost = a_v5; pure cost effect = a_c15;
  # interaction (super/sub-additivity) = a_v15 - a_v5 - a_c15.
  b <- coef(m_buy); V <- vcov(m_buy)
  L <- c(a_v5 = -1, a_v15 = 1, a_c15 = -1)
  est_int <- sum(L * b[names(L)])
  se_int  <- sqrt(as.numeric(t(L) %*% V[names(L), names(L)] %*% L))
  cat(sprintf("\nSalience effect at 5 bps (a_v5):    %+.2f pp (SE %.2f)\n", 100*b["a_v5"],  100*sqrt(V["a_v5","a_v5"])))
  cat(sprintf("Cost-only effect (a_c15):           %+.2f pp (SE %.2f)\n", 100*b["a_c15"], 100*sqrt(V["a_c15","a_c15"])))
  cat(sprintf("Bundled effect (a_v15):             %+.2f pp (SE %.2f)\n", 100*b["a_v15"], 100*sqrt(V["a_v15","a_v15"])))
  cat(sprintf("Interaction (v15 - v5 - c15):       %+.2f pp (SE %.2f, z = %.2f)\n", 100*est_int, 100*se_int, est_int/se_int))

  arm_tab <- data.frame(
    cell     = c("viral @ 5bps", "viral @ 15bps (bundle)", "cost-only @ 15bps", "interaction"),
    est_pp   = 100 * c(b["a_v5"], b["a_v15"], b["a_c15"], est_int),
    se_pp    = 100 * c(sqrt(V["a_v5","a_v5"]), sqrt(V["a_v15","a_v15"]), sqrt(V["a_c15","a_c15"]), se_int)
  )
  write_csv(arm_tab, "table_r1_arm_effects.csv")
  cat("Saved: table_r1_arm_effects.csv  -> manuscript Section 4.6 + Table 10 row\n")

  # --- SM variant invariance: is the Social Momentum response the same
  #     across the three prompt constructions? ---
  if ("sm_variant" %in% names(dfa) && dplyr::n_distinct(stats::na.omit(dfa$sm_variant)) > 1) {
    sm <- dfa %>% filter(persona == "SocialMomentum") %>%
      mutate(viral_post = as.integer(treatment_cohort %in% c(1, 2)) * post60)
    m_sm <- feols(buy_indicator ~ viral_post + i(sm_variant, viral_post, ref = "v1") | agent_id + t,
                  cluster = ~agent_id, data = sm)
    cat("\n--- SM variant invariance (deviations from v1; joint test = equality) ---\n")
    print(summary(m_sm))
    wt <- tryCatch(fixest::wald(m_sm, keep = "sm_variant"), error = function(e) NULL)
    if (!is.null(wt)) print(wt)
    cat("Feeds: Section 4.6 bracketed variant results (v1/v2/v3 effects; equality p).\n")
  }
} else {
  cat("\n[R1-A] Skipped: loaded data do not have the four-arm factorial assignment.\n")
}

# ==============================================================================
# [R1-B] PERSONA BEHAVIORAL FIDELITY CHECKS  (works on the ORIGINAL data)
# ------------------------------------------------------------------------------
# Manipulation checks for the persona layer: does each persona exhibit, in the
# common pre-treatment window (t < 60), the signature its own instructions imply?
# Output feeds manuscript Appendix Table A4. Report failures honestly.
# ==============================================================================

cat("\n==============================================================================\n")
cat("[R1-B] PERSONA BEHAVIORAL FIDELITY CHECKS (pre-treatment, t < 60)\n")
cat("==============================================================================\n")

pre_trad <- data_trading %>% filter(t < 60)
pre_att  <- data_attention %>% filter(t < 60)

meme_set <- c("AMC", "GME")
buys_pre <- pre_trad %>% filter(buy_indicator == 1, !is.na(ticker))

meme_share <- buys_pre %>% group_by(persona) %>%
  summarise(meme_share = mean(ticker %in% meme_set), n_buys = n(), .groups = "drop")
trade_rate <- pre_trad %>% group_by(persona) %>%
  summarise(trade_rate = mean(trade_indicator, na.rm = TRUE), .groups = "drop")
buy_mom <- buys_pre %>% group_by(persona) %>%
  summarise(mean_buy_momentum = mean(momentum, na.rm = TRUE), .groups = "drop")

# Attention tracking of the perceived buzz leader (from the allocation string)
leader_att <- pre_att %>%
  filter(!is.na(allocation), !is.na(perceived_highest_buzz)) %>%
  rowwise() %>%
  mutate(leader_level = {
    parts <- strsplit(allocation, ",\\s*")[[1]]
    kv <- strsplit(parts, ":")
    hit <- vapply(kv, function(z) trimws(z[1]) == perceived_highest_buzz, logical(1))
    if (any(hit)) trimws(strsplit(parts[which(hit)[1]], ":")[[1]][2]) else NA_character_
  }) %>% ungroup() %>%
  group_by(persona) %>%
  summarise(leader_researched = mean(leader_level %in% c("deep", "quick"), na.rm = TRUE), .groups = "drop")

fid <- meme_share %>% left_join(trade_rate, by = "persona") %>%
  left_join(buy_mom, by = "persona") %>% left_join(leader_att, by = "persona")

fid <- fid %>% mutate(
  signature = dplyr::case_when(
    persona == "DividendSeeker"      ~ "Avoids AMC/GME purchases (meme share ~ 0)",
    persona == "LotterySeeker"       ~ "Highest meme share of purchases",
    persona == "PassiveFollower"     ~ "Lowest trade rate",
    persona == "TechnicalTrader"     ~ "Buys momentum (highest mean buy momentum)",
    persona == "SkepticalContrarian" ~ "Below-median meme share of purchases",
    persona == "SocialMomentum"      ~ "Researches the buzz leader (top-2 leader_researched)",
    TRUE ~ ""
  ),
  pass = dplyr::case_when(
    persona == "DividendSeeker"      ~ meme_share <= 0.10,
    persona == "LotterySeeker"       ~ meme_share == max(fid$meme_share, na.rm = TRUE),
    persona == "PassiveFollower"     ~ trade_rate == min(fid$trade_rate, na.rm = TRUE),
    persona == "TechnicalTrader"     ~ mean_buy_momentum == max(fid$mean_buy_momentum, na.rm = TRUE),
    persona == "SkepticalContrarian" ~ meme_share <= stats::median(fid$meme_share, na.rm = TRUE),
    persona == "SocialMomentum"      ~ rank(-fid$leader_researched)[fid$persona == "SocialMomentum"] <= 2,
    TRUE ~ NA
  )
)

cat("\n--- Persona fidelity table (Appendix Table A4) ---\n")
print(as.data.frame(fid))
write_csv(fid, "table_a4_persona_fidelity.csv")
cat("Saved: table_a4_persona_fidelity.csv -> manuscript Appendix Table A4\n")

# ==============================================================================
# [R1-C] DISPLAYED-EDGE ALIGNMENT ANALYSIS  (works on the ORIGINAL data)
# ------------------------------------------------------------------------------
# The DEEP panel shows edge = 0.30 x buzz_adj x capped_momentum x 10000 - 2 x tc.
# This section reconstructs that deterministic quantity and reports how tightly
# decisions track its sign. Feeds manuscript Appendix Table A5 and the
# Section 6.4.1 bracketed concordance numbers.
# NOTE: buzz_adj as implemented in the reported run: SocialMomentum 1.2,
# LotterySeeker 1.1, TechnicalTrader 0.9, all others 1.0 (see Table A1 note).
# ==============================================================================

cat("\n==============================================================================\n")
cat("[R1-C] DISPLAYED-EDGE ALIGNMENT\n")
cat("==============================================================================\n")

VOLS <- c(AAPL = 0.025, NVDA = 0.045, AMC = 0.080, GME = 0.090)
buzz_adj_of <- function(p) dplyr::case_when(
  p == "SocialMomentum" ~ 1.2, p == "LotterySeeker" ~ 1.1,
  p == "TechnicalTrader" ~ 0.9, TRUE ~ 1.0)

edge_df <- data_trading %>%
  mutate(
    ref_ticker = dplyr::coalesce(ticker, best_researched_ticker),
    ref_mom    = dplyr::coalesce(momentum, best_momentum),
    vol        = VOLS[ref_ticker],
    m_cap      = sign(ref_mom) * pmin(abs(ref_mom), 3 * vol),
    tc_rec     = dplyr::case_when(
      !is.na(base_tc_bps) ~ as.numeric(base_tc_bps),
      is_treated_now & ref_ticker %in% meme_set ~ 15,
      TRUE ~ 5),
    edge_bps   = 0.30 * buzz_adj_of(persona) * m_cap * 10000 - 2 * tc_rec
  ) %>% filter(!is.na(edge_bps))

conc <- edge_df %>%
  mutate(decision = dplyr::case_when(buy_indicator == 1 ~ "buy",
                                     sell_indicator == 1 ~ "sell",
                                     TRUE ~ "hold")) %>%
  group_by(decision) %>%
  summarise(n = n(),
            share_pos_edge = mean(edge_bps > 0),
            share_neg_edge = mean(edge_bps <= 0),
            mean_edge_bps = mean(edge_bps), .groups = "drop")

cat("\n--- Decision vs displayed-edge sign ---\n")
print(as.data.frame(conc))
cat(sprintf("\nConcordance headline: %.1f%% of buys had positive displayed edge; %.1f%% of holds had non-positive displayed edge.\n",
    100 * conc$share_pos_edge[conc$decision == "buy"],
    100 * conc$share_neg_edge[conc$decision == "hold"]))

mech <- edge_df %>% filter(ref_ticker %in% meme_set) %>%
  group_by(is_treated_now) %>%
  summarise(mean_edge_bps = mean(edge_bps), n = n(), .groups = "drop")
cat("\n--- Mechanical edge shift on meme tickers (treated display vs not) ---\n")
print(as.data.frame(mech))

by_grp <- edge_df %>%
  mutate(decision = dplyr::case_when(buy_indicator == 1 ~ "buy",
                                     sell_indicator == 1 ~ "sell",
                                     TRUE ~ "hold")) %>%
  group_by(persona, is_treated_now, decision) %>%
  summarise(n = n(), share_pos_edge = mean(edge_bps > 0),
            mean_edge_bps = mean(edge_bps), .groups = "drop")
write_csv(by_grp, "table_a5_displayed_edge.csv")
cat("Saved: table_a5_displayed_edge.csv -> manuscript Appendix Table A5 + Section 6.4.1 numbers\n")

# ==============================================================================
# SAVE WORKSPACE & FINAL SUMMARY
# ==============================================================================

save.image("paper_strengthening_analyses.RData")
cat("\nSaved: paper_strengthening_analyses.RData\n")

cat("\n============================================================================\n")
cat("ALL ANALYSES COMPLETE\n")
cat(sprintf("Total runtime: %.1f minutes\n", (proc.time()[3]) / 60))
cat("============================================================================\n")
