# =============================================================================
# Attention_driven_trading_R1.R  -  THE EXPERIMENT
# -----------------------------------------------------------------------------
# Runs the LLM agent-based trading experiment for "Homo Silicus is
# Hyper-Rational" (JEIC Revision 1): 96 synthetic retail investors (6 persona
# prompts x 16 agents) trade 4 assets over 252 periods in a seeded market
# simulation, under a two-tier architecture (attention allocation:
# gpt-4o-mini; trading decision: gpt-4.1-mini; stateless calls, JSON mode).
#
# THE THREE RUNS REPORTED IN THE MANUSCRIPT
#   Original run  CONFIG$r1$enabled <- FALSE  (or design = "original"):
#                 staggered adoption (cohort 1 @ t=60, cohort 2 @ t=120,
#                 never-treated control); viral shock bundled with the
#                 5 -> 15 bps meme-ticker cost surge. All legacy code paths
#                 are preserved for exact reproduction.
#   Rerun A       design = "factorial" (script default): 2x2 viral/normal
#                 signals x 5/15 bps cost, single adoption t = 60, four arms
#                 of 24 randomized within persona strata; the cost surge is
#                 driven by the cost arm, not displayed buzz. Social Momentum
#                 is split across three prompt constructions (v1 original,
#                 v2 identity-only, v3 literature-based herding/FOMO),
#                 logged in sm_variant.
#   Rerun B       design = "original" + neutral_tickers = TRUE: identical to
#                 the original run except every model-visible ticker label is
#                 an invented symbol (VNTK/KRLO/ZMQR/QRLP); logs keep internal
#                 names (AAPL/NVDA/AMC/GME) so analysis code is unchanged.
#
# REQUIREMENTS AND COST
#   OPENAI_API_KEY in the environment. ~48,000 API calls per run
#   (2 x 96 agents x 252 periods), roughly USD 6 and several hours wall time
#   (parallel workers configurable). A disk cache (llm_cache_store/) makes
#   interrupted runs resumable.
#
# DETERMINISM
#   The market environment (price paths, signals, views, leaderboards) is
#   fully seeded (CONFIG$seed = 42) and exactly reproducible; LLM responses
#   are NOT deterministic (temperature 0.4), so re-running produces a new
#   realization of agent behavior on the identical market history.
#
# OUTPUTS (per run, in the working directory)
#   experiment_results_final.csv  main log: one attention row and one trading
#                                 row per agent-period, with arm columns
#                                 (attention_arm, cost_arm, sm_variant)
#   experiment_full_log.csv       superset log used by the worked example
#   treatment_assignment.csv      arm assignment with persona strata
#   experiment_audit.log          run audit trail
#   Behavioral interventions (forced trades) exist in the code base for pilot
#   use only and are disabled in all reported runs (is_forced_trade = FALSE
#   throughout the logs).
#
# All Revision-1 changes to the baseline script are marked with "[R1]".
# The unmodified baseline (Attention_driven_trading.R) is kept for diffing.
# =============================================================================

# ============================================================================
# ATTENTION-DRIVEN RETAIL TRADING EXPERIMENT 
# ============================================================================

# ----------------------------------------------------------------------------
# PACKAGE INSTALLATION AND LOADING
# ----------------------------------------------------------------------------

required_packages <- c(
  "R6","data.table","glue","jsonlite","httr2",
  "logger","lfe","did","ggplot2","fixest",
  "pwr","modelsummary","broom","knitr","gridExtra",
  "digest","future","future.apply","cachem", "memoise"
)

if (is.null(getOption("repos")) || getOption("repos")["CRAN"] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

missing_pkgs <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_pkgs)) {
  cat("Installing missing packages:", paste(missing_pkgs, collapse = ", "), "\n")
  install.packages(missing_pkgs, dependencies = TRUE)
}

invisible(lapply(required_packages, require, character.only = TRUE))

# Set up logging
log_appender(appender_file("experiment_audit.log"))
log_info("Experiment initialized at {Sys.time()}")

# ----------------------------------------------------------------------------
# PARALLEL PROCESSING SETUP
# ----------------------------------------------------------------------------

available_cores <- parallel::detectCores(logical = FALSE)
workers_to_use <- max(1, available_cores - 1)
plan(multisession, workers = workers_to_use)
log_info("Parallel processing enabled with {workers_to_use} workers")

# ----------------------------------------------------------------------------
# GLOBAL CACHE SETUP
# ----------------------------------------------------------------------------

CACHE_DIR <- "llm_cache_store"
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR)
DISK_CACHE <- cachem::cache_disk(dir = CACHE_DIR, max_size = 2 * 1024^3)

log_info("Global Disk Cache initialized at {CACHE_DIR}")

# ----------------------------------------------------------------------------
# HELPER FUNCTIONS
# ----------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    y
  } else {
    x
  }
}

clean_json_response <- function(raw_text) {
  cleaned <- gsub("^```json\\s*", "", raw_text)
  cleaned <- gsub("^```\\s*", "", cleaned)
  cleaned <- gsub("\\s*```$", "", cleaned)
  trimws(cleaned)
}

sanitize_rationale_text <- function(x, max_chars = NULL) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("")
  x <- as.character(x)
  
  # Strip common wrappers / markdown / code fences
  x <- gsub("```[rR]?\\s*|```", "", x)
  
  # Strip LaTeX math blocks, environments, etc.
  x <- gsub("\\$\\$[\\s\\S]*?\\$\\$", "", x, perl = TRUE)
  x <- gsub("\\\\\\[.*?\\\\\\]", "", x, perl = TRUE)
  x <- gsub("\\\\\\(.*?\\\\\\)", "", x, perl = TRUE)
  x <- gsub("\\\\begin\\{[^}]+\\}[\\s\\S]*?\\\\end\\{[^}]+\\}", "", x, perl = TRUE)
  
  # Remove any leading “json” label
  x <- gsub("^json\\s*", "", x, ignore.case = TRUE)
  
  x <- trimws(x)
  if (!is.null(max_chars) && nchar(x) > max_chars) {
    x <- substr(x, 1L, max_chars)
  }
  x
}

rationale_first_sentence <- function(txt, max_chars = 200) {
  if (is.null(txt) || length(txt) == 0) return("")
  txt <- as.character(txt[[1]])
  if (!nzchar(txt)) return("")
  
  parts <- unlist(strsplit(txt, "(?<=[.!?])\\s+", perl = TRUE))
  if (length(parts) == 0) return("")
  
  s <- trimws(parts[1])
  if (!nzchar(s)) return("")
  
  if (nchar(s) > max_chars) substr(s, 1, max_chars) else s
}

summarize_portfolio_reflection <- function(raw_txt) {
  if (is.null(raw_txt) || !nzchar(raw_txt)) return(NULL)
  
  json_candidate <- gsub("^json\\s*", "", trimws(raw_txt))
  if (!nzchar(json_candidate) || !jsonlite::validate(json_candidate)) return(NULL)
  
  parsed <- jsonlite::fromJSON(json_candidate)
  if (is.null(parsed$portfolio_reflection)) return(NULL)
  
  pf <- parsed$portfolio_reflection
  pieces <- character(0)
  
  for (tk in names(pf)) {
    entry <- pf[[tk]]
    
    # Handle both list-like and atomic entries safely
    impact <- ""
    if (is.list(entry) && !is.null(entry$impact)) {
      impact <- entry$impact %||% ""
    } else if (is.character(entry) && length(entry) > 0) {
      impact <- entry[[1]] %||% ""
    }
    
    impact <- scalar_chr(impact)
    if (nzchar(impact)) {
      pieces <- c(pieces, sprintf("%s: %s", tk, impact))
    }
  }
  
  decision_txt <- ""
  if (!is.null(parsed$trading_decision) && is.list(parsed$trading_decision)) {
    act <- parsed$trading_decision$action %||% ""
    rsn <- parsed$trading_decision$reason %||% ""
    if (nzchar(act) || nzchar(rsn)) {
      decision_txt <- sprintf(
        "Decision: %s%s",
        act,
        if (nzchar(rsn)) paste0(" – ", rsn) else ""
      )
    }
  }
  
  out <- c("Portfolio reflection:", pieces)
  if (nzchar(decision_txt)) out <- c(out, decision_txt)
  
  summary_txt <- paste(out, collapse = " ")
  if (!nzchar(summary_txt)) return(NULL)
  
  summary_txt
}

scalar_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  if (all(is.na(x))) return("")
  as.character(x[[1]])
}

drop_nulls <- function(x) Filter(Negate(is.null), x)

tc_bps <- function(sym, buzz_level, rank) {
  base <- 5
  is_viral <- (!is.na(rank) && rank <= 2) || 
    (!is.null(buzz_level) && buzz_level %in% c("high", "viral", "VIRAL"))
  surge <- if (is_viral) 10 else 0
  base + surge
}

persona_buzz_k <- function(base_k, persona) {
  adj <- switch(persona,
                "SocialMomentum"       = 1.20,
                "LotterySeeker"        = 1.10,
                "TechnicalTrader"      = 0.90,
                "DividendSeeker"       = 0.80,
                "PassiveFollower"      = 0.95,
                "SkepticalContrarian"  = 0.85,
                1.0)
  base_k * adj
}

extract_first_json_object <- function(x) {
  if (is.null(x) || !nzchar(x)) return("")
  start <- regexpr("\\{", x)
  if (start[1] == -1) return("")
  
  txt <- substring(x, start[1])
  chars <- strsplit(txt, "")[[1]]
  cnt <- 0
  end_pos <- -1
  
  for (i in seq_along(chars)) {
    if (chars[i] == "{") cnt <- cnt + 1
    if (chars[i] == "}") {
      cnt <- cnt - 1
      if (cnt == 0) {
        end_pos <- i
        break
      }
    }
  }
  
  if (end_pos != -1) {
    return(substring(txt, 1, end_pos))
  }
  ""
}

summarize_portfolio_reflection_parsed <- function(parsed) {
  if (is.null(parsed) || is.null(parsed$portfolio_reflection)) return(NULL)
  
  pf <- parsed$portfolio_reflection
  pieces <- character(0)
  
  for (tk in names(pf)) {
    entry <- pf[[tk]]
    
    impact <- ""
    if (is.list(entry) && !is.null(entry$impact)) {
      impact <- entry$impact %||% ""
    } else if (is.character(entry) && length(entry) > 0) {
      impact <- entry[[1]] %||% ""
    }
    
    impact <- scalar_chr(impact)
    if (nzchar(impact)) {
      pieces <- c(pieces, sprintf("%s: %s", tk, impact))
    }
  }
  
  decision_txt <- ""
  if (!is.null(parsed$trading_decision) && is.list(parsed$trading_decision)) {
    act <- parsed$trading_decision$action %||% ""
    rsn <- parsed$trading_decision$reason %||% ""
    if (nzchar(act) || nzchar(rsn)) {
      decision_txt <- sprintf(
        "Decision: %s%s",
        act,
        if (nzchar(rsn)) paste0(" – ", rsn) else ""
      )
    }
  }
  
  out <- c("Portfolio reflection:", pieces)
  if (nzchar(decision_txt)) out <- c(out, decision_txt)
  
  summary_txt <- paste(out, collapse = " ")
  if (!nzchar(summary_txt)) return(NULL)
  
  summary_txt
}

valid_json_or_fallback <- function(txt, fallback) {
  cand <- extract_first_json_object(txt)
  if (nzchar(cand) && isTRUE(jsonlite::validate(cand))) return(cand)
  fallback
}

extract_responses_text <- function(out) {
  if (!is.null(out$output_text) && nchar(out$output_text) > 0) return(out$output_text)
  if (!is.null(out$choices) && length(out$choices) > 0) return(out$choices[[1]]$message$content)
  "" 
}


# Momentum Calculation
calculate_momentum_with_shrinkage <- function(price_history, volatility_prior, target_periods = 5) {
  n_obs <- length(price_history)
  if (n_obs <= 1) {
    initial_prior <- rnorm(1, 0, volatility_prior * 0.5)
    return(list(
      momentum = initial_prior, 
      std_error = volatility_prior * 2,
      shrinkage_factor = 0.9,
      n_obs = 0, 
      confidence = 0.1
    ))
  }
  available_periods <- n_obs - 1
  log_return_total <- log(price_history[n_obs] / price_history[1])
  raw_momentum_per_period <- log_return_total / available_periods
  shrinkage_weight <- available_periods / (available_periods + target_periods)
  shrunk_momentum_per_period <- shrinkage_weight * raw_momentum_per_period
  
  list(
    momentum = shrunk_momentum_per_period, 
    std_error = volatility_prior / sqrt(available_periods),
    shrinkage_factor = 1 - shrinkage_weight,
    n_obs = available_periods,
    confidence = shrinkage_weight
  )
}

calculate_signal_strength <- function(momentum, volatility, t, momentum_confidence = 1) {
  if (is.na(momentum) || is.na(volatility)) {
    return(list(z_score=NA_real_, strength="none", multiplier=0, 
                direction="none", trade_signal="hold", confidence_adj=0))
  }
  if (volatility <= 1e-10) volatility <- 1e-6
  
  z_score <- momentum / volatility
  abs_z <- abs(z_score)
  
  if (abs_z > 0.50) { strength <- "strong"; base_multiplier <- 1.0 }
  else if (abs_z > 0.25) { strength <- "medium"; base_multiplier <- 0.75 }
  else if (abs_z > 0.10) { strength <- "weak"; base_multiplier <- 0.5 }
  else { strength <- "noise"; base_multiplier <- 0.25 }
  
  if (momentum > 0.001) { direction <- "up"; trade_signal <- "buy" }
  else if (momentum < -0.001) { direction <- "down"; trade_signal <- "sell" }
  else { direction <- "flat"; trade_signal <- "hold" }
  
  confidence_adj <- 0.3 + 0.7 * momentum_confidence
  multiplier <- base_multiplier * confidence_adj
  
  list(
    z_score = z_score, strength = strength, multiplier = multiplier,
    direction = direction, trade_signal = trade_signal, confidence_adj = confidence_adj
  )
}

# ----------------------------------------------------------------------------
# ATOMIC SAVE HELPERS 
# ----------------------------------------------------------------------------

save_atomic_rds <- function(object, file) {
  temp_file <- paste0(file, ".tmp_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
  tryCatch({
    saveRDS(object, file = temp_file)
    if (file.exists(file)) unlink(file)
    file.rename(from = temp_file, to = file)
  }, error = function(e) {
    if (file.exists(temp_file)) unlink(temp_file)
    stop("Failed to save RDS atomically: ", e$message)
  })
}

write_atomic_csv <- function(dt, file) {
  temp_file <- paste0(file, ".tmp_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
  tryCatch({
    data.table::fwrite(dt, file = temp_file)
    if (file.exists(file)) unlink(file)
    file.rename(from = temp_file, to = file)
  }, error = function(e) {
    if (file.exists(temp_file)) unlink(temp_file)
    stop("Failed to write CSV atomically: ", e$message)
  })
}

# ============================================================================
# SPECIFICATION VALIDATION FUNCTIONS
# ============================================================================

validate_specification_a <- function(log_dt) {
  cat("\n=== VALIDATING SPECIFICATION A ===\n")
  
  trades_without_attention <- log_dt[
    specification == "A" & 
      trade_indicator == 1 & 
      attention_allocated == "ignore"
  ]
  
  test1_pass <- nrow(trades_without_attention) == 0
  cat(sprintf("Test 1 - No trades without attention: %s\n", 
              ifelse(test1_pass, "✓ PASS", "✗ FAIL")))
  
  if (!test1_pass) {
    cat(sprintf("  Found %d trades without attention!\n", nrow(trades_without_attention)))
  }
  
  blocks <- log_dt[specification == "A", .(
    buy_blocks = sum(action == "buy" & blocked_reason == "no_attention_symmetric"),
    sell_blocks = sum(action == "sell" & blocked_reason == "no_attention_symmetric")
  )]
  
  cat(sprintf("Test 2 - Symmetric blocking: Buy blocks=%d, Sell blocks=%d\n",
              blocks$buy_blocks, blocks$sell_blocks))
  
  compliance <- log_dt[specification == "A" & trade_indicator == 1, 
                       mean(attention_allocated %in% c("deep", "quick"))]
  
  test3_pass <- compliance >= 0.99
  cat(sprintf("Test 3 - Attention compliance: %.1f%% %s\n",
              compliance * 100, ifelse(test3_pass, "✓ PASS", "✗ FAIL")))
  
  all_pass <- test1_pass && test3_pass
  cat(sprintf("\n%s\n", ifelse(all_pass, 
                               "✓ SPECIFICATION A VALIDATED", 
                               "⚠ SPECIFICATION A HAS ISSUES")))
  
  invisible(all_pass)
}

validate_specification_b <- function(log_dt) {
  cat("\n=== VALIDATING SPECIFICATION B ===\n")
  
  attention_blocks <- sum(log_dt[specification == "B"]$blocked_reason %in% 
                            c("no_attention", "no_attention_symmetric"), na.rm = TRUE)
  
  test1_pass <- attention_blocks == 0
  cat(sprintf("Test 1 - No attention blocking: %s\n",
              ifelse(test1_pass, "✓ PASS", "✗ FAIL")))
  
  if (!test1_pass) {
    cat(sprintf("  Found %d blocked trades!\n", attention_blocks))
  }
  
  trades_no_attention <- log_dt[
    specification == "B" & 
      trade_indicator == 1 & 
      attention_allocated == "ignore",
    .N
  ]
  
  test2_pass <- trades_no_attention > 0
  cat(sprintf("Test 2 - Trades without attention: %d %s\n",
              trades_no_attention, ifelse(test2_pass, "✓ PASS", "✗ FAIL")))
  
  if (nrow(log_dt[specification == "B" & stage == "trading"]) > 50) {
    trade_attn_table <- table(
      log_dt[specification == "B" & stage == "trading"]$trade_indicator,
      log_dt[specification == "B" & stage == "trading"]$attention_allocated != "ignore"
    )
    
    chisq <- tryCatch(chisq.test(trade_attn_table), error = function(e) NULL)
    
    if (!is.null(chisq)) {
      test3_pass <- chisq$p.value < 0.10
      cat(sprintf("Test 3 - Attention-trade correlation: p=%.4f %s\n",
                  chisq$p.value, ifelse(test3_pass, "✓ PASS", "→ WEAK")))
    }
  }
  
  all_pass <- test1_pass && test2_pass
  cat(sprintf("\n%s\n", ifelse(all_pass,
                               "✓ SPECIFICATION B VALIDATED",
                               "⚠ SPECIFICATION B HAS ISSUES")))
  
  invisible(all_pass)
}

validate_specification_c <- function(log_dt) {
  cat("\n=== VALIDATING SPECIFICATION C ===\n")
  
  trades_with_costs <- log_dt[
    specification == "C" & 
      trade_indicator == 1 & 
      !is.na(search_cost_penalty_bps),
    .N
  ]
  
  total_trades <- log_dt[specification == "C" & trade_indicator == 1, .N]
  
  test1_pass <- trades_with_costs == total_trades
  cat(sprintf("Test 1 - Search costs applied: %d/%d trades %s\n",
              trades_with_costs, total_trades, 
              ifelse(test1_pass, "✓ PASS", "✗ FAIL")))
  
  avg_costs <- log_dt[specification == "C" & trade_indicator == 1, .(
    avg_tc = mean(total_tc_bps, na.rm = TRUE)
  ), by = attention_allocated][order(attention_allocated)]
  
  if (nrow(avg_costs) >= 2) {
    deep_cost <- avg_costs[attention_allocated == "deep", avg_tc]
    ignore_cost <- avg_costs[attention_allocated == "ignore", avg_tc]
    
    test2_pass <- ignore_cost > deep_cost
    cat(sprintf("Test 2 - Cost ordering: deep=%.1f, ignore=%.1f %s\n",
                deep_cost, ignore_cost, ifelse(test2_pass, "✓ PASS", "✗ FAIL")))
  } else {
    test2_pass <- FALSE
    cat("Test 2 - Cost ordering: ⚠ INSUFFICIENT DATA\n")
  }
  
  total_search_costs <- sum(log_dt[specification == "C" & trade_indicator == 1]$search_cost_paid,
                            na.rm = TRUE)
  
  test3_pass <- total_search_costs > 0
  cat(sprintf("Test 3 - Search costs paid: $%.2f %s\n",
              total_search_costs, ifelse(test3_pass, "✓ PASS", "✗ FAIL")))
  
  total_savings <- sum(log_dt[specification == "C" & trade_indicator == 1]$search_cost_saved,
                       na.rm = TRUE)
  
  test4_pass <- total_savings > 0
  cat(sprintf("Test 4 - Search costs saved: $%.2f %s\n",
              total_savings, ifelse(test4_pass, "✓ PASS", "✗ FAIL")))
  
  all_pass <- test1_pass && test2_pass && test3_pass && test4_pass
  cat(sprintf("\n%s\n", ifelse(all_pass,
                               "✓ SPECIFICATION C VALIDATED",
                               "⚠ SPECIFICATION C HAS ISSUES")))
  
  invisible(all_pass)
}

run_specification_validation <- function(log_dt) {
  spec <- CONFIG$attention_specification %||% "original"
  if (spec == "A") validate_specification_a(log_dt)
  else if (spec == "B") validate_specification_b(log_dt)
  else if (spec == "C") validate_specification_c(log_dt)
}

# ----------------------------------------------------------------------------
# TEST MODE TOGGLE
# ----------------------------------------------------------------------------

TEST_MODE <- FALSE  # Set to TRUE for rapid testing with smaller sample
if (TEST_MODE) {
  cat("\n")
  cat("================================================================================\n")
  cat("*** RUNNING IN TEST MODE ***\n")
  cat("Small sample (12 agents, 30 periods) for rapid testing\n")
  cat("Set TEST_MODE = FALSE for full experiment (96 agents, 252 periods)\n")
  cat("================================================================================\n\n")
}

# ----------------------------------------------------------------------------
# CONFIGURATION
# ----------------------------------------------------------------------------

CONFIG <- list(
  n_agents  = if (TEST_MODE) 12 else 96,
  n_periods = if (TEST_MODE) 30 else 252,
  attention_specification = "original", # Change "original" to "A", "B", or "C" for varing spcifications
  
  search_cost_penalty_bps = list(ignore = 50, quick = 10, deep = 0),
  
  attention_budget     = 100,
  initial_cash         = 10000,
  transaction_cost_bps = 5,
  min_edge_bps         = 10,    
  expected_return_dampen_k = 0.30,
  max_trade_value      = 3000,    
  max_shares_per_trade = NULL,    
  
  llm = list(
    model             = "gpt-4o-mini",
    trading_model     = "gpt-4.1-mini",
    endpoint          = "chat/completions",
    max_output_tokens = 2048,  
    reasoning_effort  = "medium", # Not used for GPT-4 models
    temperature       = 0.4,
    max_parse_retries = 3L,
    use_json_mode     = TRUE
  ),
  
  seed = 42,
  
  drift_per_period = 0.0003,
  volatility = list(AAPL = 0.025, NVDA = 0.045, AMC = 0.080, GME = 0.090),
  initial_prices = list(AAPL = 150, NVDA = 450, AMC = 12, GME = 25),
  
  cohort1_treatment_period = if (TEST_MODE) 10 else 60,
  cohort2_treatment_period = if (TEST_MODE) 15 else 120,
  
  shock_control = list(
    mentions_min = 150, mentions_max = 500,
    rank_min = 45, rank_max = 67,
    buyers_min = 380, buyers_max = 450,
    buzz = "low"
  ),
  shock_treatment = list(
    mentions_min = 45000, mentions_max = 60000,
    rank_min = 1, rank_max = 1,
    buyers_min = 90000, buyers_max = 120000,
    buzz = "viral"
  ),
  shock_baseline = list(
    mentions_min = 800, mentions_max = 1200,
    rank_min = 8, rank_max = 15,
    buyers_min = 2000, buyers_max = 3000,
    buzz = "moderate"
  ),
  
  assumed_icc         = 0.30,
  target_effect_size  = 0.30,
  alpha               = 0.05,
  
  dose_response = list(
    enabled = FALSE,
    intensity_levels = list(
      control = list(
        multiplier     = 1,
        mentions_range = c(150, 500),
        rank_range     = c(45, 67),
        buzz           = "low"
      ),
      low_dose = list(
        multiplier     = 10,
        mentions_range = c(1500, 5000),
        rank_range     = c(10, 20),
        buzz           = "moderate"
      ),
      medium_dose = list(
        multiplier     = 50,
        mentions_range = c(7500, 25000),
        rank_range     = c(3, 8),
        buzz           = "high"
      ),
      high_dose = list(
        multiplier     = 100,
        mentions_range = c(15000, 50000),
        rank_range     = c(1, 3),
        buzz           = "high"
      ),
      extreme_dose = list(
        multiplier     = 200,
        mentions_range = c(30000, 100000),
        rank_range     = c(1, 2),
        buzz           = "viral"
      ),
      original = list(
        multiplier     = 400,
        mentions_range = c(45000, 60000),
        rank_range     = c(1, 1),
        buzz           = "viral"
      )
    )
  )
)

set.seed(CONFIG$seed)

# ============================================================================
# INTERVENTION TOGGLE CONFIGURATION
# ============================================================================

CONFIG$interventions <- list(
  early_buy_encouragement         = FALSE,
  forced_trading_after_inactivity = FALSE,
  auto_attention_to_shocked       = FALSE,
  any_intervention_enabled        = FALSE
)

intervention_enabled <- function(type) {
  if (!CONFIG$interventions$any_intervention_enabled) return(FALSE)
  CONFIG$interventions[[type]] %||% FALSE
}

# ============================================================================
# [R1] REVISION 1 CONFIGURATION - factorial (Rerun A) & neutral tickers (Rerun B)
# ============================================================================
CONFIG$r1 <- list(
  enabled = TRUE,
  design  = "original",     # "factorial" = Rerun A. Set "original" (or enabled = FALSE) to reproduce the submitted design.
  adoption_period = if (TEST_MODE) 10 else 60,  # single adoption time for all treated arms; auto-shortens in TEST_MODE (30-period horizon)
  neutral_tickers = TRUE,   # TRUE = Rerun B (model sees invented names; logs keep internal names)
  sm_prompt_variants = TRUE, # randomize SocialMomentum agents across 3 prompt constructions (Major 5)
  tickers_internal        = c("AAPL","NVDA","AMC","GME"),
  tickers_display_neutral = c("VNTK","KRLO","ZMQR","QRLP")  # verified against listings 2026-07-09
)
r1_factorial <- function() isTRUE(CONFIG$r1$enabled) && identical(CONFIG$r1$design, "factorial")

# [R1] Display-name layer: the model only ever sees display names; all internal state,
# logs, and analysis keep AAPL/NVDA/AMC/GME.
TICKER_DISP <- if (isTRUE(CONFIG$r1$neutral_tickers)) {
  stats::setNames(CONFIG$r1$tickers_display_neutral, CONFIG$r1$tickers_internal)
} else stats::setNames(CONFIG$r1$tickers_internal, CONFIG$r1$tickers_internal)
TICKER_FROM_DISP <- stats::setNames(names(TICKER_DISP), unname(TICKER_DISP))
disp <- function(x) { m <- TICKER_DISP[x]; ifelse(is.na(m), x, unname(m)) }
TICKER_ENUM_DISP <- paste0('\\"', unname(TICKER_DISP), '\\"', collapse = "|")
PLAIN_ENUM_DISP  <- paste(unname(TICKER_DISP), collapse = "|")
ALLOC_LINES_DISP <- paste0('\\"', unname(TICKER_DISP), '\\": \\"deep/quick/ignore\\"', collapse = ",\n    ")
ALLOC_PIPE_DISP  <- paste0('\\"', unname(TICKER_DISP), '\\":\\"deep|quick|ignore\\"', collapse = ",")

# [R1] Arm-aware transaction cost: the 10 bps surge is a property of the agent's cost
# arm in post-adoption periods on the meme tickers - independent of displayed buzz.
tc_bps_r1 <- function(sym, is_post, cost_arm) {
  meme <- CONFIG$r1$tickers_internal[3:4]
  if (identical(cost_arm, "high") && isTRUE(is_post) && sym %in% meme) return(15)
  5
}

# [R1] Social Momentum prompt-construction variants (Major 5 validation).
# v1 = original; v2 = same identity but Action Priority suppressed at the trading
# stage; v3 = literature-adapted herding/FOMO wording (distinct construction).
SM_VARIANT_PROMPTS <- list(
  v1 = "You are a Robinhood user who trades based on social signals and crowd behavior. You pay close attention to what other retail traders are buying. You experience FOMO.",
  v2 = "You are a Robinhood user who trades based on social signals and crowd behavior. You pay close attention to what other retail traders are buying. You experience FOMO.",
  v3 = "You are a retail trader who follows the herd. When you see many other investors piling into a stock - high message volume, a top leaderboard rank, surging buyer counts - you feel a strong fear of missing out and an urge to join them. You usually buy what is popular right now rather than analyzing fundamentals, because moving with the crowd feels safer and more exciting."
)
SM_VARIANT_ASSIGN <- new.env(parent = emptyenv())
sm_variant_of <- function(agent_id) {
  v <- SM_VARIANT_ASSIGN[[agent_id]]
  if (is.null(v)) NA_character_ else v
}

log_info("Configuration loaded and validated")

# CONFIGURATION SUMMARY (unchanged from original, omitted for brevity)
cat("================================================================================\n")
cat("CONFIGURATION SUMMARY\n")
cat("================================================================================\n")

cat("Sample:\n")
cat(sprintf("  Agents: %d\n", CONFIG$n_agents))
cat(sprintf("  Periods: %d\n", CONFIG$n_periods))
cat(sprintf("  Total observations: %s\n\n", 
            format(CONFIG$n_agents * CONFIG$n_periods * 3, big.mark = ",")))

cat("Treatment Parameters:\n")
cat(sprintf("  Attention shock magnitude: %.1fx to %.1fx\n",
            CONFIG$shock_treatment$mentions_min / CONFIG$shock_control$mentions_max,
            CONFIG$shock_treatment$mentions_max / CONFIG$shock_control$mentions_min))
cat(sprintf("  Control mentions: %d-%d\n", 
            CONFIG$shock_control$mentions_min, CONFIG$shock_control$mentions_max))
cat(sprintf("  Treatment mentions: %s-%s\n\n",
            format(CONFIG$shock_treatment$mentions_min, big.mark = ","),
            format(CONFIG$shock_treatment$mentions_max, big.mark = ",")))

cat("Volatility Calibration:\n")
for (ticker in c("AAPL", "NVDA", "AMC", "GME")) {
  vol_annual <- CONFIG$volatility[[ticker]] * sqrt(252) * 100
  cat(sprintf("  %s: %.3f per period → %.1f%% annualized\n",
              ticker, CONFIG$volatility[[ticker]], vol_annual))
}
cat("\n")

cat("LLM Settings:\n")
cat(sprintf("  Fast model (attention/info): %s\n", CONFIG$llm$model))
cat(sprintf("  Slow model (trading): %s\n", CONFIG$llm$trading_model))
cat(sprintf("  Temperature: %s\n",
            if (is.null(CONFIG$llm$temperature)) "n/a" else sprintf("%.2f", CONFIG$llm$temperature)))
cat(sprintf("  Max tokens: %d\n\n", CONFIG$llm$max_output_tokens))


cat("Treatment Timing:\n")
cat(sprintf("  Cohort 1 treated: t = %d\n", CONFIG$cohort1_treatment_period))
cat(sprintf("  Cohort 2 treated: t = %d\n", CONFIG$cohort2_treatment_period))
cat("  Cohort 3: Never treated (control)\n\n")

cat("================================================================================\n\n")

experiment_metadata <- list(
  timestamp = Sys.time(),
  r_version = R.version.string,
  config = CONFIG,
  features = c(
    "robust_data_handling",
    "clean_signal_processing",
    "regime_switching_volatility_added",
    "accurate_cost_basis_tracking",
    "json_parsing_protection",
    "shared_view_optimization",
    "intervention_toggle_system",
    "balanced_randomization",
    "dose_response_capability"
  ),
  git_commit = tryCatch(
    system("git rev-parse HEAD", intern = TRUE),
    error = function(e) "not_in_git"
  )
)
saveRDS(experiment_metadata, "experiment_metadata_corrected.rds")

# --------------------------------------
# LLM CLIENT 
# --------------------------------------

LLMClient <- R6::R6Class(
  "LLMClient",
  public = list(
    model = NULL,
    temperature = NULL,
    api_key = NULL,
    call_count = 0,
    total_tokens = 0,
    failed_calls = 0,
    disk_cache = NULL,
    
    initialize = function(
    model = CONFIG$llm$model,
    temperature = CONFIG$llm$temperature,
    api_key = Sys.getenv("OPENAI_API_KEY"),
    disk_cache = DISK_CACHE
    ) {
      if (api_key == "") stop("OPENAI_API_KEY environment variable not set")
      self$model       <- model
      self$temperature <- temperature
      self$api_key     <- api_key
      self$disk_cache  <- disk_cache
    },
    
    chat = function(
    system,
    user,
    max_retries = 3,
    specific_cache_key = NULL,
    response_format = NULL
    ) {
      self$call_count <- self$call_count + 1
      sys_txt  <- scalar_chr(system)
      user_txt <- scalar_chr(user)
      
      if (!is.null(specific_cache_key)) {
        specific_cache_key <- gsub("[^a-z0-9]", "", tolower(specific_cache_key))
        if (nchar(specific_cache_key) >= 80) {   # [R1] cachem requires keys < 80 chars; arm fields lengthened some keys
          specific_cache_key <- paste0("k", digest::digest(specific_cache_key, algo = "md5"))
        }
        cached_resp <- self$disk_cache$get(specific_cache_key)
        if (!is.null(cached_resp) && !inherits(cached_resp, "key_missing")) {
          return(cached_resp)
        }
      }
      
      local_max_tokens <- max(1024, CONFIG$llm$max_output_tokens)
      
      # Enhanced: route JSON prompts into JSON mode when appropriate
      effective_response_format <- response_format
      if (is.null(effective_response_format) && isTRUE(CONFIG$llm$use_json_mode)) {
        json_expected <- grepl(
          "(?i)(respond|response|output|return)[^\\n]{0,120}json",
          paste(sys_txt, user_txt),
          perl = TRUE
        )
        if (json_expected) {
          effective_response_format <- list(type = "json_object")
        }
      }
      
      body <- drop_nulls(list(
        model = self$model,
        messages = list(
          list(role = "system", content = sys_txt),
          list(role = "user",   content = user_txt)
        ),
        max_tokens      = local_max_tokens,
        temperature     = self$temperature,
        response_format = effective_response_format
      ))
      
      req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
        httr2::req_headers(Authorization = paste("Bearer", self$api_key)) |>
        httr2::req_body_json(body) |>
        httr2::req_retry(max_tries = max_retries, backoff = function(i) 2^i) |>
        httr2::req_timeout(120)
      
      resp <- NULL
      for (attempt in 1:max_retries) {
        resp <- tryCatch(
          httr2::req_perform(req),
          error = function(e) {
            if (attempt < max_retries) {
              Sys.sleep(2^attempt)
              return(NULL)
            } else {
              self$failed_calls <- self$failed_calls + 1
              return(NULL)
            }
          }
        )
        if (!is.null(resp)) break
      }
      
      if (is.null(resp)) {
        warning("API call failed after retries; returning hold decision JSON.")
        return('{"action":"hold","rationale":"api-error"}')
      }
      
      out <- httr2::resp_body_json(resp, simplifyVector = FALSE)
      
      usage_tok <- tryCatch(
        {
          if (!is.null(out$usage)) out$usage$total_tokens else 0
        },
        error = function(e) 0
      )
      self$total_tokens <- self$total_tokens + usage_tok
      
      txt <- extract_responses_text(out)
      
      json_expected_legacy <- grepl("(?i)output\\s+json", paste(sys_txt, user_txt), perl = TRUE)
      if (json_expected_legacy && is.null(effective_response_format)) {
        txt <- valid_json_or_fallback(txt, '{"action":"hold"}')
      }
      
      if (!is.null(specific_cache_key) && nchar(txt) > 0) {
        self$disk_cache$set(specific_cache_key, txt)
      }
      
      txt
    }
  )
)

# ----------------------------------------------------------------------------
# AGENT CLASS 
# ----------------------------------------------------------------------------

Agent <- R6::R6Class(
  "Agent",
  public = list(
    id = NULL, persona = NULL, memory = NULL, llm = NULL,
    attention_budget = NULL, attention_remaining = NULL,
    portfolio = NULL, cash = NULL,
    first_purchase_periods = NULL, last_trade_t = NULL, last_buy_ticker = NULL,
    
    initialize = function(id, persona, llm,
                          attention_budget = CONFIG$attention_budget,
                          initial_cash = CONFIG$initial_cash) {
      self$id <- id
      self$persona <- persona
      self$llm <- llm
      self$memory <- list()
      self$attention_budget <- attention_budget
      self$attention_remaining <- attention_budget
      self$cash <- initial_cash
      
      self$portfolio <- data.table(
        ticker = character(),
        shares = numeric(),
        cost_basis = numeric()
      )
      self$first_purchase_periods <- list()
      self$last_trade_t <- NA_integer_
      self$last_buy_ticker <- list()
    },
    
    reset_attention = function() { self$attention_remaining <- self$attention_budget },
    
    spend_attention = function(amount) {
      if (amount > self$attention_remaining) return(FALSE)
      self$attention_remaining <- self$attention_remaining - amount
      TRUE
    },
    
    pay_search_cost = function(amount_dollars) {
      if (amount_dollars <= 0) return(TRUE)
      if (self$cash < amount_dollars) return(FALSE)
      self$cash <- self$cash - amount_dollars
      TRUE
    },
    
    execute_trade = function(action, ticker, shares, price, current_t,
                             tc_bps_override = NULL) {
      tc_multiplier <- ((tc_bps_override %||% CONFIG$transaction_cost_bps) / 10000)
      fee <- shares * price * tc_multiplier
      
      if (action == "buy") {
        total_cost <- (shares * price) + fee
        if (total_cost > self$cash) {
          return(list(success = FALSE, reason = "insufficient_cash"))
        }
        self$cash <- self$cash - total_cost
        
        if (is.null(self$first_purchase_periods[[ticker]])) {
          self$first_purchase_periods[[ticker]] <- current_t
        }
        
        if (ticker %in% self$portfolio$ticker) {
          idx <- which(self$portfolio$ticker == ticker)
          old_shares <- self$portfolio$shares[idx]
          old_basis  <- self$portfolio$cost_basis[idx]
          new_shares <- old_shares + shares
          current_total_spend <- old_shares * old_basis
          new_total_spend <- total_cost
          new_basis <- (current_total_spend + new_total_spend) / new_shares
          self$portfolio[idx, `:=`(shares = new_shares, cost_basis = new_basis)]
        } else {
          self$portfolio <- rbind(
            self$portfolio,
            data.table(ticker = ticker, shares = shares, cost_basis = total_cost / shares)
          )
        }
        
        return(list(success = TRUE, cash_spent = total_cost))
        
      } else if (action == "sell") {
        if (!ticker %in% self$portfolio$ticker) {
          return(list(success = FALSE, reason = "not_owned"))
        }
        idx <- which(self$portfolio$ticker == ticker)
        owned <- self$portfolio$shares[idx]
        if (shares > owned) return(list(success = FALSE, reason = "insufficient_shares"))
        
        proceeds <- (shares * price) - fee
        cost_basis <- self$portfolio$cost_basis[idx]
        realized_pnl <- (price - cost_basis) * shares - fee
        
        self$cash <- self$cash + proceeds
        new_shares <- owned - shares
        if (new_shares < 0.01) {
          self$portfolio <- self$portfolio[-idx]
        } else {
          self$portfolio[idx, shares := new_shares]
        }
        
        return(list(success = TRUE, proceeds = proceeds, realized_pnl = realized_pnl))
      }
      
      list(success = FALSE, reason = "invalid_action")
    },
    
    get_portfolio_value = function(prices) {
      if (nrow(self$portfolio) == 0) return(self$cash)
      holdings_value <- sum(sapply(1:nrow(self$portfolio), function(i) {
        self$portfolio$shares[i] * (prices[[self$portfolio$ticker[i]]] %||% 0)
      }))
      self$cash + holdings_value
    },
    
    recall_salient = function(current_t, n = 3) {
      if (length(self$memory) == 0) return(list())
      tail(self$memory, n)
    },
    
    remember = function(entry) {
      self$memory[[length(self$memory) + 1]] <- entry
    },
    
    act = function(game_rules, history_txt, instr_object) {
      prompt_text <- if (is.list(instr_object)) instr_object$text else as.character(instr_object)
      cache_key   <- if (is.list(instr_object)) instr_object$cache_key else NULL
      
      system <- self$persona$system_prompt
      user <- paste(game_rules, history_txt, prompt_text, sep = "\n\n")
      user <- paste0(user, "\n\nIMPORTANT: Act strictly according to your Persona: ", self$persona$name)
      
      response <- self$llm$chat(system, user, specific_cache_key = cache_key)
      response
    }
  )
)

# ----------------------------------------------------------------------------
# GAME ENGINE 
# ----------------------------------------------------------------------------

Game <- R6::R6Class(
  "Game",
  public = list(
    name           = NULL,
    rules          = NULL,
    stages         = NULL,
    agents         = NULL,
    matching       = NULL,
    periods        = NULL,
    log            = NULL,
    history_writer = NULL,
    seed           = NULL,
    gc_interval    = NULL,
    
    initialize = function(
    name,
    rules,
    stages,
    agents,
    matching,
    periods,
    history_writer,
    seed = CONFIG$seed,
    gc_interval = 100
    ) {
      self$name           <- name
      self$rules          <- rules
      self$stages         <- stages
      self$agents         <- agents
      self$matching       <- matching
      self$periods        <- periods
      self$history_writer <- history_writer
      self$log            <- data.table()
      self$seed           <- seed
      self$gc_interval    <- gc_interval
      set.seed(self$seed)
    },
    
    run = function(save_interval = 10) {
      cat("\nStarting experiment (Safe Serialization Enabled)...\n")
      start_time <- Sys.time()
      
      total_capacity <- self$periods *
        length(self$agents) *
        length(self$stages) * 1.1
      
      results_accumulator <- vector("list", length = total_capacity)
      result_idx <- 1
      
      for (t in seq_len(self$periods)) {
        if (t %% self$gc_interval == 0) gc(verbose = FALSE)
        
        # Reset attention each period
        for (a in self$agents) a$reset_attention()
        
        # Reproducible matching given t
        set.seed(self$seed + t)
        pairs <- self$matching(self$agents, t)
        
        # Iterate through stages within the period
        for (s_idx in seq_along(self$stages)) {
          s <- self$stages[[s_idx]]
          
          # --- TWO-MODEL ROUTING (System 1 vs System 2) ---
          # Non-trading stages → fast model; trading stage → slow model.
          stage_llm <- if (inherits(s, "StageTradingDecision")) llm_slow else llm_fast
          # -------------------------------------------------
          
          # Snapshot state needed for this stage
          snapshot_inputs <- lapply(seq_along(pairs), function(i) {
            p <- pairs[[i]]
            a <- p$agent
            
            lite_agent_state <- list(
              id                  = a$id,
              cash                = a$cash,
              portfolio           = as.data.frame(a$portfolio),
              attention_remaining = a$attention_remaining,
              last_trade_t        = a$last_trade_t,
              last_buy_ticker     = a$last_buy_ticker,
              recent_memory       = a$recall_salient(t, 3),
              persona             = a$persona
            )
            
            list(
              id           = i,
              lite_agent   = lite_agent_state,
              pair_context = p,
              history_text = self$history_writer(a, t, pair = p),
              instruction  = s$compose(a, t, pair = p),
              rules        = self$rules
            )
          })
          
          # Run this stage for all agents (can be parallelized if desired)
          batch_results <- lapply(snapshot_inputs, function(input) {
            
            # Temporary agent that uses the stage-appropriate LLM
            temp_agent <- Agent$new(
              id               = input$lite_agent$id,
              persona          = input$lite_agent$persona,
              llm              = stage_llm,
              attention_budget = CONFIG$attention_budget,
              initial_cash     = CONFIG$initial_cash
            )
            temp_agent$cash                <- input$lite_agent$cash
            temp_agent$portfolio           <- data.table::as.data.table(input$lite_agent$portfolio)
            temp_agent$attention_remaining <- input$lite_agent$attention_remaining
            temp_agent$last_trade_t        <- input$lite_agent$last_trade_t
            temp_agent$last_buy_ticker     <- input$lite_agent$last_buy_ticker
            
            prompt_content <- paste(
              input$rules,
              input$instruction$text,
              "--- HISTORY ---",
              input$history_text,
              sep = "\n\n"
            )
            
            resp_format <- input$instruction$response_format %||% NULL
            
            response_text <- stage_llm$chat(
              system             = input$lite_agent$persona$system_prompt,
              user               = prompt_content,
              max_retries        = 3,
              specific_cache_key = input$instruction$cache_key,
              response_format    = resp_format
            )
            
            parsed <- s$apply_logic(
              response_text,
              a    = temp_agent,
              t    = t,
              pair = input$pair_context
            )
            
            final_state <- list(
              cash                = parsed$a$cash,
              portfolio           = as.data.frame(parsed$a$portfolio),
              attention_remaining = parsed$a$attention_remaining,
              last_trade_t        = parsed$a$last_trade_t,
              last_buy_ticker     = parsed$a$last_buy_ticker
            )
            
            list(
              id          = input$id,
              agent_state = final_state,
              pair_update = parsed$pair,
              log_line    = parsed$log_line,
              memory_line = parsed$memory_line
            )
          })
          
          # Merge results back into canonical agents and pairs
          for (res in batch_results) {
            a <- self$agents[[res$id]]
            
            if (!is.null(res$agent_state)) {
              a$cash <- res$agent_state$cash
              if (!is.null(res$agent_state$portfolio)) {
                a$portfolio <- as.data.table(res$agent_state$portfolio)
              }
              a$attention_remaining <- res$agent_state$attention_remaining
              if (!is.null(res$agent_state$last_trade_t)) {
                a$last_trade_t <- res$agent_state$last_trade_t
              }
              if (!is.null(res$agent_state$last_buy_ticker)) {
                a$last_buy_ticker <- res$agent_state$last_buy_ticker
              }
            }
            
            if (!is.null(res$pair_update)) {
              pairs[[res$id]] <- res$pair_update
            }
            
            if (!is.null(res$log_line)) {
              results_accumulator[[result_idx]] <- res$log_line
              result_idx <- result_idx + 1
            }
            if (!is.null(res$memory_line)) {
              a$remember(res$memory_line)
            }
          }
        }
        
        # Periodic checkpointing
        if (t %% save_interval == 0) {
          cat(sprintf("Completed %d/%d - Saving checkpoint...\n", t, self$periods))
          if (result_idx > 1) {
            current_log <- data.table::rbindlist(
              results_accumulator[1:(result_idx - 1)],
              fill = TRUE
            )
            write_atomic_csv(current_log, "experiment_log_checkpoint.csv")
          }
        }
      }
      
      valid_results <- results_accumulator[1:(result_idx - 1)]
      self$log <- data.table::rbindlist(valid_results, fill = TRUE)
      self$log
    }
  )
)


# ----------------------------------------------------------------------------
# PERSONAS
# ----------------------------------------------------------------------------

personas <- list(
  SocialMomentum = list(
    name = "SocialMomentum",
    system_prompt = "You are a Robinhood user who trades based on social signals and crowd behavior. You pay close attention to what other retail traders are buying. You experience FOMO.",
    sensitivity = "high"
  ),
  LotterySeeker = list(
    name = "LotterySeeker",
    system_prompt = if (isTRUE(CONFIG$r1$neutral_tickers)) "You treat trading like buying lottery tickets; you chase volatile, speculative stocks." else "You treat trading like buying lottery tickets; you chase volatile meme stocks like AMC and GME.",   # [R1] neutral-mode wording
    sensitivity = "high"
  ),
  PassiveFollower = list(
    name = "PassiveFollower",
    system_prompt = "You prefer familiar large-cap names and hold positions rather than trade frequently.",
    sensitivity = "medium"
  ),
  SkepticalContrarian = list(
    name = "SkepticalContrarian",
    system_prompt = "You fade social media hype and avoid viral stocks, preferring moderate attention names.",
    sensitivity = "medium"
  ),
  TechnicalTrader = list(
    name = "TechnicalTrader",
    system_prompt = "You are an active trader focused on price momentum and chart patterns.",
    sensitivity = "medium"
  ),
  DividendSeeker = list(
    name = "DividendSeeker",
    system_prompt = if (isTRUE(CONFIG$r1$neutral_tickers)) "You only trade stable blue-chip companies; you avoid volatile, speculative stocks." else "You only trade stable blue-chip companies like AAPL and NVDA; you avoid AMC and GME.",   # [R1] neutral-mode wording
    sensitivity = "low"
  )
)

# Persona definitions with system prompts and sensitivity levels

# ----------------------------------------------------------------------------
# STAGE 1: ATTENTION ALLOCATION
# ----------------------------------------------------------------------------

StageAttentionAllocation <- R6::R6Class(
  "StageAttentionAllocation",
  public = list(
    system_instruction_suffix = NULL,
    
    initialize = function() {
      self$system_instruction_suffix <- "
CRITICAL RULES:
1. The 'Leader' is the stock with the HIGHEST 'Mentions' count.
2. If multiple stocks are 'VIRAL', compare their Mention counts directly.
3. Your 'manipulation_check' accuracy is tracked. Read the numbers carefully.
"
    },
    
    compose = function(a, t, pair) {
      spec <- CONFIG$attention_specification %||% "original"
      leaderboard_str <- pair$leaderboard_text
      
      if (spec == "C") {
        cost_line <- sprintf(
          "Fees(bps): Deep-0, Quick+%d, Ignore+%d",
          CONFIG$search_cost_penalty_bps$quick, 
          CONFIG$search_cost_penalty_bps$ignore
        )
      } else {
        cost_line <- sprintf("Budget: %d pts", a$attention_remaining)
      }
      
      spec_rule <- switch(
        spec,
        "A" = "🛑 SYMMETRIC: Cannot buy/sell if Ignored.",
        "B" = "ℹ️ TRACKING: No restrictions.",
        "C" = "💰 COSTS: Ignoring raises fees.",
        "🛑 ASYMMETRIC: Must research to BUY."
      )
      
      treatment_alert <- if (pair$is_treated_now) "\n🚨 VIRAL ACTIVITY DETECTED" else ""
      
      prompt_text <- glue::glue("
### 📊 SOCIAL LEADERBOARD (t={t})
Stocks sorted by activity level:

{leaderboard_str}{treatment_alert}

---
**Status:** {cost_line}
**Rule:** {spec_rule}
{self$system_instruction_suffix}

### 🎯 TASK
1. Identify the highest-buzz stock (🏆 row, highest Mentions).
2. Allocate attention: DEEP / QUICK / IGNORE for each ticker.

### 📝 JSON OUTPUT
Respond with a single JSON object, no text before or after:

{{
  \"manipulation_check\": {{
    \"reasoning\": \"I see [TICKER] has the 🏆 flag and highest mention count...\",
    \"highest_buzz_stock\": \"TICKER\", 
    \"confidence\": 10
  }},
  \"allocation\": {{
    {ALLOC_LINES_DISP}
  }}
}}
")
      
      list(
        text = prompt_text,
        cache_key = pair$cache_key,
        response_format = list(type = "json_object")
      )
    },
    
    apply_logic = function(raw, a, t, pair) {
      spec <- CONFIG$attention_specification %||% "original"
      
      if (nrow(pair$social_signals) > 0) {
        actual_highest <- pair$social_signals[order(-mentions)][1, symbol]
      } else {
        actual_highest <- "AAPL"
      }
      
      raw_clean      <- clean_json_response(raw %||% "")
      json_candidate <- extract_first_json_object(raw_clean)
      json_ok        <- nzchar(json_candidate) && isTRUE(jsonlite::validate(json_candidate))
      parse_repaired <- FALSE
      
      if (!json_ok) {
        persona_name <- tryCatch(
          names(which(sapply(personas, identical, a$persona)))[1],
          error = function(e) "Unknown"
        )
        
        repair_system <- glue::glue(
          "You convert attention allocation responses into strict JSON.
Persona: {persona_name}.
You MUST output exactly one valid JSON object and nothing else.
Schema:
{{\"manipulation_check\":{{\"reasoning\":\"string\",\"highest_buzz_stock\":\"{PLAIN_ENUM_DISP}\",\"confidence\":integer}},\"allocation\":{{{ALLOC_PIPE_DISP}}}}}"
        )
        
        repair_user <- glue::glue(
          "Previous content was invalid JSON. Here is the leaderboard context:
{pair$leaderboard_text}

The stock with highest mentions is: {disp(actual_highest)}

Based on persona {persona_name}, produce a valid JSON allocation.
Output ONLY the JSON object, no markdown, no extra text."
        )
        
        repair_raw <- tryCatch(
          a$llm$chat(
            system = repair_system,
            user = repair_user,
            max_retries = 2,
            specific_cache_key = NULL,
            response_format = list(type = "json_object")
          ),
          error = function(e) ""
        )
        
        repair_clean     <- clean_json_response(repair_raw %||% "")
        repair_candidate <- extract_first_json_object(repair_clean)
        repair_ok        <- nzchar(repair_candidate) && isTRUE(jsonlite::validate(repair_candidate))
        
        if (repair_ok) {
          json_candidate <- repair_candidate
          json_ok        <- TRUE
          parse_repaired <- TRUE
        }
      }
      
      persona_name <- a$persona$name %||% tryCatch(   # [R1] robust to variant personas
        names(which(sapply(personas, identical, a$persona)))[1],
        error = function(e) "Unknown"
      )
      
      persona_alloc <- switch(
        persona_name,
        "LotterySeeker" = list(AAPL = "ignore", NVDA = "ignore", AMC = "deep",  GME = "deep"),
        "SocialMomentum" = list(AAPL = "quick",  NVDA = "quick",  AMC = "quick", GME = "quick"),
        "TechnicalTrader" = list(AAPL = "quick", NVDA = "quick",  AMC = "quick", GME = "quick"),
        "PassiveFollower" = list(AAPL = "deep",  NVDA = "quick",  AMC = "ignore", GME = "ignore"),
        "DividendSeeker" = list(AAPL = "deep",   NVDA = "deep",   AMC = "ignore", GME = "ignore"),
        "SkepticalContrarian" = list(AAPL = "quick", NVDA = "quick", AMC = "ignore", GME = "ignore"),
        list(AAPL = "quick", NVDA = "quick", AMC = "ignore", GME = "ignore")
      )
      
      fallback_alloc_pairs <- paste0('"', disp(c("AAPL","NVDA","AMC","GME")), '":"',
        unlist(persona_alloc[c("AAPL","NVDA","AMC","GME")]), '"', collapse = ",")   # [R1] display names
      fallback_json <- sprintf(
        '{"manipulation_check":{"reasoning":"API fallback - defaulting to leader","highest_buzz_stock":"%s","confidence":3},"allocation":{%s}}',
        disp(actual_highest), fallback_alloc_pairs
      )
      
      safe_json <- if (json_ok) json_candidate else fallback_json
      parsed    <- jsonlite::fromJSON(safe_json)
      
      manip_check <- parsed$manipulation_check
      perceived_buzz <- toupper(trimws(manip_check$highest_buzz_stock %||% "UNKNOWN"))
      if (perceived_buzz %in% names(TICKER_FROM_DISP)) perceived_buzz <- unname(TICKER_FROM_DISP[[perceived_buzz]])   # [R1] display -> internal
      
      is_correct <- if (json_ok) (perceived_buzz == actual_highest) else NA
      manip_conf <- if (json_ok) as.numeric(manip_check$confidence %||% 0) else 0
      
      if (json_ok && !is.null(parsed$allocation)) {
        alloc <- parsed$allocation
      } else {
        alloc <- persona_alloc
      }
      
      alloc_values <- sapply(c("AAPL","NVDA","AMC","GME"), function(tk) {
        val <- tolower(alloc[[disp(tk)]] %||% alloc[[tk]] %||% "ignore")   # [R1] model replies with display names
        if (!val %in% c("deep","quick","ignore")) "ignore" else val
      })
      names(alloc_values) <- c("AAPL","NVDA","AMC","GME")
      
      total_cost <- 0
      for (tk in names(alloc_values)) {
        level <- alloc_values[[tk]]
        if (level == "deep")  total_cost <- total_cost + pair$attention_costs[[tk]]$deep
        if (level == "quick") total_cost <- total_cost + pair$attention_costs[[tk]]$quick
      }
      
      initial_cost <- total_cost
      overspend_corrected <- FALSE
      
      if (total_cost > a$attention_remaining) {
        overspend_corrected <- TRUE
        
        while (total_cost > a$attention_remaining && any(alloc_values == "deep")) {
          idx <- which(alloc_values == "deep")[1]
          tk  <- names(alloc_values)[idx]
          alloc_values[idx] <- "quick"
          total_cost <- total_cost - pair$attention_costs[[tk]]$deep + pair$attention_costs[[tk]]$quick
        }
        
        while (total_cost > a$attention_remaining && any(alloc_values == "quick")) {
          idx <- which(alloc_values == "quick")[1]
          tk  <- names(alloc_values)[idx]
          alloc_values[idx] <- "ignore"
          total_cost <- total_cost - pair$attention_costs[[tk]]$quick
        }
      }
      
      expected_savings <- 0
      if (spec == "C") {
        max_penalty <- CONFIG$search_cost_penalty_bps$ignore %||% 50
        
        for (tk in names(alloc_values)) {
          lvl     <- alloc_values[[tk]]
          penalty <- CONFIG$search_cost_penalty_bps[[lvl]] %||% max_penalty
          expected_savings <- expected_savings + (max_penalty - penalty)
        }
      }
      
      pair$attention_allocation <- as.list(alloc_values)
      pair$attention_cost       <- total_cost
      
      a$attention_remaining <- a$attention_remaining - total_cost
      
      log_line <- data.table(
        t                = t,
        agent_id         = a$id,
        stage            = "attention_allocation",
        specification    = spec,
        treatment_cohort = pair$treatment_cohort,
        is_treated_now   = pair$is_treated_now,
        post_treatment   = as.integer(pair$post_treatment),  # <- use existing flag
        attention_arm    = pair$attention_arm %||% NA_character_,   # [R1]
        cost_arm         = pair$cost_arm %||% NA_character_,        # [R1]
        sm_variant       = sm_variant_of(a$id),                     # [R1]
        
        allocation          = paste(sprintf("%s:%s", names(alloc_values), alloc_values),
                                    collapse = ", "),
        initial_cost        = initial_cost,
        final_cost          = total_cost,
        budget_remaining    = a$attention_remaining,
        overspend_corrected = overspend_corrected,
        
        perceived_highest_buzz    = perceived_buzz,
        actual_highest_buzz       = actual_highest,
        manipulation_check_passed = is_correct,
        manipulation_confidence   = manip_conf,
        reasoning                 = manip_check$reasoning %||% "",
        
        spec_c_expected_savings_bps = expected_savings,
        json_fallback_used          = !json_ok,
        llm_allocation_valid        = json_ok,
        parse_repaired              = parse_repaired,
        meme_attention_level = if (pair$is_treated_now) {
          paste0("AMC:", alloc_values[["AMC"]], "|GME:", alloc_values[["GME"]])
        } else {
          NA_character_
        }
      )
      
      list(
        log_line = log_line,
        memory_line = list(
          t            = t,
          stage        = "attention_allocation",
          cost         = total_cost,
          passed_check = is_correct
        ),
        pair = pair,
        a    = a
      )
    }
  )
)

# ----------------------------------------------------------------------------
# STAGE 2: INFORMATION REVELATION
# ----------------------------------------------------------------------------

StageInformationRevelation <- R6::R6Class(
  "StageInformationRevelation",
  public = list(
    
    compose = function(a, t, pair) {
      alloc <- pair$attention_allocation %||% list(
        AAPL = "ignore",
        NVDA = "ignore",
        AMC  = "ignore",
        GME  = "ignore"
      )
      
      info_pieces <- character(0)
      
      for (tk in c("AAPL", "NVDA", "AMC", "GME")) {
        lvl <- alloc[[tk]] %||% "ignore"
        row <- pair$tickers[symbol == tk]
        
        if (lvl == "deep") {
          info_pieces <- c(info_pieces, row$txt_info_deep)
        } else if (lvl == "quick") {
          info_pieces <- c(info_pieces, sprintf(
            "%s [QUICK]: $%.2f (%.1f%%) | Social: %s | Signal: %s",
            disp(tk),   # [R1]
            row$current_price,
            row$momentum * 100,
            pair$social_signals[symbol == tk]$buzz_level,
            row$signal_trade_signal
          ))
        }
      }
      
      if (length(info_pieces) == 0) {
        prompt_text <- paste(
          "You ignored all stocks this period and therefore received no new research information.",
          "In 1–3 sentences of plain English, describe how this affects your willingness to trade in future periods.",
          "Do NOT use JSON, markdown, bullet points, or code fences. Write natural sentences only.",
          sep = "\n"
        )
      } else {
        prompt_text <- paste(
          "INCOMING RESEARCH REPORTS",
          paste(info_pieces, collapse = "\n\n"),
          "----------------",
          "TASK: In 1–3 sentences of plain English, reflect on how this information affects your view of your portfolio and future trades.",
          "Do NOT use JSON, markdown, bullet points, or code fences. Write natural sentences only.",
          sep = "\n"
        )
      }
      
      alloc_sig <- paste(names(alloc), alloc, sep = "=", collapse = "|")
      
      raw_key <- digest::digest(list(
        stage        = "info_revelation",
        alloc        = alloc_sig,
        market_state = pair$cache_key
      ))
      raw_key   <- paste0(pair$cache_key, "info")
      cache_key <- gsub("[^a-z0-9]", "", tolower(raw_key))
      
      list(
        text      = prompt_text,
        cache_key = cache_key
      )
    },
    
    apply_logic = function(raw, a, t, pair) {
      spec <- CONFIG$attention_specification %||% "original"
      
      raw_txt <- raw %||% ""
      json_candidate <- extract_first_json_object(raw_txt)
      has_valid_json <- FALSE
      if (!is.null(json_candidate) &&
          nzchar(json_candidate) &&
          isTRUE(jsonlite::validate(json_candidate))) {
        has_valid_json <- TRUE
      }
      
      if (has_valid_json) {
        reflection_full <- "Structured JSON portfolio reflection received; details omitted from log to keep rationale compact."
      } else {
        reflection_full <- sanitize_rationale_text(raw_txt, max_chars = 800)
      }
      
      max_short <- 200L
      if (!nzchar(reflection_full)) {
        reflection_short <- ""
      } else if (nchar(reflection_full) <= max_short) {
        reflection_short <- reflection_full
      } else {
        reflection_short <- paste0(substr(reflection_full, 1, max_short), "…")
      }
      
      log_line <- data.table(
        t               = t,
        agent_id        = a$id,
        stage           = "info_revelation",
        specification   = spec,
        treatment_cohort = pair$treatment_cohort,
        is_treated_now  = pair$is_treated_now,
        post_treatment   = as.integer(pair$post_treatment),  
        
        rationale_full      = reflection_full,
        rationale           = reflection_short,
        rationale_valid_json = has_valid_json
      )
      
      list(
        log_line    = log_line,
        memory_line = NULL,
        pair        = pair,
        a           = a
      )
    }
  )
)

# ----------------------------------------------------------------------------
# STAGE 3: TRADING DECISION 
# ----------------------------------------------------------------------------

StageTradingDecision <- R6::R6Class(
  "StageTradingDecision",
  public = list(
    
    compose = function(a, t, pair) {
      alloc <- pair$attention_allocation %||% list(
        AAPL = "ignore",
        NVDA = "ignore",
        AMC  = "ignore",
        GME  = "ignore"
      )
      
      persona_name <- a$persona$name %||% tryCatch(   # [R1] robust to variant personas
        names(which(sapply(personas, identical, a$persona)))[1],
        error = function(e) "Unknown"
      )
      
      can_buy    <- names(Filter(function(x) x %in% c("deep", "quick"), alloc))
      cannot_buy <- setdiff(c("AAPL", "NVDA", "AMC", "GME"), can_buy)
      
      if (length(can_buy) > 0) {
        signal_summary <- pair$tickers[symbol %in% can_buy, txt_signal_deep]
      } else {
        signal_summary <- "No stocks researched - cannot trade!"
      }
      
      holdings_str <- "NO HOLDINGS"
      if (nrow(a$portfolio) > 0) {
        holdings_with_pnl <- sapply(1:nrow(a$portfolio), function(i) {
          tk <- a$portfolio$ticker[i]
          sh <- a$portfolio$shares[i]
          cb <- a$portfolio$cost_basis[i]
          px <- if (tk %in% names(pair$current_prices)) pair$current_prices[[tk]] else cb
          upct <- 100 * (px - cb) / cb
          sprintf("%s: %.0f sh @ $%.2f (%+.1f%%)", disp(tk), sh, cb, upct)   # [R1]
        })
        holdings_str <- paste(holdings_with_pnl, collapse = " | ")
      }
      
      spec <- CONFIG$attention_specification %||% "original"
      
      rules_block <- if (spec == "A") {
        glue::glue("
RULES (SYMMETRIC CONSTRAINTS):
- ALLOWED TO TRADE: {if (length(can_buy)) paste(disp(can_buy), collapse=', ') else 'none'}
- BLOCKED: {if (length(cannot_buy)) paste(disp(cannot_buy), collapse=', ') else 'none'}
- You must have allocated attention to EITHER buy OR sell a stock.")
      } else if (spec == "B") {
        glue::glue("
RULES:
- You may trade ANY stock.
- Your attention allocation does NOT restrict your trading.")
      } else {
        glue::glue("
RULES:
- ALLOWED TO BUY: {if (length(can_buy)) paste(disp(can_buy), collapse=', ') else 'none'}
- ALLOWED TO SELL: Any stock you currently own.")
      }
      
      # --- PERSONA REINFORCEMENT ---
      # Explicitly mapping strategies to ensure heterogeneity after removing generic heuristics
      guidance_reinforcement <- switch(
        persona_name,
        "SocialMomentum"      = if (identical(sm_variant_of(a$id), "v2")) "Base your decisions strictly on your Persona traits." else "Action Priority: Follow the crowd. If a stock has high buzz/rank, buy it regardless of fundamentals.",   # [R1] v2 = identity-only variant
        "LotterySeeker"       = "Action Priority: Seek high volatility. Prefer high-risk meme stocks over boring blue chips.",
        "PassiveFollower"     = "Action Priority: Minimize activity. Only trade if absolutely necessary; prefer holding.",
        "SkepticalContrarian" = "Action Priority: Bet against the hype. If a stock is viral, be skeptical or sell.",
        "TechnicalTrader"     = "Action Priority: Focus strictly on price trends and momentum signals.",
        "DividendSeeker"      = if (isTRUE(CONFIG$r1$neutral_tickers)) "Action Priority: Safety first. Avoid volatile, speculative stocks; stick to stable assets." else "Action Priority: Safety first. Avoid volatile meme stocks (AMC/GME); stick to stable assets.",   # [R1] neutral-mode wording
        "Base your decisions strictly on your Persona traits." # Default fallback
      )
      
      # --- UPDATED PROMPT: NEUTRAL GUIDANCE ---
      prompt_text <- glue::glue("
TRADING DECISION (t={t}, persona={persona_name})
Holdings: {holdings_str}
Cash: ${sprintf('%.2f', a$cash)}

MARKET DATA (Available Research):
{paste(signal_summary, collapse = '\n')}

{rules_block}

TRADING GUIDANCE:
- Build positions early in the experiment.
- Never spend more than about 30% of your available cash in a single trade.
- If your cash is very low, prefer HOLD or SELL over BUY.
- {guidance_reinforcement}
- Periods since last trade: {t - (a$last_trade_t %||% 0)}

OUTPUT FORMAT:
You MUST respond with a single valid JSON object and nothing else.
Schema:
{{\"action\":\"buy\"|\"sell\"|\"hold\",\"ticker\":{TICKER_ENUM_DISP}|null,\"shares\":integer,\"rationale\":string}}
")
      
      list(
        text            = prompt_text,
        cache_key       = NULL,
        response_format = list(type = "json_object")
      )
    },
    
    apply_logic = function(raw, a, t, pair) {
      spec <- CONFIG$attention_specification %||% "original"
      
      holdings_str <- "NO HOLDINGS"
      if (nrow(a$portfolio) > 0) {
        holdings_with_pnl <- sapply(1:nrow(a$portfolio), function(i) {
          tk <- a$portfolio$ticker[i]
          sh <- a$portfolio$shares[i]
          cb <- a$portfolio$cost_basis[i]
          px <- if (tk %in% names(pair$current_prices)) pair$current_prices[[tk]] else cb
          upct <- 100 * (px - cb) / cb
          sprintf("%s: %.0f sh @ $%.2f (%+.1f%%)", disp(tk), sh, cb, upct)   # [R1]
        })
        holdings_str <- paste(holdings_with_pnl, collapse = " | ")
      }
      
      raw_clean       <- clean_json_response(raw %||% "")
      json_candidate <- extract_first_json_object(raw_clean)
      json_ok         <- nzchar(json_candidate) && isTRUE(jsonlite::validate(json_candidate))
      parse_repaired <- FALSE
      
      if (!json_ok) {
        researched <- names(Filter(
          function(x) x %in% c("deep", "quick"),
          pair$attention_allocation
        ))
        
        if (length(researched) > 0) {
          signal_lines <- pair$tickers[symbol %in% researched, sprintf(
            "%s: price=$%.2f, momentum=%+.2f%%, signal=%s",
            symbol, current_price, momentum * 100, signal_trade_signal
          )]
          signal_summary <- paste(signal_lines, collapse = "\n")
        } else {
          signal_summary <- "No researched stocks this period."
        }
        
        persona_name <- tryCatch(
          names(which(sapply(personas, identical, a$persona)))[1],
          error = function(e) "Unknown"
        )
        
        repair_system <- glue::glue(
          "You convert messy trader replies into strict JSON.
Persona: {persona_name}.
You MUST output exactly one valid JSON object and nothing else.
Schema:
{{\"action\":\"buy\"|\"sell\"|\"hold\",\"ticker\":{TICKER_ENUM_DISP}|null,\"shares\":integer,\"rationale\":string}}."
        )
        
        repair_user <- glue::glue(
          "Previous content was invalid JSON. Ignore it and produce a NEW decision.

Holdings: {holdings_str}
Cash: {sprintf('%.2f', a$cash)}
Researched stocks and signals:
{signal_summary}

Output ONLY the JSON object, no markdown, no extra text."
        )
        
        repair_raw <- tryCatch(
          a$llm$chat(
            system            = repair_system,
            user              = repair_user,
            max_retries       = 2,
            specific_cache_key = NULL,
            response_format   = list(type = "json_object")
          ),
          error = function(e) ""
        )
        
        repair_clean     <- clean_json_response(repair_raw %||% "")
        repair_candidate <- extract_first_json_object(repair_clean)
        repair_ok        <- nzchar(repair_candidate) && isTRUE(jsonlite::validate(repair_candidate))
        
        if (repair_ok) {
          json_candidate <- repair_candidate
          json_ok        <- TRUE
          parse_repaired <- TRUE
        }
      }
      
      fallback_json <- '{"action":"hold","ticker":null,"shares":0,"rationale":"parse-error"}'
      safe_json     <- if (json_ok) json_candidate else fallback_json
      
      decision <- tryCatch(
        jsonlite::fromJSON(safe_json),
        error = function(e) list(
          action    = "hold",
          ticker    = NA_character_,
          shares    = 0,
          rationale = "parse-error"
        )
      )
      
      llm_decision_valid <- json_ok
      
      trade_rationale_raw  <- decision$rationale %||% ""
      trade_rationale_full <- sanitize_rationale_text(trade_rationale_raw, max_chars = 800)
      if (!nzchar(trade_rationale_full)) trade_rationale_full <- "no-rationale"
      
      max_short <- 200L
      if (nchar(trade_rationale_full) <= max_short) {
        trade_rationale_short <- trade_rationale_full
      } else {
        trade_rationale_short <- paste0(substr(trade_rationale_full, 1, max_short), "…")
      }
      
      original_action       <- tolower(decision$action %||% "hold")
      original_ticker       <- decision$ticker %||% NA_character_
      original_shares_num   <- suppressWarnings(as.numeric(decision$shares %||% 0))
      original_shares       <- if (is.na(original_shares_num)) 0L else as.integer(round(original_shares_num))
      
      canonicalize_ticker <- function(x) {
        if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
        t <- scalar_chr(x)
        t <- toupper(trimws(t))
        if (t %in% names(TICKER_FROM_DISP)) return(unname(TICKER_FROM_DISP[[t]]))   # [R1] display -> internal
        if (!isTRUE(CONFIG$r1$neutral_tickers)) {   # [R1] real-name aliases only in real-ticker mode
          if (t %in% c("AAPL", "APPLE", "APPL"))     return("AAPL")
          if (t %in% c("NVDA", "NVIDIA"))            return("NVDA")
          if (t %in% c("AMC"))                       return("AMC")
          if (t %in% c("GME", "GAMESTOP"))           return("GME")
        }
        t
      }
      
      ticker_exec <- canonicalize_ticker(original_ticker)
      
      action <- original_action
      ticker <- ticker_exec
      shares <- original_shares
      
      blocked_reason    <- if (json_ok) "none" else "llm_parse_failure"
      trade_success     <- FALSE
      payoff            <- 0
      realized_pnl      <- NA_real_
      intervention_type <- "none"
      
      pre_pv <- a$get_portfolio_value(pair$current_prices)
      
      # INTERVENTION 1: Early-period buy encouragement
      if (intervention_enabled("early_buy_encouragement") &&
          llm_decision_valid && action == "hold" && t <= 5 && a$cash > 8000) {
        researched <- names(Filter(
          function(x) x %in% c("deep", "quick"),
          pair$attention_allocation
        ))
        
        if (length(researched) > 0) {
          candidates <- pair$tickers[
            symbol %in% researched & 
              signal_strength %in% c("medium", "strong")
          ]
          
          if (nrow(candidates) > 0) {
            best_stock <- candidates[which.max(momentum)]
            if (nrow(best_stock) > 0 && best_stock$momentum > 0) {
              if (runif(1) > 0.5) {
                action <- "buy"
                ticker <- best_stock$symbol
                shares <- sample(3:6, 1)
                intervention_type <- "early_encouragement"
                blocked_reason    <- "none"
              }
            }
          }
        }
      }
      
      # INTERVENTION 2: Forced trading after inactivity
      if (intervention_enabled("forced_trading_after_inactivity") &&
          llm_decision_valid && action == "hold" && a$cash > 5000 && t > 5) {
        periods_since_trade <- t - (a$last_trade_t %||% 0)
        if (periods_since_trade > 5) {
          researched <- names(Filter(
            function(x) x %in% c("deep", "quick"),
            pair$attention_allocation
          ))
          
          if (length(researched) > 0) {
            action <- "buy"
            ticker <- researched[1]
            shares <- 3L
            intervention_type <- "forced_activity"
            blocked_reason    <- "none"
          }
        }
      }
      
      ticker_row <- if (!is.na(ticker) && ticker %in% pair$tickers$symbol) {
        pair$tickers[symbol == ticker]
      } else {
        NULL
      }
      
      base_tc        <- NA_real_
      search_penalty <- NA_real_
      total_tc       <- NA_real_
      fee_paid       <- NA_real_
      s_paid         <- NA_real_
      s_saved        <- NA_real_
      
      if (action %in% c("buy", "sell") && !is.null(ticker_row)) {
        
        if (action == "buy" && t > 5) {
          last_buy <- a$last_buy_ticker[[ticker]] %||% NA
          if (!is.na(last_buy) && (t - last_buy) < 2) {
            action <- "hold"
            blocked_reason <- "cooldown"
          }
        }
        
        attn_level <- (pair$attention_allocation[[ticker]] %||% "ignore")
        if (action == "buy" && spec != "B" && attn_level == "ignore") {
          action <- "hold"
          blocked_reason <- "no_attention"
        }
        if (action == "sell" && spec == "A" && attn_level == "ignore") {
          action <- "hold"
          blocked_reason <- "no_attention_symmetric"
        }
        
        if (action != "hold") {
          social_row <- pair$social_signals[symbol == ticker]
          if (r1_factorial()) {   # [R1] execution cost driven by the cost arm
            base_tc <- tc_bps_r1(ticker, pair$post_treatment, pair$cost_arm)
          } else if (nrow(social_row) > 0) {
            base_tc <- tc_bps(
              ticker,
              social_row$buzz_level[1],
              social_row$robinhood_rank[1]
            )
          } else {
            base_tc <- 15
          }
          
          if (spec == "C") {
            search_penalty <- CONFIG$search_cost_penalty_bps[[attn_level]] %||% 0
            total_tc       <- base_tc + search_penalty
          } else {
            search_penalty <- NA_real_
            total_tc       <- base_tc
          }
          
          price <- ticker_row$current_price
          
          if (action == "buy") {
            tc_dec <- (total_tc %||% 0) / 10000
            cost_per_share <- price * (1 + tc_dec)
            max_affordable_shares <- floor(a$cash / cost_per_share)
            if (is.na(max_affordable_shares) || max_affordable_shares < 1L) {
              action <- "hold"
              blocked_reason <- "insufficient_cash"
            } else {
              shares <- min(shares, max_affordable_shares)
            }
          }
          
          if (action != "hold" && !is.null(CONFIG$max_trade_value)) {
            max_val <- CONFIG$max_trade_value
            max_shares_by_value <- floor(max_val / price)
            if (is.na(max_shares_by_value) || max_shares_by_value < 1L) {
              action <- "hold"
              blocked_reason <- "trade_value_cap"
            } else {
              shares <- min(shares, max_shares_by_value)
            }
          }
          
          if (action != "hold" && !is.null(CONFIG$max_shares_per_trade)) {
            shares <- min(shares, CONFIG$max_shares_per_trade)
          }
          
          if (action != "hold") {
            shares <- max(1L, as.integer(round(shares)))
            
            res <- a$execute_trade(
              action,
              ticker,
              shares,
              price,
              t,
              tc_bps_override = total_tc
            )
            
            trade_success <- res$success
            if (trade_success) {
              payoff       <- if (action == "buy") -res$cash_spent else res$proceeds
              realized_pnl <- if (action == "sell") res$realized_pnl else NA_real_
              
              a$last_trade_t <- t
              if (action == "buy") a$last_buy_ticker[[ticker]] <- t
              
              tc_decimal <- (total_tc %||% 5) / 10000
              fee_paid   <- shares * price * tc_decimal
              
              if (spec == "C") {
                val    <- shares * price
                s_paid  <- (search_penalty / 10000) * val
                s_saved <- (
                  (CONFIG$search_cost_penalty_bps$ignore - search_penalty) / 10000
                ) * val
              }
            } else {
              blocked_reason <- res$reason
            }
          }
        }
      } else if (action != "hold") {
        blocked_reason <- "invalid_ticker"
        action <- "hold"
      }
      
      if (!trade_success) {
        fee_paid       <- NA_real_
        base_tc        <- NA_real_
        search_penalty <- NA_real_
        total_tc       <- NA_real_
        s_paid         <- NA_real_
        s_saved        <- NA_real_
      }
      
      post_pv        <- a$get_portfolio_value(pair$current_prices)
      period_transaction_impact <- post_pv - pre_pv
      
      safe_get <- function(col, default) {
        if (!is.null(ticker_row) && nrow(ticker_row) > 0) ticker_row[[col]] else default
      }
      
      researched_stocks <- names(
        Filter(function(x) x %in% c("deep", "quick"),
               pair$attention_allocation)
      )
      
      best_ticker_val <- if (length(researched_stocks) > 0) {
        pair$tickers[symbol %in% researched_stocks][which.max(momentum), symbol]
      } else {
        NA_character_
      }
      
      best_momentum_val <- if (length(researched_stocks) > 0) {
        pair$tickers[symbol %in% researched_stocks, max(momentum, na.rm = TRUE)]
      } else {
        NA_real_
      }
      
      is_forced_trade <- (intervention_type != "none")
      made_trade      <- as.integer(trade_success && action %in% c("buy", "sell"))
      
      if (!is.null(pair$attention_allocation) &&
          length(pair$attention_allocation) > 0) {
        full_alloc_str <- paste(
          sprintf("%s:%s", names(pair$attention_allocation),
                  unlist(pair$attention_allocation)),
          collapse = ", "
        )
      } else {
        full_alloc_str <- NA_character_
      }
      
      log_line <- data.table(
        t = t,
        agent_id = a$id,
        stage = "trading",
        specification = spec,
        treatment_cohort = pair$treatment_cohort,
        is_treated_now   = pair$is_treated_now,
        post_treatment   = as.integer(pair$post_treatment),
        attention_arm    = pair$attention_arm %||% NA_character_,   # [R1]
        cost_arm         = pair$cost_arm %||% NA_character_,        # [R1]
        sm_variant       = sm_variant_of(a$id),                     # [R1]
        
        model_action = original_action,
        model_ticker = original_ticker,
        model_shares = original_shares,
        action       = action,
        ticker = if (trade_success && action != "hold") ticker else NA_character_,
        shares = if (trade_success && action != "hold") shares else 0L,
        
        trade_success     = trade_success,
        blocked_reason    = blocked_reason,
        is_forced_trade   = is_forced_trade,
        intervention_type = intervention_type,
        
        payoff              = payoff,
        realized_pnl        = realized_pnl,
        portfolio_value_pre = pre_pv,
        portfolio_value     = post_pv,
        period_transaction_impact = period_transaction_impact,
        
        rationale_full = trade_rationale_full,
        rationale      = trade_rationale_short,
        
        json_fallback_used = !json_ok,
        llm_decision_valid = llm_decision_valid,
        parse_repaired      = parse_repaired,
        
        momentum         = safe_get("momentum", NA_real_),
        lookback_periods = safe_get("lookback_periods", NA_integer_),
        signal_strength  = safe_get("signal_strength", NA_character_),
        signal_z_score   = safe_get("signal_z_score", NA_real_),
        
        trade_indicator = as.integer(trade_success),
        buy_indicator   = as.integer(trade_success && action == "buy"),
        sell_indicator  = as.integer(trade_success && action == "sell"),
        made_trade      = made_trade,
        
        trade_fee               = fee_paid,
        base_tc_bps             = base_tc,
        search_cost_penalty_bps = search_penalty,
        total_tc_bps            = total_tc,
        search_cost_paid        = s_paid,
        search_cost_saved       = s_saved,
        
        attention_allocated = if (!is.na(ticker)) {
          pair$attention_allocation[[ticker]] %||% "ignore"
        } else {
          NA_character_
        },
        full_attention_allocation = full_alloc_str,
        
        best_researched_ticker = best_ticker_val,
        best_momentum          = best_momentum_val,
        n_stocks_researched    = sum(
          unlist(pair$attention_allocation) %in% c("deep", "quick")
        )
      )
      
      list(
        log_line = log_line,
        memory_line = list(
          t             = t,
          stage         = "trading",
          last_action   = action,
          last_ticker   = ticker,
          last_payoff   = payoff,
          trade_success = trade_success
        ),
        pair = pair,
        a    = a
      )
    }
  )
)


# ----------------------------------------------------------------------------
# PRICE PATH SIMULATION (WITH REGIME SWITCHING VOLATILITY)
# ----------------------------------------------------------------------------

simulate_price_paths <- function(n_periods = CONFIG$n_periods) {
  log_info("Pre-simulating price paths for {n_periods} periods")
  cat(sprintf("Simulating %d periods of price data (with Regime Switching Volatility)...\n", n_periods))
  
  tickers <- c("AAPL","NVDA","AMC","GME")
  p0 <- c(CONFIG$initial_prices$AAPL, CONFIG$initial_prices$NVDA,
          CONFIG$initial_prices$AMC,  CONFIG$initial_prices$GME)
  vol <- c(CONFIG$volatility$AAPL, CONFIG$volatility$NVDA,
           CONFIG$volatility$AMC,  CONFIG$volatility$GME)
  
  mu <- rep(CONFIG$drift_per_period, 4)
  
  vol_mult <- rep(1, n_periods)
  shock_start <- floor(n_periods * 0.4)
  vol_mult[shock_start:n_periods] <- 2.5
  
  R <- matrix(c(
    1.00, 0.50, 0.20, 0.20,
    0.50, 1.00, 0.20, 0.20,
    0.20, 0.20, 1.00, 0.60,
    0.20, 0.20, 0.60, 1.00
  ), 4, 4, byrow = TRUE)
  
  log_ret_mat <- matrix(0, n_periods, 4)
  for (t in seq_len(n_periods)) {
    current_vol <- vol * vol_mult[t]
    Sigma <- diag(current_vol) %*% R %*% diag(current_vol)
    U <- chol(Sigma)
    z <- rnorm(4)
    log_ret_mat[t, ] <- mu + as.vector(t(U) %*% z)
  }
  
  exp_cum <- apply(log_ret_mat, 2, function(x) cumprod(exp(x)))
  prices_mat <- sweep(exp_cum, 2, p0, `*`)
  
  paths <- list()
  for (i in seq_along(tickers)) {
    paths[[tickers[i]]] <- prices_mat[, i]
  }
  
  cat("✓ Price paths simulated (with regime switching volatility)\n\n")
  paths
}

# ----------------------------------------------------------------------------
# MARKET ENVIRONMENT GENERATION (with dose-response support)
# ----------------------------------------------------------------------------

generate_market_environment <- function(price_paths, n_periods,
                                        treatment_intensity = "original") {
  cat("Pre-calculating market environment (Optimized: Pre-hashing enabled)...\n")
  
  tickers <- c("AAPL", "NVDA", "AMC", "GME")
  LOOKBACK <- 5
  
  LEADERBOARD_ROW_TMPL <- "   %s: %s (#%d) | %s mentions"
  LEADERBOARD_ROW_TOP  <- "🏆 %s: 🔥🔥🔥 VIRAL LEADER (#%d) | 🗣️ %s mentions | ⚠️ HIGHEST BUZZ"
  SIG_TMPL_DEEP <- "%s: %s %s%.1f%% momentum (%s, z=%.2f) → size: %.0f sh%s%s"
  INFO_TMPL_DEEP <- "%s [DEEP RESEARCH]:\n    %s\n    Price: $%.2f (momentum %+.1f%%)\n    Social: %s buzz (Rank #%s, %s mentions, %s retail buyers)\n    Fundamental signal: %+.1f%%\n    News: %s\n    💡 TRADING SIGNAL: %s"
  
  render_leaderboard <- function(social_dt) {
    dt_sorted <- social_dt[order(-mentions)]
    rows <- character(4)
    for (i in 1:4) {
      row <- dt_sorted[i]
      mentions_fmt <- format(round(row$mentions), big.mark=",")
      if (i == 1) {
        rows[i] <- sprintf(LEADERBOARD_ROW_TOP, disp(row$symbol), row$robinhood_rank, mentions_fmt)   # [R1]
      } else {
        buzz_desc <- if (row$buzz_level == "viral" || row$buzz_level == "VIRAL") {
          "🔥 Viral"
        } else if (row$buzz_level %in% c("high", "HIGH")) {
          "📈 High Buzz"
        } else {
          "• Normal"
        }
        rows[i] <- sprintf(LEADERBOARD_ROW_TMPL, disp(row$symbol), buzz_desc, row$robinhood_rank, mentions_fmt)   # [R1]
      }
    }
    paste(rows, collapse = "\n")
  }
  
  generate_state_strings <- function(social_dt, p_name, vec_data, noise_data, cost_arm = NULL) {   # [R1] cost_arm added
    buzz_adj <- switch(p_name, "SocialMomentum"=1.2, "LotterySeeker"=1.1,
                       "TechnicalTrader"=0.9, 1.0)
    
    edge_strs <- sapply(seq_along(vec_data$sym), function(i) {
      sym <- vec_data$sym[i]
      tc  <- if (r1_factorial() && !is.null(cost_arm)) {   # [R1] displayed edge uses the arm's actual cost schedule
        if (identical(cost_arm, "high") && sym %in% CONFIG$r1$tickers_internal[3:4]) 15 else 5
      } else tc_bps(sym, social_dt$buzz_level[i], social_dt$robinhood_rank[i])
      vol <- CONFIG$volatility[[sym]]
      m_cap <- sign(vec_data$mom[i]) * min(abs(vec_data$mom[i]), 3 * vol)
      exp_ret <- 0.30 * buzz_adj * m_cap * 10000
      edge <- exp_ret - 2 * tc
      if (edge > 30) sprintf(" [Edge: +%.0f bps ✓]", edge) else sprintf(" [Edge: %.0f bps]", edge)
    })
    
    deep_str <- sprintf(
      SIG_TMPL_DEEP,
      disp(vec_data$sym), vec_data$sig_ind, vec_data$sig_dir, abs(vec_data$mom_pct),   # [R1] display names
      vec_data$sig_str, vec_data$z, round(5 * vec_data$mult),
      vec_data$conf_note, edge_strs
    )
    names(deep_str) <- vec_data$sym
    
    trend_indicator <- ifelse(
      vec_data$mom_pct > 2, 
      "🚀 STRONG UPTREND", 
      ifelse(vec_data$mom_pct < -2, "📉 Falling", "→ Flat")
    )
    
    trading_signal <- ifelse(vec_data$mom_pct > 1, "Strong Buy", "Neutral")
    
    info_str <- sprintf(
      INFO_TMPL_DEEP,
      disp(vec_data$sym),    # [R1] display names
      
      trend_indicator,
      vec_data$prices, vec_data$mom_pct,
      social_dt$buzz_level, social_dt$robinhood_rank, 
      format(round(social_dt$mentions), big.mark=","),
      format(round(social_dt$retail_buyers), big.mark=","),
      noise_data$fund * 100, 
      noise_data$news, 
      trading_signal
    )
    names(info_str) <- vec_data$sym
    
    list(deep = deep_str, info = info_str)
  }
  
  # Intensity parameters (dose-response)
  if (isTRUE(CONFIG$dose_response$enabled)) {
    intensity <- CONFIG$dose_response$intensity_levels[[treatment_intensity]]
    if (is.null(intensity)) {
      warning(sprintf("Unknown intensity '%s', falling back to 'original'", treatment_intensity))
      intensity <- CONFIG$dose_response$intensity_levels[["original"]]
    }
  } else {
    intensity <- list(
      multiplier     = 400,
      mentions_range = c(CONFIG$shock_treatment$mentions_min,
                         CONFIG$shock_treatment$mentions_max),
      rank_range     = c(CONFIG$shock_treatment$rank_min,
                         CONFIG$shock_treatment$rank_max),
      buzz           = CONFIG$shock_treatment$buzz
    )
  }
  intensity_multiplier <- intensity$multiplier %||% 400
  
  market_data <- vector("list", n_periods)
  
  for (t in 1:n_periods) {
    
    market_momentum_cache <- lapply(tickers, function(sym) {
      calculate_momentum_with_shrinkage(
        price_history = price_paths[[sym]][1:t],
        volatility_prior = CONFIG$volatility[[sym]],
        target_periods = LOOKBACK
      )
    })
    names(market_momentum_cache) <- tickers
    
    mom_values <- sapply(market_momentum_cache, function(x) x$momentum)
    mom_conf   <- sapply(market_momentum_cache, function(x) x$confidence)
    mom_n_obs  <- sapply(market_momentum_cache, function(x) x$n_obs)
    
    global_signals <- lapply(tickers, function(sym) {
      calculate_signal_strength(mom_values[[sym]], CONFIG$volatility[[sym]], t, mom_conf[[sym]])
    })
    names(global_signals) <- tickers
    
    current_prices_vec  <- sapply(tickers, function(sym) price_paths[[sym]][t])
    names(current_prices_vec) <- tickers
    
    vec_data <- list(
      sym = tickers,
      prices = current_prices_vec,
      mom = mom_values,
      mom_pct = mom_values * 100,
      z = sapply(global_signals, function(s) s$z_score),
      sig_str = toupper(sapply(global_signals, function(s) s$strength)),
      mult = sapply(global_signals, function(s) s$multiplier),
      sig_dir = sapply(global_signals, function(s) s$direction),
      sig_ind = sapply(tickers, function(s) {
        if (global_signals[[s]]$trade_signal=="buy") "📈 BUY"
        else if (global_signals[[s]]$trade_signal=="sell") "📉 SELL"
        else "→ NEUTRAL"
      }),
      conf_note = ifelse(mom_n_obs < 5, "[Low Data]", "")
    )
    
    market_social_normal <- data.table(
      symbol = tickers,
      mentions = runif(4,
                       CONFIG$shock_control$mentions_min,
                       CONFIG$shock_control$mentions_max),
      robinhood_rank = round(runif(4,
                                   CONFIG$shock_control$rank_min,
                                   CONFIG$shock_control$rank_max)),
      retail_buyers  = round(runif(4,
                                   CONFIG$shock_control$buyers_min,
                                   CONFIG$shock_control$buyers_max)),
      buzz_level     = rep("low", 4)
    )
    
    market_social_shock <- data.table(
      symbol = tickers,
      mentions = c(
        runif(2,
              CONFIG$shock_baseline$mentions_min,
              CONFIG$shock_baseline$mentions_max),
        runif(2,
              intensity$mentions_range[1],
              intensity$mentions_range[2])
      ),
      robinhood_rank = c(
        round(runif(2,
                    CONFIG$shock_baseline$rank_min,
                    CONFIG$shock_baseline$rank_max)),
        round(runif(2,
                    intensity$rank_range[1],
                    intensity$rank_range[2]))
      ),
      retail_buyers = c(
        round(runif(2,
                    CONFIG$shock_baseline$buyers_min,
                    CONFIG$shock_baseline$buyers_max)),
        round(runif(2,
                    intensity$mentions_range[1] * 2,
                    intensity$mentions_range[2] * 2))
      ),
      buzz_level = c("low", "low", intensity$buzz, intensity$buzz)
    )
    
    lb_txt_normal <- render_leaderboard(market_social_normal)
    lb_txt_shock  <- render_leaderboard(market_social_shock)
    
    hash_normal <- digest::digest(market_social_normal[order(symbol), .(symbol, mentions, robinhood_rank, buzz_level)])
    hash_shock  <- digest::digest(market_social_shock[order(symbol), .(symbol, mentions, robinhood_rank, buzz_level)])
    
    NUM_VIEWS <- 5
    persona_names <- c("SocialMomentum", "LotterySeeker", "PassiveFollower", 
                       "SkepticalContrarian", "TechnicalTrader", "DividendSeeker")
    
    shared_views <- vector("list", NUM_VIEWS)
    
    for (v in 1:NUM_VIEWS) {
      noise_data <- list(
        fund = rnorm(4, 0, 0.03),
        trend = sample(c("Uptrend", "Downtrend", "Sideways"), 4, replace = TRUE),
        news = sample(c("Positive", "Neutral", "Negative", "Mixed"), 4, replace = TRUE)
      )
      
      costs <- list(
        AAPL = list(deep = 50 + sample(c(-5, 0, 5), 1),
                    quick = 20 + sample(c(-2, 0, 2), 1)),
        NVDA = list(deep = 50 + sample(c(-5, 0, 5), 1),
                    quick = 20 + sample(c(-2, 0, 2), 1)),
        AMC  = list(deep = 50 + sample(c(-5, 0, 5), 1),
                    quick = 20 + sample(c(-2, 0, 2), 1)),
        GME  = list(deep = 50 + sample(c(-5, 0, 5), 1),
                    quick = 20 + sample(c(-2, 0, 2), 1))
      )
      
      persona_strings <- list()
      for (p_name in persona_names) {
        if (r1_factorial()) {   # [R1] state x cost-arm string variants
          persona_strings[[p_name]] <- list(
            normal_low  = generate_state_strings(market_social_normal, p_name, vec_data, noise_data, cost_arm = "low"),
            normal_high = generate_state_strings(market_social_normal, p_name, vec_data, noise_data, cost_arm = "high"),
            shock_low   = generate_state_strings(market_social_shock,  p_name, vec_data, noise_data, cost_arm = "low"),
            shock_high  = generate_state_strings(market_social_shock,  p_name, vec_data, noise_data, cost_arm = "high")
          )
        } else {
          strs_normal <- generate_state_strings(market_social_normal, p_name, vec_data, noise_data)
          strs_shock  <- generate_state_strings(market_social_shock,  p_name, vec_data, noise_data)
          persona_strings[[p_name]] <- list(normal = strs_normal, shock = strs_shock)
        }
      }
      
      shared_views[[v]] <- list(noise = noise_data, costs = costs, strings = persona_strings)
    }
    
    market_data[[t]] <- list(
      tickers_dt = data.table(
        symbol = tickers,
        current_price = current_prices_vec,
        momentum = mom_values,
        lookback_periods = mom_n_obs, 
        signal_strength = sapply(global_signals, function(s) s$strength),
        signal_z_score = sapply(global_signals, function(s) s$z_score),
        signal_trade_signal = sapply(global_signals, function(s) s$trade_signal),
        signal_multiplier = sapply(global_signals, function(s) s$multiplier)
      ),
      current_prices_list = as.list(current_prices_vec),
      market_social_normal = market_social_normal,
      market_social_shock  = market_social_shock,
      lb_txt_normal = lb_txt_normal,
      lb_txt_shock  = lb_txt_shock,
      hash_normal = hash_normal,
      hash_shock  = hash_shock,
      shared_views = shared_views,
      treatment_intensity  = treatment_intensity,
      intensity_multiplier = intensity_multiplier
    )
  }
  
  cat("✓ Market environment ready (optimized).\n")
  market_data
}

# ----------------------------------------------------------------------------
# MATCHING FUNCTION 
# ----------------------------------------------------------------------------

create_matching_function <- function(agents, treatment_assignment, market_data) {
  
  if (!requireNamespace("digest", quietly = TRUE)) install.packages("digest")
  
  function(agents, t) {
    mkt <- market_data[[t]]
    spec <- CONFIG$attention_specification %||% "original"
    NUM_VIEWS <- 5
    
    shared_view_idx_for_period <- sample(1:NUM_VIEWS, 1)
    this_view <- mkt$shared_views[[shared_view_idx_for_period]]
    
    lapply(agents, function(agent) {
      
      agent_treatment  <- treatment_assignment[agent_id == agent$id]
      
      if (nrow(agent_treatment) == 0) {
        warning(sprintf("No treatment assignment found for agent %s, defaulting to control", agent$id))
        cohort            <- 3L
        treatment_period <- Inf
      } else {
        cohort            <- agent_treatment$treatment_cohort[1]
        treatment_period <- agent_treatment$treatment_period[1]
      }
      
      if (is.na(treatment_period)) treatment_period <- Inf
      
      attention_arm <- if (nrow(agent_treatment) > 0 && "attention_arm" %in% names(agent_treatment)) agent_treatment$attention_arm[1] else NA_character_   # [R1]
      cost_arm      <- if (nrow(agent_treatment) > 0 && "cost_arm"      %in% names(agent_treatment)) agent_treatment$cost_arm[1]      else NA_character_   # [R1]
      
      post_flag  <- (!is.infinite(treatment_period) && t >= treatment_period)                                   # [R1] post-adoption, any treated arm
      show_shock <- if (r1_factorial()) (identical(attention_arm, "viral") && post_flag) else post_flag          # [R1] viral display only in viral arms
      is_treated_now <- show_shock
      
      social_signals <- if (show_shock) mkt$market_social_shock else mkt$market_social_normal
      lb_text        <- if (show_shock) mkt$lb_txt_shock else mkt$lb_txt_normal
      
      p_name <- agent$persona$name %||% tryCatch(   # [R1] robust to variant personas
        names(which(sapply(personas, identical, agent$persona)))[1],
        error=function(e)"Unknown"
      )
      if (is.null(this_view$strings[[p_name]])) p_name <- "PassiveFollower"
      
      precalc_bundle <- this_view$strings[[p_name]]
      selected_strs <- if (r1_factorial()) {   # [R1] pick state x cost-arm variant
        skey <- if (show_shock) paste0("shock_", ifelse(identical(cost_arm, "high"), "high", "low"))
                else if (post_flag && identical(cost_arm, "high")) "normal_high" else "normal_low"
        precalc_bundle[[skey]]
      } else if (is_treated_now) precalc_bundle$shock else precalc_bundle$normal
      
      tickers_dt <- copy(mkt$tickers_dt)
      tickers_dt[, `:=`(
        txt_signal_deep = selected_strs$deep,
        txt_info_deep    = selected_strs$info,
        txt_signal_quick = "...", 
        fund_signal = this_view$noise$fund
      )]
      
      budget_key <- if (spec == "C") "static_fee_table" else as.character(agent$attention_remaining)
      base_hash  <- if (is_treated_now) mkt$hash_shock else mkt$hash_normal
      
      # 1. First, DEFINE raw_cache_key
      raw_cache_key <- paste0(
        "attnalloc",
        base_hash,
        spec,
        p_name,
        budget_key,
        cost_arm %||% "na", attention_arm %||% "na", ifelse(isTRUE(post_flag), "post", "pre"),   # [R1]
        agent$id
      )
      
      # 2. Then, SANITIZE it
      cache_key <- gsub("[^a-z0-9]", "", tolower(raw_cache_key))
      
      # 3. Then, CHECK it (Safe check)
      if (!nzchar(cache_key)) {
        stop(sprintf("Cache key became empty after sanitization: raw=%s", raw_cache_key))
      }
      
      list(
        agent = agent,
        tickers = tickers_dt,
        social_signals = social_signals,
        current_prices = mkt$current_prices_list,    
        attention_costs = this_view$costs,
        leaderboard_text = lb_text,
        cache_key = cache_key,
        attention_allocation = NULL,
        attention_cost = NULL,
        treatment_cohort = cohort,
        treatment_period = treatment_period,
        attention_arm = attention_arm,     # [R1]
        cost_arm = cost_arm,               # [R1]
        post_treatment = post_flag,        # [R1] post-adoption (any treated arm)
        is_treated_now = show_shock        # [R1] viral display active
      )
    })
  }
}

# ----------------------------------------------------------------------------
# HISTORY WRITER
# ----------------------------------------------------------------------------

salience_history_writer <- function(agent, t, pair = NULL) {
  h <- agent$recall_salient(current_t = t, n = 3)
  
  performance_str <- ""
  if (!is.null(pair) && t > 5) {
    tryCatch({
      portfolio_val <- agent$get_portfolio_value(pair$current_prices)
      pnl_pct <- 100 * (portfolio_val - CONFIG$initial_cash) / CONFIG$initial_cash
      
      perf_indicator <- if (pnl_pct > 5) {
        "💰"
      } else if (pnl_pct > 0) {
        "✓"
      } else if (pnl_pct > -5) {
        "⚠"
      } else {
        "📉"
      }
      
      performance_str <- sprintf(
        "%s Portfolio: $%.0f (%+.1f%%). ",
        perf_indicator, portfolio_val, pnl_pct
      )
    }, error = function(e) {
      performance_str <<- ""
    })
  }
  
  if (length(h) == 0) {
    if (nchar(performance_str) > 0) {
      return(paste0(performance_str, "History: First period."))
    } else {
      return("History: First period.")
    }
  }
  
  history_lines <- character(length(h))
  for (i in seq_along(h)) {
    x <- h[[i]]
    if (!is.null(x$stage) && x$stage == "trading") {
      success_str <- if (x$trade_success) "" else " [FAILED]"
      payoff_str <- sprintf("$%.2f", round(x$last_payoff, 2))
      
      history_lines[i] <- sprintf(
        "[t=%d] %s %s%s → %s",
        x$t,
        x$last_action,
        disp(x$last_ticker),   # [R1]
        success_str,
        payoff_str
      )
    } else {
      history_lines[i] <- sprintf("[t=%d] %s", x$t, x$stage)
    }
  }
  
  paste0(performance_str, "Recent: ", paste(history_lines, collapse = " | "))
}

# ----------------------------------------------------------------------------
# POWER ANALYSIS
# ----------------------------------------------------------------------------

compute_power_analysis <- function(n_agents, n_periods, assumed_icc, 
                                   effect_size, alpha = 0.05) {
  design_effect <- 1 + (n_periods - 1) * assumed_icc
  total_obs <- n_agents * n_periods
  effective_n_total <- total_obs / design_effect
  effective_n_per_group <- effective_n_total / 2
  
  if (effective_n_per_group < 2) {
    cat("\n=== A PRIORI POWER ANALYSIS ===\n")
    cat("⚠ WARNING: Effective sample size too small for reliable power analysis\n\n")
    return(list(power = NA, effective_n_total = effective_n_total, design_effect = design_effect))
  }
  
  power_result <- tryCatch({
    pwr.t.test(
      n = effective_n_per_group,
      d = effect_size,
      sig.level = alpha,
      type = "two.sample"
    )
  }, error = function(e) {
    return(list(power = NA))
  })
  
  cat("\n=== A PRIORI POWER ANALYSIS ===\n")
  cat(sprintf("Number of agents: %d\n", n_agents))
  cat(sprintf("Periods per agent: %d\n", n_periods))
  cat(sprintf("Total observations: %s\n", format(total_obs, big.mark = ",")))
  cat(sprintf("Assumed ICC: %.2f\n", assumed_icc))
  cat(sprintf("Design effect: %.2f\n", design_effect))
  cat(sprintf("Effective sample size (total): %.1f\n", effective_n_total))
  cat(sprintf("Effective n per group: %.1f\n", effective_n_per_group))
  cat(sprintf("Target effect size (Cohen's d): %.2f\n", effect_size))
  
  if (!is.na(power_result$power)) {
    cat(sprintf("Statistical power: %.1f%%\n", power_result$power * 100))
    
    if (power_result$power < 0.80) {
      cat("\n⚠ WARNING: Power < 80%\n")
    } else {
      cat("\n✓ Adequate power achieved (≥80%)\n")
    }
  }
  
  cat("\n")
  
  list(
    power = power_result$power,
    effective_n_total = effective_n_total,
    effective_n_per_group = effective_n_per_group,
    design_effect = design_effect
  )
}

# ----------------------------------------------------------------------------
# PRE-TREATMENT BALANCE DIAGNOSTICS
# ----------------------------------------------------------------------------

check_pretreatment_balance <- function(log_dt, treatment_assignment) {
  cat("\n=== PRE-TREATMENT BALANCE CHECK ===\n")
  
  pre_period <- min(CONFIG$cohort1_treatment_period,
                    CONFIG$cohort2_treatment_period) - 1
  
  pre_dt <- log_dt[stage == "trading" & t <= pre_period]
  
  if (nrow(pre_dt) == 0) {
    cat("No pre-treatment observations found.\n")
    return(NULL)
  }
  
  pre_dt <- merge(pre_dt,
                  treatment_assignment[, .(agent_id, treatment_cohort)],
                  by = "agent_id", all.x = TRUE)
  
  pre_dt[, ever_treated := treatment_cohort %in% c(1, 2)]
  
  balance_stats <- pre_dt[, .(
    buy_rate = mean(buy_indicator, na.rm = TRUE),
    sell_rate = mean(sell_indicator, na.rm = TRUE),
    trade_rate = mean(trade_indicator, na.rm = TRUE),
    avg_portfolio_value = mean(portfolio_value, na.rm = TRUE),
    n_obs = .N,
    n_agents = uniqueN(agent_id)
  ), by = ever_treated]
  
  cat("\nPre-treatment means by treatment status:\n")
  print(balance_stats)
  
  cat("\n--- Balance Tests ---\n")
  
  buy_test <- t.test(buy_indicator ~ ever_treated, data = pre_dt)
  cat(sprintf("Buy Rate: Treated=%.1f%%, Control=%.1f%%, diff=%.1f pp, p=%.4f\n",
              balance_stats[ever_treated == TRUE, buy_rate] * 100,
              balance_stats[ever_treated == FALSE, buy_rate] * 100,
              (balance_stats[ever_treated == TRUE, buy_rate] - 
                 balance_stats[ever_treated == FALSE, buy_rate]) * 100,
              buy_test$p.value))
  
  trade_test <- t.test(trade_indicator ~ ever_treated, data = pre_dt)
  cat(sprintf("Trade Rate: Treated=%.1f%%, Control=%.1f%%, diff=%.1f pp, p=%.4f\n",
              balance_stats[ever_treated == TRUE, trade_rate] * 100,
              balance_stats[ever_treated == FALSE, trade_rate] * 100,
              (balance_stats[ever_treated == TRUE, trade_rate] - 
                 balance_stats[ever_treated == FALSE, trade_rate]) * 100,
              trade_test$p.value))
  
  pv_test <- t.test(portfolio_value ~ ever_treated, data = pre_dt)
  cat(sprintf("Portfolio Value: Treated=$%.0f, Control=$%.0f, diff=$%.0f, p=%.4f\n",
              balance_stats[ever_treated == TRUE, avg_portfolio_value],
              balance_stats[ever_treated == FALSE, avg_portfolio_value],
              balance_stats[ever_treated == TRUE, avg_portfolio_value] - 
                balance_stats[ever_treated == FALSE, avg_portfolio_value],
              pv_test$p.value))
  
  cat("\n--- Standardized Mean Differences ---\n")
  pooled_sd_buy   <- sd(pre_dt$buy_indicator, na.rm = TRUE)
  pooled_sd_trade <- sd(pre_dt$trade_indicator, na.rm = TRUE)
  pooled_sd_pv    <- sd(pre_dt$portfolio_value, na.rm = TRUE)
  
  smd_buy <- (balance_stats[ever_treated == TRUE, buy_rate] - 
                balance_stats[ever_treated == FALSE, buy_rate]) / pooled_sd_buy
  smd_trade <- (balance_stats[ever_treated == TRUE, trade_rate] - 
                  balance_stats[ever_treated == FALSE, trade_rate]) / pooled_sd_trade
  smd_pv <- (balance_stats[ever_treated == TRUE, avg_portfolio_value] - 
               balance_stats[ever_treated == FALSE, avg_portfolio_value]) / pooled_sd_pv
  
  cat(sprintf("Buy Rate SMD: %.3f %s\n", smd_buy, 
              if (abs(smd_buy) > 0.1) "⚠ IMBALANCED" else "✓"))
  cat(sprintf("Trade Rate SMD: %.3f %s\n", smd_trade,
              if (abs(smd_trade) > 0.1) "⚠ IMBALANCED" else "✓"))
  cat(sprintf("Portfolio Value SMD: %.3f %s\n", smd_pv,
              if (abs(smd_pv) > 0.1) "⚠ IMBALANCED" else "✓"))
  
  invisible(list(
    balance_stats = balance_stats,
    tests = list(buy = buy_test, trade = trade_test, pv = pv_test),
    smd = c(buy = smd_buy, trade = smd_trade, pv = smd_pv)
  ))
}

# ----------------------------------------------------------------------------
# INTERVENTION-AWARE ANALYSIS FUNCTIONS
# ----------------------------------------------------------------------------

prepare_clean_sample <- function(log_dt) {
  log_dt[, intervention_affected := FALSE]
  
  log_dt[stage == "trading" & intervention_type != "none",
         intervention_affected := TRUE]
  
  intervened_agent_periods <- log_dt[
    intervention_affected == TRUE,
    .(agent_id, t)
  ][, unique(.SD)]
  
  log_dt[intervened_agent_periods,
         intervention_affected := TRUE,
         on = .(agent_id, t)]
  
  cat("\n=== INTERVENTION SUMMARY ===\n")
  intervention_counts <- log_dt[stage == "trading", .(
    total_obs = .N,
    early_encouragement = sum(intervention_type == "early_encouragement", na.rm = TRUE),
    forced_activity     = sum(intervention_type == "forced_activity", na.rm = TRUE),
    any_intervention    = sum(intervention_type != "none", na.rm = TRUE)
  )]
  print(intervention_counts)
  
  cat(sprintf("\nIntervention rate: %.2f%%\n", 
              100 * intervention_counts$any_intervention / intervention_counts$total_obs))
  
  clean_dt <- log_dt[intervention_affected == FALSE]
  
  cat(sprintf("Clean sample: %d / %d observations (%.1f%% retained)\n",
              nrow(clean_dt), nrow(log_dt),
              100 * nrow(clean_dt) / nrow(log_dt)))
  
  list(
    full = log_dt,
    clean = clean_dt,
    intervention_counts = intervention_counts
  )
}

run_intervention_sensitivity <- function(log_dt, outcome_var = "buy_indicator") {
  cat("\n=== INTERVENTION SENSITIVITY ANALYSIS ===\n")
  
  prepared <- prepare_clean_sample(log_dt)
  full_trading  <- prepared$full[stage == "trading"]
  clean_trading <- prepared$clean[stage == "trading"]
  
  cat("\n--- Model 1: Full Sample (Robustness) ---\n")
  model_full <- tryCatch({
    fixest::feols(as.formula(paste(outcome_var, "~ is_treated_now | agent_id + t")),
                  data = full_trading, cluster = "agent_id")
  }, error = function(e) {
    cat("Model failed:", e$message, "\n")
    NULL
  })
  if (!is.null(model_full)) print(summary(model_full))
  
  cat("\n--- Model 2: Clean Sample (PRIMARY) ---\n")
  model_clean <- tryCatch({
    fixest::feols(as.formula(paste(outcome_var, "~ is_treated_now | agent_id + t")),
                  data = clean_trading, cluster = "agent_id")
  }, error = function(e) {
    cat("Model failed:", e$message, "\n")
    NULL
  })
  if (!is.null(model_clean)) print(summary(model_clean))
  
  cat("\n--- Model 3: Full Sample with Intervention Control ---\n")
  model_controlled <- tryCatch({
    fixest::feols(as.formula(paste(outcome_var, 
                                   "~ is_treated_now + intervention_affected | agent_id + t")),
                  data = full_trading, cluster = "agent_id")
  }, error = function(e) {
    cat("Model failed:", e$message, "\n")
    NULL
  })
  if (!is.null(model_controlled)) print(summary(model_controlled))
  
  if (!is.null(model_full) && !is.null(model_clean)) {
    coef_full  <- coef(model_full)["is_treated_nowTRUE"]
    coef_clean <- coef(model_clean)["is_treated_nowTRUE"]
    cat("\n--- Coefficient Comparison ---\n")
    cat(sprintf("Full sample effect: %.4f\n", coef_full))
    cat(sprintf("Clean sample effect: %.4f\n", coef_clean))
    if (abs(coef_full) > 0) {
      cat(sprintf("Difference: %.4f (%.1f%%)\n", 
                  coef_clean - coef_full,
                  100 * (coef_clean - coef_full) / abs(coef_full)))
    }
  }
  
  invisible(list(
    full = model_full,
    clean = model_clean,
    controlled = model_controlled
  ))
}

# ----------------------------------------------------------------------------
# JSON FALLBACK ANALYSIS
# ----------------------------------------------------------------------------

analyze_fallback_patterns <- function(log_dt) {
  cat("\n=== JSON FALLBACK ANALYSIS ===\n")
  
  fallback_rates <- log_dt[!is.na(json_fallback_used), .(
    total_obs = .N,
    fallback_count = sum(json_fallback_used, na.rm = TRUE),
    fallback_rate = mean(json_fallback_used, na.rm = TRUE) * 100
  ), by = stage]
  
  cat("\nFallback Rates by Stage (only stages with JSON expectations):\n")
  print(fallback_rates)
  
  cat("\n--- Fallback by Treatment Status ---\n")
  fallback_by_treatment <- log_dt[!is.na(json_fallback_used), .(
    fallback_rate = mean(json_fallback_used, na.rm = TRUE) * 100,
    n = .N
  ), by = .(stage, is_treated_now)]
  
  if (nrow(fallback_by_treatment) > 0) {
    print(data.table::dcast(fallback_by_treatment,
                            stage ~ is_treated_now,
                            value.var = "fallback_rate"))
  }
  
  for (s in unique(log_dt$stage)) {
    stage_data <- log_dt[stage == s & !is.na(json_fallback_used)]
    if (nrow(stage_data) > 0 &&
        data.table::uniqueN(stage_data$is_treated_now) > 1 &&
        data.table::uniqueN(stage_data$json_fallback_used) > 1) {
      tab <- table(stage_data$json_fallback_used, stage_data$is_treated_now)
      if (all(dim(tab) == c(2, 2))) {
        test <- prop.test(tab)
        cat(sprintf("\n%s: Treatment vs Control fallback difference p = %.4f %s\n",
                    s, test$p.value,
                    if (test$p.value < 0.05) "⚠ SIGNIFICANT" else "✓"))
      }
    }
  }
  
  cat("\n--- Fallback by Persona ---\n")
  fallback_by_persona <- log_dt[!is.na(persona) & !is.na(json_fallback_used), .(
    fallback_rate = mean(json_fallback_used, na.rm = TRUE) * 100,
    n = .N
  ), by = .(stage, persona)]
  
  if (nrow(fallback_by_persona) > 0) {
    print(data.table::dcast(fallback_by_persona,
                            persona ~ stage,
                            value.var = "fallback_rate"))
  }
  
  cat("\n--- Outcomes by Fallback Status (trading stage) ---\n")
  trading_dt <- log_dt[stage == "trading"]
  outcome_by_fallback <- trading_dt[!is.na(json_fallback_used), .(
    buy_rate   = mean(buy_indicator, na.rm = TRUE) * 100,
    sell_rate  = mean(sell_indicator, na.rm = TRUE) * 100,
    trade_rate = mean(trade_indicator, na.rm = TRUE) * 100,
    n = .N
  ), by = json_fallback_used]
  print(outcome_by_fallback)
  
  invisible(fallback_rates)
}

run_fallback_sensitivity <- function(log_dt, outcome_var = "buy_indicator") {
  cat("\n=== FALLBACK SENSITIVITY ANALYSIS ===\n")
  
  trading_dt <- log_dt[stage == "trading"]
  
  cat("\n--- Full Sample ---\n")
  model_full <- fixest::feols(
    as.formula(paste(outcome_var, "~ is_treated_now | agent_id + t")),
    data = trading_dt, cluster = "agent_id"
  )
  print(summary(model_full))
  
  cat("\n--- Excluding Fallback Observations ---\n")
  clean_trading <- trading_dt[json_fallback_used == FALSE]
  
  model_no_fallback <- fixest::feols(
    as.formula(paste(outcome_var, "~ is_treated_now | agent_id + t")),
    data = clean_trading, cluster = "agent_id"
  )
  print(summary(model_no_fallback))
  
  cat(sprintf("\nObservations dropped: %d (%.1f%%)\n",
              nrow(trading_dt) - nrow(clean_trading),
              100 * (nrow(trading_dt) - nrow(clean_trading)) / nrow(trading_dt)))
  
  invisible(list(full = model_full, no_fallback = model_no_fallback))
}

# ----------------------------------------------------------------------------
# DOSE-RESPONSE EXPERIMENT
# ----------------------------------------------------------------------------

run_dose_response_experiment <- function(intensities = c("low_dose", "medium_dose",
                                                         "high_dose", "extreme_dose",
                                                         "original"),
                                         n_periods = CONFIG$n_periods) {
  cat("\n")
  cat("========================================================================\n")
  cat("DOSE-RESPONSE EXPERIMENT\n")
  cat("========================================================================\n\n")
  
  CONFIG$dose_response$enabled <<- TRUE
  
  results_list <- list()
  
  local_persona_list <- rep(names(personas), length.out = CONFIG$n_agents)
  
  for (intensity in intensities) {
    mult <- CONFIG$dose_response$intensity_levels[[intensity]]$multiplier
    cat(sprintf("\n--- Running intensity: %s (x%.0f) ---\n", intensity, mult))
    
    local_agents <- lapply(1:CONFIG$n_agents, function(i) {
      Agent$new(
        id = sprintf("A%03d", i),
        persona = personas[[local_persona_list[i]]],
        llm = llm_fast,  # use fast model for attention/info, trading routed separately
        attention_budget = CONFIG$attention_budget,
        initial_cash = CONFIG$initial_cash
      )
    })
    
    local_market <- generate_market_environment(price_paths, n_periods,
                                                treatment_intensity = intensity)
    local_matching <- create_matching_function(local_agents, treatment_assignment, local_market)
    
    local_game <- Game$new(
      name = sprintf("DoseResponse_%s", intensity),
      rules = game_rules,
      stages = stages,
      agents = local_agents,
      matching = local_matching,
      periods = n_periods,
      history_writer = salience_history_writer,
      seed = CONFIG$seed + which(intensities == intensity)
    )
    
    local_log <- local_game$run()
    local_log[, treatment_intensity := intensity]
    local_log[, intensity_multiplier := mult]
    
    results_list[[intensity]] <- local_log
    
    trading_dt <- local_log[stage == "trading"]
    effect_est <- trading_dt[, .(
      buy_rate_treated = mean(buy_indicator[is_treated_now == TRUE], na.rm = TRUE),
      buy_rate_control = mean(buy_indicator[is_treated_now == FALSE], na.rm = TRUE)
    )]
    
    cat(sprintf("Effect estimate on buy rate (treated - control): %.1f pp\n",
                100 * (effect_est$buy_rate_treated - effect_est$buy_rate_control)))
  }
  
  combined_dt <- data.table::rbindlist(results_list, fill = TRUE)
  
  cat("\n=== DOSE-RESPONSE SUMMARY ===\n")
  dose_response_summary <- combined_dt[stage == "trading", .(
    buy_rate_treated = mean(buy_indicator[is_treated_now == TRUE], na.rm = TRUE),
    buy_rate_control = mean(buy_indicator[is_treated_now == FALSE], na.rm = TRUE),
    n = .N
  ), by = .(treatment_intensity, intensity_multiplier)]
  
  dose_response_summary[, effect := buy_rate_treated - buy_rate_control]
  dose_response_summary[, effect_pp := 100 * effect]
  
  print(dose_response_summary[order(intensity_multiplier)])
  
  invisible(list(
    results = results_list,
    summary = dose_response_summary
  ))
}

# ============================================================================
# RUN ALL SPECIFICATIONS SEQUENTIALLY (OPTIONAL)
# ============================================================================

run_all_specifications <- function() {
  specs_to_run <- c("original", "A", "B", "C")
  results_list <- list()
  
  for (spec in specs_to_run) {
    cat("\n")
    cat("========================================================================\n")
    cat(sprintf("RUNNING SPECIFICATION: %s\n", toupper(spec)))
    cat("========================================================================\n\n")
    
    CONFIG$attention_specification <<- spec
    
    game <- Game$new(
      name = sprintf("AttentionShock_Spec_%s", spec),
      rules = game_rules,
      stages = list(
        StageAttentionAllocation$new(),
        StageInformationRevelation$new(),
        StageTradingDecision$new()
      ),
      agents = agents,
      matching = matching_func,
      periods = CONFIG$n_periods,
      history_writer = salience_history_writer,
      seed = CONFIG$seed + which(specs_to_run == spec)
    )
    
    log_dt <- game$run()
    
    results_list[[spec]] <- log_dt
    fwrite(log_dt, sprintf("results_spec_%s.csv", spec))
    
    run_specification_validation(log_dt)
    
    cat(sprintf("\n✓ Specification %s complete\n", toupper(spec)))
  }
  
  saveRDS(results_list, "all_specifications_results.rds")
  cat("\n✓ All specifications complete. Results saved.\n\n")
  
  invisible(results_list)
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("EXPERIMENT EXECUTION\n")
cat("================================================================================\n\n")

cat("STEP 1: POWER ANALYSIS\n")
power_results <- compute_power_analysis(
  n_agents = CONFIG$n_agents,
  n_periods = CONFIG$n_periods,
  assumed_icc = CONFIG$assumed_icc,
  effect_size = CONFIG$target_effect_size,
  alpha = CONFIG$alpha
)

power_summary <- data.table::data.table(
  n_agents              = CONFIG$n_agents,
  n_periods             = CONFIG$n_periods,
  assumed_icc           = CONFIG$assumed_icc,
  effect_size           = CONFIG$target_effect_size,
  alpha                 = CONFIG$alpha,
  design_effect         = power_results$design_effect,
  effective_n_total     = power_results$effective_n_total,
  effective_n_per_group = power_results$effective_n_per_group,
  power                 = power_results$power
)
data.table::fwrite(power_summary, "power_summary.csv")

cat("STEP 2: PRICE PATH SIMULATION\n")
price_paths <- simulate_price_paths()

cat("STEP 2b: PRE-CALCULATING MARKET ENVIRONMENT\n")
market_data <- generate_market_environment(price_paths, CONFIG$n_periods)

cat("STEP 3: AGENT INITIALIZATION\n")

# Two-model design: fast (System 1) vs slow (System 2)
llm_fast <- LLMClient$new(
  model       = CONFIG$llm$model,
  temperature = CONFIG$llm$temperature,
  disk_cache  = DISK_CACHE
)

llm_slow <- LLMClient$new(
  model       = CONFIG$llm$trading_model,
  temperature = CONFIG$llm$temperature,
  disk_cache  = DISK_CACHE
)

# Backwards-compatibility alias where code still expects `llm`
llm <- llm_fast

probe <- llm_fast$chat(
  system = "Reply exactly with OK — uppercase, no punctuation, no quotes, no code fences.",
  user   = "Say OK",
  max_retries = 5
)
cat("Probe:\n", probe, "\n")

ok <- identical(trimws(probe), "OK")

if (!ok) {
  cat("First probe not exact 'OK'; retrying once with reinforced instruction...\n")
  probe <- llm_fast$chat(
    system = "Return ONLY the two letters OK. No punctuation, no spaces, no quotes, no markdown.",
    user   = "Respond: OK"
  )
  cat("Probe (retry):\n", probe, "\n")
  ok <- identical(trimws(probe), "OK")
}

if (!ok) {
  stop(sprintf("LLM probe failed; expected 'OK' got: %s", probe))
}

cat("✓ LLM probe succeeded.\n")


persona_list <- rep(names(personas), length.out = CONFIG$n_agents)

agents <- lapply(1:CONFIG$n_agents, function(i) {
  Agent$new(
    id = sprintf("A%03d", i),
    persona = personas[[persona_list[i]]],
    llm = llm_fast,  # fast model for non-trading persona stuff
    attention_budget = CONFIG$attention_budget,
    initial_cash = CONFIG$initial_cash
  )
})

cat(sprintf("✓ Initialized %d agents across %d personas\n\n", 
            CONFIG$n_agents, length(unique(persona_list))))

cat("STEP 4: TREATMENT ASSIGNMENT\n")

treatment_assignment <- data.table(
  agent_id = sprintf("A%03d", 1:CONFIG$n_agents),
  persona = persona_list
)

treatment_assignment[, sensitivity := sapply(persona, function(p) {
  personas[[p]]$sensitivity
})]

balance_randomization <- function(dt, seed, max_attempts = 1000, n_arms = 3L) {   # [R1] generalized
  best_assignment <- NULL
  best_imbalance <- Inf
  best_seed <- seed
  
  for (attempt in 1:max_attempts) {
    set.seed(seed + attempt - 1)
    
    candidate <- copy(dt)
    candidate[, treatment_cohort := NA_integer_]
    
    for (p in unique(candidate$persona)) {
      idx <- which(candidate$persona == p)
      n <- length(idx)
      if (n == 0) next
      
      n_per_cohort <- n %/% n_arms   # [R1]
      remainder    <- n %% n_arms    # [R1]
      
      # Start with equal counts in each cohort
      base_counts <- rep(n_per_cohort, n_arms)   # [R1]
      
      # Randomly choose which cohort(s) get the extra agent(s)
      if (remainder > 0L) {
        extra_idx <- sample(seq_len(n_arms), size = remainder, replace = FALSE)   # [R1]
        base_counts[extra_idx] <- base_counts[extra_idx] + 1L
      }
      
      # Build the cohort label vector (e.g. 6,5,5 in some random order)
      base_cohorts <- rep(seq_len(n_arms), times = base_counts)   # [R1]
      
      # Randomly assign these labels within the persona
      candidate$treatment_cohort[idx] <- sample(base_cohorts)
    }
    
    cohort_counts <- candidate[, .N, by = treatment_cohort]
    cohort_imbalance <- max(cohort_counts$N) - min(cohort_counts$N)
    persona_arm_counts <- candidate[, .N, by = .(persona, treatment_cohort)]              # [R1] generalized to n_arms
    persona_imbalance  <- persona_arm_counts[, .(d = max(N) - min(N)), by = persona]$d    # [R1]
    total_persona_imbalance <- sum(persona_imbalance)
    
    imbalance_score <- 10 * cohort_imbalance + total_persona_imbalance
    
    if (imbalance_score < best_imbalance) {
      best_imbalance <- imbalance_score
      best_assignment <- copy(candidate)
      best_seed <- seed + attempt - 1
      
      if (imbalance_score == 0) {
        cat(sprintf("✓ Perfect balance found at attempt %d (seed %d)\n", attempt, best_seed))
        break
      }
    }
  }
  
  cat(sprintf("Best balance achieved: imbalance score = %d (seed = %d)\n", 
              best_imbalance, best_seed))
  
  list(assignment = best_assignment, seed = best_seed, score = best_imbalance)
}

balance_result <- balance_randomization(treatment_assignment, CONFIG$seed, n_arms = if (r1_factorial()) 4L else 3L)   # [R1]
treatment_assignment <- balance_result$assignment
CONFIG$actual_randomization_seed <- balance_result$seed

cat("\n=== BALANCE VERIFICATION ===\n")
cat("\nCohort Distribution:\n")
cohort_counts <- treatment_assignment[, .N, by = treatment_cohort][order(treatment_cohort)]
print(cohort_counts)

max_diff <- max(cohort_counts$N) - min(cohort_counts$N)
if (max_diff > 2) {
  stop(sprintf("❌ CRITICAL: Cohort imbalance detected! Max difference: %d", max_diff))
} else {
  cat("✓ Cohort balance verified (max difference ≤ 2)\n")
}

cat("\nPersona × Cohort Cross-tabulation:\n")
persona_cohort_tab <- data.table::dcast(
  treatment_assignment[, .N, by = .(persona, treatment_cohort)],
  persona ~ treatment_cohort, value.var = "N", fill = 0
)
print(persona_cohort_tab)

persona_cohort_matrix <- as.matrix(persona_cohort_tab[, -1, with = FALSE])
rownames(persona_cohort_matrix) <- persona_cohort_tab$persona
chisq_result <- suppressWarnings(chisq.test(persona_cohort_matrix))
cat(sprintf("\nChi-square test for persona-cohort independence: χ² = %.2f, p = %.4f\n",
            chisq_result$statistic, chisq_result$p.value))

if (chisq_result$p.value < 0.10) {
  warning("⚠ Persona-cohort association detected - consider re-randomizing")
}

sensitivity_by_cohort <- treatment_assignment[, .N, by = .(sensitivity, treatment_cohort)]
sensitivity_by_cohort_wide <- data.table::dcast(
  sensitivity_by_cohort,
  sensitivity ~ treatment_cohort, value.var = "N", fill = 0
)
cat("\nSensitivity by Cohort:\n")
print(sensitivity_by_cohort_wide)

sens_imbalance <- sensitivity_by_cohort[, .(max_diff = max(N) - min(N)), by = sensitivity]
if (any(sens_imbalance$max_diff > 1)) {
  warning("⚠ Sensitivity-cohort imbalance > 1 in at least one stratum (acceptable for small N)")
}

# [R1] Arm mapping. Factorial (Rerun A): cohort 1 = viral@5bps, 2 = viral@15bps
# (replicates original treatment), 3 = cost-only (normal signals @15bps), 4 = control.
if (r1_factorial()) {
  arm_map <- data.table(
    treatment_cohort = 1:4,
    attention_arm = c("viral",  "viral",   "normal",          "normal"),
    cost_arm      = c("low",    "high",    "high",            "low"),
    arm_label     = c("viral_5bps", "viral_15bps", "cost_only_15bps", "control")
  )
  treatment_assignment <- arm_map[treatment_assignment, on = "treatment_cohort"]
  treatment_assignment[, treatment_period := fifelse(arm_label == "control", Inf, as.numeric(CONFIG$r1$adoption_period))]
} else {
  treatment_assignment[, attention_arm := fifelse(treatment_cohort %in% c(1L, 2L), "viral", "normal")]
  treatment_assignment[, cost_arm      := fifelse(treatment_cohort %in% c(1L, 2L), "high", "low")]
  treatment_assignment[, arm_label     := fifelse(treatment_cohort == 1L, "cohort1_t60",
                                          fifelse(treatment_cohort == 2L, "cohort2_t120", "control"))]
  treatment_assignment[, treatment_period := fcase(
    treatment_cohort == 1, CONFIG$cohort1_treatment_period,
    treatment_cohort == 2, CONFIG$cohort2_treatment_period,
    default = Inf
  )]
}

treatment_assignment[, never_treated := is.infinite(treatment_period)]
treatment_assignment[, treatment_period_encoded := ifelse(
  is.infinite(treatment_period), 0, treatment_period
)]

cat("\nTreatment Assignment Summary:\n")
summary_table <- treatment_assignment[, .(
  n_agents = .N,
  treatment_period = first(treatment_period)
), by = treatment_cohort][order(treatment_cohort)]
print(summary_table)

cat("\nBy Persona:\n")
print(treatment_assignment[, .N, by = .(persona, treatment_cohort)][order(persona, treatment_cohort)])

# [R1] Social Momentum prompt-variant assignment (balanced across arms), Major 5 validation
treatment_assignment[, sm_variant := NA_character_]
if (isTRUE(CONFIG$r1$sm_prompt_variants) && r1_factorial()) {
  sm_rows <- treatment_assignment[persona == "SocialMomentum"]
  setorder(sm_rows, treatment_cohort, agent_id)
  sm_rows[, sm_variant := rep(c("v1", "v2", "v3"), length.out = .N)]   # rotates across arms for balance
  treatment_assignment[sm_rows, sm_variant := i.sm_variant, on = "agent_id"]
  for (k in seq_len(nrow(sm_rows))) SM_VARIANT_ASSIGN[[sm_rows$agent_id[k]]] <- sm_rows$sm_variant[k]
  for (ag in agents) {
    v <- sm_variant_of(ag$id)
    if (!is.na(v)) {
      ag$persona <- list(name = "SocialMomentum",
                         system_prompt = SM_VARIANT_PROMPTS[[v]],
                         sensitivity = "high")
    }
  }
  cat("\n[R1] SocialMomentum variant assignment:\n")
  print(treatment_assignment[persona == "SocialMomentum", .N, by = .(treatment_cohort, sm_variant)])
}

fwrite(treatment_assignment, "treatment_assignment.csv")
cat("\n✓ Treatment assignment saved\n\n")

cat("STEP 4.5: CREATING OPTIMIZED MATCHING FUNCTION\n")
matching_func <- create_matching_function(agents, treatment_assignment, market_data)

stages <- list(
  StageAttentionAllocation$new(),
  StageInformationRevelation$new(),
  StageTradingDecision$new()
)

game_rules <- "You are a retail trader on Robinhood. Maximize profit through strategic attention allocation and trading. Transaction costs apply. Follow JSON format exactly."

# ============================================================================
# VALIDATION TEST RUN
# ============================================================================

RUN_VALIDATION_TEST <- TRUE

if (RUN_VALIDATION_TEST) {
  
  cat("\n")
  cat("================================================================================\n")
  cat("VALIDATION TEST RUN\n")
  cat("================================================================================\n\n")
  
  cat("Running 2-period test with 2 agents to validate logging...\n")
  
  test_agents <- agents[1:2]
  test_agent_ids <- sapply(test_agents, function(a) a$id)
  
  test_treatment_assignment <- treatment_assignment[agent_id %in% test_agent_ids]
  
  test_matching <- create_matching_function(
    agents = test_agents, 
    treatment_assignment = test_treatment_assignment,
    market_data = market_data
  )
  
  test_stages <- list(
    StageAttentionAllocation$new(),
    StageInformationRevelation$new(),
    StageTradingDecision$new()
  )
  
  test_game <- Game$new(
    name = "ValidationTest",
    rules = game_rules,
    stages = test_stages,
    agents = test_agents,
    matching = test_matching,
    periods = 2,
    history_writer = salience_history_writer,
    seed = CONFIG$seed + 999
  )
  
  test_log <- tryCatch({
    test_game$run(save_interval = 1)
  }, error = function(e) {
    cat("\n❌ VALIDATION TEST FAILED!\n")
    cat("Error message: ", e$message, "\n")
    cat("Error class: ", class(e), "\n")
    cat("Error call: ")
    print(e$call)
    cat("\nCondition object:\n")
    print(str(e))
    cat("\nFull traceback:\n")
    traceback()
    cat("\n")
    stop("Fix the errors above before running full experiment")
  })
  
  cat("\n=== Validation Test Results ===\n")
  cat(sprintf("Total rows: %d\n", nrow(test_log)))
  
  if (nrow(test_log) == 0) {
    cat("\n❌ VALIDATION FAILED: No data was logged!\n\n")
    stop("No rows in test log - logging is completely broken")
  }
  
  test_summary <- test_log[, .N, by = stage]
  cat("\nRows by stage:\n")
  print(test_summary)
  
  required_stages <- c("attention_allocation", "info_revelation", "trading")
  missing_stages <- setdiff(required_stages, test_log$stage)
  
  if (length(missing_stages) > 0) {
    cat("\n❌ VALIDATION FAILED!\n")
    cat(sprintf("Missing stages: %s\n", paste(missing_stages, collapse = ", ")))
    cat("\n")
    stop("Fix logging issues before running full experiment")
  }
  
  test_trades <- test_log[stage == "trading", sum(trade_indicator, na.rm = TRUE)]
  cat(sprintf("\nTotal trades in test: %d\n", test_trades))
  
  if (test_trades == 0) {
    cat("\n⚠ WARNING: No trades in validation test\n")
    cat("This may indicate:\n")
    cat("  1. Agents are too conservative\n")
    cat("  2. Prompts need adjustment\n")
    cat("  3. Attention constraint is too restrictive\n\n")
    cat("Proceeding with full run, but results may have low trading activity.\n")
  }
  
  cat("\n✓ VALIDATION TEST PASSED - All stages logging correctly\n")
  cat("Proceeding with full experiment...\n\n")
  
  cat("================================================================================\n\n")
}

rm(test_game, test_log, test_agents, test_matching, test_stages, 
   test_summary, test_agent_ids, test_treatment_assignment)
gc()

# ---------------------------------------------------------------------------
# RESET AGENTS AFTER VALIDATION TEST
# ---------------------------------------------------------------------------
# Ensure main experiment starts from a clean initial state for all agents
agents <- lapply(1:CONFIG$n_agents, function(i) {
  Agent$new(
    id = sprintf("A%03d", i),
    persona = personas[[persona_list[i]]],
    llm = llm_fast,
    attention_budget = CONFIG$attention_budget,
    initial_cash = CONFIG$initial_cash
  )
})

# ============================================================================
# PRIMARY GAME EXECUTION
# ============================================================================
game <- Game$new(
  name = "AttentionShockTrading",
  rules = game_rules,
  stages = stages,
  agents = agents,
  matching = matching_func,
  periods = CONFIG$n_periods,
  history_writer = salience_history_writer,
  seed = CONFIG$seed
)


cat("STEP 5: RUNNING EXPERIMENT\n")
cat(sprintf("Periods: %d\n", CONFIG$n_periods))
cat(sprintf("Total observations: %s\n", 
            format(CONFIG$n_agents * CONFIG$n_periods * 3, big.mark = ",")))
cat("================================================================================\n\n")

start_time <- Sys.time()
log_dt <- game$run()
end_time <- Sys.time()
runtime <- difftime(end_time, start_time, units = "mins")
runtime

cat("\n=== MOMENTUM CALCULATION DIAGNOSTICS ===\n")

early_momentum <- log_dt[
  stage == "trading" & t <= 10 & !is.na(ticker),
  .(
    avg_momentum = mean(momentum, na.rm = TRUE),
    sd_momentum  = sd(momentum, na.rm = TRUE)
  ),
  by = .(t, ticker)
]

cat("\nEarly period momentum by ticker (should be ~0 at t=1, no systematic bias):\n")
print(early_momentum)

lookback_dist <- log_dt[stage == "trading", .(
  min_lookback = min(lookback_periods, na.rm = TRUE),
  max_lookback = max(lookback_periods, na.rm = TRUE),
  median_lookback = median(lookback_periods, na.rm = TRUE)
), by = .(period_bin = cut(t, breaks = c(0, 5, 20, 100, 200)))]

cat("\nLookback window progression:\n")
print(lookback_dist)

signal_dist <- log_dt[stage == "trading" & !is.na(signal_strength), .(
  N = .N,
  pct = .N / nrow(log_dt[stage == "trading" & !is.na(signal_strength)])
), by = signal_strength]

cat("\nSignal strength distribution (should have noise/weak/medium/strong):\n")
print(signal_dist)

cat("================================================================================\n")
cat("SAVING RESULTS (ATOMIC)\n")
cat("================================================================================\n\n")

# Add treatment metadata
log_dt <- merge(
  log_dt,
  treatment_assignment,
  by = "agent_id",
  all.x = TRUE,
  suffixes = c("", "_treat")
)

log_dt[, `:=`(
  treated       = !never_treated,
  cohort1_post  = (treatment_cohort == 1 & is_treated_now),
  cohort2_post  = (treatment_cohort == 2 & is_treated_now)
)]

setorder(log_dt, agent_id, t, stage)

log_dt[stage == "trading", `:=`(
  lag_portfolio_value = shift(portfolio_value, 1, type = "lag"),
  next_portfolio_value = shift(portfolio_value, 1, type = "lead"),
  forward_return = (shift(portfolio_value, 1, type = "lead") - portfolio_value) / 
    pmax(portfolio_value, 1)
), by = agent_id]


# Intervention-aware datasets
cat("Creating intervention-clean and full datasets...\n")
prepared_data <- prepare_clean_sample(log_dt)

write_atomic_csv(prepared_data$full,  "experiment_results_full.csv")
write_atomic_csv(prepared_data$clean, "experiment_results_clean.csv")

cat("\n✓ Saved intervention-flagged full and clean datasets\n\n")

cat("Saving final results...\n")
write_atomic_csv(log_dt, "experiment_results_final.csv")
save_atomic_rds(log_dt, "experiment_results_final.rds")
fwrite(log_dt, "experiment_full_log.csv")

runtime_info <- list(
  start_time = start_time,
  end_time = end_time,
  runtime_minutes = as.numeric(runtime),
  total_observations = nrow(log_dt),
  llm_stats = list(
    fast_model        = CONFIG$llm$model,
    slow_model        = CONFIG$llm$trading_model,
    total_calls_fast  = llm_fast$call_count,
    total_tokens_fast = llm_fast$total_tokens,
    total_calls_slow  = llm_slow$call_count,
    total_tokens_slow = llm_slow$total_tokens
  )
)
save_atomic_rds(runtime_info, "experiment_runtime_info.rds")

cat(sprintf("✓ Results saved safely: %s rows\n", format(nrow(log_dt), big.mark = ",")))
cat(sprintf("✓ Runtime: %.2f minutes\n", as.numeric(runtime)))

log_info("Full log saved: {nrow(log_dt)} observations")

cat("\n")
cat("================================================================================\n")
cat("EXPERIMENT COMPLETE\n")
cat("================================================================================\n\n")
