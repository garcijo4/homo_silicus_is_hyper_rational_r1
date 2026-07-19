# =============================================================================
# worked_example_appendix_B.R
# -----------------------------------------------------------------------------
# Generates manuscript Appendix B: a complete, code-regenerated reconstruction
# of one agent-period from the original reported run of "Homo Silicus is
# Hyper-Rational" (JEIC Revision 1) - the persona system prompt, game rules,
# rendered memory block, all three stage prompts (attention allocation with
# the social leaderboard, information revelation, trading decision with the
# displayed edge lines and Action Priority instruction), paired with the
# agent's logged decisions. No API calls are made.
#
# HOW IT WORKS
#   All market and rendering functions below are verbatim copies extracted
#   from the experiment script (Attention_driven_trading_R1.R), so the
#   reconstruction uses the exact production templates. The market
#   environment is re-simulated deterministically from the experiment seed
#   (CONFIG$seed = 42); holdings, cash, and decisions are replayed from the
#   run log.
#
# PROVENANCE NOTES (three deliberate departures from the raw extraction)
#   1. Logger stubs: the extracted functions reference the experiment's
#      logger; no-op stubs are defined here so the file is self-contained.
#   2. CONFIG$r1$design is set to "original" (the experiment script default
#      is "factorial" for Rerun A): the worked example reconstructs the
#      ORIGINAL reported run, and this setting selects the original-design
#      display-string branch.
#   3. Paths are configurable below instead of hard-coded.
#
# CAVEATS (stated in the manuscript)
#   Prompts were not logged verbatim in the original run, so the output is a
#   code-regenerated reconstruction, not a transcript. The per-period view
#   index is fixed at 1, whereas the realized run drew one of five
#   overlay-noise views: the fundamental draw, news label, and small
#   attention-cost jitter may differ from what the agent saw. The example
#   period t = 61 is the SECOND treated period for Cohort 1 (exposure begins
#   at t = 60).
#
# USAGE
#   Rscript R/worked_example_appendix_B.R
#     -> writes Appendix_B_worked_example.txt to output/
#   Data: data/original_run/experiment_full_log.csv and
#         data/original_run/treatment_assignment.csv (override root with
#         ADT_DATA_ROOT). Requires: data.table, glue, digest.
#   Sourcing with WE_FUNCTIONS_ONLY <- TRUE defined loads the market and
#   rendering functions without running the driver (used by
#   reproduce_results.R for the deterministic price paths).
# =============================================================================


suppressPackageStartupMessages({
  library(data.table); library(glue); library(digest)
})
TEST_MODE <- FALSE


# Logger stubs (provenance note 1): the experiment's logging framework is
# not needed for reconstruction.
log_info <- function(...) invisible(NULL); log_warn <- function(...) invisible(NULL); log_error <- function(...) invisible(NULL); log_debug <- function(...) invisible(NULL)
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    y
  } else {
    x
  }
}

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

CONFIG$r1 <- list(
  enabled = TRUE,
  design  = "original",     # provenance note 2: "original" reproduces the reported run (experiment default is "factorial")
  adoption_period = 60,      # single adoption time for all treated arms (factorial needs no staggering)
  neutral_tickers = FALSE,   # TRUE = Rerun B (model sees invented names; logs keep internal names)
  sm_prompt_variants = TRUE, # randomize SocialMomentum agents across 3 prompt constructions (Major 5)
  tickers_internal        = c("AAPL","NVDA","AMC","GME"),
  tickers_display_neutral = c("VNTA","KRLO","ZMTX","QRLP")  # VERIFY these are not real listed symbols before running
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

tc_bps <- function(sym, buzz_level, rank) {
  base <- 5
  is_viral <- (!is.na(rank) && rank <= 2) || 
    (!is.null(buzz_level) && buzz_level %in% c("high", "viral", "VIRAL"))
  surge <- if (is_viral) 10 else 0
  base + surge
}

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

game_rules <- "You are a retail trader on Robinhood. Maximize profit through strategic attention allocation and trading. Transaction costs apply. Follow JSON format exactly."


# ============================================================================
# DRIVER (skipped when WE_FUNCTIONS_ONLY is defined by a sourcing script)
# ============================================================================
if (!exists("WE_FUNCTIONS_ONLY")) {

# Choices below mirror the reported run: treated Social Momentum agent, first
# post-treatment period of Cohort 1 (t = 61), original (v1) persona construction.

EX_T    <- 61       # example period (Cohort 1 treated from t = 60)
VIEW_IX <- 1        # one of the five pre-calculated views (see caveat below)

# Point these at the reported run's outputs (used to pair the reconstruction
# with the agent's actual logged decisions). Adjust paths as needed.
DATA <- Sys.getenv("ADT_DATA_ROOT", file.path(dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)), "..", "data"))
LOG_CSV    <- file.path(DATA, "original_run", "experiment_full_log.csv")
ASSIGN_CSV <- file.path(DATA, "original_run", "treatment_assignment.csv")

set.seed(CONFIG$seed)
cat("Re-simulating the seeded market environment (no API calls)...\n")
price_paths <- simulate_price_paths(CONFIG$n_periods)
market_data <- generate_market_environment(price_paths, CONFIG$n_periods)
mkt  <- market_data[[EX_T]]
view <- mkt$shared_views[[VIEW_IX]]

# --- pick the example agent (SocialMomentum, Cohort 1) --------------------
EX_AGENT <- "A001"
assign_ok <- file.exists(ASSIGN_CSV)
if (assign_ok) {
  ta <- data.table::fread(ASSIGN_CSV)
  cand <- ta[persona == "SocialMomentum" & treatment_cohort == 1]
  if (nrow(cand) > 0) EX_AGENT <- cand$agent_id[1]
}
cat(sprintf("Example agent: %s (SocialMomentum, Cohort 1), period t = %d\n", EX_AGENT, EX_T))

log_ok <- file.exists(LOG_CSV)
lg <- NULL
if (log_ok) {
  lg <- data.table::fread(LOG_CSV)
  lg <- lg[agent_id == EX_AGENT]
}

# --- replay holdings and cash from the log (execute_trade arithmetic) -----
cash <- CONFIG$initial_cash
port <- data.table::data.table(ticker = character(), shares = numeric(), cost_basis = numeric())
last_trade_t <- 0
if (!is.null(lg)) {
  tr <- lg[stage == "trading" & t < EX_T & trade_success == TRUE & !is.na(ticker)][order(t)]
  for (k in seq_len(nrow(tr))) {
    tk <- tr$ticker[k]; sh <- as.numeric(tr$shares[k]); px <- price_paths[[tk]][tr$t[k]]
    tc <- suppressWarnings(as.numeric(tr$base_tc_bps[k])); if (is.na(tc)) tc <- 5
    fee <- sh * px * tc / 10000
    if (tr$action[k] == "buy") {
      total <- sh * px + fee; cash <- cash - total
      if (tk %in% port$ticker) {
        i <- which(port$ticker == tk)
        ns <- port$shares[i] + sh
        nb <- (port$shares[i] * port$cost_basis[i] + total) / ns
        port[i, `:=`(shares = ns, cost_basis = nb)]
      } else port <- rbind(port, data.table::data.table(ticker = tk, shares = sh, cost_basis = (total) / sh))
      last_trade_t <- tr$t[k]
    } else if (tr$action[k] == "sell") {
      i <- which(port$ticker == tk)
      if (length(i)) {
        cash <- cash + sh * px - fee
        ns <- port$shares[i] - sh
        if (ns < 0.01) port <- port[-i] else port[i, shares := ns]
      }
      last_trade_t <- tr$t[k]
    }
  }
}
prices_now <- lapply(c("AAPL","NVDA","AMC","GME"), function(s) price_paths[[s]][EX_T])
names(prices_now) <- c("AAPL","NVDA","AMC","GME")
pv <- cash + if (nrow(port)) sum(sapply(seq_len(nrow(port)), function(i) port$shares[i] * prices_now[[port$ticker[i]]])) else 0

# --- history block (mirrors salience_history_writer: last 3 + performance) --
hist_lines <- "History: First period."
if (!is.null(lg)) {
  h <- lg[stage == "trading" & t < EX_T][order(t)]
  h <- utils::tail(h, 3)
  if (nrow(h)) {
    hl <- sprintf("[t=%d] %s %s%s → $%.2f", h$t, h$action,
                  ifelse(is.na(h$ticker), "", disp(h$ticker)),
                  ifelse(h$trade_success, "", " [FAILED]"),
                  round(as.numeric(h$payoff), 2))
    pnl_pct <- 100 * (pv - CONFIG$initial_cash) / CONFIG$initial_cash
    perf <- if (pnl_pct > 5) "💰" else if (pnl_pct > 0) "✓" else if (pnl_pct > -5) "⚠" else "📉"
    hist_lines <- sprintf("%s Portfolio: $%.0f (%+.1f%%). Recent: %s", perf, pv, pnl_pct, paste(hl, collapse = " | "))
  }
}

# --- Stage 1: attention-allocation prompt (treated => shock leaderboard) ----
suffix <- "
CRITICAL RULES:
1. The 'Leader' is the stock with the HIGHEST 'Mentions' count.
2. If multiple stocks are 'VIRAL', compare their Mention counts directly.
3. Your 'manipulation_check' accuracy is tracked. Read the numbers carefully.
"
attn_prompt <- glue::glue("
### 📊 SOCIAL LEADERBOARD (t={EX_T})
Stocks sorted by activity level:

{mkt$lb_txt_shock}
🚨 VIRAL ACTIVITY DETECTED

---
**Status:** Budget: 100 pts
**Rule:** 🛑 ASYMMETRIC: Must research to BUY.
{suffix}

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

# --- the agent's actual logged allocation at EX_T ---------------------------
alloc <- c(AAPL = "quick", NVDA = "quick", AMC = "quick", GME = "quick")  # fallback
attn_row <- NULL
if (!is.null(lg)) {
  attn_row <- lg[stage == "attention_allocation" & t == EX_T]
  if (nrow(attn_row)) {
    parts <- strsplit(attn_row$allocation[1], ",\\s*")[[1]]
    for (p in parts) { kv <- strsplit(p, ":")[[1]]; alloc[trimws(kv[1])] <- trimws(kv[2]) }
  }
}

# --- Stage 2: revealed information panel ------------------------------------
strs <- view$strings[["SocialMomentum"]]$shock
info_pieces <- character(0)
for (tk in c("AAPL","NVDA","AMC","GME")) {
  lvl <- alloc[[tk]]
  if (identical(lvl, "deep")) {
    info_pieces <- c(info_pieces, strs$info[[tk]])
  } else if (identical(lvl, "quick")) {
    row_sig <- mkt$tickers_dt[symbol == tk]
    info_pieces <- c(info_pieces, sprintf(
      "%s [QUICK]: $%.2f (%.1f%%) | Social: %s | Signal: %s",
      disp(tk), row_sig$current_price, row_sig$momentum * 100,
      mkt$market_social_shock[symbol == tk]$buzz_level, row_sig$signal_trade_signal))
  }
}
info_prompt <- paste(
  "INCOMING RESEARCH REPORTS",
  paste(info_pieces, collapse = "\n\n"),
  "----------------",
  "TASK: In 1–3 sentences of plain English, reflect on how this information affects your view of your portfolio and future trades.",
  "Do NOT use JSON, markdown, bullet points, or code fences. Write natural sentences only.",
  sep = "\n")

# --- Stage 3: trading-decision prompt ---------------------------------------
can_buy <- names(alloc)[alloc %in% c("deep","quick")]
holdings_str <- if (nrow(port)) paste(sapply(seq_len(nrow(port)), function(i) {
  tk <- port$ticker[i]; px <- prices_now[[tk]]
  sprintf("%s: %.0f sh @ $%.2f (%+.1f%%)", disp(tk), port$shares[i], port$cost_basis[i],
          100 * (px - port$cost_basis[i]) / port$cost_basis[i])
}), collapse = " | ") else "NO HOLDINGS"
signal_summary <- unname(strs$deep[can_buy])
rules_block <- glue::glue("
RULES:
- ALLOWED TO BUY: {if (length(can_buy)) paste(disp(can_buy), collapse=', ') else 'none'}
- ALLOWED TO SELL: Any stock you currently own.")
guidance_reinforcement <- "Action Priority: Follow the crowd. If a stock has high buzz/rank, buy it regardless of fundamentals."
trade_prompt <- glue::glue("
TRADING DECISION (t={EX_T}, persona=SocialMomentum)
Holdings: {holdings_str}
Cash: ${sprintf('%.2f', cash)}

MARKET DATA (Available Research):
{paste(signal_summary, collapse = '\n')}

{rules_block}

TRADING GUIDANCE:
- Build positions early in the experiment.
- Never spend more than about 30% of your available cash in a single trade.
- If your cash is very low, prefer HOLD or SELL over BUY.
- {guidance_reinforcement}
- Periods since last trade: {EX_T - last_trade_t}

OUTPUT FORMAT:
You MUST respond with a single valid JSON object and nothing else.
Schema:
{{\"action\":\"buy\"|\"sell\"|\"hold\",\"ticker\":{TICKER_ENUM_DISP}|null,\"shares\":integer,\"rationale\":string}}
")

sys_prompt <- personas$SocialMomentum$system_prompt
# Full user message for any stage = game_rules + "\n\n" + hist_lines + "\n\n" + stage prompt
# + "\n\nIMPORTANT: Act strictly according to your Persona: SocialMomentum" (see Agent$act).

out <- c(
"================================================================================",
"APPENDIX B - ONE AGENT-PERIOD IN FULL (code-regenerated reconstruction)",
"================================================================================",
sprintf("Agent: %s | Persona: SocialMomentum (v1 construction) | Period: t = %d (Cohort 1, post-treatment)", EX_AGENT, EX_T),
"",
"CAVEATS: Prompts were not logged verbatim in the original run. This reconstruction",
"re-renders them from the seeded market generator and the Stage templates in the",
"experiment script. The per-period view index is fixed at 1 here; the realized run",
"drew one of five views whose overlay noise (fundamental draw, news label, small",
"attention-cost jitter) may differ. Holdings/cash are replayed from the run log.",
"",
"--- MODEL CALL STRUCTURE (both models; no shared conversation state) -----------",
"system  = persona identity (below)",
"user    = game rules + rendered history + stage prompt (below)",
"",
"--- PERSONA SYSTEM PROMPT ------------------------------------------------------",
sys_prompt,
"",
"--- GAME RULES (prepended to every user message) -------------------------------",
game_rules,
"",
"--- RENDERED HISTORY / MEMORY (last 3 periods + performance line) ---------------",
hist_lines,
"",
"================= STAGE 1: ATTENTION ALLOCATION (GPT-4o-mini) ==================",
attn_prompt,
"",
"--- AGENT'S LOGGED STAGE-1 RESPONSE ---------------------------------------------",
if (!is.null(attn_row) && nrow(attn_row)) sprintf("allocation: %s\nmanipulation check: perceived=%s actual=%s passed=%s\nreasoning: %s",
    attn_row$allocation[1], attn_row$perceived_highest_buzz[1], attn_row$actual_highest_buzz[1],
    attn_row$manipulation_check_passed[1], attn_row$reasoning[1]) else "[log not found - set LOG_CSV]",
"",
"================= STAGE 2: INFORMATION REVELATION (GPT-4o-mini) ================",
info_prompt,
"",
"--- AGENT'S LOGGED STAGE-2 REFLECTION -------------------------------------------",
if (!is.null(lg)) { r2 <- lg[stage == "info_revelation" & t == EX_T]; if (nrow(r2)) r2$rationale_full[1] else "[none logged]" } else "[log not found]",
"",
"================= STAGE 3: TRADING DECISION (GPT-4.1-mini) =====================",
trade_prompt,
"",
"--- AGENT'S LOGGED STAGE-3 DECISION ---------------------------------------------",
if (!is.null(lg)) { r3 <- lg[stage == "trading" & t == EX_T]; if (nrow(r3)) sprintf("action=%s ticker=%s shares=%s\nrationale: %s",
    r3$action[1], r3$ticker[1], r3$shares[1], r3$rationale_full[1]) else "[none logged]" } else "[log not found]",
"",
"================================================================================")

script_dir <- dirname(gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE))
output_dir <- file.path(script_dir, "..", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(out, file.path(output_dir, "Appendix_B_worked_example.txt"))
cat(paste(out, collapse = "\n"))
cat("\n\nSaved: Appendix_B_worked_example.txt  -> paste into manuscript Appendix B\n")

}  # end WE_FUNCTIONS_ONLY guard
