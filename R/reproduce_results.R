# =============================================================================
# reproduce_results.R
# -----------------------------------------------------------------------------
# Replication pipeline for:
#   "Homo Silicus is Hyper-Rational: Why LLM Agents Fail to Replicate
#    Attention-Driven Trading" - Journal of Economic Interaction and
#    Coordination.
#
# This script reproduces, from the raw experiment logs, the Experiment 2
# (factorial) statistics quoted in the manuscript (all cell
# effects, factorial main effects, contrasts, asset-class outcomes, the
# direct buy-minus-sell and target-minus-comparison difference outcomes,
# focal randomization inference, and the construction sensitivity), the
# Experiment 1 items (Table 3 balance, Table 4 first-stage attention
# associations, PGR/PLR with fee sensitivity, the forward-return-on-
# attention-depth regression, and the descriptive gain/loss moderation
# comparison), and the Experiment 3 fixed-effects and cross-run statistics
# plus the corrected common-window balance.
# NOT reproduced here (they come from the full analysis script, which is
# included under R/experiment/ and whose scope warning applies): the
# Callaway-Sant'Anna estimates for either staggered run, the Experiment 3
# target/comparison asset interactions, the Experiment 3 disposition ratio,
# the Cohort-1 placebo, and Appendix Tables A4/A5. See README.md for the
# statistic-to-script map. Experiment labels: Experiment 1 = original
# staggered bundled run, Experiment 2 = factorial rerun (data/rerun_A),
# Experiment 3 = neutral-label rerun (data/rerun_B); the data folders keep
# their original rerun_A/rerun_B names for checksum stability.
# No API calls are made: the experiment logs are inputs, and the market
# environment is re-derived deterministically from its fixed seed where needed.
#
# USAGE
#   Rscript R/reproduce_results.R [stage]
#     stage 1    Experiment 1 (original reported run): common-window balance
#                (Table 3), first-stage attention associations (Table 4),
#                opportunity-adjusted disposition (PGR/PLR), the forward-
#                return-on-attention-depth regression, and the descriptive
#                gain/loss realization moderation comparison.
#     stage 2    Experiment 2, the 2x2 factorial: planned specification,
#                post-outcome design-aligned adjustment, randomization
#                inference, factorial main effects, contrasts, buy-minus-sell
#                and target-minus-comparison difference outcomes,
#                exclude-SocialMomentum robustness, construction descriptives.
#     stage 3    Experiment 3, neutral tickers: TWFE cohort estimates and the
#                stacked cross-run contrast.
#     all        (default) run all three stages.
#
# DATA (not committed to the repository; see README for availability)
#   data/original_run/experiment_results_final.csv   original reported run
#   data/original_run/experiment_full_log.csv        full log (worked example)
#   data/rerun_A/experiment_results_final.csv        factorial rerun
#   data/rerun_B/experiment_results_final.csv        neutral-ticker rerun
#   Override the data root with environment variable ADT_DATA_ROOT.
#
# REQUIREMENTS
#   R >= 4.3 with packages: data.table, fixest, glue, digest.
#   Optional: did (Callaway & Sant'Anna estimator). The CS estimates quoted in
#   the manuscript come from the full analysis script
#   (R/experiment/Attention_driven_trading_Statistical_Analysis_R1.R); CS
#   bootstrap standard errors vary slightly across seeds, point estimates are
#   exact. One further version note: the joint Wald test of the two viral
#   cells returns p = .028-.032 across fixest versions; the manuscript
#   therefore quotes "p = .03".
#
# INFERENCE SEQUENCE DISCLOSURE (Experiment 2, factorial)
#   The planned specification is agent + period fixed effects with
#   agent-clustered SEs (viral@5bps: p = .119). Persona-by-period fixed
#   effects were added AFTER that estimate was observed, as a design-aligned
#   precision adjustment motivated by the within-persona stratified
#   randomization (p = .006). The focal within-persona Monte Carlo
#   randomization inference (viral@5bps vs never-treated, four-versus-four
#   reassignment within each persona stratum, R = 500,000 draws, seed
#   20260731, (b+1)/(R+1) correction) respects the assignment scheme and does
#   not depend on the adjustment (any-buy p = .0078; trade p = .0016; sell
#   p = .037). Because the focal contrast was itself selected after results
#   were observed, the manuscript labels this RI a post-outcome design-based
#   sensitivity analysis rather than a pre-specified test. This script
#   reports the full sequence, matching the disclosure in the manuscript.
#
# DETERMINISM
#   Market re-simulation uses the experiment seed (CONFIG$seed = 42, set
#   inside the sourced generator). Randomization inference uses seed
#   20260731 (focal RI). All other computations are deterministic.
#
# OUTPUT
#   Written to output/ (created if absent); console output states each
#   quoted statistic with its manuscript location. The script halts on error.
# =============================================================================

## --- locate repository root (R/ is one level below it) and resolve paths ---
.file <- gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)
ROOT  <- normalizePath(file.path(dirname(.file), ".."))
DATA  <- Sys.getenv("ADT_DATA_ROOT", file.path(ROOT, "data"))
PATHS <- list(
  orig_csv = file.path(DATA, "original_run", "experiment_results_final.csv"),
  rerunA   = file.path(DATA, "rerun_A",      "experiment_results_final.csv"),
  rerunB   = file.path(DATA, "rerun_B",      "experiment_results_final.csv"),
  we_fns   = file.path(ROOT, "R", "worked_example_appendix_B.R"),
  out      = file.path(ROOT, "output")
)
dir.create(PATHS$out, showWarnings = FALSE, recursive = TRUE)
missing <- names(Filter(Negate(file.exists), PATHS[c("orig_csv","rerunA","rerunB","we_fns")]))
if (length(missing)) stop("Missing inputs: ", paste(unlist(PATHS[missing]), collapse = "; "),
                          "\nPlace the data as described in README.md or set ADT_DATA_ROOT.")

suppressPackageStartupMessages({ library(data.table); library(fixest) })
stage <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "all"

## --- small reporting helpers -------------------------------------------------
say  <- function(...) cat(sprintf(...), "\n")
rep3 <- function(m, ks, lab) {                     # coefficient lines in pp
  b <- coef(m); V <- vcov(m); df <- degrees_freedom(m, "t")
  for (k in intersect(ks, names(b)))
    say("%s %-10s %+7.2f pp (SE %.2f, p=%.4f)", lab, k,
        100*b[k], 100*sqrt(V[k,k]), 2*pt(-abs(b[k]/sqrt(V[k,k])), df))
}
lc <- function(m, w, lab) {                        # linear combination of coefs
  b <- coef(m); V <- vcov(m)
  e <- sum(w*b[names(w)]); s <- sqrt(as.numeric(t(w) %*% V[names(w),names(w)] %*% w))
  say("%-38s %+7.2f pp (SE %.2f, p=%.4f)", lab, 100*e, 100*s,
      2*pt(-abs(e/s), degrees_freedom(m, "t")))
}

# =============================================================================
# STAGE 1 - EXPERIMENT 1 (ORIGINAL REPORTED RUN)
# Reproduces: Table 3 (common-window randomization balance); Table 4
# (first-stage attention associations, computed under the single documented
# specification below); the opportunity-adjusted disposition
# measures PGR/PLR with replay validation and the fee-convention sensitivity;
# the forward-return-on-attention-depth regression; and the descriptive
# gain/loss realization moderation comparison.
# =============================================================================
if (stage %in% c("1", "all")) {
  say("== STAGE 1: Experiment 1 (original reported run) ==")
  O <- fread(PATHS$orig_csv)[stage == "trading"]

  ## Table 3: agent-level means over the common untreated window t = 1-59,
  ## treated (cohorts 1-2) vs never-treated (cohort 3), Holm-adjusted p-values.
  pre <- O[t < 60]; pre[, treated := treatment_cohort %in% c(1, 2)]
  ag  <- pre[, .(buy = mean(buy_indicator), sell = mean(sell_indicator),
                 trade = mean(trade_indicator), pv = mean(portfolio_value)),
             by = .(agent_id, treated)]
  T3 <- rbindlist(lapply(c("buy","sell","trade","pv"), function(v) {
    tt <- t.test(ag[treated == TRUE][[v]], ag[treated == FALSE][[v]])
    data.table(var = v, treated = mean(ag[treated == TRUE][[v]]),
               control = mean(ag[treated == FALSE][[v]]),
               t = tt$statistic, p = tt$p.value) }))
  T3[, p_holm := p.adjust(p, "holm")]
  print(T3); fwrite(T3, file.path(PATHS$out, "table3_common_window.csv"))

  ## Table 4: first-stage attention associations, computed from the raw log
  ## under ONE documented specification:
  ##   Attention levels are parsed from full_attention_allocation
  ##   ("TICKER:level" pairs). meme_att = deep=2 / quick=1 / ignore=0 summed
  ##   over AMC and GME (range 0-4); any_meme_deep = 1 if either meme ticker
  ##   received deep attention.
  ##   Rows 1-2: outcome ~ cohort1_post + cohort2_post | agent_id + t,
  ##             agent-clustered, full trading panel (N = 24,192).
  ##   Row  3:   meme share of EXECUTED trades: 1{ticker in AMC,GME} on the
  ##             same cohort-post terms, restricted to agent-periods with a
  ##             successfully executed buy or sell (N = 6,917).
  ##   Row  4:   descriptive dose association among treated agents in
  ##             post-treatment periods only (N = 10,432): buy_indicator on
  ##             meme_att with agent + period FE. Attention is model-chosen
  ##             (endogenous), so this row is a descriptive association, not
  ##             a causal dose response.
  att <- function(s, tk) fcase(grepl(paste0(tk, ":deep"),  s), 2L,
                               grepl(paste0(tk, ":quick"), s), 1L, default = 0L)
  O[, meme_att := att(full_attention_allocation, "AMC") + att(full_attention_allocation, "GME")]
  O[, any_meme_deep := as.integer(grepl("AMC:deep", full_attention_allocation) |
                                  grepl("GME:deep", full_attention_allocation))]
  O[, c1p := as.integer(cohort1_post)]; O[, c2p := as.integer(cohort2_post)]
  TP <- O[action %chin% c("buy", "sell") & trade_success == TRUE &
          !is.na(ticker) & ticker != ""]
  TP[, meme_trade := as.integer(ticker %chin% c("AMC", "GME"))]
  DP <- O[treated == TRUE & post_treatment == TRUE]
  t4row <- function(m, k, outcome, N) {
    b <- coef(m); V <- vcov(m); df <- degrees_freedom(m, "t")
    se <- sqrt(V[k, k]); p <- 2*pt(-abs(b[k]/se), df); cr <- qt(.975, df)
    data.table(outcome = outcome, term = k, est = b[k], se = se, p = p,
               ci_lo = b[k] - cr*se, ci_hi = b[k] + cr*se, N = N)
  }
  m41 <- feols(meme_att      ~ c1p + c2p | agent_id + t, cluster = ~agent_id, data = O)
  m42 <- feols(any_meme_deep ~ c1p + c2p | agent_id + t, cluster = ~agent_id, data = O)
  m43 <- feols(meme_trade    ~ c1p + c2p | agent_id + t, cluster = ~agent_id, data = TP)
  m44 <- feols(buy_indicator ~ meme_att  | agent_id + t, cluster = ~agent_id, data = DP)
  T4 <- rbind(t4row(m41, "c1p", "meme_attention_score", nobs(m41)),
              t4row(m41, "c2p", "meme_attention_score", nobs(m41)),
              t4row(m42, "c1p", "any_meme_deep",        nobs(m42)),
              t4row(m42, "c2p", "any_meme_deep",        nobs(m42)),
              t4row(m43, "c1p", "meme_share_executed_trades", nobs(m43)),
              t4row(m43, "c2p", "meme_share_executed_trades", nobs(m43)),
              t4row(m44, "meme_att", "buy_on_attention_treated_post", nobs(m44)))
  say("Table 4 (recomputed): meme attention  c1 %+0.2f (SE %.2f) c2 %+0.2f (SE %.2f), N = %d",
      coef(m41)["c1p"], sqrt(vcov(m41)["c1p","c1p"]),
      coef(m41)["c2p"], sqrt(vcov(m41)["c2p","c2p"]), nobs(m41))
  say("Table 4 (recomputed): deep meme attn  c1 %+0.1f pp c2 %+0.1f pp, N = %d",
      100*coef(m42)["c1p"], 100*coef(m42)["c2p"], nobs(m42))
  say("Table 4 (recomputed): meme trade share c1 %+0.1f pp c2 %+0.1f pp, N = %d (executed trades)",
      100*coef(m43)["c1p"], 100*coef(m43)["c2p"], nobs(m43))
  say("Table 4 (recomputed): buy on attention (treated-post, descriptive) %+0.2f pp per point (SE %.2f, p = %.2g), N = %d",
      100*coef(m44)["meme_att"], 100*sqrt(vcov(m44)["meme_att","meme_att"]),
      T4[outcome == "buy_on_attention_treated_post", p], nobs(m44))
  fwrite(T4, file.path(PATHS$out, "table4_first_stage.csv"))

  ## Deterministic price paths: source the worked-example script in
  ## functions-only mode (it contains verbatim copies of the experiment's
  ## market generator) and re-simulate under the experiment seed.
  WE_FUNCTIONS_ONLY <- TRUE
  source(PATHS$we_fns, local = FALSE)
  set.seed(CONFIG$seed)
  pp <- simulate_price_paths(CONFIG$n_periods)
  PX <- sapply(c("AAPL","NVDA","AMC","GME"), function(s) pp[[s]])

  ## PGR/PLR (Odean 1998 sale-day denominators). Positions are replayed from
  ## the trade log at regenerated prices; the replay is validated against the
  ## logged realized P&L of every sale. Realized gains/losses are classified
  ## by price relative to volume-weighted average cost BEFORE fees; the
  ## fee-inclusive alternative (logged P&L sign) is reported as sensitivity.
  tr <- O[order(agent_id, t)]; ev <- list(); k <- 0
  for (a in unique(tr$agent_id)) {
    da <- tr[agent_id == a]
    sh <- setNames(numeric(4), colnames(PX)); tc <- setNames(numeric(4), colnames(PX))
    for (i in seq_len(nrow(da))) {
      t0 <- da$t[i]; act <- da$action[i]; tk <- da$ticker[i]; q <- da$shares[i]
      if (identical(act, "sell") && !is.na(tk) && q > 0 && sh[tk] > 0) {
        avg <- tc[tk]/sh[tk]; px <- PX[t0, tk]; k <- k + 1
        pg <- 0L; pl <- 0L                       # paper gains/losses on OTHER holdings
        for (o in setdiff(colnames(PX), tk)) if (sh[o] > 0) {
          if (PX[t0, o] > tc[o]/sh[o]) pg <- pg + 1L else pl <- pl + 1L }
        ev[[k]] <- data.table(agent_id = a, t = t0, rp = q*(px - avg),
                              lp = da$realized_pnl[i],
                              RG = as.integer(px > avg), RL = as.integer(px <= avg),
                              PG = pg, PL = pl)
      }
      if (identical(act, "buy")  && !is.na(tk) && q > 0) { tc[tk] <- tc[tk] + q*PX[t0, tk]; sh[tk] <- sh[tk] + q }
      if (identical(act, "sell") && !is.na(tk) && q > 0 && sh[tk] > 0) {
        avg <- tc[tk]/sh[tk]; sh[tk] <- sh[tk] - q; tc[tk] <- avg*sh[tk] }
    }
  }
  E <- rbindlist(ev); stopifnot(nrow(E) == 3859)
  say("replay validation: corr(replay P&L, logged P&L) = %.6f; sign flips = %d/%d",
      cor(E$rp, E$lp), sum(sign(E$rp) != sign(E$lp)), nrow(E))
  pgr <- sum(E$RG)/(sum(E$RG)+sum(E$PG)); plr <- sum(E$RL)/(sum(E$RL)+sum(E$PL))
  say("PGR = %.3f, PLR = %.3f, PGR/PLR = %.3f  [manuscript 6.6; human benchmark ~1.5]", pgr, plr, pgr/plr)
  pgr2 <- sum(E$lp > 0)/(sum(E$lp > 0)+sum(E$PG)); plr2 <- sum(E$lp <= 0)/(sum(E$lp <= 0)+sum(E$PL))
  say("fee-inclusive sensitivity: PGR = %.3f, PLR = %.3f, ratio = %.3f", pgr2, plr2, pgr2/plr2)
  A2 <- E[, .(RG=sum(RG), RL=sum(RL), PG=sum(PG), PL=sum(PL)), by = agent_id][(RG+PG) > 0 & (RL+PL) > 0]
  tt <- t.test(A2$RL/(A2$RL+A2$PL), A2$RG/(A2$RG+A2$PG), paired = TRUE)
  say("agent-level paired PLR-PGR = %.3f (t = %.1f, p = %.2e, n = %d)",
      tt$estimate, tt$statistic, tt$p.value, nrow(A2))
  fwrite(E, file.path(PATHS$out, "pgr_plr_events.csv"))

  ## H4: among buys, does deep attention to the bought ticker predict lower
  ## next-period returns than quick attention? (Returns are exogenous noise
  ## by design, so this test is weakly diagnostic; see manuscript 6.5.)
  B <- tr[action == "buy" & !is.na(ticker) & t < 252]
  B[, lvl := mapply(function(s, tk) {
      if (is.na(s) || s == "") return(NA_character_)
      parts <- strsplit(s, ",\\s*")[[1]]; kv <- strsplit(parts, ":")
      hit <- vapply(kv, function(z) trimws(z[1]) == tk, logical(1))
      if (any(hit)) trimws(strsplit(parts[which(hit)[1]], ":")[[1]][2]) else NA_character_
    }, full_attention_allocation, ticker)]
  B <- B[lvl %in% c("deep","quick")]; B[, deep := as.integer(lvl == "deep")]
  B[, ci := match(ticker, colnames(PX))]
  B[, fr1 := PX[cbind(t+1, ci)]/PX[cbind(t, ci)] - 1]
  rep3(feols(fr1 ~ deep,      cluster = ~agent_id, data = B), "deep", "H4 (no FE)  ")
  rep3(feols(fr1 ~ deep | t,  cluster = ~agent_id, data = B), "deep", "H4 (t FE)   ")

  ## Descriptive gain/loss realization moderation comparison (previously
  ## labeled H5b). Share of executed sales that realize a winner (logged
  ## realized P&L > 0): never-treated sales in the post-adoption window
  ## (t >= 60) versus treated post-treatment sales, matching the full
  ## analysis script's construction. This is a descriptive two-group
  ## comparison - it omits treated-pre sales and has no equivalence margin,
  ## so non-rejection does NOT establish independence; the all-periods
  ## control sensitivity below shows the comparison is window-sensitive.
  SL <- O[action == "sell" & trade_success == TRUE & !is.na(ticker) & ticker != ""]
  SL[, winner := as.integer(realized_pnl > 0)]
  g1 <- SL[never_treated == TRUE & t >= 60]             # never-treated, post-adoption window
  g2 <- SL[treated == TRUE & post_treatment == TRUE]    # treated, post only
  ptst <- prop.test(c(sum(g2$winner), sum(g1$winner)), c(nrow(g2), nrow(g1)))
  say("moderation (descriptive): winners among sales - never-treated (t>=60) %d/%d = %.1f%%, treated-post %d/%d = %.1f%%",
      sum(g1$winner), nrow(g1), 100*mean(g1$winner),
      sum(g2$winner), nrow(g2), 100*mean(g2$winner))
  say("moderation (descriptive): difference %+0.1f pp, 95%% CI [%+0.1f, %+0.1f], p = %.3f (no equivalence margin)",
      100*(mean(g2$winner) - mean(g1$winner)), 100*ptst$conf.int[1], 100*ptst$conf.int[2], ptst$p.value)
  g1a <- SL[never_treated == TRUE]                      # all-periods control sensitivity
  pta <- prop.test(c(sum(g2$winner), sum(g1a$winner)), c(nrow(g2), nrow(g1a)))
  say("moderation sensitivity (all-periods control): %d/%d = %.1f%% vs %.1f%%, diff %+0.1f pp, p = %.3f",
      sum(g1a$winner), nrow(g1a), 100*mean(g1a$winner), 100*mean(g2$winner),
      100*(mean(g2$winner) - mean(g1a$winner)), pta$p.value)
}

# =============================================================================
# STAGE 2 - EXPERIMENT 2: 2x2 FACTORIAL (viral/normal signals x 5/15 bps cost)
# Reproduces the factorial design and results sections: the planned
# specification, the post-outcome design-aligned adjustment, randomization
# inference, factorial main effects, decomposition contrasts, activity/sell
# outcomes, direct difference outcomes, the exclude-SocialMomentum robustness
# check, the persona variance decomposition, and the Social Momentum
# construction descriptives.
# Arm map (treatment_cohort): 1 = viral@5bps, 2 = viral@15bps (bundle),
# 3 = cost-only@15bps, 4 = never-treated control; adoption at t = 60.
# =============================================================================
if (stage %in% c("2", "all")) {
  say("== STAGE 2: Experiment 2 factorial ==")
  A <- fread(PATHS$rerunA)[stage == "trading"]
  A[, post60 := as.integer(t >= 60)]
  A[, `:=`(a_v5  = as.integer(treatment_cohort == 1)*post60,
           a_v15 = as.integer(treatment_cohort == 2)*post60,
           a_c15 = as.integer(treatment_cohort == 3)*post60)]
  ks <- c("a_v5","a_v15","a_c15")

  ## Planned specification (agent + period FE) - reported FIRST in the paper.
  mP <- feols(buy_indicator ~ a_v5 + a_v15 + a_c15 | agent_id + t,
              cluster = ~agent_id, data = A)
  rep3(mP, ks, "BUY planned ")
  w <- fixest::wald(mP, keep = "a_v", print = FALSE)
  say("joint test, two viral cells (planned): p = %.4f  [quoted as p = .03; varies .028-.032 across fixest versions]", w$p)

  ## Post-outcome design-aligned adjustment (persona x period FE). Motivated by
  ## the stratified randomization; adopted after the planned estimate was
  ## observed - see header and manuscript Section 5.3 for the disclosure.
  mA <- feols(buy_indicator ~ a_v5 + a_v15 + a_c15 | agent_id + persona^t,
              cluster = ~agent_id, data = A)
  rep3(mA, ks, "BUY adjusted")
  lc(mA, c(a_v5 = .5,  a_v15 = .5, a_c15 = -.5), "viral main effect (adjusted)")
  lc(mA, c(a_v5 = -.5, a_v15 = .5, a_c15 =  .5), "cost main effect (adjusted)")
  lc(mA, c(a_v5 = -1,  a_v15 = 1,  a_c15 =  0),  "bundle - viral@5 (adjusted)")
  lc(mA, c(a_v5 =  0,  a_v15 = 1,  a_c15 = -1),  "bundle - cost-only (adjusted)")

  ## Activity, sell, and asset-class margins - planned AND adjusted.
  A[, meme_buy := as.integer(buy_indicator == 1 & !is.na(ticker) & ticker %chin% c("AMC","GME"))]
  A[, blue_buy := as.integer(buy_indicator == 1 & !is.na(ticker) & ticker %chin% c("AAPL","NVDA"))]
  for (yv in c("trade_indicator", "sell_indicator", "meme_buy", "blue_buy")) {
    rep3(feols(as.formula(paste(yv, "~ a_v5 + a_v15 + a_c15 | agent_id + t")),
               cluster = ~agent_id, data = A), ks, paste0(substr(yv, 1, 5), " plan "))
    rep3(feols(as.formula(paste(yv, "~ a_v5 + a_v15 + a_c15 | agent_id + persona^t")),
               cluster = ~agent_id, data = A), ks, paste0(substr(yv, 1, 5), " adj  "))
  }
  ## Direct viral-versus-cost main-effect difference (no ranking claimed).
  for (nm in c("planned","adjusted")) {
    mm <- if (nm == "planned") mP else mA
    lc(mm, c(a_v5 = 1, a_v15 = 0, a_c15 = -1), paste0("viral-vs-cost difference (", nm, ")"))
  }

  ## Direct difference outcomes (exploratory, computed post-outcome):
  ##   buy_minus_sell  - the fixed-cost buy-versus-sell contrast estimated in
  ##                     ONE model on the difference outcome (this replaces
  ##                     the earlier draft practice of differencing separately
  ##                     aggregated estimates and borrowing inference from a
  ##                     differently specified stacked model);
  ##   meme_minus_blue - the target-minus-comparison purchase component
  ##                     difference.
  A[, buy_minus_sell  := buy_indicator - sell_indicator]
  A[, meme_minus_blue := meme_buy - blue_buy]
  for (yv in c("buy_minus_sell", "meme_minus_blue")) {
    rep3(feols(as.formula(paste(yv, "~ a_v5 + a_v15 + a_c15 | agent_id + t")),
               cluster = ~agent_id, data = A), "a_v5", paste0(substr(yv, 1, 5), " plan "))
    rep3(feols(as.formula(paste(yv, "~ a_v5 + a_v15 + a_c15 | agent_id + persona^t")),
               cluster = ~agent_id, data = A), "a_v5", paste0(substr(yv, 1, 5), " adj  "))
  }

  ## Robustness: the decomposition does not depend on the contested persona.
  rep3(feols(buy_indicator ~ a_v5 + a_v15 + a_c15 | agent_id + persona^t,
             cluster = ~agent_id, data = A[persona != "SocialMomentum"]),
       "a_v5", "BUY exclSM  ")

  ## Persona share of variance in agent-level outcome changes (one-way ANOVA;
  ## quoted as 65.7% in Section 4.6).
  ag <- A[, .(d   = mean(buy_indicator[t >= 60])   - mean(buy_indicator[t < 60]),
              dtr = mean(trade_indicator[t >= 60]) - mean(trade_indicator[t < 60]),
              dsl = mean(sell_indicator[t >= 60])  - mean(sell_indicator[t < 60])),
          by = .(agent_id, persona, treatment_cohort)]
  av <- anova(lm(d ~ persona, data = ag))
  say("ANOVA between-persona share of Delta(buy) variance: %.3f", av$`Sum Sq`[1]/sum(av$`Sum Sq`))

  ## FOCAL within-persona Monte Carlo randomization inference: retain only the
  ## viral/5bps and control arms and reassign four-versus-four labels within
  ## each persona stratum. Monte Carlo p uses the (b+1)/(R+1) correction.
  af <- ag[ag$treatment_cohort %in% c(1, 4), ]
  set.seed(20260731); R <- 500000
  strata <- split(seq_len(nrow(af)), af$persona)
  Y <- as.matrix(af[, .(d, dtr, dsl)])
  obs <- colMeans(Y[af$treatment_cohort == 1, , drop = FALSE]) -
         colMeans(Y[af$treatment_cohort == 4, , drop = FALSE])
  hits <- c(0L, 0L, 0L)
  for (r in seq_len(R)) {
    lab <- integer(nrow(af))
    for (s in strata) lab[sample(s, 4)] <- 1L
    st <- colMeans(Y[lab == 1L, , drop = FALSE]) - colMeans(Y[lab == 0L, , drop = FALSE])
    hits <- hits + as.integer(abs(st) >= abs(obs) - 1e-12)
  }
  p <- (hits + 1)/(R + 1)
  say("FOCAL RI viral@5 any-buy: obs %+.2f pp, p = %.6f", 100*obs[1], p[1])
  say("FOCAL RI viral@5 trade:   obs %+.2f pp, p = %.6f", 100*obs[2], p[2])
  say("FOCAL RI viral@5 sell:    obs %+.2f pp, p = %.6f", 100*obs[3], p[3])
  fwrite(data.table(outcome = c("any_buy","trade","sell"), obs_pp = 100*obs, ri_p = p),
         file.path(PATHS$out, "rerunA_focal_ri.csv"))

  ## Tidy, manuscript-matching results table (planned + adjusted, all outcomes,
  ## with exact t-based 95% confidence intervals).
  tidy <- rbindlist(lapply(c("buy_indicator","trade_indicator","sell_indicator","meme_buy","blue_buy",
                             "buy_minus_sell","meme_minus_blue"), function(yv) {
    rbindlist(lapply(c("planned","adjusted"), function(sp) {
      f <- as.formula(paste(yv, if (sp == "planned") "~ a_v5 + a_v15 + a_c15 | agent_id + t"
                                 else "~ a_v5 + a_v15 + a_c15 | agent_id + persona^t"))
      m <- feols(f, cluster = ~agent_id, data = A); b <- coef(m); V <- vcov(m); df <- degrees_freedom(m, "t")
      cr <- qt(.975, df)
      cells <- rbindlist(lapply(names(b), function(k) data.table(
        outcome = yv, spec = sp, term = k, est_pp = 100*b[k], se_pp = 100*sqrt(V[k,k]),
        p = 2*pt(-abs(b[k]/sqrt(V[k,k])), df),
        ci_lo_pp = 100*(b[k] - cr*sqrt(V[k,k])), ci_hi_pp = 100*(b[k] + cr*sqrt(V[k,k])))))
      combos <- list(viral_main = c(.5,.5,-.5), cost_main = c(-.5,.5,.5),
                     interaction = c(-1,1,-1), viral_minus_cost = c(1,0,-1))
      lcs <- rbindlist(lapply(names(combos), function(nm) {
        w <- combos[[nm]]; names(w) <- c("a_v5","a_v15","a_c15")
        e <- sum(w*b[names(w)]); se <- sqrt(as.numeric(t(w) %*% V[names(w),names(w)] %*% w))
        data.table(outcome = yv, spec = sp, term = nm, est_pp = 100*e, se_pp = 100*se,
                   p = 2*pt(-abs(e/se), df),
                   ci_lo_pp = 100*(e - cr*se), ci_hi_pp = 100*(e + cr*se)) }))
      rbind(cells, lcs) })) }))
  fwrite(tidy, file.path(PATHS$out, "rerunA_results_table.csv"))
  say("saved rerunA_results_table.csv (%d rows: planned + adjusted, all outcomes and contrasts, with 95%% CIs)", nrow(tidy))

  ## Social Momentum construction variants - DESCRIPTIVE ONLY. Sixteen agents
  ## sit one or two per arm-by-construction cell, so no formal equality test
  ## is attempted (see manuscript Section 4.6).
  sm <- A[persona == "SocialMomentum"]
  sm[, vp := as.integer(treatment_cohort %in% c(1, 2))*post60]
  msm <- feols(buy_indicator ~ vp + i(sm_variant, vp, ref = "v1") | agent_id + t,
               cluster = ~agent_id, data = sm)
  print(coeftable(msm))
  ## Sensitivity: construction-by-period fixed effects (quoted in Section 4.6 as
  ## -6.17 / +1.42 / -5.89; shows the deviation sizes are specification-sensitive).
  msm2 <- feols(buy_indicator ~ i(sm_variant, vp) | agent_id + sm_variant^t,
                cluster = ~agent_id, data = sm)
  say("construction-by-period sensitivity (v1/v2/v3 associations):")
  print(coeftable(msm2))
  fwrite(as.data.table(coeftable(msm2), keep.rownames = "term"),
         file.path(PATHS$out, "rerunA_sm_construction_sensitivity.csv"))
  say("(descriptive diagnostics only: 16 agents, 1-2 per arm-by-construction cell)")
}

# =============================================================================
# STAGE 3 - EXPERIMENT 3: NEUTRAL TICKERS (same staggered design as Experiment 1)
# Reproduces the neutral-label replication: TWFE cohort estimates for the
# neutral run and the stacked cross-run contrast on the identical seeded
# price paths. The
# Callaway-Sant'Anna estimates quoted beside these (-2.72, SE 5.49, p = .620)
# come from the full analysis script; see header note on the did package.
# =============================================================================
if (stage %in% c("3", "all")) {
  say("== STAGE 3: Experiment 3 neutral tickers ==")
  Bd <- fread(PATHS$rerunB)[stage == "trading"]
  Bd[, `:=`(c1p = as.integer(treatment_cohort == 1 & t >= 60),
            c2p = as.integer(treatment_cohort == 2 & t >= 120))]
  rep3(feols(buy_indicator ~ c1p + c2p | agent_id + t, cluster = ~agent_id, data = Bd),
       c("c1p","c2p"), "B TWFE      ")

  ## Stacked cross-run contrast: both runs share the seeded price paths and
  ## assignment structure, so period-by-run fixed effects absorb the common
  ## market history; the interaction tests whether the fixed-effects
  ## treatment effect differs between real-name and neutral-name runs.
  O <- fread(PATHS$orig_csv)[stage == "trading",
        .(agent_id = paste0("orig_", agent_id), t, treatment_cohort, buy_indicator, run = "orig")]
  S <- rbind(O, Bd[, .(agent_id = paste0("neut_", agent_id), t, treatment_cohort, buy_indicator, run = "neut")])
  S[, tp   := as.integer((treatment_cohort == 1 & t >= 60) | (treatment_cohort == 2 & t >= 120))]
  S[, neut := as.integer(run == "neut")]
  m <- feols(buy_indicator ~ tp + tp:neut | agent_id + t^run, cluster = ~agent_id, data = S)
  rep3(m, c("tp", "tp:neut"), "STACKED     ")
  z <- (-11.67 + 2.72)/sqrt(4.92^2 + 5.49^2)
  say("group-time ATT difference, independent-runs diagnostic: z = %.2f, p = %.3f (not a final test; runs share design elements)", z, 2*pnorm(-abs(z)))
  ## Corrected Experiment 3 baseline balance on the common untreated window t = 1-59
  ## (replaces the invalid mixed-window table_a2 artifact).
  pre <- Bd[t < 60]; pre[, treated := treatment_cohort %in% c(1, 2)]
  agB <- pre[, .(buy = mean(buy_indicator), sell = mean(sell_indicator),
                 trade = mean(trade_indicator), pv = mean(portfolio_value)),
             by = .(agent_id, treated)]
  T3B <- rbindlist(lapply(c("buy","sell","trade","pv"), function(v) {
    tt <- t.test(agB[treated == TRUE][[v]], agB[treated == FALSE][[v]])
    data.table(var = v, treated = mean(agB[treated == TRUE][[v]]),
               control = mean(agB[treated == FALSE][[v]]), p = tt$p.value) }))
  T3B[, p_holm := p.adjust(p, "holm")]
  say("Experiment 3 common-window balance (t = 1-59):"); print(T3B)
  fwrite(T3B, file.path(PATHS$out, "rerunB_balance_common_window.csv"))
  say("NOTE: the distributed placebo (p = .92) is a Cohort-1 placebo; the code's t < 60 filter excludes Cohort 2's pseudo-window.")
}

say("DONE (stage = %s). Outputs in %s", stage, PATHS$out)
