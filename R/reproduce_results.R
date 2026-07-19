# =============================================================================
# reproduce_results.R
# -----------------------------------------------------------------------------
# Replication pipeline for:
#   "Homo Silicus is Hyper-Rational: Why GPT-4-Family Agents Do Not Replicate
#    Attention-Driven Trading" - Journal of Economic Interaction and
#    Coordination, Revision 1.
#
# This single script reproduces, from the raw experiment logs, every statistic
# quoted in the revised manuscript and in the response to the referee report.
# No API calls are made: the experiment logs are inputs, and the market
# environment is re-derived deterministically from its fixed seed where needed.
#
# USAGE
#   Rscript R/reproduce_results.R [stage]
#     stage 1    Original reported run: common-window balance (Table 3),
#                opportunity-adjusted disposition (PGR/PLR, Section 6.6),
#                and the H4 forward-return-on-attention-depth regression
#                (Section 6.5).
#     stage 2    Rerun A, the 2x2 factorial (Section 4.6): planned
#                specification, post-hoc design-aligned adjustment,
#                randomization inference, factorial main effects, contrasts,
#                exclude-SocialMomentum robustness, construction descriptives.
#     stage 3    Rerun B, neutral tickers (Section 4.6): TWFE cohort
#                estimates and the stacked cross-run contrast.
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
# INFERENCE SEQUENCE DISCLOSURE (Rerun A)
#   The planned specification is agent + period fixed effects with
#   agent-clustered SEs (viral@5bps: p = .119). Persona-by-period fixed
#   effects were added AFTER that estimate was observed, as a design-aligned
#   precision adjustment motivated by the within-persona stratified
#   randomization (p = .006). Randomization inference within persona strata
#   (10,000 permutations, seed 20260718) is design-exact and does not depend
#   on the adjustment (p = .009). This script reports all three in sequence,
#   matching the disclosure in the manuscript (Section 4.6) and the response
#   letter.
#
# DETERMINISM
#   Market re-simulation uses the experiment seed (CONFIG$seed = 42, set
#   inside the sourced generator). Randomization inference uses seed
#   20260718. All other computations are deterministic.
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
# STAGE 1 - ORIGINAL REPORTED RUN
# Reproduces: Table 3 (common-window randomization balance, Section 6.1);
# the opportunity-adjusted disposition measures PGR/PLR with replay validation
# and the fee-convention sensitivity (Section 6.6); and the H4 forward-return-
# on-attention-depth regression (Section 6.5).
# =============================================================================
if (stage %in% c("1", "all")) {
  say("== STAGE 1: original reported run ==")
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
}

# =============================================================================
# STAGE 2 - RERUN A: 2x2 FACTORIAL (viral/normal signals x 5/15 bps cost)
# Reproduces Section 4.6 and Table 10: the planned specification, the
# post-hoc design-aligned adjustment, randomization inference, factorial main
# effects, decomposition contrasts, activity/sell outcomes, the exclude-
# SocialMomentum robustness check, the persona variance decomposition, and
# the Social Momentum construction descriptives.
# Arm map (treatment_cohort): 1 = viral@5bps, 2 = viral@15bps (bundle),
# 3 = cost-only@15bps, 4 = never-treated control; adoption at t = 60.
# =============================================================================
if (stage %in% c("2", "all")) {
  say("== STAGE 2: Rerun A factorial ==")
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

  ## Post-hoc design-aligned adjustment (persona x period FE). Motivated by
  ## the stratified randomization; adopted after the planned estimate was
  ## observed - see header and manuscript Section 4.6 for the disclosure.
  mA <- feols(buy_indicator ~ a_v5 + a_v15 + a_c15 | agent_id + persona^t,
              cluster = ~agent_id, data = A)
  rep3(mA, ks, "BUY adjusted")
  lc(mA, c(a_v5 = .5,  a_v15 = .5, a_c15 = -.5), "viral main effect (adjusted)")
  lc(mA, c(a_v5 = -.5, a_v15 = .5, a_c15 =  .5), "cost main effect (adjusted)")
  lc(mA, c(a_v5 = -1,  a_v15 = 1,  a_c15 =  0),  "bundle - viral@5 (adjusted)")
  lc(mA, c(a_v5 =  0,  a_v15 = 1,  a_c15 = -1),  "bundle - cost-only (adjusted)")

  ## Activity and sell margins (adjusted specification).
  for (yv in c("trade_indicator", "sell_indicator"))
    rep3(feols(as.formula(paste(yv, "~ a_v5 + a_v15 + a_c15 | agent_id + persona^t")),
               cluster = ~agent_id, data = A), ks,
         paste0(toupper(substr(yv, 1, 5)), " adj  "))

  ## Robustness: the decomposition does not depend on the contested persona.
  rep3(feols(buy_indicator ~ a_v5 + a_v15 + a_c15 | agent_id + persona^t,
             cluster = ~agent_id, data = A[persona != "SocialMomentum"]),
       "a_v5", "BUY exclSM  ")

  ## Persona share of variance in agent-level outcome changes (one-way ANOVA;
  ## quoted as 65.7% in Section 4.6).
  ag <- A[, .(d   = mean(buy_indicator[t >= 60])   - mean(buy_indicator[t < 60]),
              dtr = mean(trade_indicator[t >= 60]) - mean(trade_indicator[t < 60])),
          by = .(agent_id, persona, treatment_cohort)]
  av <- anova(lm(d ~ persona, data = ag))
  say("ANOVA between-persona share of Delta(buy) variance: %.3f", av$`Sum Sq`[1]/sum(av$`Sum Sq`))

  ## Design-exact randomization inference: permute arm labels within persona
  ## strata (the actual assignment mechanism); statistic = difference in mean
  ## post-minus-pre change, viral@5bps arm vs never-treated control.
  set.seed(20260718); R <- 10000
  strata <- split(seq_len(nrow(ag)), ag$persona)
  st  <- function(coh, y) mean(y[coh == 1]) - mean(y[coh == 4])
  obs <- c(st(ag$treatment_cohort, ag$d), st(ag$treatment_cohort, ag$dtr))
  per <- matrix(NA_real_, R, 2)
  for (r in seq_len(R)) {
    coh <- ag$treatment_cohort
    for (s in strata) coh[s] <- sample(coh[s])
    per[r, ] <- c(st(coh, ag$d), st(coh, ag$dtr))
  }
  say("RI viral@5 buy:   obs %+.2f pp, two-sided p = %.4f", 100*obs[1], mean(abs(per[,1]) >= abs(obs[1])))
  say("RI viral@5 trade: obs %+.2f pp, two-sided p = %.4f", 100*obs[2], mean(abs(per[,2]) >= abs(obs[2])))

  ## Social Momentum construction variants - DESCRIPTIVE ONLY. Sixteen agents
  ## sit one or two per arm-by-construction cell, so no formal equality test
  ## is attempted (see manuscript Section 4.6 and the response letter).
  sm <- A[persona == "SocialMomentum"]
  sm[, vp := as.integer(treatment_cohort %in% c(1, 2))*post60]
  msm <- feols(buy_indicator ~ vp + i(sm_variant, vp, ref = "v1") | agent_id + t,
               cluster = ~agent_id, data = sm)
  print(coeftable(msm))
  say("(descriptive diagnostics only: 16 agents, 1-2 per cell)")
}

# =============================================================================
# STAGE 3 - RERUN B: NEUTRAL TICKERS (original staggered design)
# Reproduces Section 4.6: TWFE cohort estimates for the neutral run and the
# stacked cross-run contrast on the identical seeded price paths. The
# Callaway-Sant'Anna estimates quoted beside these (-2.72, SE 5.49, p = .620)
# come from the full analysis script; see header note on the did package.
# =============================================================================
if (stage %in% c("3", "all")) {
  say("== STAGE 3: Rerun B neutral tickers ==")
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
}

say("DONE (stage = %s). Outputs in %s", stage, PATHS$out)
