#!/usr/bin/env Rscript
# =============================================================================
# Opportunity-adjusted PGR/PLR for Experiments 1-3, with agent-cluster
# bootstrap CIs for the PGR/PLR ratio.
#
# Method: verbatim port of the disposition section of R/reproduce_results.R
# (positions replayed from the trading log at the seed-42 regenerated price
# paths; Odean 1998 sale-day denominators; pre-fee gain/loss classification
# with the fee-inclusive alternative as sensitivity), applied to all three
# experiment logs. Validation gates:
#   - every run: corr(replay P&L, logged P&L) must exceed .9999
#   - Experiment 1 must reproduce the published PGR=.478, PLR=.638,
#     ratio=.750, n=3,859 before any new number is trusted.
# Bootstrap: resample agents with replacement (cluster bootstrap, B=2000,
# seed 42), percentile 95% CI for the pooled PGR/PLR ratio.
# =============================================================================
suppressPackageStartupMessages(library(data.table))

.file <- gsub("~+~", " ", sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)
ROOT  <- normalizePath(file.path(dirname(.file), ".."))
PATHS <- list(orig   = file.path(ROOT, "data", "original_run", "experiment_results_final.csv"),
              rerunA = file.path(ROOT, "data", "rerun_A",      "experiment_results_final.csv"),
              rerunB = file.path(ROOT, "data", "rerun_B",      "experiment_results_final.csv"),
              we_fns = file.path(ROOT, "R",    "worked_example_appendix_B.R"))

## Regenerate the deterministic price paths exactly as the pipeline does.
WE_FUNCTIONS_ONLY <- TRUE
source(PATHS$we_fns, local = FALSE)
set.seed(CONFIG$seed)
pp <- simulate_price_paths(CONFIG$n_periods)
PX <- sapply(c("AAPL", "NVDA", "AMC", "GME"), function(s) pp[[s]])

replay <- function(csv) {
  tr <- fread(csv)[stage == "trading"][order(agent_id, t)]
  ev <- list(); k <- 0
  for (a in unique(tr$agent_id)) {
    da <- tr[agent_id == a]
    sh <- setNames(numeric(4), colnames(PX)); tc <- setNames(numeric(4), colnames(PX))
    for (i in seq_len(nrow(da))) {
      t0 <- da$t[i]; act <- da$action[i]; tk <- da$ticker[i]; q <- da$shares[i]
      if (identical(act, "sell") && !is.na(tk) && q > 0 && sh[tk] > 0) {
        avg <- tc[tk]/sh[tk]; px <- PX[t0, tk]; k <- k + 1
        pg <- 0L; pl <- 0L
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
  list(E = rbindlist(ev), agents = unique(tr$agent_id))
}

boot_ci <- function(E, agents, B = 2000, seed = 42) {
  set.seed(seed)
  byA <- E[, .(RG = sum(RG), RL = sum(RL), PG = sum(PG), PL = sum(PL)), by = agent_id]
  byA <- merge(data.table(agent_id = agents), byA, by = "agent_id", all.x = TRUE)
  for (v in c("RG","RL","PG","PL")) byA[is.na(get(v)), (v) := 0L]
  n <- nrow(byA); out <- numeric(B)
  for (b in seq_len(B)) {
    S <- byA[sample.int(n, n, replace = TRUE)]
    num <- sum(S$RG); den1 <- num + sum(S$PG)
    rl  <- sum(S$RL); den2 <- rl + sum(S$PL)
    out[b] <- if (den1 > 0 && den2 > 0 && rl > 0) (num/den1) / (rl/den2) else NA_real_
  }
  quantile(out, c(.025, .975), na.rm = TRUE)
}

summarise_run <- function(label, csv, expect_n = NA) {
  res <- replay(csv); E <- res$E
  cr  <- cor(E$rp, E$lp); flips <- sum(sign(E$rp) != sign(E$lp))
  pgr <- sum(E$RG)/(sum(E$RG)+sum(E$PG)); plr <- sum(E$RL)/(sum(E$RL)+sum(E$PL))
  pgr2 <- sum(E$lp > 0)/(sum(E$lp > 0)+sum(E$PG)); plr2 <- sum(E$lp <= 0)/(sum(E$lp <= 0)+sum(E$PL))
  A2 <- E[, .(RG=sum(RG), RL=sum(RL), PG=sum(PG), PL=sum(PL)), by = agent_id][(RG+PG) > 0 & (RL+PL) > 0]
  tt <- t.test(A2$RL/(A2$RL+A2$PL), A2$RG/(A2$RG+A2$PG), paired = TRUE)
  ci <- boot_ci(E, res$agents)
  cat(sprintf("== %s ==\n  sales n = %d | replay corr = %.6f | sign flips = %d\n", label, nrow(E), cr, flips))
  cat(sprintf("  RG=%d RL=%d PG=%d PL=%d\n", sum(E$RG), sum(E$RL), sum(E$PG), sum(E$PL)))
  cat(sprintf("  PGR = %.3f  PLR = %.3f  ratio = %.3f  [boot 95%% CI %.3f, %.3f]\n", pgr, plr, pgr/plr, ci[1], ci[2]))
  cat(sprintf("  fee-inclusive ratio = %.3f\n", pgr2/plr2))
  cat(sprintf("  agent-level paired PLR-PGR = %.3f (t = %.1f, p = %.2e, n = %d)\n\n",
              tt$estimate, tt$statistic, tt$p.value, nrow(A2)))
  if (!is.na(expect_n)) stopifnot(nrow(E) == expect_n)
  if (cr < 0.9999) stop(label, ": replay validation FAILED (corr = ", cr, ")")
  data.table(run = label, n_sales = nrow(E), replay_corr = cr, sign_flips = flips,
             RG = sum(E$RG), RL = sum(E$RL), PG = sum(E$PG), PL = sum(E$PL),
             PGR = pgr, PLR = plr, ratio = pgr/plr,
             ci_lo = ci[1], ci_hi = ci[2], ratio_fee_incl = pgr2/plr2,
             paired_diff = unname(tt$estimate), t = unname(tt$statistic),
             p = tt$p.value, n_agents_paired = nrow(A2))
}

out <- rbind(summarise_run("Experiment 1 (original)", PATHS$orig, expect_n = 3859),
             summarise_run("Experiment 2 (factorial rerun A)", PATHS$rerunA),
             summarise_run("Experiment 3 (neutral rerun B)", PATHS$rerunB))
fwrite(out, file.path(ROOT, "output", "pgr_plr_experiments_1_3.csv"))
cat("written: pgr_plr_experiments_1_3.csv\n")
