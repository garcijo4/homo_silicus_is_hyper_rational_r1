# Data

Raw logs from the three LLM-agent experiments reported in the paper. No human
subjects or confidential data are involved; every "agent" is an LLM instance.
Integrity: `md5sum -c CHECKSUMS.md5`.

## Runs

| Folder | Run | Design | Field dates |
|---|---|---|---|
| `original_run/` | Original reported run | Staggered adoption: Cohort 1 treated from t = 60, Cohort 2 from t = 120, Cohort 3 never treated (32 agents each). Viral signals bundled with the 5→15 bps meme-ticker cost surge. Real ticker labels. | Dec 2025 |
| `rerun_A/` | Rerun A — factorial | 2×2: viral/normal model-visible signals × 5/15 bps meme-ticker cost; single adoption t = 60; four arms of 24, randomized within persona strata (cohort 1 = viral@5bps, 2 = viral@15bps, 3 = cost-only@15bps, 4 = never-treated). Social Momentum split across 3 prompt constructions (`sm_variant` v1/v2/v3). | Jul 9–15, 2026 |
| `rerun_B/` | Rerun B — neutral tickers | Identical to the original design except every model-visible ticker label is an invented symbol (VNTK/KRLO/ZMQR/QRLP). Logs keep internal names. | Jul 15–17, 2026 |

## Files per run

- `experiment_results_final.csv` — the analysis log: 72,576 rows per run
  (96 agents × 252 periods × row types; the `stage` column separates
  `attention_allocation` from `trading` rows; analyses filter on
  `stage == "trading"`, 24,192 agent-period trading decisions).
- `treatment_assignment.csv` — randomization record: agent, persona stratum,
  treatment cohort/arm (and `sm_variant` in Rerun A).
- `experiment_audit.log` — run audit trail (initialization, workers,
  configuration validation, save timestamps; documents the field dates above).
- `power_summary.csv` — design-stage power calculation snapshot.
- `original_run/experiment_full_log.csv` — retained under its production name
  because `R/worked_example_appendix_B.R` reads it. In these runs it is
  byte-identical to `experiment_results_final.csv` (verify via CHECKSUMS.md5).

## Key columns (`experiment_results_final.csv`)

Identifiers and design: `agent_id`, `t` (period 1–252), `stage`, `persona`,
`treatment_cohort`, `is_treated_now`, `post_treatment`, and in Rerun A
`attention_arm` (viral/normal), `cost_arm` (low/high), `sm_variant`.

Trading rows: `action` (buy/sell/hold), `ticker` (internal name:
AAPL/NVDA/AMC/GME in every run), `shares`, `realized_pnl`,
`portfolio_value`, `trade_fee`, `buy_indicator`, `sell_indicator`,
`trade_indicator`, `is_forced_trade` (FALSE throughout: interventions
disabled), `rationale` (model text), `forward_return` (portfolio-level).

Attention rows: `allocation` / `full_attention_allocation` (per-ticker
deep/quick/ignore), `perceived_highest_buzz`, `actual_highest_buzz`,
`manipulation_check_passed`.

The complete column set is produced by
`R/experiment/Attention_driven_trading_R1.R`; consult its logging section for
any field not listed here.

## Notes

- Prices are not logged per ticker-period; the market is fully seeded
  (seed 42) and scripts re-derive exact price paths deterministically
  (validated against logged realized P&L at correlation 0.999997).
- In `rerun_B`, model-facing text (rationales) contains no real ticker
  names; internal columns intentionally retain them so analysis code is
  unchanged across runs.
- CSVs are 45–50 MB: under GitHub's 100 MB hard limit but above its 50 MB
  warning threshold in some cases — consider Git LFS or a release asset if
  the repository host complains.
