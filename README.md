# Replication package — *Homo Silicus is Hyper-Rational*

Code for **"Homo Silicus is Hyper-Rational: Why GPT-4-Family Agents Fail to
Replicate Attention-Driven Trading"** (under review, *Journal of
Economic Interaction and Coordination*).

The study runs 96 GPT-4-family LLM agents (6 persona prompts × 16 agents)
trading four assets over 252 periods in a seeded market simulation, and asks
whether exogenous viral-attention shocks produce the attention-driven buying
documented for human retail investors. Three experiments are reported: **Experiment 1**, the
original staggered run (viral signals bundled with a 5→15 bps cost surge on
the shocked tickers); **Experiment 2** (`rerun_A/`), a 2×2 factorial separating viral signals
from the cost level; and **Experiment 3** (`rerun_B/`), the same staggered design with invented
ticker symbols replacing the real ones in everything the model sees.

## Repository layout

```
R/
  verify_data.R                Cross-platform, base-R verification of every raw
                               file against data/CHECKSUMS.md5.
  reproduce_results.R          Focused pipeline for the core results quoted
                               in the manuscript. Stages: 1 = Experiment 1,
                               2 = Experiment 2, 3 = Experiment 3.
  run_full_analysis.R          Portable wrapper for the comprehensive original
                               and neutral-ticker analysis battery.
  worked_example_appendix_B.R  Regenerates manuscript Appendix B (a complete
                               agent-period: all prompts, panels, and logged
                               decisions) deterministically from the seeded
                               market generator. Also sourced by the pipeline
                               (functions-only mode) for exact price paths.
  experiment/
    Attention_driven_trading_R1.R
                               The experiment itself (requires OPENAI_API_KEY;
                               ~48k API calls ≈ $6/run). Config toggles select
                               Experiment 1, 2, or 3 — see the
                               file header.
    Attention_driven_trading_Statistical_Analysis_R1.R
                               The full analysis battery (all estimators,
                               figures, publication tables), including the
                               R1-A/B/C sections that produce the factorial
                               arm-effects table and Appendix Tables A4/A5.
                               See its header for an important scope warning.
data/                          Raw experiment logs, all three runs (included;
                               inventory and codebook in data/README.md).
output/                        Written by the scripts; generated results are not
                               committed except `sessionInfo.txt`.
```

## Data

The raw experiment logs for all three runs are included under `data/`
(~185 MB total). See `data/README.md` for the run inventory and key-column
codebook. Verify all files portably from the repository root with:

```sh
Rscript R/verify_data.R
```

```
data/original_run/   experiment_results_final.csv, experiment_full_log.csv,
                     treatment_assignment.csv, experiment_audit.log,
                     power_summary.csv
data/rerun_A/        experiment_results_final.csv, treatment_assignment.csv,
                     experiment_audit.log, power_summary.csv
data/rerun_B/        experiment_results_final.csv, treatment_assignment.csv,
                     experiment_audit.log, power_summary.csv
```

An alternative data location can be supplied via the environment variable
`ADT_DATA_ROOT`. No confidential or human-subjects data are involved; all
"agents" are LLM instances. Every individual file is below GitHub's 100 MB
hard limit. `.gitattributes` preserves the raw files byte-for-byte so the
published checksums are stable across operating systems.

## How to reproduce

```sh
git clone https://github.com/garcijo4/homo_silicus_is_hyper_rational_r1.git
cd homo_silicus_is_hyper_rational_r1
Rscript R/verify_data.R                # validate all included raw files
Rscript R/reproduce_results.R            # all stages (~2–3 min)
Rscript R/reproduce_results.R 2          # Experiment 2 (factorial) only
Rscript R/worked_example_appendix_B.R    # Appendix B text
Rscript R/run_full_analysis.R original_run
Rscript R/run_full_analysis.R rerun_B
```

`reproduce_results.R`, the checksum validator, and the worked example require
R ≥ 4.3 and packages `data.table`, `fixest`, `glue`, and `digest` (the checksum
validator itself uses base R only). Install the minimal set with:

```r
install.packages(c("data.table", "fixest", "glue", "digest"))
```

The focused pipeline was last verified with R 4.3.3, `data.table` 1.17.8,
`fixest` 0.13.2, `glue` 1.8.0, and `digest` 0.6.37 (`output/sessionInfo.txt`).
Exact package versions matter for a handful of quoted p-values (see the known
inference sensitivities below); when archiving a release, record a fresh
`sessionInfo()` — or an `renv::snapshot()` lockfile — from the machine that
produced the submitted numbers.

The comprehensive analysis wrapper additionally requires `tidyverse`, `did`,
`bacondecomp`, `modelsummary`, `kableExtra`, `scales`, `sandwich`, and `lmtest`.
`patchwork`, `clubSandwich`, and `ggrepel` enable optional branches:

```r
install.packages(c(
  "tidyverse", "did", "bacondecomp", "modelsummary", "kableExtra",
  "scales", "sandwich", "lmtest", "patchwork", "clubSandwich", "ggrepel"
))
```

The package was developed under R 4.3.3. The full-analysis wrapper keeps
generated tables and figures under `output/full_analysis/<run>/`. The
factorial Experiment 2 (`data/rerun_A/`) must be analyzed with
`reproduce_results.R 2`, not the staggered-design full battery.

### What maps where

Experiment labels: Experiment 1 = original staggered bundled run
(`data/original_run/`), Experiment 2 = factorial rerun (`data/rerun_A/`),
Experiment 3 = neutral-label rerun (`data/rerun_B/`). Data folders keep their
original names for checksum stability.

| Manuscript location | Statistic | Produced by |
|---|---|---|
| Experiment 2 results | Factorial arm effects: planned (p = .119), post-outcome adjustment (p = .006), focal RI (any-buy p = .0078, trade p = .0016, sell p = .037), main effects, contrasts, target/comparison-asset outcomes with exact 95% CIs, buy-minus-sell and target-minus-comparison difference outcomes, excl-SM, SM construction sensitivity | pipeline stage 2 (saves `rerunA_results_table.csv`, `rerunA_focal_ri.csv`, `rerunA_sm_construction_sensitivity.csv`) |
| Experiment 3 results | Neutral-ticker TWFE, stacked cross-run contrast (p = .89), corrected common-window balance | pipeline stage 3 |
| Experiment 3 results | Experiment 3 target/comparison-asset interactions, disposition ratio, placebo (Cohort-1 only) | full analysis script on `rerun_B` data |
| Experiment 3 results | CS ATT for Experiment 3 (−2.72, SE 5.49, p = .620) | full analysis script on `rerun_B` data |
| Table 3 | Common-window randomization balance | pipeline stage 1 |
| Table 4 | First-stage attention associations, recomputed under the single documented specification (saves `table4_first_stage.csv`) | pipeline stage 1 |
| Results (exploratory) | Forward-return-on-attention-depth regression | pipeline stage 1 |
| Results (disposition) | PGR/PLR = 0.478/0.638 (ratio 0.75), replay validation, fee sensitivity; descriptive gain/loss moderation comparison with window sensitivity | pipeline stage 1 |
| Appendix B | Worked example | `worked_example_appendix_B.R` |
| Appendix Tables A4/A5 | Persona fidelity; displayed-edge alignment | full analysis script on `original_run` data (sections R1-B/R1-C) |

## Notes on inference and determinism

- **Specification sequence (Experiment 2, factorial).** The planned specification is agent +
  period fixed effects (focal estimate −4.41 pp, p = .119). Persona-by-period
  fixed effects were added *after* that estimate was observed, as a
  design-aligned precision adjustment motivated by the within-persona
  stratified randomization (same point estimate, p = .006). Focal within-persona
  Monte Carlo randomization inference — viral/5 bps versus control, four-versus-four
  label reassignment within each persona stratum, R = 500,000, seed 20260731,
  (b+1)/(R+1) correction — does not depend on that adjustment (any-buy p = .0078,
  trade p = .0016, sell p = .037). Because the focal contrast was selected
  after results were observed, the manuscript labels the focal RI a
  post-outcome design-based sensitivity analysis. The pipeline prints the full
  sequence in this order, matching the manuscript.
- **Determinism.** The market environment is fully seeded (seed 42) and the
  position replay behind PGR/PLR matches the logged realized P&L of all
  3,859 sales with correlation 0.999997. LLM responses themselves are not
  deterministic (temperature 0.4), so re-running the *experiment* produces a
  new behavioral realization on the identical market history.
- **Known inference sensitivities**, disclosed in the text: CS bootstrap SEs
  vary slightly across seeds (point estimates exact); the joint viral-cell
  Wald p varies .028–.032 across `fixest` versions (quoted as ".03").
- The Social Momentum construction comparison is descriptive only (16
  agents, 1–2 per arm-by-construction cell); no formal equality test is
  reported.

## Citation, provenance, and license

The original-run data are byte-identical to the CSVs in the archived
[`homo_silicus_is_hyper_rational`](https://github.com/garcijo4/homo_silicus_is_hyper_rational)
package. The present repository supersedes that package by adding the factorial
experiment (`rerun_A/`), the neutral-ticker experiment (`rerun_B/`), the
extended analysis code, and portable validation.

Please cite:

> Garcia, John, *Homo Silicus is Hyper-Rational: Why GPT-4-Family Agents Fail to
> Replicate Attention-Driven Trading* (December 10, 2025), SSRN 5901742.
> https://doi.org/10.2139/ssrn.5901742

Machine-readable citation metadata are provided in `CITATION.cff`. Code is
released under the Apache License 2.0; see `LICENSE`. For questions or issues,
please use the GitHub issue tracker or the contact information on the SSRN page.
