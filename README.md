# Replication package — *Homo Silicus is Hyper-Rational*

Code for **"Homo Silicus is Hyper-Rational: Why GPT-4-Family Agents Do Not
Replicate Attention-Driven Trading"** (revision under review, *Journal of
Economic Interaction and Coordination*).

The study runs 96 GPT-4-family LLM agents (6 persona prompts × 16 agents)
trading four assets over 252 periods in a seeded market simulation, and asks
whether exogenous viral-attention shocks produce the attention-driven buying
documented for human retail investors. Three experiments are reported: the
original staggered run (viral signals bundled with a 5→15 bps cost surge on
the shocked tickers), **Rerun A** (a 2×2 factorial separating viral signals
from the cost level), and **Rerun B** (the original design with invented
ticker symbols replacing the real ones in everything the model sees).

## Repository layout

```
R/
  reproduce_results.R          One-command pipeline: reproduces every statistic
                               quoted in the revised manuscript and the response
                               to the referee from the raw experiment logs.
                               Stages: 1 = original run, 2 = Rerun A, 3 = Rerun B.
  worked_example_appendix_B.R  Regenerates manuscript Appendix B (a complete
                               agent-period: all prompts, panels, and logged
                               decisions) deterministically from the seeded
                               market generator. Also sourced by the pipeline
                               (functions-only mode) for exact price paths.
  experiment/
    Attention_driven_trading_R1.R
                               The experiment itself (requires OPENAI_API_KEY;
                               ~48k API calls ≈ $6/run). Config toggles select
                               the original run, Rerun A, or Rerun B — see the
                               file header.
    Attention_driven_trading_Statistical_Analysis_R1.R
                               The full analysis battery (all estimators,
                               figures, publication tables), including the
                               R1-A/B/C sections that produce the factorial
                               arm-effects table and Appendix Tables A4/A5.
                               See its header for an important scope warning.
data/                          Not committed (≈50 MB per run log). Layout below.
output/                        Written by the scripts; not committed.
```

## Data

The experiment logs are too large for the repository and are distributed
separately (journal replication archive / on request). Place them as:

```
data/original_run/experiment_results_final.csv
data/original_run/experiment_full_log.csv        (worked example only)
data/original_run/treatment_assignment.csv       (worked example only)
data/rerun_A/experiment_results_final.csv
data/rerun_B/experiment_results_final.csv
```

or point the environment variable `ADT_DATA_ROOT` at an equivalent tree.
No confidential or human-subjects data are involved; all "agents" are LLM
instances.

## How to reproduce

```sh
Rscript R/reproduce_results.R            # all stages (~2–3 min)
Rscript R/reproduce_results.R 2          # Rerun A only
Rscript R/worked_example_appendix_B.R    # Appendix B text
```

Requirements: R ≥ 4.3; packages `data.table`, `fixest`, `glue`, `digest`
(optional `did` for the Callaway–Sant'Anna estimator, whose quoted estimates
come from the full analysis script). Developed under R 4.3.3.

### What maps where

| Manuscript location | Statistic | Produced by |
|---|---|---|
| §4.6, Table 10 | Factorial arm effects: planned (p = .119), design-aligned adjustment (p = .006), randomization inference (p = .009), main effects, contrasts, excl-SM | pipeline stage 2 |
| §4.6 | Neutral-ticker TWFE and the stacked cross-run contrast (p = .89) | pipeline stage 3 |
| §4.6 | CS ATT for Rerun B (−2.72, SE 5.49, p = .620) | full analysis script on `rerun_B` data |
| §6.1, Table 3 | Common-window randomization balance | pipeline stage 1 |
| §6.5 | H4 forward-return-on-attention-depth regression | pipeline stage 1 |
| §6.6 | PGR/PLR = 0.478/0.638 (ratio 0.75), replay validation, fee sensitivity | pipeline stage 1 |
| Appendix B | Worked example | `worked_example_appendix_B.R` |
| Appendix Tables A4/A5 | Persona fidelity; displayed-edge alignment | full analysis script on `original_run` data (sections R1-B/R1-C) |

## Notes for reviewers

- **Specification sequence (Rerun A).** The planned specification is agent +
  period fixed effects (focal estimate −4.41 pp, p = .119). Persona-by-period
  fixed effects were added *after* that estimate was observed, as a
  design-aligned precision adjustment motivated by the within-persona
  stratified randomization (same point estimate, p = .006). Randomization
  inference within persona strata does not depend on that adjustment
  (p = .009). The pipeline prints all three in this order, matching the
  disclosure in the manuscript and response letter.
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

## License and citation

Code is released under the Apache License 2.0; see `LICENSE`. Please cite the
paper (citation to be added on acceptance).
