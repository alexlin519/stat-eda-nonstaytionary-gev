# Conditional extremes pipeline (BRIDG | NTHMB) — handoff

**Project:** Non-stationary modelling of stream flow extremes under climate
change scenarios. Alex Lin, supervisor Natalia Nolde. This folder is the
complete, self-contained state of the conditional-extremes analysis as of
2026-08-22, written so a new chat/agent (or a reader) can pick it up with
zero prior context.

## The files

| File | What it is |
|---|---|
| `code_report_v5.Rmd` | THE pipeline. Consolidates code_report_v4 plus every patch applied during development: ladder caching, the five investigation chunks (windowed chi, per-season dependence, constraint-off refit, tensor threshold, per-season GP), the PIT diagnostics (rung audit, per-bar noise band, month/era residual maps), and the 20 threshold-window figures. Reads `daily_sf_bf_rcp45_only_1945_2099_9avg.csv` by name on its first data line (file NOT in the repo — supply locally). |
| `walkthrough_v4.pdf` (+ .tex) | The presentation & algorithm companion: every figure explained (axes, theory, our numbers, Q&A drill), every algorithm opened with pseudocode (qgam line by line, qgam-vs-CV, the dependence fitter), the verification ladder, the status table, and the fix list. **Read its Status page first.** |
| `model_understanding_v8.tex/.pdf` | The authoritative model note (the "v8 §x, eq. (n), p. y" citation target used everywhere). |
| `results/code_report_v4_run_2026-08-22.html` | The real run (macOS, all packages: qgam 2.0.0, evgam, texmex, extRemes, mgcv) on the definitive data — the source of every number quoted in the walkthrough. |
| `research_context_v3.md`, `meeting_62_todo_list_EN.md`, `AGENT_BRIEF_stat_project.md` | The decision log, the meeting-62 to-do that drove the current design, and the working rules (evidence-with-numbers, no silent defaults, user decides). |

## State in one paragraph

The **marginal stage passes every check** the professor asked for: data
forensics clean (streamflow tie-free, no gaps/dups/NAs), seasonal+trend
threshold tracking the annual cycle with the local-5% check, GP tail with
significant scale trends (xi = 0.038 / 0.118 / −0.058), PIT flat within
serial-dependence noise in every season and era slice. The additive
seasonal design mis-calibrates the first era (SON 0.149 vs 0.05); the
tensor `te(doy, tt)` threshold repairs it (0.039) — **adopt it**. The
**dependence stage has a known blocking defect**: the Keef constraint's
hard 1e10 wall stalled BFGS, so the reported fit is its starting value
(objective 2698.6 for all model sizes, trend statistic exactly 0.00, a(t)
flat 0.095); the constraint-off refit reaches 2022.2 with a(t) rising
0.396 → 0.490, agreeing with the model-free windowed chi (0.253 → 0.403).
**No dependence-stage number (incl. the functionals, P_co ≈ 0.35 flat) is
quotable until the repair.**

## The fix list (priority order)

1. **Dependence optimiser** — replace the wall with a smooth penalty
   (`kappa * violation^2`), Nelder-Mead then BFGS, multiple starts incl.
   the unconstrained solution, print the violation at the solution.
2. **Adopt the tensor threshold** as production.
3. **Decide with the professor**: season-relative extremes vs flood-season
   extremes (per-season a: JJA 0.172, DJF −0.055; freshet-only 0.227 vs
   pooled 0.095 — the conditioning event changes the answer).
4. **Lift the tau = 0.01 rung** (sits ~0.5% low; the short first PIT bar).
5. **B = 2000 bootstrap** with selections inside the loop, after fix 1.
6. **Bring in RCP 8.5** (same schema; the pipeline reads the axis from the
   file) for the scenario contrast.

## Data notes

Current file: `daily_sf_bf_rcp45_only_1945_2099_9avg.csv` (owner confirms
correctness; "9avg" intentionally not documented here). 1945–1968 has no
usable streamflow yet — the report drops years < `start_year` (YAML, 1969);
when early data arrive, that parameter is the only switch. Ladder fits are
cached as `cache_ladder_*.rds` next to the Rmd (filename carries route +
data fingerprint; `refit_ladders: true` forces a refit).

## papers/ and context/archive/

`papers/` holds the eight reference papers the pipeline cites (the five
source papers HT04, JER14, FWSS19, LL20, MWCY plus Keef et al. 2013 and the
two qgam papers). **Caution: this repo is public and several of these PDFs
are publisher-copyrighted — consider making the repo private if that is a
concern.**

## NOT in this repo (kept local on purpose)

- `daily_sf_bf_rcp45_only_1945_2099_9avg.csv` — the definitive data file,
  to be added by the owner NEXT TO `code_report_v5.Rmd` (the Rmd reads it
  by bare filename from its own folder).
- `cache_ladder_*.rds` — local fit caches; regenerate with
  `refit_ladders: true`.
