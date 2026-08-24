# Research context — authoritative statement (version 3)

Drafted 2026-08-11 by Claude, aligned with model_understanding_v5; supersedes both the installed 2026-07-14 file and the version-2 draft of 2026-08-07. For Alex to review, correct, and install. This file stays the short anchor of record; the full reasoning lives in model_understanding_v5, whose additions relative to Alex's edited version 4 are printed in blue for review — blue turns black once Alex accepts it.

## 1. Project identity and goal

- Researcher: Alex Lin, statistics master's student. Supervisor: Natalia Nolde.
- Target paper: "Non-stationary modelling of stream flow extremes under climate change scenarios" (Alex Lin and Natalia Nolde).
- Research direction: extreme value theory (GEV / GPD) and time series — applied research with serious theoretical and methodological grounding.
- Distinguishing feature for the Introduction: existing rainfall–streamflow association work mostly answers questions about the mean level of flow; this project answers questions about extremes, not means.
- End goal: return levels of extreme streamflow, marginal and conditional; the stated research interest is how results differ between the two emission scenarios ("how much worse"); scenario analysis, not prediction.
- The empirical chapter is expected to be the main contribution: what the data are, why chosen, and how every choice was implemented.

## 2. Data (unchanged decisions; one wording note)

- Twelve dependence-screened BC stations; daily streamflow and baseflow, baseflow measured directly. Codes in daily_hist_sf_bf_1968_2012_dependence_stations.csv: BARRM, BARRS, BRIDG, EAGLE, ELKFE, FRSHA, FRSHP, FRSMG, ILLEC, MCGRE, NTHMB, SMRAW — complete 1968-01-01 to 2012-12-31, 16,437 days per station, no missing values.
- Climate-model daily series under RCP 4.5 and RCP 8.5, each 1968–2099 with its own simulated historical segment.
- Decision: one fitting axis, 1968–2099, RCP 4.5 alone; the observed record is not used in fitting (v5 wording, Alex's edit: "the history record is not considered"); the reason is the offset at the 2012/2013 join. RCP 8.5 refit later for the scenario comparison.
- Flag for Alex: the v4 sentences defining debiasing and stating the simulated-world limitation ("every fitted curve describes the simulated world rather than the observed one") were removed by Alex's edit and are not in v5. The underlying decision (no debiasing; limitation to be acknowledged) still stands here; decide whether that limitation sentence returns to the document or lives only in the report's limitations section.
- Data-provider name still to verify (transcribed CIMPC; probably PCIC or CMIP). Report data section needs: source and website, map, time-series figures, station names, choice reasons, the fit-through-2099 idea, the splicing problem.

## 3. Modelling framework (decided)

- Two-stage conditional extremes workflow: marginal POT per variable with probability-integral transform to Gumbel; Heffernan–Tawn dependence extended to non-stationarity after Jonathan et al. (2014).
- Covariate: single running day index $t$ over 1968–2099. Seasonality deliberately outside the parameter curves; enforced by the knot spacing, checked by sliced diagnostics.
- Estimability framing (v5, Section 2.3): each date occurs once, so nothing is fitted pointwise; the smoothness assumption shares information locally — each B-spline coefficient is the local height of the curve over its bump and is estimated from the thousands of days under that bump; "pooling" only in this local sense, never the stationary sense of one common distribution for all days.
- Knots (decided as design, v5): fixed in advance, never data-selected; equal spacing over 1968–2099 (uniform daily density makes equal spacing exactly right); spacing at least a year (so within-year cycles are unrepresentable) and well under a few decades (so drift is representable), around five years as the working figure, giving on the order of thirty coefficients per curve; generous because the penalty can remove but not create flexibility; safeguard is a halve-and-double sensitivity check, not bootstrap inclusion; boundary knots exactly at the axis ends (source of the no-extrapolation property); periodicity off. Only the penalty weight $\lambda$ is selected from data — Jonathan et al.'s own arrangement (uniform knots, single $\lambda$ per fit, cross-validation), verified against the paper.
- Margins: moving threshold by penalized non-crossing quantile regression on a grid ($\tau^*$ provisionally 0.95 by stability); GP above with covariate scale on the log scale; constant $\xi$ per station.
- Below-threshold body: the quantile-ladder linear interpolation — Jonathan et al.'s Section 3.3 construction, in essence the empirical distribution function with its $1/n$ steps smoothed along the slope between grid levels, translated to covariate-aware form (rungs = fitted quantile curves, step heights = grid probabilities); inessential at our sample sizes, kept for tidiness.
- Decision 1 (model size), two moves per parameter: move one is the sharp slope-zero test — likelihood ratio or AIC at the GP margin, approximate guide plus block-bootstrap interval at the dependence stage, slope against bootstrap standard error at the threshold stage (the threshold meaning the curve $u(t;\tau^*)$, not the level $\tau^*$ and not the dependence constant $v$); a constant winning at the threshold is a legitimate outcome. Move two reads whether the penalized spline collapses to the line. Safeguards: flat fitted spline falls back to a constant; the all-constant stationary competitor is always fitted alongside.
- Decision 2 (smoothness): cross-validation now (no package exists for it — project code, with contiguous-block folds because of serial dependence); qgam calibration / evgam REML as the automatic alternative.
- Decision 3 (software): agreement rule across quantreg / qgam / evgam-ald, then standardize. Verified findings recorded in the document: quantregGrowth::gcrq (native non-crossing penalized ladders) and quantreg's constrained method (exact non-crossing, linear family); texmex as the stationary reference implementation — the hand-coded dependence fitter with all curves constant must reproduce it, its mexMonteCarlo is the stationary version of the generative recipe, and its Laplace-margin default with the Keef-style constraint is the reference implementation behind open item 6; evgam's extremal-index family gives a covariate-dependent $\theta(t)$ if needed. Full audit in package_audit_2026-08-07.md.
- Dependence: four curves $a, b, m, s$; parametric in the $y$ direction, estimated in the $t$ direction; working Gaussian likelihood (scoring device, never believed); residuals kept as a table, one row per historical extreme day, one column per associated station — the table is the empirical $G$, row reuse across eras licensed by the covariate-free-$G$ assumption and checked by the sliced residual diagnostic; whole rows resampled at generation time; dependence threshold a single Gumbel-scale constant $v$.
- Exploratory motivation and cross-check: moving-window $\chi$ and $\bar\chi$ sequences (summaries, not models); post-fit $\hat a(t)$ overlay. Note: this paragraph was absent from Alex's edited v4 and is restored in blue in v5 because open item 7 depends on it — Alex to confirm keep or strike.
- Serial dependence (decided): days are treated as independent only inside two formulas (fitting criteria; the annual chaining product) as working assumptions; in the data, exceedances arrive in runs. The chaining is corrected by the extremal index — $\Pr(M_Y \le x) \approx [\prod_t (1 - p(x,t))]^{\theta}$, $1/\theta$ the mean run length, verified against Lee and Lee ($G$ versus $G^{\theta}$; "severely biased if the data are dependent") — and all uncertainty comes from the block bootstrap with every selection step inside. Declustering rejected. Distribution-per-day and independence-between-days are different questions; the marginal Gumbel promise is about margins only.
- Return levels: per-reference-year $x_T(Y)$; Route A root finding, Route B Monte Carlo generation (the workhorse). Vocabulary kept separate: Monte Carlo generation from the fitted model; the bootstrap resamples observed data (uncertainty only); a simulation study plants a known truth to validate the method (Jonathan et al.'s Section 4; none currently planned for us); the climate-model series is simulated input data. Per-day conditional quantile $Q_t(\tau \mid B_{t-\ell} \ge b)$ — equation (13) in v5 after renumbering — estimated generatively (order ten thousand repetitions, hundreds above the target quantile).

## 4. Conditioning structures and work order

- Structure A, spatial: twelve stations, eleven pair fits per conditioning direction, jointness in the residual rows. Six reporting pairs fixed (Case 6 held pending a data-version discrepancy).
- Structure B: C1 fixed lag, $(B_{t-\ell}, X_t)$, $\ell \ge 1$, single lagged day as the conditioning value; the pre-event window average is an option to be discussed (open decision 2), not current convention; $\hat a_\ell$ decay plot; lag selection inside the bootstrap; event-level alternative falls with the rejected declustering route.
- Work order (Alex's v5 wording): the full pipeline runs end to end on a single pair, and will repeat for more results later. No specific first pair is recorded in the document.

## 5. Open decisions (mirrors v5 Section 6)

1. Threshold levels: marginal $\tau^*$ and dependence $\kappa$, by stability ladders.
2. Structure B timing: the lag $\ell$; whether to adopt a pre-event window average at all (single lagged day for now) and, if adopted, the window $w$.
3. Per-parameter model-size outcomes — await the code.
4. Smoothing mechanism (CV now vs automatic) and threshold-software standardization after the agreement check.
5. Sharing across stations.
6. Working scale: Gumbel now, Laplace alternative (references to be located; texmex is the reference implementation for the Laplace side).
7. Window width for the moving-window $\chi$ / $\bar\chi$ screening.

## 6. Superseded presumptions — do not reuse

- Everything listed in version 2 remains superseded (old Options A/B; climate data as test set; combined axis with seam; open shape parameter; declustering as live option).
- The version-2 draft itself is superseded by this file.
- The window average as part of the C1 convention: superseded — single lagged day is the convention; the average is open decision 2.
- The grow-by-one-to-two-pages version cap: overridden for version 5 by Alex's instruction to include the chat explanations at full depth (v5 is 15 pages).

## 7. Writing principles of record

- Citation discipline: original references only; be able to define every term and name who introduced it; no second-hand terms; Wikipedia cannot be cited.
- Every return-level statement is a probability statement about a reference year or day, never a waiting-time forecast; the scenario stage is analysis, not prediction.
- Review convention for the living document: additions arrive in blue; Alex's acceptance turns them black in the next version.
- The course-book diagnostic framework reference (6.2.3 versus "chapter 4") still to be verified against course materials.
