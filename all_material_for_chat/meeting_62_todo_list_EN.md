# Meeting 62 — To-do Summary

Source: meeting_62_transcript.md (28 min 02 s, machine transcription, lower quality than meetings 60/61, no speaker labels; speaker attribution and a few word readings are my inference from content, so check against the audio before quoting anything verbatim). This file does one thing: it collects everything from this meeting that needs to be done. Timestamps follow the transcript.

## 0. The core verdict of the meeting (the premise behind every to-do)

The professor's overall judgment this time: the marginal stage has not passed yet. Your PIT check (probability integral transform: plug the observations into the fitted distribution function $\hat{F}_t$; if the marginal model is correct, the result should be approximately uniform on $(0,1)$) shows clear seasonality and is not uniform (04:31–05:04, 22:30–22:46). From this she concludes that something in the code or the model is wrong, and she stresses that if the margins are wrong, everything downstream is wrong: the multivariate representations assume a certain standardized marginal behavior, so it is "garbage in, garbage out" (23:13–24:06). You meet again on Friday, with the goal of fixing "the three problems" first (27:37; the transcript never names the three explicitly — based on the checking order she gives at 23:43–24:28, my inference is: data cleaning, the threshold, and the excesses plus PIT).

## 1. Three things that must be done before Friday

1. Data cleaning (09:19–12:52, 23:43–24:06).
   - Drop the first year: use the data from 1969 onward and discard all of 1968 (the professor, roughly verbatim: of course you should throw away that whole first year; you first clean the data).
   - Track down the repeated values: identical observations appear to have entered the data more than once; she asks you to find out exactly which values these are, and whether it is only that year or other years as well (24:06: find out which values those are).
   - Investigate winter data quality: the lower-tail problems concentrate in December–February; her guess is measurement issues such as freezing, and she wants you to confirm whether these are measurement errors identifiable by eye.
   - If particular months or data points turn out to be bad, exclude them from the analysis, and explain in the report what you cleaned and why (12:34–12:52).

2. Threshold verification (03:26–06:48).
   - The current time-varying threshold uses the $\tau = 0.95$ quantile; the long-horizon overview plots show nothing useful, so you must plot 1–2 year windows and check that within a year the threshold tracks the seasonal cycle in a reasonable way (the data have strong seasonality, so the threshold must move with the seasons).
   - Check the exceedance proportion: since the threshold is set at the 95% quantile, roughly 5% of points should exceed it, and this should hold locally, not just overall.

3. Re-check of excesses and PIT (06:48–07:09, 24:15–24:35).
   - Look at the excesses above the threshold: after the cleaning and the threshold are fixed, the excess series should show no seasonal pattern any more.
   - Redo the PIT histogram: it must come out approximately uniform; the current seasonal shape means there is a bug somewhere in the transformation chain, which you need to locate and fix (22:30–22:46).

## 2. Remaining checks in the marginal stage

- Print and inspect the GPD (generalized Pareto distribution) parameter estimates: look at the scale parameter over time, and also in smaller chunks; for the shape parameter (the tail index, currently held constant) you could not state the estimated value when asked, so print that number out and check that it is sensible (07:09–07:53). The tool is evgam (transcribed as EV gamma, which cross-confirms the guess from meeting 60).
- QQ plots (quantile–quantile plots, checking whether the fitted distribution's quantiles match the data's): you already have per-year QQ, chunked QQ, and seasonal QQ; keep the exponential-scale version the professor asked for earlier; the fact that lower-tail deviations concentrate in winter should be explained together with the cleaning in item 1 (08:09–09:19). The phrase "wrong in the load" about the chunked QQ is garbled in the transcript — check yourself whether something in the loading or code is off.
- The summer-slice shift: you observed that the summer data are shifted as a whole on the Gumbel scale, and the professor said this should not happen for Gumbel quantiles — treat this as another symptom of a transformation error and debug it together with the PIT problem (10:37–11:48).

## 3. Switch the marginal transformation to Laplace

- Decision: change everything to Laplace (the double exponential distribution, which has mass on the whole real line) instead of Gumbel; in the meeting you said you need to redo everything in Laplace, and the professor confirmed that Laplace would be better (21:17–21:35, 24:35–24:40).
- Order of operations: first fix items 1 and 2 so the PIT is approximately uniform, then apply the Laplace quantile function to the uniform variables, and everything is on Laplace margins (22:30–22:46).
- Perspective: the professor noted that your two variables are positively dependent, so the Gumbel restriction is actually not so bad in your case; the other student (transcribed as Nixon, name unresolved) works with negatively dependent or unassociated variables, where the problem is serious (24:54–25:13). Switching to Laplace remains the plan, but no panic-level rework is needed.
- Check the parameter range along with it: under Gumbel margins $\alpha \in [0,1]$; under Laplace margins $\alpha \in [-1,1]$; there was a back-and-forth in the meeting about the range of $\alpha$ and you said you would check your code — align the constraint on $\alpha$ in the code with the Laplace version (14:29–14:55).

## 4. Rework of the conditional-simulation display plots

The professor's conclusion: the generation itself is fine; the display is wrong (19:15). What to change:

- Condition on the day of the year. You had been conditioning on a randomly picked date, which is why the simulated points came out smaller than the real extremes: the real annual-maximum flows sit on particular days of the season, while your random day may fall in a low-flow period; the year alone is not enough, you must condition on day of year (17:36–19:02).
- Do not color by year after transforming to standard margins. Once the time-varying distribution has been used for the transformation, the data at all time points follow the same distribution (they are stationary), so red versus blue for 1973 versus 2094 carries no meaning — the year has been absorbed by the transformation (20:16–21:17, 21:35–21:53).
- The meaningful contrast is the scenario contrast: compare RCP 4.5 against RCP 8.5 in the same display (your suggestion, which the professor ultimately endorsed, 21:53–22:06). Implication (mine, not verbatim from the transcript): the RCP 8.5 data, which in meeting 61 you had "on hand but not plugged in," now need to be brought into the pipeline.
- Display scale to be decided: the professor raised whether to draw this plot on Gumbel/Laplace standard margins or back on the original data scale; the half-sentence about which option "hides the time-varying nature of the distribution" is garbled in the transcript (22:08–22:27), so confirm in person on Friday. My reading (flagged as my inference): standard margins wipe out the marginal time variation, while the original scale displays it.
- Cosmetic fixes: the plot title wrongly says A (what is plotted is actually the yc and yx model values); the broken "0.00 to 0.00" label; you promised a better demonstration version (14:13–15:47).
- Both conditioning directions are needed: given that streamflow at one station (transcribed NTHMB, possibly North Arm) exceeds the threshold (about 3 on the Gumbel scale, i.e., the 95% quantile, since $-\log(-\log 0.95) \approx 2.97$ — the conversion is mine), generate the other station; and the reverse, conditioning on the other station (transcribed as the bridge station) (16:12–16:54). Later, extend to the baseflow-by-streamflow version (15:55–16:12).

## 5. Report and writing

- The rest of the report focuses on these two stations and this one pair (03:12–03:26, continuing the decision from meeting 61).
- Write the full data-cleaning story into the report: what was cleaned, why, and on what evidence (12:34–12:52).
- (The citation rule from meeting 60 stays in force: every term gets its original reference.)

## 6. Administrative

- Register for the project course (3 credits): you only need to register once; registering in the fall is fine (at the latest, the spring before you graduate); no forms or signatures — you register yourself in the system; it appears on the transcript, and you need the credits and a grade to graduate (00:51–02:36). About the summer non-registration, the professor's stance was that if it caused no problem, it is fine; if you want peace of mind, confirm once with the graduate office (this last suggestion is mine).

## 7. Reading, and materials to request from the professor

- Ask the professor for the Laplace-related material she mentioned: more theoretical, with an application, apparently in slide form; she previously sent it to the other student and suggested you read it too for another perspective (24:54–25:13, 27:37–27:54).
- You asked in the meeting whether there are other papers on Laplace-related material: also search the post-Heffernan–Tawn literature on Laplace margins yourself. My pointer (not from the meeting, and I have no literature database, so I may be misremembering — verify the original before citing): the standard reference on this line is usually Keef, Papastathopoulos and Tawn (2013).

## 8. Schedule

- Meet again on Friday, bringing the results of the three fixed problems (27:37–27:54).

## 9. Transcription items to verify (before quoting or writing anything up)

- From 25:13 to 27:37 about 2.4 minutes contain no detected speech; re-listen to confirm whether content was missed.
- Unresolved words: Leon (11:51, the name of some analysis or plot you made), Nixon (the other student's name), the two station names (NTHMB / the bridge station), and "you cube constant" (probably "you keep constant," referring to the shape parameter being held constant).
- The list of "the three problems" is my inference (data, threshold, excesses plus PIT); open Friday's meeting by confirming it with the professor.
- The sentence about the display scale at 22:08–22:27 is ambiguous in its direction; see Section 4.

## 10. Decisions worth writing back into research_context.md

Marginal transformation set to Laplace; data start moved to 1969 (1968 discarded); the conditional-simulation display centered on the scenario contrast (RCP 4.5 versus 8.5) with the condition set by day of year; the report focused on the chosen pair of stations.
