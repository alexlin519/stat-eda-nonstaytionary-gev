# ============================================================================
# PRO-ECO T-TRiPS: Exploratory Data Analysis
# Author: statistical consulting draft
# Purpose: Understand the data structure, response variable, and key predictors
#          before fitting any model. Designed to surface the specific oddities
#          in this dataset (unbalanced pre/post, centres with no follow-up,
#          heterogeneous item behaviour, implausible experience values).
# ============================================================================

# ---- 0. Setup --------------------------------------------------------------
# install.packages(c("readxl","dplyr","tidyr","ggplot2","stringr",
#                    "forcats","scales","janitor","skimr","patchwork"))
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(scales)
library(patchwork)
library(skimr)

theme_set(theme_minimal(base_size = 11))

# ---- 1. Load ---------------------------------------------------------------
raw <- read_excel("PROECO_TTRIPS_Dataset.xlsx")

# Strip accidental trailing spaces in column names (there are a few)
names(raw) <- trimws(names(raw))

cat("Rows x Cols:", dim(raw), "\n")
cat("Number of T-TRiPS items per timepoint:",
    sum(str_detect(names(raw), "^trips_.*_pre$")),  "(pre),",
    sum(str_detect(names(raw), "^trips_.*_post$")), "(post)\n")
# NOTE: Overview doc says 25 items; the dataset has 26. Worth confirming with PI.

# ---- 2. Clean --------------------------------------------------------------
# The T-TRiPS item columns mix numeric 0/1 with the strings "NA", "na ",
# and "Unknown". Coerce those strings to true NA and everything to numeric.
clean_binary <- function(x) {
  x <- trimws(as.character(x))
  x <- ifelse(tolower(x) %in% c("na", "unknown", "", "nan"), NA, x)
  suppressWarnings(as.numeric(x))
}

pre_items  <- grep("^trips_.*_pre$",  names(raw), value = TRUE)
post_items <- grep("^trips_.*_post$", names(raw), value = TRUE)

dat <- raw |>
  mutate(across(all_of(c(pre_items, post_items)), clean_binary),
         study      = factor(study,  levels = c(1, 2), labels = c("Study 1", "Study 2")),
         sex        = factor(sex),
         age        = factor(age),
         language   = factor(language),
         role       = factor(role),
         centre     = factor(centre),
         baseline   = as.integer(baseline),
         `follow-up`= as.integer(`follow-up`))

# Rename `follow-up` (backticks are annoying) to followup for convenience
dat <- dat |> rename(followup = `follow-up`)

# Educator-level scores: sum of yes-responses, and count of items actually answered.
# Using rowwise sum with na.rm=TRUE + counting non-NAs lets us compute a MEAN
# proportion that isn't distorted by the ~5% of items people skipped.
dat <- dat |>
  rowwise() |>
  mutate(
    score_pre   = if (baseline == 1) sum(c_across(all_of(pre_items)),  na.rm = TRUE) else NA_real_,
    n_pre       = if (baseline == 1) sum(!is.na(c_across(all_of(pre_items))))        else NA_integer_,
    score_post  = if (followup == 1) sum(c_across(all_of(post_items)), na.rm = TRUE) else NA_real_,
    n_post      = if (followup == 1) sum(!is.na(c_across(all_of(post_items))))       else NA_integer_
  ) |>
  ungroup() |>
  mutate(
    prop_pre   = score_pre  / n_pre,
    prop_post  = score_post / n_post,
    paired     = (baseline == 1) & (followup == 1)
  )


# ============================================================================
# PART A: OVERALL DATA STRUCTURE
# ============================================================================

# ---- A1. Quick skim of the whole frame -------------------------------------
skim(dat |> select(study, centre, sex, age, language, role,
                   field_experience, centre_experience,
                   baseline, followup, score_pre, score_post))

# ---- A2. Who contributed what: baseline / follow-up / paired ---------------
dat |> summarise(
  n_total       = n(),
  n_baseline    = sum(baseline == 1),
  n_followup    = sum(followup == 1),
  n_paired      = sum(paired),
  n_baseline_only = sum(baseline == 1 & followup == 0),
  n_followup_only = sum(baseline == 0 & followup == 1)
) |> print()
# Expected: 134 total, 112 baseline, 60 follow-up, 38 paired,
#           74 baseline-only, 22 follow-up-only

# ---- A3. Cluster structure: educators per centre, by time ------------------
centre_tab <- dat |>
  group_by(study, centre) |>
  summarise(n_total = n(),
            n_pre   = sum(baseline == 1),
            n_post  = sum(followup == 1),
            n_paired = sum(paired),
            .groups = "drop")
print(centre_tab, n = Inf)
# WATCH: Centres A, I, J contribute ZERO follow-up data. Decide with PI whether
# to retain them (they only inform centre random-intercept variance) or drop.

# ---- A4. Item-level missingness among those who did take the survey --------
dat |> filter(baseline == 1) |>
  summarise(median_items_answered = median(n_pre),
            pct_complete_all = mean(n_pre == length(pre_items)) * 100)
dat |> filter(followup == 1) |>
  summarise(median_items_answered = median(n_post),
            pct_complete_all = mean(n_post == length(post_items)) * 100)


# ============================================================================
# PART B: RESPONSE VARIABLE (T-TRiPS SCORE) ON ITS OWN
# ============================================================================

# ---- B1. Reshape to long so we can look at pre/post side-by-side ----------
long_scores <- bind_rows(
  dat |> filter(baseline == 1) |>
    transmute(record_id, centre, study, time = "Pre",  score = score_pre,
              prop = prop_pre, n_items = n_pre,
              sex, age, language, role, field_experience, centre_experience),
  dat |> filter(followup == 1) |>
    transmute(record_id, centre, study, time = "Post", score = score_post,
              prop = prop_post, n_items = n_post,
              sex, age, language, role, field_experience, centre_experience)
) |>
  mutate(time = factor(time, levels = c("Pre", "Post")))

# ---- B2. Summary statistics by time ----------------------------------------
long_scores |>
  group_by(time) |>
  summarise(n = n(),
            mean = mean(score), sd = sd(score),
            median = median(score),
            q25 = quantile(score, .25), q75 = quantile(score, .75),
            min = min(score), max = max(score))

# ---- B3. Distribution: density + boxplot -----------------------------------
p_dens <- ggplot(long_scores, aes(score, fill = time)) +
  geom_density(alpha = 0.45) +
  labs(title = "Distribution of T-TRiPS total score",
       x = "Score (0–26)", y = "Density", fill = NULL)

p_box <- ggplot(long_scores, aes(time, score, fill = time)) +
  geom_boxplot(alpha = 0.6, width = 0.4, outlier.shape = 1) +
  geom_jitter(width = 0.08, alpha = 0.35, size = 1) +
  labs(title = "T-TRiPS score by timepoint", x = NULL, y = "Score (0–26)") +
  theme(legend.position = "none")

print(p_dens + p_box)

# ---- B4. Q-Q for each timepoint (is the score ~normal?) --------------------
ggplot(long_scores, aes(sample = score)) +
  stat_qq() + stat_qq_line() +
  facet_wrap(~ time) +
  labs(title = "Q-Q plot by timepoint — checks approximate normality for LMM")
# Modestly heavy-tailed is fine; a clear floor/ceiling would push you toward
# the binomial GLMM specification.


# ============================================================================
# PART C: PREDICTORS ON THEIR OWN
# ============================================================================

# ---- C1. Categorical demographics -----------------------------------------
demo_cats <- c("study", "sex", "age", "language", "role")
for (v in demo_cats) {
  cat("\n---", v, "---\n")
  print(table(dat[[v]], useNA = "ifany"))
}
# FLAGS expected:
#   sex:   121 Female, 5 Male, 8 Unknown  -> not useful as a covariate
#   role:  4 categories + 7 Unknown       -> OK but small cells
#   language: 97 Eng / 31 other / 6 Unk   -> strong imbalance, big baseline effect

# ---- C2. Experience variables (continuous, skewed) -------------------------
summary(dat$field_experience)
summary(dat$centre_experience)

# WATCH: field_experience max is 1000 months (~83 years). Almost certainly
# a data-entry issue. Check with PI.
dat |> filter(field_experience > 480) |>   # anyone with >40 years in field
  select(record_id, centre, age, role, field_experience, centre_experience)

p_field <- ggplot(dat, aes(field_experience)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = .7) +
  scale_x_continuous(trans = "log1p",
                     breaks = c(0, 6, 24, 60, 120, 240, 600)) +
  labs(title = "Field experience (months, log1p scale)",
       x = "Months in ECE field")

p_centre_exp <- ggplot(dat, aes(centre_experience)) +
  geom_histogram(bins = 30, fill = "seagreen", alpha = .7) +
  scale_x_continuous(trans = "log1p",
                     breaks = c(0, 6, 24, 60, 120, 240)) +
  labs(title = "Centre experience (months, log1p scale)",
       x = "Months at current centre")

print(p_field + p_centre_exp)

# Recommendation: for modelling, use log1p(field_experience) and
# log1p(centre_experience), or categorise into e.g. <1y / 1-5y / 5-10y / 10+y.


# ============================================================================
# PART D: RESPONSE × PREDICTOR (MARGINAL RELATIONSHIPS)
# ============================================================================

# ---- D1. Score by study, time ---------------------------------------------
# This is the most important plot in the whole EDA — it previews the
# time × study interaction that is your secondary objective.
study_time <- long_scores |>
  group_by(study, time) |>
  summarise(mean = mean(score), se = sd(score)/sqrt(n()), n = n(),
            .groups = "drop")
print(study_time)

p_study <- ggplot(study_time, aes(time, mean, group = study, colour = study)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  labs(title = "Score by time, stratified by study",
       subtitle = "Parallel lines = no interaction; crossing/diverging = interaction",
       y = "Mean T-TRiPS score", x = NULL, colour = NULL)
print(p_study)

# ---- D2. Score by centre (the clustering you must respect) ----------------
centre_means <- long_scores |>
  group_by(study, centre, time) |>
  summarise(mean = mean(score), n = n(), .groups = "drop")

p_centre <- ggplot(centre_means,
                   aes(time, mean, group = centre, colour = study)) +
  geom_line(alpha = .6) +
  geom_point(aes(size = n), alpha = .6) +
  facet_wrap(~ study) +
  labs(title = "Centre-level mean score, pre vs post",
       subtitle = "Each line is one centre (cluster). Point size = # educators contributing.",
       y = "Mean T-TRiPS score", x = NULL, size = "n educators") +
  theme(legend.position = "bottom")
print(p_centre)

# ---- D3. Score by demographic covariate at baseline -----------------------
baseline_df <- long_scores |> filter(time == "Pre")

p_lang <- ggplot(baseline_df, aes(language, score, fill = language)) +
  geom_boxplot(alpha = .6) +
  geom_jitter(width = .1, alpha = .4, size = 1) +
  labs(title = "Baseline score by language", x = NULL, y = "Score") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))

p_age <- ggplot(baseline_df, aes(age, score, fill = age)) +
  geom_boxplot(alpha = .6) +
  labs(title = "Baseline score by age", x = NULL, y = "Score") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 25, hjust = 1))

p_role <- ggplot(baseline_df, aes(role, score, fill = role)) +
  geom_boxplot(alpha = .6) +
  labs(title = "Baseline score by role", x = NULL, y = "Score") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))

print((p_lang | p_age) / p_role)

# ---- D4. Score vs continuous experience ------------------------------------
p_field_score <- ggplot(baseline_df, aes(log1p(field_experience), score)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Baseline score vs. log(1 + field experience)",
       x = "log(1 + months in ECE field)", y = "Score")

p_centreexp_score <- ggplot(baseline_df, aes(log1p(centre_experience), score)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Baseline score vs. log(1 + centre experience)",
       x = "log(1 + months at centre)", y = "Score")

print(p_field_score + p_centreexp_score)


# ============================================================================
# PART E: INTERACTIONS (DOES THE EFFECT OF TIME DIFFER BY GROUP?)
# ============================================================================

# ---- E1. Time × Study (secondary aim, previewed non-statistically) --------
long_scores |>
  group_by(study, time) |>
  summarise(mean = mean(score), .groups = "drop") |>
  pivot_wider(names_from = time, values_from = mean) |>
  mutate(change = Post - Pre)

# ---- E2. Time × Language --------------------------------------------------
long_scores |>
  group_by(language, time) |>
  summarise(mean = mean(score), n = n(), .groups = "drop") |>
  pivot_wider(names_from = time, values_from = c(mean, n)) |>
  mutate(change = mean_Post - mean_Pre)

p_time_lang <- ggplot(
  long_scores |> filter(!is.na(language)) |>
    group_by(language, time) |> summarise(m = mean(score), .groups = "drop"),
  aes(time, m, group = language, colour = language)) +
  geom_line(linewidth = 1) + geom_point(size = 2.5) +
  labs(title = "Score by time × language", y = "Mean score", x = NULL)
print(p_time_lang)

# ---- E3. Within-person change among matched pairs -------------------------
paired_df <- dat |> filter(paired) |>
  transmute(record_id, centre, study, language, role, age,
            score_pre, score_post, delta = score_post - score_pre)

summary(paired_df$delta)
table(sign(paired_df$delta))  # negatives, zeros, positives

p_paired_hist <- ggplot(paired_df, aes(delta)) +
  geom_histogram(binwidth = 1, fill = "grey60", colour = "white") +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_vline(xintercept = mean(paired_df$delta), linewidth = 1.1) +
  labs(title = sprintf("Within-person change, matched pairs (n = %d)",
                       nrow(paired_df)),
       subtitle = sprintf("Mean change = %.2f", mean(paired_df$delta)),
       x = "Post − Pre", y = "Count")

p_paired_slope <- ggplot(paired_df,
                         aes(x = 0, xend = 1, y = score_pre, yend = score_post, colour = study)) +
  geom_segment(alpha = .5) +
  geom_point(aes(x = 0, y = score_pre),  size = 1.5) +
  geom_point(aes(x = 1, y = score_post), size = 1.5) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Pre", "Post")) +
  labs(title = "Individual slopes (matched pairs only)",
       x = NULL, y = "Score", colour = NULL)

print(p_paired_hist + p_paired_slope)


# ============================================================================
# PART F: ITEM-LEVEL BEHAVIOUR (HETEROGENEOUS RESPONSE!)
# ============================================================================
# Not every item will move in the same direction. This plot tells you
# whether a single total score is a reasonable summary, or whether you have
# differential item response.

item_stems <- str_remove(pre_items, "_pre$") |> str_remove("^trips_")

item_change <- purrr::map_dfr(item_stems, function(stem) {
  pc <- paste0("trips_", stem, "_pre")
  oc <- paste0("trips_", stem, "_post")
  tibble(
    item = stem,
    p_pre  = mean(dat[[pc]], na.rm = TRUE),
    p_post = mean(dat[[oc]], na.rm = TRUE)
  )
}) |>
  mutate(delta = p_post - p_pre,
         item = fct_reorder(item, delta))

p_items <- ggplot(item_change, aes(delta, item, fill = delta > 0)) +
  geom_col() +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = c("FALSE" = "#c0392b", "TRUE" = "#2ecc71")) +
  labs(title = "Item-level change in P(yes) from pre to post",
       subtitle = "Green = more permissive after intervention; red = less permissive",
       x = "Δ proportion 'yes' (post − pre)", y = NULL) +
  theme(legend.position = "none")
print(p_items)

# Lollipop version for the pre/post comparison
p_items_compare <- item_change |>
  pivot_longer(c(p_pre, p_post), names_to = "time", values_to = "p") |>
  mutate(time = factor(time, c("p_pre","p_post"), c("Pre","Post"))) |>
  ggplot(aes(p, item, colour = time)) +
  geom_line(aes(group = item), colour = "grey70") +
  geom_point(size = 2.5) +
  labs(title = "Pre vs post 'yes' proportion, every item",
       x = "Proportion 'yes'", y = NULL, colour = NULL)
print(p_items_compare)


# ============================================================================
# PART G: MISSINGNESS AS A POTENTIAL BIAS SOURCE
# ============================================================================
# Before trusting MAR for the mixed model, check whether baseline
# characteristics differ between "completed follow-up" vs "did not".

dat_baseline <- dat |> filter(baseline == 1) |>
  mutate(completed_followup = factor(followup,
                                     levels = c(0, 1),
                                     labels = c("Lost to follow-up",
                                                "Completed follow-up")))

# Baseline score by follow-up status
dat_baseline |>
  group_by(completed_followup) |>
  summarise(n = n(), mean = mean(score_pre), sd = sd(score_pre))

# Quick logistic: which baseline features predict completing follow-up?
m_attrit <- glm(followup ~ study + language + age + role +
                  log1p(field_experience) + log1p(centre_experience) +
                  score_pre,
                data = dat_baseline, family = binomial)
summary(m_attrit)
# If study or baseline score are strong predictors of follow-up completion,
# MAR is more plausible (attrition is explained by observed variables).
# If unobserved drivers dominate, consider a sensitivity analysis.

p_attrit <- ggplot(dat_baseline, aes(completed_followup, score_pre,
                                     fill = completed_followup)) +
  geom_boxplot(alpha = .6) +
  geom_jitter(width = .1, alpha = .4) +
  labs(title = "Baseline score by follow-up status",
       x = NULL, y = "Baseline T-TRiPS score") +
  theme(legend.position = "none")
print(p_attrit)


# ============================================================================
# PART H: FINAL CHECKLIST OF THINGS TO DISCUSS WITH PI BEFORE MODELLING
# ============================================================================
# 1. 25 vs 26 items — which is correct?
# 2. Centres A, I, J have no follow-up — drop, or retain for variance only?
# 3. field_experience = 1000 months — data-entry error?
# 4. Large language effect at baseline — include as fixed covariate (yes)
# 5. Sex has too few non-Female observations to model meaningfully — drop
# 6. Study 1 vs Study 2 show very different pre/post patterns — the time×study
#    interaction should be the primary model specification, not just a
#    secondary stratification.
# ============================================================================