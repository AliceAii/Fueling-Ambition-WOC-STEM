# =============================================================================
# 02_descriptive_analysis.R
# -----------------------------------------------------------------------------
# Project : Fueling Ambition - STEM Graduate Degree Aspirations among WOC
# Purpose : Descriptive statistics (Table 1), race distribution, two-way
#           cross-tabulations, and chi-square tests of categorical covariates
#           against the three-level graduate-degree aspiration outcome.
# Assumes : `data.woc.stemmajor` and helper functions from 01_setup.R are
#           loaded in the workspace.
# =============================================================================

# =============================================================================
# 1. Table 1 - column percentages by graduate-degree aspiration
# =============================================================================
descr_gradegasp <- table1(
  ~ as.factor(east_asian) + as.factor(south_asian) + as.factor(southeast_asian) +
    as.factor(other_asian) + as.factor(black) + as.factor(chicana) +
    as.factor(latina) + as.factor(indigenous) + as.factor(other) +
    parentaledu + parentalcareer + major.stemsub + collgpa |
    as.factor(gradegasp),
  data = data.woc.stemmajor
)
descr_gradegasp

# 25.7% (n = 348) WOC identify as multiracial group (RACEGROUP == 7)
data.woc.stemmajor %>%
  summarise(
    n_multiracial   = sum(RACEGROUP == 7, na.rm = TRUE),
    pct_multiracial = mean(RACEGROUP == 7, na.rm = TRUE) * 100
  )


# =============================================================================
# 2. Race distribution across the WOC STEM sample
# =============================================================================
total_n <- nrow(data.woc.stemmajor)

data.woc.stemmajor %>%
  summarise(
    east_asian      = sum(east_asian, na.rm = TRUE),
    south_asian     = sum(south_asian, na.rm = TRUE),
    southeast_asian = sum(southeast_asian, na.rm = TRUE),
    other_asian     = sum(other_asian, na.rm = TRUE),
    black           = sum(black, na.rm = TRUE),
    chicana         = sum(chicana, na.rm = TRUE),
    latina          = sum(latina, na.rm = TRUE),
    indigenous      = sum(indigenous, na.rm = TRUE),
    other           = sum(other, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "race_group", values_to = "n") %>%
  mutate(
    percent = round(100 * n / total_n, 2),
    label   = paste0(n, " (", percent, "%)")
  )


# =============================================================================
# 3. Two-way cross-tabulations vs. graduate-degree aspiration
# =============================================================================

# ---- Parental education ----------------------------------------------------
parentaledu_gradegasp <- make_tabyl_table(data.woc.stemmajor, parentaledu)
print(as_flextable(parentaledu_gradegasp, preview = "docx"))

parentaledu_dist <- dist_summary(data.woc.stemmajor, parentaledu)
print(as_flextable(parentaledu_dist, preview = "docx"))

# ---- Parental STEM career --------------------------------------------------
parentalcareer_gradegasp <- make_tabyl_table(data.woc.stemmajor, parentalcareer)
print(as_flextable(parentalcareer_gradegasp, preview = "docx"))

parentalcareer_dist <- dist_summary(data.woc.stemmajor, parentalcareer)
print(as_flextable(parentalcareer_dist, preview = "docx"))

# ---- STEM major subfield ---------------------------------------------------
stemsub_gradegasp <- make_tabyl_table(data.woc.stemmajor, major.stemsub)
print(as_flextable(stemsub_gradegasp, preview = "docx"))

stemsub_dist <- dist_summary(data.woc.stemmajor, major.stemsub)
print(as_flextable(stemsub_dist, preview = "docx"))


# =============================================================================
# 4. Chi-square tests of categorical covariates vs. gradegasp
# =============================================================================

# Background covariates
chi_parentaledu    <- run_chi_test(data.woc.stemmajor, parentaledu)
chi_parentalcareer <- run_chi_test(data.woc.stemmajor, parentalcareer)
chi_stemsub        <- run_chi_test(data.woc.stemmajor, major.stemsub)

# Race - categorical
chi_race <- run_chi_test(data.woc.stemmajor, race.c)

# Race - each binary indicator tested independently
chi_east_asian      <- run_chi_test(data.woc.stemmajor, east_asian)
chi_south_asian     <- run_chi_test(data.woc.stemmajor, south_asian)
chi_southeast_asian <- run_chi_test(data.woc.stemmajor, southeast_asian)
chi_other_asian     <- run_chi_test(data.woc.stemmajor, other_asian)
chi_black           <- run_chi_test(data.woc.stemmajor, black)
chi_chicana         <- run_chi_test(data.woc.stemmajor, chicana)
chi_latina          <- run_chi_test(data.woc.stemmajor, latina)
chi_indigenous      <- run_chi_test(data.woc.stemmajor, indigenous)
chi_other           <- run_chi_test(data.woc.stemmajor, other)

# Bundle results for inspection
list(
  parental_edu    = chi_parentaledu,
  parental_career = chi_parentalcareer,
  stem_subfield   = chi_stemsub,
  race_overall    = chi_race,
  race_binary     = list(
    east_asian      = chi_east_asian,
    south_asian     = chi_south_asian,
    southeast_asian = chi_southeast_asian,
    other_asian     = chi_other_asian,
    black           = chi_black,
    chicana         = chi_chicana,
    latina          = chi_latina,
    indigenous      = chi_indigenous,
    other           = chi_other
  )
)
