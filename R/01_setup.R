# =============================================================================
# 01_setup.R
# -----------------------------------------------------------------------------
# Project : Fueling Ambition - STEM Graduate Degree Aspirations among WOC
# Purpose : Select analytic variables, filter to the WOC STEM-major sample
#           (n = 1,353), and define helper functions used in subsequent
#           descriptive and inferential scripts.
# =============================================================================

# ---- Select analytic variables and filter to WOC STEM majors ---------------
data.woc.stemmajor <- data %>%
  select(
    ACERECODE, SUBJID, YEAR,

    # Institutional context
    insttype, selectivity.c, instcont, hbcu, hsi,

    # Outcome & major
    gradegasp, major, major_tfs, major.stemsub,

    # Background / person inputs
    firstgen, parentaledu, parentalcareer, collgpa,

    # Race / ethnicity
    race.c,
    east_asian, south_asian, southeast_asian, other_asian,
    black, chicana, latina, indigenous, other,

    # CCW: aspirational capital
    GOAL19, GOAL04, GOAL20,

    # SCCT: college learning experience
    COLACT16, COLACT41, COLACT39,

    # CCW: social-navigational capital
    FAC_INTERACTION, COLLEGE_INVOLVEMENT_TFS, ACT36,

    # CCW: resistant capital
    SOCIAL_AGENCY, CIVIC_AWARENESS,

    # SD-scaled & z-scored versions
    facultyinteraction, collegeinvolvement_tfs, socialagency, civicawareness,
    GOAL19_z, GOAL04_z, GOAL20_z, ACT36_z,
    FAC_INTERACTION_z, COLLEGE_INVOLVEMENT_TFS_z,
    SOCIAL_AGENCY_z, CIVIC_AWARENESS_z,

    # Additional measures retained for sensitivity / supplementary checks
    SCIENCE_IDENTITY, SCIENCE_SELF_EFFICACY,
    SCIENCE_IDENTITY_TFS, SCIENCE_SELF_EFFICACY_TFS,
    CARCON01, CARCON02, CARCON04, CARCON05, CARCON06,
    CARCON08, CARCON09, CARCON10, CARCON11, CARCON12,
    RACEGROUP
  ) %>%
  # Restrict to Women of Color (any of the binary race indicators == 1)
  filter(east_asian == 1 | south_asian == 1 | southeast_asian == 1 | other_asian == 1 |
         black == 1 | chicana == 1 | latina == 1 | indigenous == 1 | other == 1) %>%
  # Restrict to graduating STEM majors (CSS senior-year major)
  filter(major == "STEM Major (CSS)")

# Quick check
skim(data.woc.stemmajor)


# =============================================================================
# Helper functions for descriptive / inferential tables
# =============================================================================

# Row-percentage cross-tab against gradegasp, with totals and counts
make_tabyl_table <- function(df, var_col) {
  var_col_enquo <- enquo(var_col)
  df %>%
    filter(!is.na(!!var_col_enquo), !is.na(gradegasp)) %>%
    tabyl(!!var_col_enquo, gradegasp) %>%
    adorn_totals(c("row", "col")) %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 2) %>%
    adorn_ns()
}

# Univariate distribution summary with percentage labels
dist_summary <- function(df, var_col) {
  var_col_enquo <- enquo(var_col)
  df %>%
    filter(!is.na(!!var_col_enquo)) %>%
    count(!!var_col_enquo) %>%
    mutate(
      total_n    = sum(n),
      percentage = round(100 * n / total_n, 2),
      label      = paste0(percentage, "% (", n, ")")
    ) %>%
    select(-total_n)
}

# Pearson chi-square test of a covariate against gradegasp
run_chi_test <- function(df, var_col) {
  var_col_enquo <- enquo(var_col)
  table_data <- df %>%
    filter(!is.na(!!var_col_enquo), !is.na(gradegasp)) %>%
    select(!!var_col_enquo, gradegasp)
  chisq.test(table(table_data[[1]], table_data[[2]]))
}
