# =============================================================================
# 04_interaction_models.R
# -----------------------------------------------------------------------------
# Project : Fueling Ambition - STEM Graduate Degree Aspirations among WOC
# Purpose : Multinomial logistic regression models with interaction terms
#           addressing RQ2 - whether the relationships between graduate-degree
#           aspirations and CCW capital vary by race/ethnicity:
#             Model 2_1: Race x Aspiration to contribute to science (GOAL19)
#             Model 2_2: Race x Faculty mentorship (FAC_INTERACTION)
#             Model 2_3: Race x Social agency (SOCIAL_AGENCY)
#             Model 2_4a: Bio-major x UGR program (COLACT16)
#             Model 2_4b: Bio-major x STEM-career campus program (COLACT41)
#           Also tests overall race-group differences.
# Assumes : `data.woc.stemmajor` from 01_setup.R and `add_signif()` from
#           03_main_models.R are loaded in the workspace.
# =============================================================================

# Re-define add_signif() here so this script can be run standalone if needed
add_signif <- function(tidy_df) {
  tidy_df %>%
    mutate(Signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    ))
}

# Indicator used in models 2_4a / 2_4b
data.woc.stemmajor$biomajor <- ifelse(
  data.woc.stemmajor$major.stemsub == "Biological & Life Sciences", 1, 0
)


# =============================================================================
# Model 2_1 - Race x Aspirational Capital (GOAL19: contribute to science)
# -----------------------------------------------------------------------------
# Indigenous and Other dropped due to small cell sizes in interaction terms.
# =============================================================================
multinom_model2_1 <- multinom(
  gradegasp ~
    insttype + selectivity.c + instcont + hbcu +
    east_asian + south_asian + southeast_asian +
    black + chicana + latina +
    firstgen +
    parentaledu + parentalcareer + collgpa +
    major_tfs +
    major.stemsub +
    # SCCT college learning experience
    COLACT16 + COLACT41 + COLACT39 +
    # CCW: aspirational capital
    GOAL19_z + GOAL04_z + GOAL20_z +
    # CCW: social-navigational capital
    FAC_INTERACTION_z + COLLEGE_INVOLVEMENT_TFS_z + ACT36_z +
    # CCW: resistant capital
    SOCIAL_AGENCY_z + CIVIC_AWARENESS_z +
    # Race x aspirational capital interactions
    east_asian:GOAL19_z + south_asian:GOAL19_z + southeast_asian:GOAL19_z +
    black:GOAL19_z + chicana:GOAL19_z + latina:GOAL19_z,
  data = data.woc.stemmajor
)

tidy_multinom2_1 <- tidy(multinom_model2_1, conf.int = TRUE, exponentiate = TRUE) %>%
  add_signif()
view(tidy_multinom2_1)


# =============================================================================
# Model 2_2 - Race x Faculty Mentorship (FAC_INTERACTION)
# =============================================================================
multinom_model2_2 <- multinom(
  gradegasp ~
    insttype + selectivity.c + instcont + hbcu +
    east_asian + south_asian + southeast_asian +
    black + chicana + latina + indigenous + other +
    firstgen +
    parentaledu + parentalcareer + collgpa +
    major_tfs +
    major.stemsub +
    COLACT16 + COLACT41 + COLACT39 +
    GOAL19_z + GOAL04_z + GOAL20_z +
    FAC_INTERACTION_z + COLLEGE_INVOLVEMENT_TFS_z + ACT36_z +
    SOCIAL_AGENCY_z + CIVIC_AWARENESS_z +
    # Race x faculty mentorship interactions
    east_asian:FAC_INTERACTION_z + south_asian:FAC_INTERACTION_z +
    southeast_asian:FAC_INTERACTION_z + black:FAC_INTERACTION_z +
    chicana:FAC_INTERACTION_z + latina:FAC_INTERACTION_z +
    indigenous:FAC_INTERACTION_z + other:FAC_INTERACTION_z,
  data = data.woc.stemmajor
)
nrow(model.frame(multinom_model2_2))

tidy_multinom2_2 <- tidy(multinom_model2_2, conf.int = TRUE, exponentiate = TRUE) %>%
  add_signif()
view(tidy_multinom2_2)


# =============================================================================
# Model 2_3 - Race x Social Agency (SOCIAL_AGENCY)
# =============================================================================
multinom_model2_3 <- multinom(
  gradegasp ~
    insttype + selectivity.c + instcont + hbcu +
    east_asian + south_asian + southeast_asian +
    black + chicana + latina +
    firstgen +
    parentaledu + parentalcareer + collgpa +
    major_tfs +
    major.stemsub +
    COLACT16 + COLACT41 + COLACT39 +
    GOAL19_z + GOAL04_z + GOAL20_z +
    FAC_INTERACTION_z + COLLEGE_INVOLVEMENT_TFS_z + ACT36_z +
    SOCIAL_AGENCY_z + CIVIC_AWARENESS_z +
    # Race x social agency interactions
    east_asian:SOCIAL_AGENCY_z + south_asian:SOCIAL_AGENCY_z +
    southeast_asian:SOCIAL_AGENCY_z + black:SOCIAL_AGENCY_z +
    chicana:SOCIAL_AGENCY_z + latina:SOCIAL_AGENCY_z,
  data = data.woc.stemmajor
)

tidy_multinom2_3 <- tidy(multinom_model2_3, conf.int = TRUE, exponentiate = TRUE) %>%
  add_signif()
view(tidy_multinom2_3)

AIC.4 <- AIC(multinom_model2_3); BIC.4 <- BIC(multinom_model2_3)
AIC.4; BIC.4


# =============================================================================
# Model 2_4a - Bio-Major x Undergraduate Research (COLACT16)
# -----------------------------------------------------------------------------
# No significant interaction terms.
# =============================================================================
multinom_model2_4a <- multinom(
  gradegasp ~
    insttype + selectivity.c + instcont + hbcu +
    east_asian + south_asian + southeast_asian +
    black + chicana + latina +
    firstgen +
    parentaledu + parentalcareer + collgpa +
    major_tfs +
    major.stemsub +
    COLACT16 + COLACT41 + COLACT39 +
    GOAL19_z + GOAL04_z + GOAL20_z +
    FAC_INTERACTION_z + COLLEGE_INVOLVEMENT_TFS_z + ACT36_z +
    SOCIAL_AGENCY_z + CIVIC_AWARENESS_z +
    # Bio-major x undergraduate research
    biomajor:COLACT16,
  data = data.woc.stemmajor
)

tidy_multinom2_4a <- tidy(multinom_model2_4a, conf.int = TRUE, exponentiate = TRUE) %>%
  add_signif()
View(tidy_multinom2_4a)


# =============================================================================
# Model 2_4b - Bio-Major x STEM-Career Campus Program (COLACT41)
# -----------------------------------------------------------------------------
# Southeast Asian:COLACT41 is the only significant interaction (non-STEM grad).
# =============================================================================
multinom_model2_4b <- multinom(
  gradegasp ~
    insttype + selectivity.c + instcont + hbcu +
    east_asian + south_asian + southeast_asian +
    black + chicana + latina +
    firstgen +
    parentaledu + parentalcareer + collgpa +
    major_tfs + major.stemsub +
    COLACT16 + COLACT41 + COLACT39 +
    GOAL19_z + GOAL04_z + GOAL20_z +
    FAC_INTERACTION_z + COLLEGE_INVOLVEMENT_TFS_z + ACT36_z +
    SOCIAL_AGENCY_z + CIVIC_AWARENESS_z +
    # Bio-major x STEM-career campus program
    biomajor:COLACT41,
  data = data.woc.stemmajor
)

# Inspect cell sizes and multicollinearity
table(data.woc.stemmajor$major.stemsub,
      data.woc.stemmajor$COLACT41,
      data.woc.stemmajor$gradegasp)
vif(multinom_model2_4b)

tidy_multinom2_4b <- tidy(multinom_model2_4b, conf.int = TRUE, exponentiate = TRUE) %>%
  add_signif()
View(tidy_multinom2_4b)


# =============================================================================
# Race-only model - overall race-group differences (no significant effects)
# =============================================================================
multinom_race <- multinom(
  gradegasp ~
    east_asian + south_asian + southeast_asian +
    black + chicana + latina + indigenous + other,
  data = data.woc.stemmajor
)
nrow(model.frame(multinom_race))

tidy_multinom_race <- tidy(multinom_race, conf.int = TRUE, exponentiate = TRUE) %>%
  add_signif()
# view(tidy_multinom_race)
