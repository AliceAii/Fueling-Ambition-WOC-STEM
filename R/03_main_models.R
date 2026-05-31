# =============================================================================
# 03_main_models.R
# -----------------------------------------------------------------------------
# Project : Fueling Ambition - STEM Graduate Degree Aspirations among WOC
# Purpose : (a) ICC check via null GLMM with institution random intercept;
#           (b) Nested multinomial logistic regression models predicting
#               three-level `gradegasp`:
#                  Model 1: SCCT background + STEM subfield
#                  Model 2: + SCCT college learning experience
#                  Model 3: Full model (+ aspirational / social-navigational
#                                       / resistant capital)
#           Reports tidied odds ratios with significance stars and AIC/BIC.
# =============================================================================

# =============================================================================
# 1. ICC check - null GLMM with institution random intercept
# -----------------------------------------------------------------------------
# Singular fits indicate no meaningful institution-level clustering for any
# of the three outcome categories - justifies fixed-effect multinomial models.
# =============================================================================
data.woc.stemmajor <- data.woc.stemmajor %>%
  mutate(ACERECODE = factor(ACERECODE))

cluster_var <- "ACERECODE"

fit_null_glmm <- function(cat) {
  glmer(
    as.formula(
      paste0("I(gradegasp == '", cat, "') ~ 1 + (1 | ", cluster_var, ")")
    ),
    data   = data.woc.stemmajor,
    family = binomial(link = "logit"),
    nAGQ   = 0   # Laplace ~ fine for ICC approximation
  )
}

icc_tbl <- map_dfr(levels(data.woc.stemmajor$gradegasp), function(cat) {
  mod         <- fit_null_glmm(cat)
  var_between <- as.numeric(VarCorr(mod)[[cluster_var]][1])
  icc_val     <- var_between / (var_between + pi^2 / 3) # logistic residual variance
  tibble(category = cat, var_between = var_between, ICC = icc_val)
})

icc_tbl


# =============================================================================
# Helper: add significance-star column to a tidied multinom result
# =============================================================================
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


# =============================================================================
# 2. Model 1 - SCCT Background + STEM Major Subfield
# =============================================================================
multinom_model1 <- multinom(
  gradegasp ~
    insttype + selectivity.c + instcont + hbcu + hsi +
    east_asian + south_asian + southeast_asian +
    black + chicana + latina + indigenous + other +    # other_asian dropped (n=22)
    firstgen +
    parentaledu + parentalcareer + collgpa +
    major_tfs +
    major.stemsub,
  data = data.woc.stemmajor
)

tidy_multinom1 <- tidy(multinom_model1, conf.int = TRUE, exponentiate = TRUE) %>%
  add_signif()
view(tidy_multinom1)

AIC.1 <- AIC(multinom_model1); BIC.1 <- BIC(multinom_model1)
AIC.1; BIC.1


# =============================================================================
# 3. Model 2 - + SCCT College Learning Experience
# =============================================================================
multinom_model2 <- multinom(
  gradegasp ~
    insttype + selectivity.c + instcont + hbcu + hsi +
    east_asian + south_asian + southeast_asian +
    black + chicana + latina + indigenous + other +
    firstgen +
    parentaledu + parentalcareer + collgpa +
    major_tfs +
    major.stemsub +
    # SCCT college learning experience
    COLACT16 + COLACT41 + COLACT39,
  data = data.woc.stemmajor
)

tidy_multinom2 <- tidy(multinom_model2, conf.int = TRUE, exponentiate = TRUE) %>%
  add_signif()
view(tidy_multinom2)

AIC.2 <- AIC(multinom_model2); BIC.2 <- BIC(multinom_model2)
AIC.2; BIC.2


# =============================================================================
# 4. Model 3 - Full model (+ CCW aspirational / social-nav / resistant)
# =============================================================================
multinom_model3 <- multinom(
  gradegasp ~
    insttype + selectivity.c + instcont + hbcu + hsi +
    east_asian + south_asian + southeast_asian +
    black + chicana + latina + indigenous + other +
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
    SOCIAL_AGENCY_z + CIVIC_AWARENESS_z,
  data = data.woc.stemmajor
)
nrow(model.frame(multinom_model3))

tidy_multinom3 <- tidy(multinom_model3, conf.int = TRUE, exponentiate = TRUE) %>%
  add_signif()
view(tidy_multinom3)

AIC.3 <- AIC(multinom_model3); BIC.3 <- BIC(multinom_model3)
AIC.3; BIC.3


# =============================================================================
# 5. Diagnostic check - UGR effect before faculty mentorship enters the model
# -----------------------------------------------------------------------------
# Before FAC_INTERACTION is added, undergraduate research (COLACT16) is
# significantly positive for both Non-STEM (OR ~ 2.22) and STEM (OR ~ 1.37)
# aspiration relative to no graduate aspiration.
# =============================================================================
multinom_test <- multinom(
  gradegasp ~
    insttype + selectivity.c + instcont + hbcu +
    east_asian + south_asian + southeast_asian +
    black + chicana + latina + indigenous + other +
    firstgen +
    parentaledu + parentalcareer + collgpa +
    major_tfs + major.stemsub +
    # aspirational capital
    GOAL19 + GOAL04 + GOAL20 +
    # SCCT college learning experience
    COLACT16,
  data = data.woc.stemmajor
)

tidy_multinom_test <- tidy(multinom_test, conf.int = TRUE, exponentiate = TRUE) %>%
  add_signif()
view(tidy_multinom_test)
