# =============================================================================
# 00_data_preparation.R
# -----------------------------------------------------------------------------
# Project : Fueling Ambition - STEM Graduate Degree Aspirations among WOC
# Purpose : Load raw HERI TFS/CSS data, derive the dependent variable
#           (gradegasp), construct race/ethnicity indicators, recode
#           background, institutional, and college-experience covariates,
#           and create z-scored / SD-scaled CCW measures.
# =============================================================================

rm(list = ls())

# ---- Packages --------------------------------------------------------------
library(skimr)
library(tidyverse)
library(haven)      # read .sav files (SPSS)
library(tidyr)      # data manipulation and reshaping
library(janitor)    # cleaning and tabulating data
library(ggplot2)
library(flextable)  # table presentation
library(openxlsx)   # save results to .xlsx
library(broom)      # tidy model output
library(nnet)       # multinomial logistic regression
library(car)        # multicollinearity (VIF)
library(table1)
library(rlang)
library(dplyr)
library(lme4)       # GLMM (for ICC check)
library(purrr)
library(performance)
library(lmtest)

# ---- Working directory / Read data -----------------------------------------
# Place the restricted-use HERI .sav file and ACE-HSI crosswalk in data/
setwd("/Users/aishuhan/Desktop/Fueling Ambition 299/")

data <- read_sav("Data and Code/CSS2022_2023_2024 for SHUHAN AI.sav")
hsi  <- read_csv("acehsi.csv")

# Merge the ACE-HSI indicator onto the main data by institutional ACE code
data <- data %>% left_join(hsi, by = "ACERECODE")


# =============================================================================
# 1. Dependent variable & STEM major coding
# =============================================================================
data <- data %>%
  mutate(
    # ---- DV: Graduate-degree aspiration (3 levels) -------------------------
    gradegasp = case_when(
      GRADMAJOR %in% c(112:123, 226, 442, 444:772, 876, 988)              ~ 2, # STEM Grad Aspiration
      GRADMAJOR %in% c(1:11, 224:225, 227:341, 873:875, 877:987, 989:990) ~ 1, # Non-STEM Grad Aspiration
      is.na(GRADMAJOR)                                                    ~ 0  # No Grad Aspiration
    ),
    gradegasp = factor(
      gradegasp,
      levels = c(0, 1, 2),
      labels = c("No Grad Degree Aspiration",
                 "Non-STEM Grad Degree Aspiration",
                 "STEM Grad Degree Aspiration")
    ),

    # ---- CSS senior-year major ---------------------------------------------
    major = case_when(
      MAJOR1 %in% c(112:123, 226, 442, 444:772, 876, 988)              ~ 1,
      MAJOR1 %in% c(1:11, 224:225, 227:341, 873:874, 877:987, 989:990) ~ 0,
      is.na(MAJOR1)                                                    ~ NA_real_
    ),

    # ---- TFS freshman-year major -------------------------------------------
    major_tfs = case_when(
      MAJOR_TFS %in% c(112:123, 226, 442, 444:772, 876, 988)              ~ 1,
      MAJOR_TFS %in% c(1:11, 224:225, 227:341, 873:874, 877:987, 989:990) ~ 0,
      is.na(MAJOR_TFS)                                                    ~ NA_real_
    ),

    major     = factor(major,     levels = c(0, 1), labels = c("Non-STEM Major (CSS)", "STEM Major (CSS)")),
    major_tfs = factor(major_tfs, levels = c(0, 1), labels = c("Non-STEM Major (TFS)", "STEM Major (TFS)")),

    # ---- STEM subfield ------------------------------------------------------
    major.stemsub = case_when(
      MAJOR1 %in% 112:123 ~ 0, # Biological & Life Sciences
      MAJOR1 %in% 663:665 ~ 1, # Math & Computer Science
      MAJOR1 %in% 442:454 ~ 2, # Engineering
      MAJOR1 %in% 766:772 ~ 3, # Physical Science
      MAJOR1 %in% 555:562 ~ 4, # Health Professions
      TRUE                ~ NA_real_
    ),
    major.stemsub = factor(
      major.stemsub,
      levels = c(0, 1, 2, 3, 4),
      labels = c("Biological & Life Sciences", "Math & Computer Science",
                 "Engineering", "Physical Science", "Health Professions")
    )
  )


# =============================================================================
# 2. Race / ethnicity construction
# =============================================================================

# ---- Binary indicators (one per group; not mutually exclusive) -------------
data <- data %>%
  mutate(
    east_asian      = if_else(RACE10 == 2, 1, 0),
    south_asian     = if_else(RACE13 == 2, 1, 0),
    southeast_asian = if_else(RACE11 == 2 | RACE12 == 2, 1, 0),
    other_asian     = if_else(RACE14 == 2, 1, 0),
    black           = if_else(RACE2  == 2, 1, 0),
    chicana         = if_else(RACE6  == 2, 1, 0),
    latina          = if_else(RACE7  == 2 | RACE8  == 2 | RACE15 == 2 | RACE16 == 2, 1, 0),
    indigenous      = if_else(RACE3  == 2 | RACE5  == 2, 1, 0),
    other           = if_else(RACE9  == 2, 1, 0)
  )

# ---- Categorical race variable (mutually exclusive labels) -----------------
data <- data %>%
  mutate(race.c = case_when(
    # White
    RACEGROUP == 5 & RACE1 == 2 ~ "White",

    # Black (purely Black or White-Black)
    (RACEGROUP == 3 & RACE2 == 2) |
      (RACEGROUP == 7 & RACE1 == 2 & RACE2 == 2) ~ "Black",

    # Indigenous (American Indian/Alaska Native or NHPI; pure and White-mixed)
    (RACE3 == 2 | RACE5 == 2) |
      (RACEGROUP == 7 & RACE1 == 2 & RACE3 == 2) |
      (RACEGROUP == 7 & RACE1 == 2 & RACE5 == 2) ~ "Indigenous",

    # East Asian
    (RACEGROUP == 2 & RACE10 == 2) |
      (RACEGROUP == 7 & RACE1 == 2 & RACE10 == 2) ~ "East Asian",

    # Southeast Asian
    (RACEGROUP == 2 & RACE11 == 2) |
      (RACEGROUP == 2 & RACE12 == 2) |
      (RACEGROUP == 7 & RACE1 == 2 & RACE11 == 2) |
      (RACEGROUP == 7 & RACE1 == 2 & RACE12 == 2) ~ "Southeast Asian",

    # South Asian
    (RACEGROUP == 2 & RACE13 == 2) |
      (RACEGROUP == 7 & RACE1 == 2 & RACE13 == 2) ~ "South Asian",

    # Chicana/o/e/x
    (RACEGROUP == 4 & RACE6 == 2) |
      (RACEGROUP == 7 & RACE1 == 2 & RACE6 == 2) ~ "Chicana/o/e/x",

    # Latina/o/e/x
    (RACEGROUP == 4 & RACE7  == 2) |
      (RACEGROUP == 4 & RACE15 == 2) |
      (RACEGROUP == 4 & RACE16 == 2) |
      (RACEGROUP == 4 & RACE8  == 2) |
      (RACEGROUP == 7 & RACE1 == 2 & RACE7  == 2) |
      (RACEGROUP == 7 & RACE1 == 2 & RACE15 == 2) |
      (RACEGROUP == 7 & RACE1 == 2 & RACE16 == 2) |
      (RACEGROUP == 7 & RACE1 == 2 & RACE8  == 2) ~ "Latina/o/e/x",

    # Other Asian / Other
    RACE14 == 2 ~ "Other Asian",
    RACE9  == 2 ~ "Other",

    TRUE ~ NA_character_
  ))


# =============================================================================
# 3. Background, institutional, and college-experience covariates
# =============================================================================
data <- data %>%
  mutate(
    # ---- First-generation status -------------------------------------------
    firstgen = case_when(
      FIRSTGEN_TFS == 2 ~ 1,
      FIRSTGEN_TFS == 1 ~ 0,
      is.na(FIRSTGEN_TFS) ~ NA_real_
    ),
    firstgen = factor(firstgen, levels = c(0, 1), labels = c("Non First-gen", "First-gen")),

    # ---- Parental education -------------------------------------------------
    parentaledu = case_when(
      (is.na(PAREDUC1_TFS) & is.na(PAREDUC2_TFS)) | (PAREDUC1_TFS == 9 & PAREDUC2_TFS == 9) ~ NA_real_,
      PAREDUC1_TFS %in% 1:4 | PAREDUC2_TFS %in% 1:4 ~ 0, # Less than college
      PAREDUC1_TFS %in% 5:6 | PAREDUC2_TFS %in% 5:6 ~ 1, # College
      PAREDUC1_TFS %in% 7:8 | PAREDUC2_TFS %in% 7:8 ~ 2  # Graduate
    ),
    parentaledu = factor(parentaledu, levels = c(0, 1, 2),
                         labels = c("Less than college", "College", "Graduate")),

    # ---- Parental STEM career ----------------------------------------------
    parentalcareer = case_when(
      is.na(PCAREER1_TFS) & is.na(PCAREER2_TFS) ~ NA_real_,
      PCAREER1_TFS %in% c(7, 29, 41, 43, 44, 49, 50, 51, 52, 55, 56, 72, 81) |
      PCAREER2_TFS %in% c(7, 29, 41, 43, 44, 49, 50, 51, 52, 55, 56, 72, 81) ~ 1,
      TRUE ~ 0
    ),
    parentalcareer = factor(parentalcareer, levels = c(0, 1),
                            labels = c("No one works in STEM", "One of them works in STEM")),

    # ---- College GPA (recoded to 0.5-4.0 numeric) --------------------------
    collgpa = case_when(
      COLLGPA == 1 ~ 0.5, COLLGPA == 2 ~ 1.0, COLLGPA == 3 ~ 1.5, COLLGPA == 4 ~ 2.0,
      COLLGPA == 5 ~ 2.5, COLLGPA == 6 ~ 3.0, COLLGPA == 7 ~ 3.5, COLLGPA == 8 ~ 4.0,
      is.na(COLLGPA) ~ NA_real_
    ),

    # ---- Institutional context ---------------------------------------------
    selectivity.c = as.numeric(SELECTIVITY) / 100,

    insttype = case_when(INSTTYPE == 1 ~ 1, INSTTYPE == 2 ~ 0, is.na(INSTTYPE) ~ NA_real_),
    insttype = factor(insttype, levels = c(0, 1), labels = c("4-year College", "University")),

    instcont = case_when(INSTCONT == 2 ~ 1, INSTCONT == 1 ~ 0, is.na(INSTCONT) ~ NA_real_),
    instcont = factor(instcont, levels = c(0, 1), labels = c("Public", "Private")),

    hbcu = case_when(HBCU == 2 ~ 1, HBCU == 1 ~ 0, is.na(HBCU) ~ NA_real_),
    hbcu = factor(hbcu, levels = c(0, 1), labels = c("Non HBCU", "HBCU")),

    hsi = factor(HSI, levels = c(0, 1), labels = c("Non HSI", "HSI"))
  )


# =============================================================================
# 4. SCCT college-experience & CCW standardization
# =============================================================================
data <- data %>%
  mutate(
    # ---- SD-scaled CCW scales (divide by 10) -------------------------------
    facultyinteraction     = FAC_INTERACTION / 10,
    collegeinvolvement_tfs = COLLEGE_INVOLVEMENT_TFS / 10,
    socialagency           = SOCIAL_AGENCY / 10,
    civicawareness         = CIVIC_AWARENESS / 10,

    # ---- Z-scored CCW / aspirational / advisor items -----------------------
    across(
      .cols = c(GOAL19, GOAL04, GOAL20, ACT36,
                FAC_INTERACTION, COLLEGE_INVOLVEMENT_TFS,
                SOCIAL_AGENCY, CIVIC_AWARENESS),
      .fns   = ~ as.numeric(scale(.x)),
      .names = "{.col}_z"
    )
  )

# `data` is now ready for variable selection / sample filtering (see 01_setup.R)
