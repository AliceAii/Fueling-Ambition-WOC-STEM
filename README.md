# Fueling Ambition: Graduate Degree Aspirations Among Women of Color in STEM
Using HERI TFS/CSS data, this study examines how Community Cultural Wealth shapes STEM graduate aspirations among 1,353 senior Women of Color in STEM. Multinomial logistic regression finds faculty mentorship and college involvement boost STEM graduate aspirations, while social-change commitment is associated with lower STEM aspirations.

## Overview

This repository contains the R code used in the paper *"Fueling Ambition: Graduate Degree Aspirations Among Women of Color in STEM,"* published in *CBE—Life Sciences Education*.

Drawing on Community Cultural Wealth (CCW) and Social Cognitive Career Theory (SCCT), this study examines how aspirational, social-navigational, and resistant capital predict STEM graduate degree aspirations among undergraduate Women of Color (WOC) majoring in STEM, and whether these relationships vary by racial/ethnic identity. The analysis uses multinomial logistic regression with nested main-effect and interaction-effect specifications.

## Research Questions

1. **Predictors of Graduate Degree Aspirations:** Controlling for relevant background characteristics, do measures of aspirational, social-navigational, and resistant capital predict intentions to pursue STEM graduate degrees among Women of Color (WOC) in undergraduate STEM majors?
2. **Moderating Role of Race:** Do the relationships between STEM graduate degree aspirations and WOC's aspirational, social-navigational, and resistant capital vary by racial/ethnic identity?

## Data

This study uses restricted-use data from the [Higher Education Research Institute (HERI)](https://heri.ucla.edu/) at UCLA, drawing on two linked surveys:

- **The Freshman Survey (TFS)** administered in 2018–2020 (entry to college)
- **The College Senior Survey (CSS)** administered in 2022–2024 (senior year)

The data are not included in this repository due to licensing restrictions. Researchers can apply for access through HERI.

The analytic sample includes **1,353 senior Women of Color** graduating with a STEM major who responded to both surveys. Grounded in QuantCrit's principle that racial categories are neither natural nor static (Gillborn et al., 2018), race/ethnicity is disaggregated as granularly as possible using binary indicators. The sample includes 24% East Asian, 9% South Asian, 18% Southeast Asian, 1.6% Other Asian, 19% Black, 20% Chicana, 11% Latina, 4% Indigenous, and 5% Other.

## Repository Structure

```
├── README.md
├── R/
│   ├── 00_data_preparation.R      # Read raw HERI TFS/CSS files, derive DV (gradegasp), recode covariates, construct race indicators
│   ├── 01_setup.R                  # Select analytic variables, filter to WOC STEM majors, helper functions
│   ├── 02_descriptive_analysis.R   # Descriptive statistics, cross-tabulations, chi-square tests
│   ├── 03_main_models.R            # ICC check and nested multinomial logistic regression models (Models 1–3)
│   └── 04_interaction_models.R     # Interaction-effects models (race × capital, race × college experience)
└── data/                           # Place HERI CSS/TFS .sav files here (not included)
```

## How to Reproduce

1. Obtain restricted-use HERI TFS and CSS data and place the raw `.sav` file (`CSS2022_2023_2024 for SHUHAN AI.sav`) and the `acehsi.csv` crosswalk in the `data/` folder.
2. Run `00_data_preparation.R` first to load packages, read raw data, derive the dependent variable (`gradegasp`), and construct race/ethnicity indicators and other covariates.
3. Run the remaining scripts in order (`01_setup.R` → `04_interaction_models.R`). Each script assumes objects created in the preceding steps are loaded in the workspace.
4. Output tables and model summaries print to the console; tidied results can be exported via `openxlsx` to `299CGradDegreAsp_Results.xlsx`.

## Analytic Approach

| Step | Script | Description |
|------|--------|-------------|
| **Data Preparation** | `00_data_preparation.R` | Load packages; read HERI CSS `.sav` data and ACE–HSI crosswalk; derive the three-level dependent variable `gradegasp` (No / Non-STEM / STEM grad aspiration); construct binary and categorical race/ethnicity variables; recode background, institutional, and college-experience covariates; create z-scored and SD-scaled CCW measures |
| **Setup** | `01_setup.R` | Select analytic variables; filter to Women of Color graduating with a STEM major (n = 1,353); define helper functions (`make_tabyl_table`, `dist_summary`, `run_chi_test`) for descriptive and inferential tables |
| **Descriptive Analysis** | `02_descriptive_analysis.R` | Generate Table 1 by graduate-degree aspiration group; compute race distribution; produce two-way cross-tabulations for parental education, parental STEM career, and STEM major subfield; run Pearson chi-square tests for categorical covariates |
| **Main Effects Models** | `03_main_models.R` | Check ICC via null GLMM (singular fit confirms no meaningful clustering at the institution level); estimate nested multinomial logistic regression models — Model 1 (SCCT background + STEM subfield), Model 2 (+ SCCT college learning experience), Model 3 (full model with aspirational, social-navigational, and resistant capital); report AIC/BIC |
| **Interaction Models** | `04_interaction_models.R` | Estimate multinomial logistic regressions with race-by-capital interactions: race:aspirational capital (`GOAL19`), race:faculty mentorship (`FAC_INTERACTION`), race:social agency (`SOCIAL_AGENCY`); also test biology-major:undergraduate research (`COLACT16`) and biology-major:STEM-career campus program (`COLACT41`); test overall race-group differences |

## Key Variables

- **Dependent Variable** — `gradegasp`: Graduate degree aspiration with three levels: *No Grad Degree Aspiration* (ref), *Non-STEM Grad Degree Aspiration*, *STEM Grad Degree Aspiration*.
- **Institutional Context** — `insttype` (University vs. 4-year College), `selectivity.c` (avg. SAT/100), `instcont` (Private vs. Public), `hbcu`, `hsi`.
- **SCCT Person Inputs & Background** — Binary race indicators (`east_asian`, `south_asian`, `southeast_asian`, `other_asian`, `black`, `chicana`, `latina`, `indigenous`, `other`), `parentaledu`, `parentalcareer`, `collgpa`, `major_tfs`, `major.stemsub`.
- **SCCT College Learning Experience** — `COLACT16` (undergraduate research), `COLACT41` (STEM-career campus program), `COLACT39` (pre-professional/departmental club).
- **CCW Aspirational Capital** — `GOAL19` (contribute to science), `GOAL04` (authority in field), `GOAL20` (peer recognition).
- **CCW Social-Navigational Capital** — `FAC_INTERACTION` (faculty mentorship), `COLLEGE_INVOLVEMENT_TFS` (college involvement), `ACT36` (advisor/counselor meetings).
- **CCW Resistant Capital** — `SOCIAL_AGENCY`, `CIVIC_AWARENESS`.

## Software & Packages

All analyses were conducted in **R**. Key packages include:

- **Modeling:** `nnet` (multinomial logistic regression), `lme4` (null GLMM for ICC)
- **Diagnostics:** `car` (VIF), `performance`, `lmtest`
- **Data import & wrangling:** `haven`, `tidyverse`, `tidyr`, `dplyr`, `purrr`, `rlang`, `janitor`
- **Descriptives & tables:** `skimr`, `table1`, `flextable`, `broom`
- **Output:** `openxlsx`, `ggplot2`

## Citation

> Ai, S., Eagan, M. K., Jr., & Wu, J. (2026). Fueling ambition: Graduate degree aspirations among Women of Color in STEM. *CBE—Life Sciences Education*. https://doi.org/10.1187/cbe.25-10-0229

## License

This repository is provided for academic reproducibility purposes. The code is available under the [MIT License](https://opensource.org/licenses/MIT). The HERI TFS/CSS data are subject to HERI restricted-use data license agreements.
