# expfamily: randomized experiments for jamovi

Menu: Experiments

| Analysis | What it does |
|---|---|
| **Treatment Effects** | Difference in means, covariate adjustment, Lin (2013) interacted adjustment, block fixed effects, several arms; HC0 to HC3, CR1 and CR2 (Satterthwaite df) standard errors |
| **Randomization Inference** | Fisher tests of the sharp null: exact enumeration or simulation, blocked and clustered assignment, difference in means / rank / t statistics, interval by test inversion |
| **Multiple Outcomes** | Adjusted p-values: Bonferroni, Holm, Sidak, Hochberg, BH, BY, Westfall-Young step-down, Romano-Wolf |
| **Power and Sample Size** | Power, minimum detectable effect, sample size; individual and cluster designs, covariate R2, unequal cluster sizes, values from the data |
| **Bounds under Attrition** | Lee (2009) trimming bounds for the effect when outcomes are missing and attrition differs by group; optional tightening by a categorical covariate; bootstrap standard errors and Imbens-Manski interval |
| **Subgroup Effects** | Treatment effect in the subgroups of categorical variables, Wald interaction tests (does the effect differ?), Holm / Bonferroni / Benjamini-Hochberg adjusted p-values, forest plot |
| **Factorial Design** | Two crossed treatments: cell means, main effects, simple effects, interaction contrasts and their joint test, effects ignoring the other factor |
| **Worst-Case Bounds** | Manski bounds on the average effect: random assignment with missing outcomes, or non-random selection with monotone treatment response and monotone treatment selection; Imbens-Manski intervals |
| **Balance and Attrition** | Standardized differences, joint F and permutation test, differential attrition |

All engines are in base R (`R/engine_*.R`). `tests/reference.R` checks them against `estimatr` (all standard errors, Lin adjustment, CR2), `ri2` and `randomizr` (exact and simulated randomization inference), `pwr` (power), `p.adjust`, `lm`, `t.test` and `anova`. It needs those packages in a library named by `EXP_REFLIB` (or the default library). `tests/manski.R` checks the worst-case bounds against extreme imputation of the unobserved outcomes, the results of Manski and Pepper (2000) and simulations. `tests/subfact.R` checks subgroup effects and factorial contrasts against `estimatr` (lm_robust, lh_robust; HC1-HC3, classical, CR1) and the Wald F tests against `lmtest::waldtest` with `sandwich` (to rounding error). `tests/lee.R` checks the Lee bounds (hand-computed example, tightening, coverage in simulations; Stata `leebounds` in `stata_validation/16_leebounds.do`). `tests/smoke.R` runs every analysis class headless, also in jamovi's own R.

Example data (`data/`): `sim_trial.csv` (600 people, 3 arms, 6 blocks, 4 outcomes, differential attrition), `sim_cluster_trial.csv` (40 schools) and `sim_small_trial.csv` (12 units, 924 randomizations). The generator is `dev/make_data.R`.

## Ribbon icon

`icon/analysis-exp.svg` (74 x 44): units split at random into a control (blue) and a treated (orange) group and the difference of the group means with its interval. `dev/icon_preview.ps1` adds one CSS rule for the menu group Experiments to jamovi's client stylesheet (administrator PowerShell, restart jamovi afterwards).
