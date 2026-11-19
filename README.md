# expfamily — randomized experiments in jamovi

A jamovi module for analysing randomized experiments: treatment effects with regression adjustment and robust inference, randomization inference,
multiple outcomes, power, attrition and bounds, subgroup and factorial effects. Chinese summary: [README.zh.md](README.zh.md).

## Analyses (jamovi menu: Experiments)

| Analysis | What it does |
|---|---|
| **Treatment Effects** | Difference in means, covariate adjustment, Lin (2013) interacted adjustment, block fixed effects, several arms; HC0 to HC3, CR1 and CR2 (Satterthwaite df) standard errors |
| **Randomization Inference** | Fisher tests of the sharp null: exact enumeration or simulation, blocked and clustered assignment, difference in means / rank / t statistics, interval by test inversion |
| **Multiple Outcomes** | Adjusted p-values: Bonferroni, Holm, Sidak, Hochberg, BH, BY, Westfall-Young step-down, Romano-Wolf |
| **Power and Sample Size** | Power, minimum detectable effect, sample size; individual and cluster designs, covariate R2, unequal cluster sizes, values from the data |
| **Subgroup Effects** | Treatment effect in the subgroups of categorical variables, Wald interaction tests (does the effect differ?), Holm / Bonferroni / Benjamini-Hochberg adjusted p-values, forest plot |
| **Factorial Design** | Two crossed treatments: cell means, main effects, simple effects, interaction contrasts and their joint test, effects ignoring the other factor |
| **Balance and Attrition** | Standardized differences, joint F and permutation test, differential attrition |
| **Bounds under Attrition** | Lee (2009) trimming bounds when outcomes are missing and attrition differs by group; optional tightening by a categorical covariate; bootstrap standard errors and Imbens-Manski interval |
| **Worst-Case Bounds** | Manski bounds on the average effect: random assignment with missing outcomes, or non-random selection with monotone treatment response and monotone treatment selection; Imbens-Manski intervals |

Every analysis has *Method notes* and *Data and setup guide* in its Output section.

## Install
- **jamovi library**: in jamovi open the modules button (top right), *jamovi library*, find *Experiments Family* and install (once the module is listed).
- **Sideload**: download the `.jmo` file and use *Install from file* in the same menu. A `.jmo` works only on the operating system, processor type and jamovi version it was built for.
- **From source** (needs R and the jamovi compiler `jmvtools`; adjust the path to your jamovi folder):
```r
jmvtools::install(home = "C:/Program Files/jamovi 28.17.0.0")
```

## Use from R (Rj editor, scripts)
Every analysis is also an R function, for example
```r
expfamily::exeffect(data = d, outcome = "y1", treat = "arm", covs = c("baseline", "age"), block = "block", method = "lin")
```

## Data format
One row per unit. A treatment variable (two or more arms; the first level is the control), an outcome (numeric), optional covariates, block and cluster
variables. Rows with a missing treatment are dropped; missing outcomes are handled by the attrition and bounds analyses. Rows with missing values are reported.

## Example data
Shown in jamovi under **Open > Data Library > Experiments Family**; each entry says which analysis to use and which variable goes where.
Files in `data/` (generators `dev/make_data.R` and `dev/make_data_sub.R`):

| File | Design |
|---|---|
| `sim_trial.csv` | 600 people, 3 arms, 6 blocks, 4 outcomes, differential attrition |
| `sim_cluster_trial.csv` | 40 schools assigned as whole clusters |
| `sim_small_trial.csv` | 12 units, small enough to enumerate all 924 randomizations |
| `sim_subgroup_trial.csv` | 800 people, treatment effects that differ by gender, age group and region |
| `sim_factorial_trial.csv` | 900 people, a message (none, info, reminder) crossed with a cash incentive |
| `sim_selection_program.csv` | 1,500 people who choose a programme; employment outcome (worst-case bounds) |

## References
Papers and R packages behind each analysis appear in the References section of the jamovi output (defined in `jamovi/00refs.yaml`).

## Validation
All engines are in base R (`R/engine_*.R`). `tests/reference.R` checks them against `estimatr` (all standard errors, Lin adjustment, CR2), `ri2` and
`randomizr` (exact and simulated randomization inference), `pwr` (power), `p.adjust`, `lm`, `t.test` and `anova`; it needs those packages in a library named
by `EXP_REFLIB` (or the default library). `tests/subfact.R` checks subgroup effects and factorial contrasts against `estimatr` (`lm_robust`, `lh_robust`;
HC1 to HC3, classical, CR1) and the Wald tests against `lmtest::waldtest` with `sandwich`, to rounding error. `tests/lee.R` checks the Lee bounds (hand-computed
example, tightening, coverage in simulations; the Stata comparison with `leebounds` is in `stata_validation/16_leebounds.do`). `tests/manski.R` checks the worst-case
bounds against extreme imputation of the unobserved outcomes, the results of Manski and Pepper (2000) and simulations. `tests/smoke.R` runs every analysis
headless, including bad input.

## Known limitations
- Treatment effects are for a binary or multi-arm treatment assigned by the researcher; observational data need `matchfamily` or `ivfamily`.
- Lee bounds assume monotone selection (treatment affects attrition in one direction); Manski bounds are wide unless the monotonicity assumptions are added.
- Randomization inference by simulation depends on the seed and the number of draws; exact enumeration is used when the number of assignments is small.
- Factorial and subgroup analyses cover two crossed factors and categorical subgroup variables; continuous moderators are in `hetfamily`.

## Notes for developers
- Layout: `R/engine_*.R` is plain R with no jamovi dependency; `R/*.b.R` and `jamovi/*.yaml` are the jamovi front end (`dev/gen_exp.py` generates the analysis definitions).
- Tests: `Rscript tests/smoke.R`, `tests/reference.R`, `tests/lee.R`, `tests/subfact.R`, `tests/manski.R`; `tests/ui_checklist.md` lists what to check by hand in jamovi.
- Ribbon icon: `icon/analysis-exp.svg` (74 x 44): units split at random into a control and a treated group, with the difference of the means and its interval. jmvtools 28.17 cannot attach an icon to a module, so `dev/icon_preview.ps1` adds one CSS rule for the menu group to jamovi's client stylesheet (administrator PowerShell, restart jamovi afterwards).

## How to cite
Sun, M. *expfamily: Experiments Family for jamovi* (jamovi module). https://github.com/mingchao-sun/expfamily. Please also cite the papers behind the analysis you use (listed in the output).

## License
GPL-3, as stated in `DESCRIPTION`; third-party code and data are listed in `NOTICE.md`. Bug reports and suggestions: https://github.com/mingchao-sun/expfamily/issues
