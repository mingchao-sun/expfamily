# expfamily 1.0.0

* First public release (jamovi library submission). Contains everything listed below; the earlier 0.x entries are development history.

# expfamily 0.3.0

* New: **Worst-Case Bounds**: Manski bounds on the average treatment effect with random assignment and missing outcomes, or with non-random selection under no assumptions, monotone treatment response, monotone treatment selection and both (Manski, 1990; Manski and Pepper, 2000), with Imbens-Manski intervals. Complements the Lee bounds of *Bounds under Attrition*.

# expfamily 0.2.0

* New: **Subgroup Effects**: the treatment effect within the subgroups of categorical variables, Wald interaction tests, adjusted p-values (Holm, Bonferroni, Benjamini-Hochberg), forest plot.
* New: **Factorial Design**: two crossed treatments with 2 to 6 levels each: cell means, main effects (equal-weighted averages of the simple effects), simple effects, interaction contrasts with a joint F test, and the effect of each factor ignoring the other. Equal to estimatr and lmtest on the same data.

# expfamily 0.1.0

First version. Six analyses in the menu group Exp:

* **Treatment Effects**: difference in means, covariate adjustment and Lin (2013) fully interacted adjustment, block fixed effects, classical, HC0 to HC3 and cluster-robust (CR1, CR2 with Satterthwaite degrees of freedom) standard errors, several treatment arms.
* **Randomization Inference**: Fisher randomization tests (exact enumeration or simulation) for the sharp null, blocked and clustered designs, difference in means, rank and t statistics, confidence interval by test inversion.
* **Multiple Outcomes**: adjusted p-values by Bonferroni, Holm, Sidak, Hochberg, Benjamini-Hochberg, Benjamini-Yekutieli, Westfall-Young step-down (min-p and max-T) and Romano-Wolf.
* **Power and Sample Size**: power, minimum detectable effect and sample size for individual and cluster randomization, with covariate R2, cluster-size variation and values taken from the data.
* **Bounds under Attrition**: Lee (2009) trimming bounds for the effect when the outcome is missing for some units and the treatment may change who is observed; optional tightening by a categorical covariate, bootstrap standard errors, Imbens-Manski confidence interval.
* **Balance and Attrition**: standardized differences, joint balance test with permutation p-value, differential attrition tests.

The regression and randomization results equal the R packages `estimatr` and `ri2`, and the power results equal `pwr`, on the same data. Method notes and a data and setup guide are in the Output section of each analysis.
