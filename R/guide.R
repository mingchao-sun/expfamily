# "Data and setup guide" and "Method notes" for each analysis.

ex_settings <- function(rows) {
    paste0("<table style='border-collapse:collapse;margin:4px 0 8px 0'>",
           "<tr><th style='text-align:left;padding:2px 14px 2px 0;border-bottom:1px solid #999'>Field</th>",
           "<th style='text-align:left;padding:2px 0;border-bottom:1px solid #999'>Set to</th></tr>",
           paste0("<tr><td style='padding:2px 14px 2px 0'>", names(rows), "</td><td style='padding:2px 0'>",
                  unlist(rows), "</td></tr>", collapse = ""), "</table>")
}

ex_layout <- paste0(
    "<h4>Data layout</h4><p>One row per unit (a person, a school, a firm). The treatment variable holds the group of each unit: text such as <i>control</i>, <i>T1</i>, <i>T2</i>, or numbers such as 0, 1, 2. ",
    "The control group is the first value in alphabetical or numerical order unless you type its value in <i>Control group value</i>. A block variable names the strata within which assignment was randomized; ",
    "a cluster variable names the groups (schools, villages) that were assigned as a whole. Rows with missing values in the variables used are dropped and counted.</p>")

ex_guide_raw <- function(key) {
    switch(key,
    exeffect = paste0(
        "<h3>Treatment Effects: data and setup</h3>",
        "<p>Estimates the effect of each treatment arm against the control in a randomized experiment, with standard errors that are valid for randomized designs, optional baseline covariates and blocks, and cluster randomization.</p>",
        "<h4>Sample data 1: an individually randomized trial</h4>",
        ex_settings(list("Dataset" = "<b>Simulated randomized trial</b>", "Outcome" = "y1", "Treatment (arms)" = "arm (control group value: leave blank, <i>control</i> comes first)", "Estimator" = "Difference in means", "Standard errors" = "Automatic (HC2)")),
        "<p>600 people in 3 arms (control, T1, T2) randomized within 6 blocks. Expected: T1 0.275 (s.e. 0.109, p 0.012, 95% CI 0.060 to 0.490; 0.256 control standard deviations), T2 0.368 (0.110, p 0.0008, CI 0.153 to 0.584). ",
        "With Estimator <i>Covariates interacted with the arms (Lin)</i>, covariates <i>baseline</i>, <i>age</i>, <i>female</i> and block <i>block</i> the estimates are 0.254 (s.e. 0.097) and 0.423 (0.097): the covariates make the estimates more precise.</p>",
        "<h4>Sample data 2: a cluster randomized trial</h4>",
        ex_settings(list("Dataset" = "<b>Simulated cluster randomized trial</b>", "Outcome" = "y", "Treatment (arms)" = "group", "Cluster" = "school", "Standard errors" = "Automatic (CR2)")),
        "<p>40 schools of 25 students, assigned to treatment as whole schools. Expected: effect 0.174, CR2 standard error 0.147 with 38 degrees of freedom (p 0.24). ",
        "Choosing <i>Robust HC2</i> (ignoring the clusters) gives 0.077 and p 0.024: it wrongly looks significant because students of the same school are not independent.</p>",
        ex_layout),
    exri = paste0(
        "<h3>Randomization Inference: data and setup</h3>",
        "<p>Tests the sharp null hypothesis that the treatment had no effect on any unit by re-randomizing the assignment, with no assumption about the distribution of the outcome. Works with small samples, blocks and clusters.</p>",
        "<h4>Sample data 1: a small experiment</h4>",
        ex_settings(list("Dataset" = "<b>Simulated small experiment</b>", "Outcome" = "y", "Treatment (arms)" = "group (control group value: <i>control</i>)", "Test statistic" = "Difference in means")),
        "<p>12 units, 6 treated. All 924 possible randomizations are enumerated. Expected: observed difference 1.433, two-sided p 0.130, p for a larger treated mean 0.065; the 95% interval for a constant effect is -0.500 to 3.350. ",
        "With the rank statistic the observed value is 3.000 and p 0.180.</p>",
        "<h4>Sample data 2: a larger trial</h4>",
        ex_settings(list("Dataset" = "<b>Simulated randomized trial</b>", "Outcome" = "y1", "Treatment (arms)" = "arm", "Block" = "block")),
        "<p>Two comparisons (T1 and T2 against the control), permuting within the 6 blocks. When there are more randomizations than the enumeration limit (default 20,000), the p-values are simulated from the chosen number of randomizations and the table reports the Monte Carlo standard error.</p>",
        ex_layout),
    exmht = paste0(
        "<h3>Multiple Outcomes: data and setup</h3>",
        "<p>When an experiment measures several outcomes, some will look significant by chance. This analysis adjusts the p-values of the treatment effects for the number of outcomes.</p>",
        "<h4>Sample data</h4>",
        ex_settings(list("Dataset" = "<b>Simulated randomized trial</b>", "Outcomes" = "y1, y2, y3, y4", "Treatment (arms)" = "arm", "Treatment group value" = "T2", "Covariates" = "baseline", "Methods" = "Bonferroni, Holm, Benjamini-Hochberg, Westfall-Young min-p, Romano-Wolf (defaults)")),
        "<p>Effects of T2 against the control: y1 0.432 (p &lt; .001), y2 0.395 (p 0.0002), y3 -0.134 (p 0.146), y4 0.089 (p 0.405). Adjusted p-values for y2: Bonferroni 0.0007, Holm 0.0006, Benjamini-Hochberg 0.0004; for y3: Bonferroni 0.583, Holm 0.292, Benjamini-Hochberg 0.194, ",
        "Westfall-Young 0.263, Romano-Wolf 0.255 (permutation and bootstrap results depend on the seed; with seed 1234 and the defaults they are as stated). The treatment must have exactly two groups in the comparison: type the group of interest in <i>Treatment group value</i> when there are more.</p>",
        ex_layout),
    exlee = paste0(
        "<h3>Bounds under Attrition: data and setup</h3>",
        "<p>Gives bounds for the treatment effect when the outcome is missing for some units, and the missingness may depend on the treatment.</p>",
        "<h4>Sample data: differential attrition</h4>",
        ex_settings(list("Dataset" = "<b>Simulated randomized trial</b>", "Outcome" = "endline (missing = not observed)", "Treatment (arms)" = "arm", "Treatment group value" = "T2")),
        "<p>Response rates are 91.7% in the control and 71.2% in T2, so 22.3% of the observed T2 outcomes are trimmed. Expected: difference in means of the observed 0.526; Lee bounds 0.106 to 0.944 (bootstrap s.e. 0.144 and 0.143); 95% Imbens-Manski interval -0.130 to 1.179. ",
        "With T1 as the treatment group (rates 86.4% and 91.7%): bounds 0.219 to 0.487, interval -0.006 to 0.708. With covariate <i>block</i> in the tightening box, T2: bounds 0.166 to 0.916.</p>",
        "<p>The outcome variable must be numeric with an empty cell (missing value) for every unit that was not observed. Units whose treatment value is missing are dropped.</p>"),
    exbounds = paste0(
        "<h3>Worst-Case Bounds: data and setup</h3>",
        "<p>Bounds on the average treatment effect that rely on no, or only weak, assumptions: instead of one number that needs strong assumptions, a range that the data alone support. Use it to see how much of a result comes from the data and how much from assumptions, when outcomes are missing or when people chose the treatment.</p>",
        "<h4>Sample data 1: random assignment, missing outcomes</h4>",
        ex_settings(list("Dataset" = "<b>Simulated randomized trial</b>", "Outcome" = "endline", "Treatment (arms)" = "arm", "Treatment group value" = "T2", "Situation" = "Treatment assigned at random; some outcomes are missing")),
        "<p>29% of the T2 outcomes and 8% of the control outcomes are missing. The range of the outcome is the observed range, -3.684 to 3.939. Expected: difference in the observed means 0.526 (s.e. 0.125); bounds on the average effect -1.056 to 1.774, 95% interval -1.374 to 2.040. The bounds are wide because the missing outcomes could be anywhere in the range; the analysis <i>Bounds under Attrition</i> gives much narrower bounds for the units that respond under both assignments, at the price of the monotonicity assumption.</p>",
        "<h4>Sample data 2: people chose the programme</h4>",
        ex_settings(list("Dataset" = "<b>Simulated selection into a programme</b>", "Outcome" = "employed", "Treatment (arms)" = "program", "Situation" = "Treatment not random", "Lowest and highest possible outcome" = "0 and 1")),
        "<p>1,500 people, 48% joined the programme; the true average effect is 0.233. The difference in means is 0.427 (s.e. 0.023), too large because people with better prospects join. Expected bounds on the effect: no assumptions -0.288 to 0.712 (the width is always the range of the outcome); monotone treatment response 0.000 to 0.712; monotone treatment selection -0.288 to 0.427; both 0.000 to 0.427, interval 0.000 to 0.465: the effect is not negative and at most the difference in means.</p>",
        "<h4>Data layout</h4><p>One row per unit. The outcome has an empty cell when it was not observed (only in the situation with random assignment). The treatment variable has two groups. Type the lowest and highest possible values of the outcome when you know them (0 and 1 for a yes or no outcome); otherwise the observed range is used.</p>"),
    exsub = paste0(
        "<h3>Subgroup Effects: data and setup</h3>",
        "<p>Estimates the treatment effect separately in the subgroups of one or more categorical variables (gender, age group, region), tests whether the effect really differs between the subgroups of each variable, and adjusts the p-values for the number of subgroups examined.</p>",
        "<h4>Sample data</h4>",
        ex_settings(list("Dataset" = "<b>Simulated subgroup trial</b>", "Outcome" = "y", "Treatment (arms)" = "arm (control group and treatment group: leave blank)", "Subgroup variables" = "gender, agegroup, region")),
        "<p>800 people randomized to control or treated; the effect is 0.15 for men and 0.75 for women and does not depend on age group or region. Expected: effect in the whole sample 0.505 (s.e. 0.080, interval 0.348 to 0.663); men 0.118 (s.e. 0.114, p 0.303), women 0.869 (s.e. 0.108, p &lt; .001); under 40 0.578 and 40 and over 0.423; north 0.434, south 0.308, east 0.772. ",
        "Interaction tests: gender F(1, 796) = 22.94, p &lt; .001; age group F = 0.92, p 0.339; region F(2, 794) = 2.71, p 0.067 (Holm-adjusted 0.134): east looks larger but the data do not show that regions differ. With covariate <i>baseline</i> and the estimator <i>Covariates and blocks added</i> the effects become 0.198 for men and 0.917 for women.</p>",
        "<h4>Data layout</h4><p>One row per unit. The treatment variable holds two groups (or more: pick the two to compare with the control and treatment group values). Subgroup variables are categorical: text, or numbers set to Nominal in the Data tab. Units with a missing value in a variable used are dropped.</p>"),
    exfact = paste0(
        "<h3>Factorial Design: data and setup</h3>",
        "<p>For experiments in which two treatments were assigned independently and every combination occurs (a 2 x 2 or 3 x 2 factorial design): the effect of each treatment, averaged over the other and within each of its levels, and whether the effect of one depends on the other.</p>",
        "<h4>Sample data</h4>",
        ex_settings(list("Dataset" = "<b>Simulated factorial trial</b>", "Outcome" = "savings", "Factor A" = "message (control level: none)", "Factor B" = "incentive (control level: no)")),
        "<p>900 people randomized to a message (none, info, reminder) crossed with a cash incentive (no, yes); the true effects are info 0.4, reminder 0.6, incentive 0.5 and 0.7 more for a reminder with the incentive. Expected: main effects info 0.633 (s.e. 0.143), reminder 1.091 (0.145), incentive 0.667 (0.117); simple effects of the incentive: 0.368 without a message, 0.276 with the info, 1.357 with the reminder; interaction (reminder x incentive) 0.989 (s.e. 0.289, p 0.001), (info x incentive) -0.091 (p 0.749); joint test of the interactions F(2, 894) = 8.71, p &lt; .001. ",
        "Ignoring the other factor, the incentive effect is 0.619 and the reminder effect 1.047.</p>",
        "<h4>Data layout</h4><p>One row per unit with a variable for each factor (text or numbers, set numeric factors to Nominal) and the outcome. The control level of each factor is the first value unless you type it. Every cell (combination of levels) needs units.</p>"),
    expower = paste0(
        "<h3>Power and Sample Size: data and setup</h3>",
        "<p>Plans an experiment: the power of a design, the smallest effect it can detect, or the sample needed. No data are required; an outcome, a cluster and covariates from an open dataset can supply the standard deviation, the intracluster correlation and R2.</p>",
        "<h4>Examples</h4>",
        ex_settings(list("Calculate" = "Power; standard deviation 1; effect 0.3; total units 200; share treated 0.5", "Result" = "power 0.560")),
        ex_settings(list("Calculate" = "Minimum detectable effect; 200 units; target power 0.8", "Result" = "0.398 standard deviations")),
        ex_settings(list("Calculate" = "Sample size; effect 0.3; target power 0.8", "Result" = "176 treated and 175 control, 351 units")),
        ex_settings(list("Design" = "Cluster randomization; 40 clusters; 25 units per cluster; intracluster correlation 0.05; effect 0.25; Calculate power", "Result" = "power 0.738 (design effect 2.2)")),
        ex_settings(list("Design" = "Cluster randomization; Calculate sample size; effect 0.25; target power 0.8", "Result" = "47 clusters (24 treated, 23 control)")),
        "<p>To take the standard deviation and correlation from data: open <b>Simulated cluster randomized trial</b>, tick <i>Take standard deviation, correlation and R2 from the data</i>, put <i>y</i> under Outcome (standard deviation 1.223), <i>school</i> under Cluster (intracluster correlation 0.109) and <i>baseline</i> under Covariates.</p>",
        "<h4>Options</h4><ul><li><b>Effect to detect</b>, in the units of the outcome; the standard deviation converts it to standard deviations.</li><li><b>R2 and number of covariates</b>: baseline covariates that predict the outcome reduce the variance.</li>",
        "<li><b>Cluster design</b>: total number of clusters, units per cluster (the average), the intracluster correlation and the coefficient of variation of the cluster sizes.</li></ul>"),
    exbalance = paste0(
        "<h3>Balance and Attrition: data and setup</h3>",
        "<p>Checks that randomization produced comparable groups at baseline and that the outcome was observed equally often in every group.</p>",
        "<h4>Sample data</h4>",
        ex_settings(list("Dataset" = "<b>Simulated randomized trial</b>", "Treatment (arms)" = "arm", "Covariates" = "baseline, age, female", "Block" = "block", "Outcome (for attrition)" = "endline")),
        "<p>Expected: all standardised differences below 0.13 and all p-values above 0.2; joint tests T1 F 0.24 (p 0.87), T2 F 0.92 (p 0.43), with randomization p-values 0.86 and 0.42. ",
        "Response rates: control 91.7%, T1 86.4%, T2 71.2%; the T2 difference from the control is -0.205 (p &lt; .001): differential attrition. The covariates also predict response differently across groups (F 3.48, p 0.002).</p>",
        ex_layout),
    stop("unknown guide: ", key))
}

ex_notes_raw <- function(key) {
    switch(key,
    exeffect = paste0(
        "<h3>Treatment Effects: method notes</h3>",
        "<h4>What it does</h4><p>In a randomized experiment the effect of a treatment is identified by comparing the groups. The estimators here differ in how they use baseline information; none needs a model of the outcome to be correct.</p>",
        "<h4>Estimator</h4><ul><li><b>Difference in means</b>: the control mean subtracted from the arm mean. With HC2 standard errors this is the conservative Neyman variance.</li>",
        "<li><b>Covariates added</b>: ordinary regression of the outcome on the arms and the baseline covariates; usually more precise, but the arm coefficient can be biased by the way the covariates enter when the groups differ in size.</li>",
        "<li><b>Covariates interacted with the arms (Lin, 2013)</b>: the covariates, centred at their means, are interacted with every arm. This estimates the average effect without bias and is never less precise than the difference in means asymptotically.</li>",
        "<li><b>Block</b>: blocks of randomization enter as fixed effects.</li></ul>",
        "<h4>Standard errors</h4><ul><li>Classical; robust HC1 (as Stata <i>robust</i>), HC2 and HC3, which correct for leverage and are better in small samples (HC2 is the default); for cluster randomization CR1 (as Stata <i>cluster</i>) or CR2 (Bell and McCaffrey, 2002), which corrects the bias of the cluster-robust variance and uses Satterthwaite degrees of freedom (default with a cluster variable).</li>",
        "<li>Intervals and p-values use the t distribution with the degrees of freedom named in the table note.</li></ul>",
        "<h4>Limits</h4><p>Intention-to-treat effects of the assignment: no allowance for non-compliance (see the IV family) or attrition (see Balance and Attrition). A design with blocks estimated by regression weights the blocks by their treatment variance; with equal blocks and shares this equals the block-weighted effect. ",
        "Results equal the R package estimatr (lm_robust, lm_lin) and Stata regress with the vce options.</p>"),
    exri = paste0(
        "<h3>Randomization Inference: method notes</h3>",
        "<h4>What it does</h4><p>The sharp null says that no unit is affected by the treatment, so every unit's outcome would be the same under any assignment. The assignment is then re-drawn with the actual randomization scheme and the statistic is recomputed; the p-value is the share of re-draws with a statistic at least as extreme as the observed one (Fisher, 1935). ",
        "It is exact in any sample size, because the only randomness is the assignment.</p>",
        "<h4>Options</h4><ul><li><b>Block</b>: assignments are permuted within blocks. <b>Cluster</b>: whole clusters are permuted; statistics are computed on the units.</li>",
        "<li><b>Test statistic</b>: difference in means (sensitive to a shift of the mean), difference in mean ranks (robust to outliers) or the studentised difference.</li>",
        "<li><b>Hypothesised constant effect</b>: tests that every unit's outcome changes by exactly this amount (default 0).</li>",
        "<li><b>Enumerate all randomizations up to</b> and <b>Number of simulated randomizations</b>: when the number of possible assignments is not above the limit, all are used and the p-value is exact; otherwise the stated number is drawn using the seed, and the table gives the Monte Carlo standard error.</li>",
        "<li><b>Confidence interval</b>: the constant effects not rejected at the chosen level (test inversion), for the difference in means.</li></ul>",
        "<h4>Limits</h4><p>The interval assumes one common effect; it is not a confidence interval for the average effect when effects vary. For more than two arms each treated arm is compared with the control using only the units of the two groups. ",
        "Results equal the R package ri2 on the same randomizations.</p>"),
    exmht = paste0(
        "<h3>Multiple Outcomes: method notes</h3>",
        "<h4>What it does</h4><p>With m outcomes tested at the 5% level, the chance of at least one false positive is far above 5%. The adjustments change the p-values so that the error rate is controlled over all outcomes together.</p>",
        "<h4>Methods</h4><ul><li><b>Family-wise error rate</b> (the probability of any false rejection): Bonferroni and Sidak (conservative), Holm and Hochberg (uniformly more powerful), Westfall-Young step-down and Romano-Wolf step-down. ",
        "The last three use the correlation between outcomes: Westfall-Young permutes the treatment (min-p or max-t version), Romano-Wolf uses a studentised bootstrap. They are less conservative when outcomes are correlated.</li>",
        "<li><b>False discovery rate</b> (the expected share of false findings among the findings): Benjamini-Hochberg and, allowing any dependence, Benjamini-Yekutieli.</li></ul>",
        "<h4>Estimation</h4><p>Each outcome gets a classical t test of the treatment effect (adjusted for the covariates and blocks if given); the permutation and bootstrap are based on the same statistics. Permutations stay within blocks. <b>Random seed</b> makes the resampling reproducible.</p>",
        "<h4>Limits</h4><p>Two groups at a time. The outcomes must be a pre-specified family. Results equal stats::p.adjust for the formula-based methods; the resampling methods agree with Stata wyoung and rwolf up to resampling error.</p>"),
    exlee = paste0(
        "<h3>Bounds under Attrition: method notes</h3>",
        "<h4>The problem</h4><p>When the outcome is missing for some units (they dropped out, could not be found, or have no wage because they do not work), comparing the observed outcomes of the groups is no longer a randomized comparison if the treatment changes who is observed. The estimate then mixes the effect with a difference in who responds.</p>",
        "<h4>Lee (2009) bounds</h4><p>Assume the treatment moves response in one direction for everybody (<i>monotonicity</i>). Then the group with the higher response rate contains a share q = (higher rate - lower rate) / higher rate of units that would not have responded in the other group. If you remove that share of the observed outcomes of the group, what is left is comparable with the other group. Which outcomes to remove is unknown, so remove the highest ones for the lower bound and the lowest ones for the upper bound. The bounds are for the effect on units that respond under either assignment.</p>",
        "<h4>Inference</h4><p>The standard errors are bootstrap standard errors of the two bounds (resampling within groups). The confidence interval is the Imbens-Manski (2004) interval, [lower - c se, upper + c se], with c chosen so that the interval covers the effect, not only the bounds, with the stated probability; it is narrower than the union of two ordinary intervals when the bounds are wide.</p>",
        "<h4>Tightening</h4><p>A categorical baseline covariate (for example the block) lets the trimming be done within its cells; the cell bounds are averaged with the cell shares of the units observed in the group that is not trimmed. The tightened bounds are never wider than the plain ones, in large samples.</p>",
        "<h4>Limits</h4><p>Two groups at a time; the outcome is continuous. Monotonicity cannot be tested, but the sign of the effect on response is shown. A huge difference in response rates gives bounds too wide to be useful. The trimmed means remove the boundary observation in part (exact trimming), as Stata leebounds does.</p>"),
    exbounds = paste0(
        "<h3>Worst-Case Bounds: method notes</h3>",
        "<h4>The idea</h4><p>The average effect is the mean outcome if everybody were treated minus the mean outcome if nobody were. For each unit only one of the two outcomes is observed. Manski (1990) asks what can be learned if nothing is assumed about the other one except that it lies between the lowest and the highest possible value: replace each unobserved outcome by the lowest value to get one end of the range, by the highest to get the other. The width of the bounds on the average effect is then the range of the outcome, whatever the data.</p>",
        "<h4>Two situations</h4><ul><li><b>Random assignment, missing outcomes</b>: the treatment arms are comparable, but some outcomes are missing. In each arm the observed units give the mean of the units observed and the missing ones are put at the lowest or highest value. With no missing outcomes the bounds collapse to the difference in means.</li>",
        "<li><b>Treatment not random</b>: units chose or were selected. The outcome of a treated unit under no treatment, and of a control under treatment, is unobserved. Two weak assumptions narrow the range: <b>monotone treatment response</b> (the treatment never lowers the outcome of anybody, so the effect is not negative and the unobserved outcome of a control under treatment is at least its observed outcome) and <b>monotone treatment selection</b> (the units that took the treatment would have had at least as high an average outcome under either treatment as those that did not, as when people with better prospects join). Together they give the bounds from 0 to the difference in means (Manski and Pepper, 2000).</li></ul>",
        "<h4>Inference</h4><p>Bootstrap standard errors of the two bounds and the Imbens-Manski (2004) interval, which covers the effect itself, not only its bounds, with the stated probability. A set of assumptions that the data contradict (a lower bound above the upper bound) is shown empty.</p>",
        "<h4>Limits</h4><p>The outcome must be bounded and its range is known only by you: the observed range is used if you do not type it, which makes the bounds depend on the sample. Wide bounds are not a failure of the method but the honest size of the uncertainty without strong assumptions. Binary and bounded outcomes work best. Results equal the formulas of Manski and Pepper (2000); the calculation was checked against extreme imputation of the unobserved outcomes.</p>"),
    exsub = paste0(
        "<h3>Subgroup Effects: method notes</h3>",
        "<h4>What it does</h4><p>In each subgroup the treatment effect is the difference in mean outcomes between treated and control units (with covariates and block effects added if chosen), with a robust standard error. A separate regression of the outcome on the treatment, the subgroup variable and their interaction gives the <b>interaction test</b>: a Wald F test of whether the effects differ between the subgroups of a variable.</p>",
        "<h4>Reading the results</h4><ul><li>Do not conclude that subgroups differ because one effect is significant and another is not: the difference between a significant and a non-significant result is not itself significant. The interaction test is the right question.</li>",
        "<li>With many subgroups some effects will be significant by chance. The adjusted p-values (Holm and Bonferroni control the chance of any false finding; Benjamini-Hochberg the share of false findings) correct for the number of subgroup effects in the table; the interaction tests are adjusted across the subgroup variables.</li>",
        "<li>Subgroups should be chosen before looking at the data. Splitting a continuous variable into groups after the fact is a form of searching for a result.</li></ul>",
        "<h4>Limits</h4><p>Subgroups must be defined by pre-treatment variables. Two groups are compared; effects are for the subgroup as it is in the sample. Results equal estimatr::lm_robust within each subgroup and lmtest::waldtest with the HC2 covariance for the interaction test.</p>"),
    exfact = paste0(
        "<h3>Factorial Design: method notes</h3>",
        "<h4>What it does</h4><p>A regression with one mean per cell (combination of the two factors), plus covariates and block effects if given, gives the cell means; every effect is a linear combination of them with a robust standard error: <b>simple effects</b> (the effect of a factor within a level of the other), <b>main effects</b> (the average of the simple effects over the other factor, equal weights), <b>interactions</b> (the difference between two simple effects) and a joint F test of all interactions.</p>",
        "<h4>Reading the results</h4><ul><li>If the interaction is small, the main effects summarise the design well and the factors act separately. If it is large, report the simple effects: the effect of one treatment depends on the other.</li>",
        "<li>The table <i>ignoring the other factor</i> compares all units by one factor alone. In a design where both factors were randomized independently this is valid and uses the whole sample; with an interaction it averages the simple effects with the shares of the other factor's levels, and not with equal weights as the main effects do.</li>",
        "<li>Factorial designs have power for main effects but little for interactions: a non-significant interaction does not show there is none (Muralidharan, Romero and Wuthrich, 2019).</li></ul>",
        "<h4>Limits</h4><p>Two factors, each with 2 to 6 levels, all cells present. Results equal estimatr::lm_robust and linear hypotheses (lh_robust) with HC2, and lmtest::waldtest for the joint test.</p>"),
    expower = paste0(
        "<h3>Power and Sample Size: method notes</h3>",
        "<h4>What it does</h4><p>Power is the probability that the experiment, if the effect is the stated one, gives a significant result. It is computed exactly from the noncentral t distribution for the difference between two means; the same formula gives the smallest effect detectable with a given power and the smallest sample reaching it.</p>",
        "<h4>Inputs</h4><ul><li><b>Standard deviation</b> of the outcome in the control group; <b>effect</b> in the outcome units.</li><li><b>R2 of covariates</b>: baseline covariates that explain a share R2 of the variance shrink the residual standard deviation by sqrt(1 - R2) and use one degree of freedom each.</li>",
        "<li><b>Cluster randomization</b>: units in a cluster are correlated, so the variance is multiplied by the design effect 1 + ((CV^2 + 1) m - 1) rho; the t distribution has (clusters - 2) degrees of freedom.</li></ul>",
        "<h4>Limits</h4><p>Continuous outcome, two groups, equal-sized clusters unless a coefficient of variation is given; multiple comparisons are not allowed for (use a smaller significance level). Results equal the R package pwr and Stata power twomeans.</p>"),
    exbalance = paste0(
        "<h3>Balance and Attrition: method notes</h3>",
        "<h4>Balance</h4><p>If assignment was random, baseline covariates should differ only by chance. The table shows the group means, the standardised difference (difference over the pooled standard deviation) and a Welch t test for each covariate and treated arm. ",
        "Do not read single p-values as failures: with many covariates a few will be below .05. The <b>joint test</b> asks whether the covariates together predict assignment (F test of the regression of the arm on the covariates), with a randomization p-value from permuting the assignment (within blocks).</p>",
        "<h4>Attrition</h4><p>Add the outcome to see how many units have it. Different response rates by group (differential attrition) can bias the estimates even in a randomized trial. The second test asks whether the covariates predict response differently across groups (arm by covariate interactions). ",
        "The analysis <i>Bounds under Attrition</i> gives Lee bounds for the effect when outcomes are missing.</p>",
        "<h4>Limits</h4><p>Balance tests do not prove that randomization worked and cannot detect unobserved imbalance. Standardised differences of binary covariates use the sample variances of the two groups.</p>"),
    stop("unknown notes: ", key))
}

ex_guide_html <- function(key) {
    x <- ex_guide_raw(key)
    sub("</p>", paste0("</p><p><i>Open the sample data from File &gt; Open &gt; Data Library &gt; Experiments Family; the library lists each data set by the bold name below.</i></p>"), x, fixed = TRUE)
}
ex_notes_html <- function(key) ex_notes_raw(key)
