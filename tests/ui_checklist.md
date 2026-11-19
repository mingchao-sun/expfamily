# expfamily: checklist for testing in jamovi

Menu: Experiments. Install: `jmvtools::install(home = "C:/Program Files/jamovi 28.17.0.0")`.
Open sample data from File > Open > Data Library > Experiments Family. Defaults: HC2 / CR2 standard errors, 95% level.

## 1. Treatment Effects
Dataset: Simulated randomized trial. Outcome `y1`; Treatment `arm`.
Expect: T1 0.275 (s.e. 0.109, p 0.012, CI 0.060 to 0.490), T2 0.368 (0.110, p 0.0008). Both arms are compared with `control`.
Then: Estimator "Covariates interacted with the arms (Lin)", covariates `baseline`, `age`, `female`, block `block` -> 0.254 (0.097) and 0.423 (0.097). Standard errors HC1 / HC3 change the s.e. slightly; classical differs more. Control group value `T1` -> the arms become control and T2 versus T1. Level 90 narrows the interval. Method notes and guide on/off.
Dataset: Simulated cluster randomized trial. Outcome `y`; Treatment `group`; Cluster `school` -> effect 0.174, CR2 s.e. 0.147, 38 df, p 0.24. Choose CR1: same estimate, s.e. smaller.
Messages: CR2 without a cluster variable -> "need a cluster variable"; a control value that is not an arm -> message listing the arms.

## 2. Randomization Inference
Dataset: Simulated small experiment. Outcome `y`; Treatment `group`; Control group `control`.
Expect: observed difference 1.433; all 924 randomizations enumerated; two-sided p 0.130, p for larger treated mean 0.065; 95% interval -0.500 to 3.350.
Then: statistic Rank or t; hypothesised effect 0.5; untick the interval; the plot of the randomization distribution shows the observed value.
Dataset: Simulated randomized trial, outcome `y1`, `arm`, block `block`: simulation is used (the note says how many draws) and the Monte Carlo error is shown; change the seed and the number of draws.
Messages: cluster variable that varies treatment within a cluster -> message.

## 3. Multiple Outcomes
Dataset: Simulated randomized trial. Outcomes `y1` to `y4`; Treatment `arm`; compare group T2.
Expect: a table with raw and adjusted p-values, one row per outcome, for Bonferroni, Holm and Benjamini-Hochberg; y1 is significant after every adjustment. Tick Westfall-Young and Romano-Wolf (a few seconds); adjusted p-values are never smaller than the raw ones and Westfall-Young / Romano-Wolf are not larger than Holm.
Then: covariates `baseline`, block `block`; treatment group T1. Messages: one outcome only -> hint; no method ticked -> message.

## 4. Power and Sample Size
No variables needed. Defaults: calculate power, 200 units, effect 0.3, sd 1.
Expect: power about 0.56. Switch to Minimum detectable effect (about 0.40) and Sample size (about 352 in total, 176 per arm). One-sided lowers the sample size. Share assigned to treatment 0.3 changes the split.
Design Cluster randomization: clusters, units per cluster, intracluster correlation; the table shows the design effect. Tick "take values from the data" with the cluster trial data: outcome `y`, cluster `school`, covariate `baseline`. The plot shows power against total size with the current sample marked.
Messages: "take values from the data" with no outcome -> message.

## 5. Balance and Attrition
Dataset: Simulated randomized trial. Treatment `arm`; covariates `baseline`, `age`, `female`; block `block`; outcome for attrition `endline`.
Expect: standardised differences below 0.13 and p above 0.2; joint tests T1 F 0.24 (p 0.87), T2 F 0.92 (p 0.43); response rates control 91.7%, T1 86.4%, T2 71.2%, T2 versus control -0.205 (p < .001); covariates predict response differently by group (F 3.48, p 0.002).
Then: add `income` (has missing values: the note says how many rows are used); remove the outcome -> the attrition table disappears. Messages: no covariates -> hint.

## 6. Bounds under Attrition
Dataset: Simulated randomized trial. Outcome `endline`; Treatment `arm`; Treatment group value `T2`.
Expect: response rates 91.7% (control) and 71.2% (T2); difference in means of the observed 0.526; Lee bounds 0.106 to 0.944 (s.e. 0.144 and 0.143); interval -0.130 to 1.179. The note says 22.3% of the observed control outcomes are trimmed and that treatment lowers response.
Then: Treatment group value `T1`: bounds 0.219 to 0.487. Put `block` in the tightening box: T2 bounds 0.166 to 0.916 and a note about 6 cells. Direction "Treatment raises the response rate" -> message that it contradicts the response rates. Level 90 narrows the interval. Method notes and guide on/off.
Messages: leave the outcome empty -> hint; a treatment group value that is not an arm -> message listing the arms.

## 7. Subgroup Effects
Dataset: Simulated subgroup trial. Outcome `y`; Treatment `arm`; Subgroup variables `gender`, `agegroup`, `region`.
Expect: whole sample 0.505 (s.e. 0.080); men 0.118 (p 0.303), women 0.869 (p < .001); under 40 0.578, 40 and over 0.423; north 0.434, south 0.308, east 0.772; adjusted p-values larger than the raw ones. Interaction table: gender F 22.94 (p < .001, df 1 and 796), age group F 0.92 (p 0.339), region F 2.71 (p 0.067, adjusted 0.134). The forest plot shows the seven subgroup effects with the orange band of the whole-sample effect.
Then: covariate `baseline` with the estimator "Covariates and blocks added" -> men 0.198, women 0.917; adjustment Benjamini-Hochberg or None; standard errors HC3; level 90; untick the plot. Method notes and guide on/off.
Messages: CR1 without a cluster -> message; outcome also in the subgroup box -> "one box only"; no subgroup variable -> hint.

## 8. Factorial Design
Dataset: Simulated factorial trial. Outcome `savings`; Factor A `message` (control level none); Factor B `incentive` (control level no).
Expect: six cells of 123 to 160 people; main effects info 0.633 (s.e. 0.143), reminder 1.091 (0.145), incentive 0.667 (0.117); simple effects of the incentive 0.368 (none), 0.276 (info), 1.357 (reminder); interaction (reminder x incentive) 0.989 (s.e. 0.289, p 0.001) and (info x incentive) -0.091; the note under the interaction table gives the joint test F(2, 894) = 8.71, p < .001, and the notes say the interaction is significant. Ignoring the other factor: incentive 0.619, info 0.613, reminder 1.047.
Then: swap the two factors; covariates `age`, `baseline` (standard errors shrink); HC1 / classical; level 90. Method notes and guide on/off.
Messages: the same variable in both boxes -> "one box only"; a factor with one value -> message; remove a cell of the data -> message naming the empty cell; no second factor -> hint.

## 9. Worst-Case Bounds
Dataset: Simulated randomized trial. Outcome `endline`; Treatment `arm`; Treatment group value `T2`; Situation "Treatment assigned at random; some outcomes are missing".
Expect: difference in observed means 0.526 (s.e. 0.125); range used -3.684 to 3.939; share missing 28.8% (T2) and 8.3% (control); one row of bounds -1.056 to 1.774 (width 2.830), interval -1.374 to 2.040; the second table shows the bounds of the two average outcomes; the plot shows one blue bar with a grey interval, an orange line at 0.526 and a dashed line at zero.
Dataset: Simulated selection into a programme. Outcome `employed`; Treatment `program`; Situation "Treatment not random"; lowest 0, highest 1.
Expect: difference in means 0.427 (s.e. 0.023); four rows: no assumptions -0.288 to 0.712 (width 1.000), monotone response 0.000 to 0.712, monotone selection -0.288 to 0.427, both 0.000 to 0.427 with interval 0.000 to 0.465. Leave the range empty: the same numbers (the observed range is 0 to 1) and a note about the range. Swap the group values (type `no` as treatment group and `yes` as control): the row "Both assumptions" is empty (the lower bound 0 would exceed the upper bound -0.427) and a note says the data contradict it; monotone selection alone then gives -0.712 to -0.427.
Messages: a range that is not a number; outcomes outside the stated range; no treatment variable -> hint.
