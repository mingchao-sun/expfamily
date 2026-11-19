# Generates the .a/.r/.u.yaml files of the five analyses. Run from the module root:  python dev/gen_exp.py
import os

NL = chr(10)


def q(s):
    return "'" + s.replace("'", "''") + "'"


def var(name, title, multi=False, perm=("numeric",), sug=None):
    s = f"    - name: {name}{NL}      title: {q(title)}{NL}      type: {'Variables' if multi else 'Variable'}{NL}"
    if sug:
        s += f"      suggested:{NL}" + "".join(f"          - {x}{NL}" for x in sug)
    s += f"      permitted:{NL}" + "".join(f"          - {x}{NL}" for x in perm) + NL
    return s


def num(name, title, default, mn=None, mx=None, typ="Number"):
    s = f"    - name: {name}{NL}      title: {q(title)}{NL}      type: {typ}{NL}      default: {default}{NL}"
    if mn is not None:
        s += f"      min: {mn}{NL}"
    if mx is not None:
        s += f"      max: {mx}{NL}"
    return s + NL


def txt(name, title, default=""):
    return f"    - name: {name}{NL}      title: {q(title)}{NL}      type: String{NL}      default: {q(default)}{NL}{NL}"


def lst(name, title, opts, default):
    s = f"    - name: {name}{NL}      title: {q(title)}{NL}      type: List{NL}      options:{NL}"
    for n, t in opts:
        s += f"          - name: {n}{NL}            title: {q(t)}{NL}"
    return s + f"      default: {default}{NL}{NL}"


def boo(name, title, default="false"):
    return f"    - name: {name}{NL}      title: {q(title)}{NL}      type: Bool{NL}      default: {default}{NL}{NL}"


def head_a(name, title, mtitle, msub):
    return (f"---{NL}name: {name}{NL}title: {q(title)}{NL}jas: '1.2'{NL}version: '1.0.0'{NL}menuGroup: Experiments{NL}"
            f"menuTitle: {q(mtitle)}{NL}menuSubtitle: {q(msub)}{NL}{NL}options:{NL}    - name: data{NL}      type: Data{NL}{NL}")


def tail_a():
    return boo("showHelp", "Method notes") + boo("showGuide", "Data and setup guide") + "..." + NL


ARMV = lambda: (var("treat", "Treatment (arms)", False, ("numeric", "factor", "id"), ("nominal",)) + txt("control", "Control group value (blank = the first)"))
COVS = lambda: var("covs", "Covariates (baseline)", True, ("numeric", "factor"), ("continuous", "nominal"))
BLOCK = lambda: var("block", "Block (randomization strata)", False, ("numeric", "factor", "id"), ("nominal",))
CLUSTER = lambda: var("cluster", "Cluster", False, ("numeric", "factor", "id"), ("nominal",))


def gen_a(key):
    if key == "exeffect":
        s = head_a("exeffect", "Treatment Effects", "Treatment Effects", "Difference in means, covariate adjustment, blocks and clusters")
        s += var("outcome", "Outcome") + ARMV() + COVS() + BLOCK() + CLUSTER()
        s += lst("method", "Estimator", [("diff", "Difference in means"), ("adjust", "Covariates added (regression adjustment)"), ("lin", "Covariates interacted with the arms (Lin)")], "diff")
        s += lst("se", "Standard errors", [("auto", "Automatic: HC2, or CR2 with clusters"), ("classical", "Classical"), ("hc1", "Robust HC1 (as Stata robust)"), ("hc2", "Robust HC2"),
                                           ("hc3", "Robust HC3"), ("cr1", "Cluster CR1 (as Stata cluster)"), ("cr2", "Cluster CR2 (Bell-McCaffrey)")], "auto")
        s += num("level", "Confidence level (%)", 95, 50, 99.9)
    elif key == "exri":
        s = head_a("exri", "Randomization Inference", "Randomization Inference", "Exact and simulated p-values under the sharp null")
        s += var("outcome", "Outcome") + ARMV() + BLOCK() + CLUSTER()
        s += lst("stat", "Test statistic", [("diff", "Difference in means"), ("rank", "Difference in mean ranks"), ("tstat", "Studentised difference (Welch t)")], "diff")
        s += num("tau", "Hypothesised constant effect", 0)
        s += num("sims", "Number of simulated randomizations", 5000, 100, 500000, "Integer")
        s += num("exactMax", "Enumerate all randomizations up to", 20000, 100, 2000000, "Integer")
        s += num("seed", "Random seed", 1234, 1, 999999, "Integer")
        s += boo("ci", "Confidence interval for a constant effect", "true") + num("level", "Confidence level (%)", 95, 50, 99.9) + boo("plot", "Distribution plot", "true")
    elif key == "exmht":
        s = head_a("exmht", "Multiple Outcomes", "Multiple Outcomes", "Adjusted p-values for several outcomes")
        s += var("outcomes", "Outcomes", True, ("numeric",), ("continuous",)) + ARMV() + txt("armTreat", "Treatment group value (blank = the second)") + COVS() + BLOCK()
        for n, t, d in (("bonf", "Bonferroni", "true"), ("sidak", "Sidak", "false"), ("holm", "Holm", "true"), ("hochberg", "Hochberg", "false"), ("bh", "Benjamini-Hochberg (false discovery rate)", "true"),
                        ("by", "Benjamini-Yekutieli (false discovery rate)", "false"), ("wymin", "Westfall-Young step-down (min-p)", "true"), ("wymax", "Westfall-Young step-down (max-t)", "false"),
                        ("rw", "Romano-Wolf step-down (bootstrap)", "true")):
            s += boo(n, t, d)
        s += num("nperm", "Permutations (Westfall-Young)", 2000, 100, 100000, "Integer") + num("nboot", "Bootstrap samples (Romano-Wolf)", 1000, 100, 50000, "Integer")
        s += num("seed", "Random seed", 1234, 1, 999999, "Integer") + num("alpha", "Significance level for the flags", 0.05, 0.001, 0.5)
    elif key == "exlee":
        s = head_a("exlee", "Bounds under Attrition", "Bounds under Attrition", "Lee (2009) trimming bounds when outcomes are missing")
        s += var("outcome", "Outcome (missing = not observed)") + ARMV() + txt("armTreat", "Treatment group value (blank = the second)")
        s += var("tight", "Covariates to tighten the bounds (categorical)", True, ("factor", "id"), ("nominal",))
        s += lst("direction", "Effect of the treatment on response", [("auto", "Decided by the response rates"), ("up", "Treatment raises the response rate"), ("down", "Treatment lowers the response rate")], "auto")
        s += num("level", "Confidence level (%)", 95, 50, 99.9) + num("nboot", "Bootstrap samples", 1000, 100, 50000, "Integer") + num("seed", "Random seed", 1234, 1, 999999, "Integer")
    elif key == "exbounds":
        s = head_a("exbounds", "Worst-Case Bounds", "Worst-Case Bounds", "Manski bounds on the average effect without or with weak assumptions")
        s += var("outcome", "Outcome") + ARMV() + txt("armTreat", "Treatment group value (blank = the second)")
        s += lst("mode", "Situation", [("random", "Treatment assigned at random; some outcomes are missing"), ("selection", "Treatment not random (units chose it); all outcomes observed")], "random")
        s += txt("ymin", "Lowest possible outcome (blank = lowest observed)") + txt("ymax", "Highest possible outcome (blank = highest observed)")
        s += num("level", "Confidence level (%)", 95, 50, 99.9) + num("nboot", "Bootstrap samples", 1000, 100, 50000, "Integer") + num("seed", "Random seed", 1234, 1, 999999, "Integer") + boo("plot", "Bounds plot", "true")
    elif key == "exsub":
        s = head_a("exsub", "Subgroup Effects", "Subgroup Effects", "Treatment effects by subgroup, interaction tests, adjusted p-values")
        s += var("outcome", "Outcome") + ARMV() + txt("armTreat", "Treatment group value (blank = the second)")
        s += var("subgroups", "Subgroup variables (categorical)", True, ("factor", "id", "numeric"), ("nominal",)) + COVS() + BLOCK() + CLUSTER()
        s += lst("method", "Estimator in each subgroup", [("diff", "Difference in means"), ("adjust", "Covariates and blocks added")], "diff")
        s += lst("se", "Standard errors", [("auto", "Automatic: HC2, or CR1 with clusters"), ("classical", "Classical"), ("hc1", "Robust HC1"), ("hc2", "Robust HC2"), ("hc3", "Robust HC3"), ("cr1", "Cluster CR1")], "auto")
        s += lst("adjust", "Adjust p-values for the number of subgroups", [("holm", "Holm"), ("bonferroni", "Bonferroni"), ("bh", "Benjamini-Hochberg"), ("none", "None")], "holm")
        s += num("level", "Confidence level (%)", 95, 50, 99.9) + boo("plot", "Forest plot", "true")
    elif key == "exfact":
        s = head_a("exfact", "Factorial Design", "Factorial Design", "Two crossed treatments: main effects, simple effects, interaction")
        s += var("outcome", "Outcome")
        s += var("factorA", "Factor A (treatment 1)", False, ("numeric", "factor", "id"), ("nominal",)) + txt("controlA", "Control level of A (blank = the first)")
        s += var("factorB", "Factor B (treatment 2)", False, ("numeric", "factor", "id"), ("nominal",)) + txt("controlB", "Control level of B (blank = the first)")
        s += COVS() + BLOCK() + CLUSTER()
        s += lst("se", "Standard errors", [("auto", "Automatic: HC2, or CR1 with clusters"), ("classical", "Classical"), ("hc1", "Robust HC1"), ("hc2", "Robust HC2"), ("hc3", "Robust HC3"), ("cr1", "Cluster CR1")], "auto")
        s += num("level", "Confidence level (%)", 95, 50, 99.9)
    elif key == "expower":
        s = head_a("expower", "Power and Sample Size", "Power and Sample Size", "Power, minimum detectable effect and sample size")
        s += lst("mode", "Calculate", [("power", "Power"), ("mde", "Minimum detectable effect"), ("n", "Sample size")], "power")
        s += lst("design", "Design", [("indiv", "Individual randomization"), ("cluster", "Cluster randomization")], "indiv")
        s += num("sd", "Standard deviation of the outcome", 1, 0.0000001) + num("effect", "Effect to detect (outcome units)", 0.3)
        s += num("alpha", "Significance level", 0.05, 0.0001, 0.5) + num("target", "Target power", 0.8, 0.5, 0.9999)
        s += lst("sided", "Test", [("two", "Two-sided"), ("one", "One-sided")], "two")
        s += num("share", "Share assigned to treatment", 0.5, 0.01, 0.99) + num("ntotal", "Total number of units (individual design)", 200, 6, None, "Integer")
        s += num("r2", "Share of variance explained by covariates (R2)", 0, 0, 0.99) + num("ncov", "Number of covariates", 0, 0, 200, "Integer")
        s += num("clusters", "Total number of clusters", 40, 6, None, "Integer") + num("msize", "Units per cluster", 25, 1, None, "Number")
        s += num("rho", "Intracluster correlation", 0.05, 0, 0.99) + num("cv", "Coefficient of variation of the cluster sizes", 0, 0, 5)
        s += var("dataOutcome", "Outcome (to take the standard deviation and correlation from)") + var("dataCluster", "Cluster (to take the correlation from)", False, ("numeric", "factor", "id"), ("nominal",))
        s += var("dataCovs", "Covariates (to take R2 from)", True, ("numeric", "factor"), ("continuous", "nominal"))
        s += boo("useData", "Take standard deviation, correlation and R2 from the data") + boo("plot", "Power curve", "true")
    else:
        s = head_a("exbalance", "Balance and Attrition", "Balance and Attrition", "Baseline balance, joint test and attrition")
        s += ARMV() + COVS() + BLOCK() + var("outcome", "Outcome (for attrition)")
        s += num("nperm", "Permutations for the joint test", 2000, 100, 100000, "Integer") + num("seed", "Random seed", 1234, 1, 999999, "Integer")
        s += num("flag", "Flag standardised differences above", 0.1, 0.01, 2)
    return s + tail_a()


def col(name, title, typ, extra=""):
    return f"        - name: {name}{NL}          title: {q(title)}{NL}          type: {typ}{NL}" + (extra + NL if extra else "")


def tbl(name, title, clear, cols, visible=None):
    s = f"    - name: {name}{NL}      title: {q(title)}{NL}      type: Table{NL}      rows: 0{NL}"
    if visible:
        s += f"      visible: {visible}{NL}"
    s += f"      clearWith: [{clear}]{NL}      columns:{NL}" + "".join(cols) + NL
    return s


def gen_r(key, title):
    s = (f"---{NL}name: {key}{NL}title: {q(title)}{NL}jrs: '1.1'{NL}{NL}items:{NL}"
         f"    - name: guide{NL}      title: Data and setup guide{NL}      type: Html{NL}      visible: (showGuide){NL}{NL}"
         f"    - name: helpText{NL}      title: Method notes{NL}      type: Html{NL}      visible: (showHelp){NL}{NL}"
         f"    - name: notes{NL}      title: Notes{NL}      type: Html{NL}      visible: true{NL}{NL}")
    if key == "exeffect":
        c = "outcome, treat, control, covs, block, cluster, method, se, level"
        s += tbl("desc", "Outcome by group", "outcome, treat, control", [col("arm", "Group", "text"), col("n", "N", "integer"), col("mean", "Mean", "number"), col("sd", "Std. deviation", "number")])
        s += tbl("est", "Effect of each treatment against the control", c, [col("arm", "Treatment", "text"), col("estimate", "Effect", "number"), col("se", "Std. error", "number"), col("t", "t", "number"),
                                                                              col("df", "df", "number"), col("p", "p", "number", "          format: zto,pvalue"), col("lo", "CI lower", "number"), col("hi", "CI upper", "number"),
                                                                              col("std", "Effect in control SDs", "number")])
        refs = "lin2013, bellmccaffrey2002, pustejovskytipton2018, estimatr"
    elif key == "exri":
        c = "outcome, treat, control, block, cluster, stat, tau, sims, exactMax, seed, ci, level"
        s += tbl("res", "Randomization test", c, [col("arm", "Comparison", "text"), col("stat", "Statistic", "number"), col("p2", "p (two-sided)", "number", "          format: zto,pvalue"),
                                                    col("pr", "p (treated larger)", "number", "          format: zto,pvalue"), col("pl", "p (treated smaller)", "number", "          format: zto,pvalue"),
                                                    col("nrand", "Randomizations", "integer"), col("how", "Method", "text"), col("lo", "CI lower", "number"), col("hi", "CI upper", "number")])
        s += (f"    - name: riPlot{NL}      title: 'Distribution of the statistic under the sharp null'{NL}      type: Image{NL}      width: 560{NL}      height: 360{NL}"
              f"      renderFun: .plotRi{NL}      visible: (plot){NL}      clearWith: [{c}]{NL}{NL}")
        refs = "fisher1935, rosenbaum2002, ri2"
    elif key == "exmht":
        c = "outcomes, treat, control, armTreat, covs, block, bonf, sidak, holm, hochberg, bh, by, wymin, wymax, rw, nperm, nboot, seed, alpha"
        cols = [col("outcome", "Outcome", "text"), col("estimate", "Effect", "number"), col("se", "Std. error", "number"), col("p", "Unadjusted p", "number", "          format: zto,pvalue")]
        for n, t in (("bonf", "Bonferroni"), ("sidak", "Sidak"), ("holm", "Holm"), ("hochberg", "Hochberg"), ("bh", "Benjamini-Hochberg"), ("by", "Benjamini-Yekutieli"),
                     ("wymin", "Westfall-Young (min-p)"), ("wymax", "Westfall-Young (max-t)"), ("rw", "Romano-Wolf")):
            cols.append(col("a_" + n, t, "number", f"          format: zto,pvalue{NL}          visible: ({n})"))
        s += tbl("res", "Adjusted p-values", c, cols)
        refs = "holm1979, benjamini1995, westfallyoung1993, romanowolf2005, clarke2020"
    elif key == "exlee":
        c = "outcome, treat, control, armTreat, tight, direction, level, nboot, seed"
        s += tbl("sel", "Response by group", "outcome, treat, control, armTreat, tight", [col("arm", "Group", "text"), col("n", "N", "integer"), col("obs", "Outcome observed", "integer"), col("rate", "Response rate", "number"), col("mean", "Mean of the observed", "number")])
        s += tbl("res", "Effect of the treatment", c, [col("what", "", "text"), col("lower", "Lower bound", "number"), col("upper", "Upper bound", "number"), col("sel", "Std. error (lower)", "number"),
                                                         col("seu", "Std. error (upper)", "number"), col("lo", "CI lower", "number"), col("hi", "CI upper", "number")])
        refs = "lee2009, imbensmanski2004"
    elif key == "exbounds":
        c = "outcome, treat, control, armTreat, mode, ymin, ymax, level, nboot, seed"
        s += tbl("ref", "What the data show", c, [col("what", "", "text"), col("value", "", "number")])
        s += tbl("res", "Bounds on the average treatment effect", c, [col("what", "Assumptions", "text"), col("lower", "Lower bound", "number"), col("upper", "Upper bound", "number"), col("width", "Width", "number"),
                                                                       col("sel", "Std. error (lower)", "number"), col("seu", "Std. error (upper)", "number"), col("lo", "CI lower", "number"), col("hi", "CI upper", "number")])
        s += tbl("means", "Bounds on the average potential outcomes", c, [col("what", "Assumptions", "text"), col("e1lo", "Treated: lower", "number"), col("e1hi", "Treated: upper", "number"), col("e0lo", "Control: lower", "number"), col("e0hi", "Control: upper", "number")])
        s += (f"    - name: bndPlot{NL}      title: 'Bounds on the average treatment effect'{NL}      type: Image{NL}      width: 560{NL}      height: 340{NL}"
              f"      renderFun: .plotBounds{NL}      visible: (plot){NL}      clearWith: [{c}]{NL}{NL}")
        refs = "manski1990, manskipepper2000, imbensmanski2004"
    elif key == "exsub":
        c = "outcome, treat, control, armTreat, subgroups, covs, block, cluster, method, se, adjust, level"
        s += tbl("overall", "Effect in the whole sample", c, [col("what", "", "text"), col("n", "N", "integer"), col("estimate", "Effect", "number"), col("se", "Std. error", "number"), col("p", "p", "number", "          format: zto,pvalue"), col("lo", "CI lower", "number"), col("hi", "CI upper", "number")])
        s += tbl("eff", "Treatment effect by subgroup", c, [col("variable", "Variable", "text"), col("level", "Subgroup", "text"), col("n", "N", "integer"), col("n1", "Treated", "integer"), col("n0", "Control", "integer"),
                                                              col("estimate", "Effect", "number"), col("se", "Std. error", "number"), col("p", "p", "number", "          format: zto,pvalue"), col("padj", "Adjusted p", "number", "          format: zto,pvalue"),
                                                              col("lo", "CI lower", "number"), col("hi", "CI upper", "number")])
        s += tbl("inter", "Does the effect differ between the subgroups of a variable? (treatment x subgroup)", c, [col("variable", "Variable", "text"), col("F", "F", "number"), col("df1", "df1", "integer"), col("df2", "df2", "number"),
                                                                                                                      col("p", "p", "number", "          format: zto,pvalue"), col("padj", "Adjusted p", "number", "          format: zto,pvalue")])
        s += (f"    - name: subPlot{NL}      title: 'Treatment effect by subgroup'{NL}      type: Image{NL}      width: 560{NL}      height: 420{NL}"
              f"      renderFun: .plotSub{NL}      visible: (plot){NL}      clearWith: [{c}]{NL}{NL}")
        refs = "estimatr, wager2018, holm1979, benjamini1995"
    elif key == "exfact":
        c = "outcome, factorA, controlA, factorB, controlB, covs, block, cluster, se, level"
        s += tbl("cells", "Cell means", c, [col("A", "Factor A", "text"), col("B", "Factor B", "text"), col("n", "N", "integer"), col("mean", "Mean", "number"), col("sd", "Std. deviation", "number")])
        s += tbl("main", "Main effects (averaged over the other factor, equal weights)", c, [col("lab", "Effect", "text"), col("estimate", "Estimate", "number"), col("se", "Std. error", "number"), col("t", "t", "number"), col("df", "df", "number"), col("p", "p", "number", "          format: zto,pvalue"), col("lo", "CI lower", "number"), col("hi", "CI upper", "number")])
        s += tbl("simple", "Simple effects (within each level of the other factor)", c, [col("lab", "Effect", "text"), col("estimate", "Estimate", "number"), col("se", "Std. error", "number"), col("t", "t", "number"), col("df", "df", "number"), col("p", "p", "number", "          format: zto,pvalue"), col("lo", "CI lower", "number"), col("hi", "CI upper", "number")])
        s += tbl("inter", "Interaction: does the effect of one factor depend on the other?", c, [col("lab", "Effect", "text"), col("estimate", "Estimate", "number"), col("se", "Std. error", "number"), col("t", "t", "number"), col("df", "df", "number"), col("p", "p", "number", "          format: zto,pvalue"), col("lo", "CI lower", "number"), col("hi", "CI upper", "number")])
        s += tbl("ign", "Effect of each factor ignoring the other", c, [col("lab", "Effect", "text"), col("estimate", "Estimate", "number"), col("se", "Std. error", "number"), col("t", "t", "number"), col("df", "df", "number"), col("p", "p", "number", "          format: zto,pvalue"), col("lo", "CI lower", "number"), col("hi", "CI upper", "number")])
        refs = "estimatr, muralidharan2019"
    elif key == "expower":
        c = ("mode, design, sd, effect, alpha, target, sided, share, ntotal, r2, ncov, clusters, msize, rho, cv, dataOutcome, dataCluster, dataCovs, useData")
        s += tbl("inputs", "Inputs used", c, [col("item", "", "text"), col("value", "", "number")])
        s += tbl("res", "Result", c, [col("item", "", "text"), col("value", "", "number")])
        s += (f"    - name: powPlot{NL}      title: 'Power against the sample size'{NL}      type: Image{NL}      width: 520{NL}      height: 340{NL}"
              f"      renderFun: .plotPower{NL}      visible: (plot){NL}      clearWith: [{c}]{NL}{NL}")
        refs = "cohen1988, eldridge2006"
    else:
        c = "treat, control, covs, block, outcome, nperm, seed, flag"
        s += tbl("bal", "Baseline covariates by group", c, [col("covariate", "Covariate", "text"), col("arm", "Treatment", "text"), col("m0", "Control mean", "number"), col("m1", "Treatment mean", "number"),
                                                                col("diff", "Difference", "number"), col("std", "Standardised difference", "number"), col("p", "p (Welch t)", "number", "          format: zto,pvalue")])
        s += tbl("joint", "Joint test of balance", c, [col("arm", "Treatment", "text"), col("F", "F", "number"), col("df1", "df1", "integer"), col("df2", "df2", "integer"), col("p", "p (F test)", "number", "          format: zto,pvalue"),
                                                        col("pri", "p (randomization)", "number", "          format: zto,pvalue"), col("n", "N", "integer")])
        s += tbl("att", "Response by group (outcome observed)", c, [col("arm", "Group", "text"), col("n", "N", "integer"), col("obs", "Outcome observed", "integer"), col("rate", "Response rate", "number"),
                                                                    col("diff", "Difference from control", "number"), col("p", "p (Welch t)", "number", "          format: zto,pvalue")])
        s += tbl("attj", "Do covariates predict response differently across groups", c, [col("F", "F", "number"), col("df1", "df1", "integer"), col("df2", "df2", "integer"), col("p", "p", "number", "          format: zto,pvalue")])
        refs = "bruhn2009, hansenbowers2008"
    return s + f"refs:{NL}" + "".join(f"    - {r.strip()}{NL}" for r in refs.split(",")) + f"{NL}...{NL}"


def box(label, name, single=True):
    s = f"          - type: TargetLayoutBox{NL}            label: {q(label)}{NL}            children:{NL}                - type: VariablesListBox{NL}                  name: {name}{NL}                  isTarget: true{NL}                  height: small{NL}"
    if single:
        s += f"                  maxItemCount: 1{NL}"
    return s


def tb(name, label):
    return f"          - type: TextBox{NL}            name: {name}{NL}            label: {q(label)}{NL}            format: number{NL}"


def cb(name):
    return f"          - type: CheckBox{NL}            name: {name}{NL}"


def cmb(name):
    return f"          - type: ComboBox{NL}            name: {name}{NL}"


def collapse(label, body, collapsed=True):
    return f"    - type: CollapseBox{NL}      label: {q(label)}{NL}      collapsed: {str(collapsed).lower()}{NL}      children:{NL}" + body


def gen_u(key, title):
    boxes = {"exeffect": [("Outcome", "outcome", 1), ("Treatment (arms)", "treat", 1), ("Covariates (baseline)", "covs", 0), ("Block (randomization strata)", "block", 1), ("Cluster", "cluster", 1)],
             "exri": [("Outcome", "outcome", 1), ("Treatment (arms)", "treat", 1), ("Block (randomization strata)", "block", 1), ("Cluster", "cluster", 1)],
             "exmht": [("Outcomes", "outcomes", 0), ("Treatment (arms)", "treat", 1), ("Covariates (baseline)", "covs", 0), ("Block (randomization strata)", "block", 1)],
             "exlee": [("Outcome (missing = not observed)", "outcome", 1), ("Treatment (arms)", "treat", 1), ("Covariates to tighten the bounds", "tight", 0)],
             "exbounds": [("Outcome", "outcome", 1), ("Treatment (arms)", "treat", 1)],
             "exsub": [("Outcome", "outcome", 1), ("Treatment (arms)", "treat", 1), ("Subgroup variables (categorical)", "subgroups", 0), ("Covariates (baseline)", "covs", 0), ("Block (randomization strata)", "block", 1), ("Cluster", "cluster", 1)],
             "exfact": [("Outcome", "outcome", 1), ("Factor A (treatment 1)", "factorA", 1), ("Factor B (treatment 2)", "factorB", 1), ("Covariates (baseline)", "covs", 0), ("Block (randomization strata)", "block", 1), ("Cluster", "cluster", 1)],
             "expower": [("Outcome (for the standard deviation)", "dataOutcome", 1), ("Cluster (for the correlation)", "dataCluster", 1), ("Covariates (for R2)", "dataCovs", 0)],
             "exbalance": [("Treatment (arms)", "treat", 1), ("Covariates (baseline)", "covs", 0), ("Block (randomization strata)", "block", 1), ("Outcome (for attrition)", "outcome", 1)]}[key]
    s = (f"title: {q(title)}{NL}name: {key}{NL}jus: '3.0'{NL}stage: 0{NL}compilerMode: tame{NL}children:{NL}"
         f"    - type: VariableSupplier{NL}      persistentItems: false{NL}      stretchFactor: 1{NL}      children:{NL}")
    for lab, nm, sg in boxes:
        s += box(lab, nm, bool(sg))
    if key == "exeffect":
        s += collapse("Estimation", tb("control", "Control group value") .replace("format: number", "format: string") + cmb("method") + cmb("se") + tb("level", "Confidence level (%)"), False)
    elif key == "exri":
        s += collapse("Estimation", tb("control", "Control group value").replace("format: number", "format: string") + cmb("stat") + tb("tau", "Hypothesised constant effect") + tb("sims", "Number of simulated randomizations")
                      + tb("exactMax", "Enumerate all randomizations up to") + tb("seed", "Random seed") + cb("ci") + tb("level", "Confidence level (%)") + cb("plot"), False)
    elif key == "exmht":
        s += collapse("Adjustment methods", "".join(cb(n) for n in ("bonf", "sidak", "holm", "hochberg", "bh", "by", "wymin", "wymax", "rw")), False)
        s += collapse("Estimation", tb("control", "Control group value").replace("format: number", "format: string") + tb("armTreat", "Treatment group value").replace("format: number", "format: string") + tb("nperm", "Permutations (Westfall-Young)") + tb("nboot", "Bootstrap samples (Romano-Wolf)")
                      + tb("seed", "Random seed") + tb("alpha", "Significance level for the flags"))
    elif key == "exlee":
        s += collapse("Estimation", tb("control", "Control group value").replace("format: number", "format: string") + tb("armTreat", "Treatment group value").replace("format: number", "format: string") + cmb("direction")
                      + tb("level", "Confidence level (%)") + tb("nboot", "Bootstrap samples") + tb("seed", "Random seed"), False)
    elif key == "exbounds":
        s += collapse("Situation and range of the outcome", cmb("mode") + tb("ymin", "Lowest possible outcome").replace("format: number", "format: string") + tb("ymax", "Highest possible outcome").replace("format: number", "format: string"), False)
        s += collapse("Estimation", tb("control", "Control group value").replace("format: number", "format: string") + tb("armTreat", "Treatment group value").replace("format: number", "format: string") + tb("level", "Confidence level (%)") + tb("nboot", "Bootstrap samples") + tb("seed", "Random seed") + cb("plot"))
    elif key == "exsub":
        s += collapse("Estimation", tb("control", "Control group value").replace("format: number", "format: string") + tb("armTreat", "Treatment group value").replace("format: number", "format: string") + cmb("method") + cmb("se") + cmb("adjust") + tb("level", "Confidence level (%)") + cb("plot"), False)
    elif key == "exfact":
        s += collapse("Estimation", tb("controlA", "Control level of A").replace("format: number", "format: string") + tb("controlB", "Control level of B").replace("format: number", "format: string") + cmb("se") + tb("level", "Confidence level (%)"), False)
    elif key == "expower":
        s += collapse("Design", cmb("mode") + cmb("design") + tb("sd", "Standard deviation of the outcome") + tb("effect", "Effect to detect (outcome units)") + tb("alpha", "Significance level")
                      + tb("target", "Target power") + cmb("sided") + tb("share", "Share assigned to treatment") + tb("ntotal", "Total number of units (individual design)")
                      + tb("r2", "Share of variance explained by covariates (R2)") + tb("ncov", "Number of covariates"), False)
        s += collapse("Cluster randomization", tb("clusters", "Total number of clusters") + tb("msize", "Units per cluster") + tb("rho", "Intracluster correlation") + tb("cv", "Coefficient of variation of the cluster sizes"))
        s += collapse("Plot and data", cb("useData") + cb("plot"))
    else:
        s += collapse("Estimation", tb("control", "Control group value").replace("format: number", "format: string") + tb("nperm", "Permutations for the joint test") + tb("seed", "Random seed") + tb("flag", "Flag standardised differences above"), False)
    s += collapse("Output", f"          - type: CheckBox{NL}            name: showHelp{NL}            label: Method notes{NL}          - type: CheckBox{NL}            name: showGuide{NL}            label: Data and setup guide{NL}")
    return s


TITLES = {"exeffect": "Treatment Effects", "exri": "Randomization Inference", "exmht": "Multiple Outcomes", "exlee": "Bounds under Attrition", "exbounds": "Worst-Case Bounds", "exsub": "Subgroup Effects", "exfact": "Factorial Design", "expower": "Power and Sample Size", "exbalance": "Balance and Attrition"}
for k, t in TITLES.items():
    for ext, txt_ in (("a", gen_a(k)), ("r", gen_r(k, t)), ("u", gen_u(k, t))):
        with open(f"jamovi/{k}.{ext}.yaml", "w", encoding="utf-8", newline="") as f:
            f.write(txt_)
print("written")
