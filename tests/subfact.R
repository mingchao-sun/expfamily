# Subgroup effects and factorial designs against estimatr (coefficients, robust variances, linear hypotheses) and lmtest/sandwich (Wald F tests).
# Needs estimatr, lmtest and sandwich in a library named by EXP_REFLIB. Run from the module root:  Rscript tests/subfact.R
lib <- Sys.getenv("EXP_REFLIB"); if (nzchar(lib)) .libPaths(c(lib, .libPaths()))
for (f in c("R/common.R", "R/engine_effects.R", "R/engine_sub.R")) source(f)
if (!all(vapply(c("estimatr", "lmtest", "sandwich"), requireNamespace, TRUE, quietly = TRUE))) { cat("estimatr, lmtest or sandwich missing: skipped\n"); quit(status = 0) }
nfail <- 0
cmp <- function(label, a, b, tol = 1e-9) { ok <- isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol)); cat(sprintf("[%s] %s  (max diff %.1e)\n", if (ok) "PASS" else "FAIL", label, max(abs(as.numeric(a) - as.numeric(b))))); if (!ok) nfail <<- nfail + 1 }
ok <- function(cond, msg) { cat(sprintf("[%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", msg)); if (!isTRUE(cond)) nfail <<- nfail + 1 }

set.seed(31); n <- 500
female <- factor(sample(c("man", "woman"), n, TRUE)); region <- factor(sample(c("north", "south", "east"), n, TRUE, c(.4, .35, .25)))
arm <- factor(sample(c("control", "treated"), n, TRUE), levels = c("control", "treated")); age <- rnorm(n, 40, 10); blk <- factor(sample(1:4, n, TRUE))
y <- 1 + 0.4 * (arm == "treated") + 0.5 * (arm == "treated") * (female == "woman") - 0.3 * (arm == "treated") * (region == "east") + 0.02 * age + rnorm(n)
dat <- data.frame(y, arm, female, region, age, blk)

# ---- subgroup effects
for (se in c("hc2", "hc1", "hc3", "classical")) {
    r <- ex_subgroup(y, arm, list(female = female, region = region), method = "diff", se_type = se, adjust = "none")
    ref <- lapply(c("man", "woman"), function(l) estimatr::lm_robust(y ~ arm, data = dat[female == l, ], se_type = if (se == "classical") "classical" else toupper(se)))
    cmp(sprintf("subgroup effects (female), %s: estimates and s.e.", se), c(r$effects$estimate[1:2], r$effects$se[1:2]), c(sapply(ref, function(m) coef(m)[2]), sapply(ref, function(m) m$std.error[2])))
}
r <- ex_subgroup(y, arm, list(female = female, region = region), X = cbind(age = age), block = blk, method = "adjust", se_type = "hc2", adjust = "none")
ref <- estimatr::lm_robust(y ~ arm + age + blk, data = dat[region == "east", ], se_type = "HC2")
cmp("subgroup effect with covariates and block fixed effects (region east)", c(r$effects$estimate[r$effects$level == "east"], r$effects$se[r$effects$level == "east"]), c(coef(ref)["armtreated"], ref$std.error["armtreated"]))
cmp("overall effect row equals the whole-sample estimate", r$overall$estimate, coef(estimatr::lm_robust(y ~ arm + age + blk, data = dat, se_type = "HC2"))["armtreated"])
# interaction tests: Wald F against lmtest::waldtest with sandwich HC2
for (v in c("female", "region")) {
    rr <- ex_subgroup(y, arm, setNames(list(get(v)), v), method = "diff", se_type = "hc2")
    full <- lm(as.formula(sprintf("y ~ arm * %s", v)), data = dat); restr <- lm(as.formula(sprintf("y ~ arm + %s", v)), data = dat)
    w <- lmtest::waldtest(restr, full, vcov = function(x) sandwich::vcovHC(x, type = "HC2"), test = "F")
    cmp(sprintf("interaction test, %s: F and p equal lmtest::waldtest (HC2)", v), c(rr$interaction$F, rr$interaction$p), c(w$F[2], w[["Pr(>F)"]][2]))
}
# adjustment of p-values
ra <- ex_subgroup(y, arm, list(female = female, region = region), method = "diff", adjust = "holm"); rb <- ex_subgroup(y, arm, list(female = female, region = region), method = "diff", adjust = "none")
cmp("Holm adjustment equals p.adjust over all subgroup effects", ra$effects$p_adj, p.adjust(rb$effects$p, "holm"))
cmp("Benjamini-Hochberg adjustment of the interaction tests", ex_subgroup(y, arm, list(female = female, region = region), adjust = "bh")$interaction$p_adj, p.adjust(rb$interaction$p, "BH"))
# clusters
cl <- sample(1:40, n, TRUE); rc <- ex_subgroup(y, arm, list(female = female), cluster = cl, se_type = "auto", adjust = "none")
dat$cl <- cl; refc <- estimatr::lm_robust(y ~ arm, data = dat[female == "woman", ], clusters = cl, se_type = "stata")
cmp("clustered subgroup effect equals estimatr CR1 (stata)", c(rc$effects$estimate[rc$effects$level == "woman"], rc$effects$se[rc$effects$level == "woman"], rc$effects$df[rc$effects$level == "woman"]), c(coef(refc)[2], refc$std.error[2], refc$df[2]))

# ---- factorial design
set.seed(32); m <- 480
A <- factor(sample(c("none", "info", "reminder"), m, TRUE), levels = c("none", "info", "reminder")); B <- factor(sample(c("no", "yes"), m, TRUE), levels = c("no", "yes"))
x <- rnorm(m); yy <- 0.5 + 0.4 * (A == "info") + 0.7 * (A == "reminder") + 0.3 * (B == "yes") + 0.5 * (A == "reminder") * (B == "yes") + 0.3 * x + rnorm(m)
fd <- data.frame(yy, A, B, x)
fa <- ex_factorial(yy, A, B, se_type = "hc2")
fit <- estimatr::lm_robust(yy ~ A * B, data = fd, se_type = "HC2")
# coding: lm uses treatment contrasts; Ainfo is the effect of info when B = no
simp <- fa$simple
cmp("simple effects when B = no: info and reminder against none", simp$estimate[simp$effect %in% c("info vs none, when B = no", "reminder vs none, when B = no")], coef(fit)[c("Ainfo", "Areminder")])
cmp("simple effects when B = no: standard errors", simp$se[1:2], fit$std.error[c("Ainfo", "Areminder")])
cmp("simple effect of B when A = none equals the B coefficient (estimate and s.e.)", c(simp$estimate[simp$effect == "yes vs no, when A = none"], simp$se[simp$effect == "yes vs no, when A = none"]), c(coef(fit)["Byes"], fit$std.error["Byes"]))
lh2 <- estimatr::lh_robust(yy ~ A * B, data = fd, se_type = "HC2", linear_hypothesis = c("Areminder + Areminder:Byes = 0", "Byes + Areminder:Byes = 0", "Areminder:Byes = 0"))
cmp("simple effect of reminder when B = yes (linear hypothesis, estimate and s.e.)", unlist(simp[simp$effect == "reminder vs none, when B = yes", c("estimate", "se")]), unlist(lh2$lh[1, c("coefficients", "std.error")]), 1e-8)
cmp("simple effect of B when A = reminder", unlist(simp[simp$effect == "yes vs no, when A = reminder", c("estimate", "se")]), unlist(lh2$lh[2, c("coefficients", "std.error")]), 1e-8)
cmp("interaction contrast (reminder x yes) equals the coefficient", unlist(fa$interaction[fa$interaction$effect == "(reminder vs none) x (yes vs no)", c("estimate", "se")]), c(coef(fit)["Areminder:Byes"], fit$std.error["Areminder:Byes"]))
lh3 <- estimatr::lh_robust(yy ~ A * B, data = fd, se_type = "HC2", linear_hypothesis = c("Areminder + 0.5 * Areminder:Byes = 0", "Byes + 0.3333333333333333 * Ainfo:Byes + 0.3333333333333333 * Areminder:Byes = 0"))
cmp("main effect of reminder (average over B, equal weights)", unlist(fa$main[fa$main$effect == "A: reminder vs none, averaged over B", c("estimate", "se")]), unlist(lh3$lh[1, c("coefficients", "std.error")]), 1e-8)
cmp("main effect of B (average over the three A levels)", unlist(fa$main[fa$main$effect == "B: yes vs no, averaged over A", c("estimate", "se")]), unlist(lh3$lh[2, c("coefficients", "std.error")]), 1e-8)
full <- lm(yy ~ A * B, data = fd); restr <- lm(yy ~ A + B, data = fd)
w <- lmtest::waldtest(restr, full, vcov = function(x) sandwich::vcovHC(x, type = "HC2"), test = "F")
cmp("joint test of the interactions equals lmtest::waldtest (HC2)", c(fa$joint[["F"]], fa$joint[["p"]]), c(w$F[2], w[["Pr(>F)"]][2]))
ign <- estimatr::lm_robust(yy ~ B, data = fd, se_type = "HC2")
cmp("effect of B ignoring A equals the simple comparison", unlist(fa$ignoring[fa$ignoring$effect == "B: yes vs no, ignoring the other factor", c("estimate", "se")]), c(coef(ign)["Byes"], ign$std.error["Byes"]))
# covariates: the cell effects are adjusted
fx <- ex_factorial(yy, A, B, X = cbind(x = x), se_type = "hc2")
fitx <- estimatr::lm_robust(yy ~ A * B + x, data = fd, se_type = "HC2")
cmp("with a covariate: interaction coefficient and s.e.", unlist(fx$interaction[fx$interaction$effect == "(reminder vs none) x (yes vs no)", c("estimate", "se")]), c(coef(fitx)["Areminder:Byes"], fitx$std.error["Areminder:Byes"]))
ok(fx$interaction$se[1] < fa$interaction$se[1], "adjusting for a covariate that predicts the outcome shrinks the standard error")
cells <- fa$cells; ok(nrow(cells) == 6 && sum(cells$n) == m && abs(cells$mean[1] - mean(yy[A == "none" & B == "no"])) < 1e-12, "cell table: six cells, counts and means")
ok(inherits(try(ex_factorial(yy, A, factor(rep("a", m))), silent = TRUE), "try-error"), "a factor with one level is refused")
ok(inherits(try(ex_subgroup(y, factor(sample(c("a", "b", "c"), n, TRUE)), list(female = female)), silent = TRUE), "try-error"), "subgroup effects need exactly two treatment groups")
cat(if (nfail == 0) "\nAll subgroup and factorial checks passed.\n" else sprintf("\n%d check(s) FAILED.\n", nfail))
if (nfail > 0) quit(status = 1)
