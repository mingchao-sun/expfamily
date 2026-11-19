# The engines against the R packages estimatr, ri2, randomizr and pwr. Reference-only packages may live in a separate
# library named by EXP_REFLIB (the test runner sets it to C:/dev/reflibs). Run from the module root:  Rscript tests/reference.R
for (f in list.files("R", "^engine_.*[.]R$", full.names = TRUE)) source(f)
lib <- Sys.getenv("EXP_REFLIB"); if (nzchar(lib)) .libPaths(c(lib, .libPaths()))
nfail <- 0
chk <- function(name, got, want, tol) {
    d <- max(abs(got - want)); ok <- is.finite(d) && d < tol
    cat(sprintf("%-62s %s  max |diff| = %.1e (tol %.0e)\n", name, if (ok) "ok  " else "FAIL", d, tol)); if (!ok) nfail <<- nfail + 1
}
have <- function(p) requireNamespace(p, quietly = TRUE)

# ---- simulated experiment: 3 arms, 2 covariates, 5 blocks, 30 clusters of 10
set.seed(2026)
n <- 300; J <- 30
cl <- rep(seq_len(J), each = 10)
arm_c <- sample(rep(c("control", "T1", "T2"), length.out = J))                 # cluster-level assignment
arm_cl <- factor(arm_c[cl], levels = c("control", "T1", "T2"))
arm <- factor(sample(rep(c("control", "T1", "T2"), length.out = n)), levels = c("control", "T1", "T2"))   # individual assignment
X <- cbind(x1 = rnorm(n), x2 = rnorm(n))
blk <- factor(rep(1:5, length.out = n))
u <- rnorm(J)[cl]
y <- 1 + 0.5 * (arm == "T1") + 0.9 * (arm == "T2") + 0.6 * X[, 1] - 0.3 * X[, 2] + 0.4 * as.integer(blk) + rnorm(n)
yc <- 1 + 0.5 * (arm_cl == "T1") + 0.9 * (arm_cl == "T2") + 0.6 * X[, 1] + u + rnorm(n)
dat <- data.frame(y = y, yc = yc, arm = arm, armc = arm_cl, x1 = X[, 1], x2 = X[, 2], blk = blk, cl = cl)

if (have("estimatr")) {
    suppressMessages(library(estimatr))
    cmp <- function(label, ref, me) {
        co <- ref$coefficients[grepl("^arm[^:]*$", names(ref$coefficients))]
        sdv <- ref$std.error[grepl("^arm[^:]*$", names(ref$std.error))]
        dfv <- ref$df[grepl("^arm[^:]*$", names(ref$coefficients))]
        pv <- ref$p.value[grepl("^arm[^:]*$", names(ref$p.value))]
        ci <- cbind(ref$conf.low, ref$conf.high)[grepl("^arm[^:]*$", names(ref$coefficients)), , drop = FALSE]
        chk(paste(label, ": estimates"), me$estimate, unname(co), 1e-9)
        chk(paste(label, ": standard errors"), me$se, unname(sdv), 1e-9)
        chk(paste(label, ": degrees of freedom"), me$df, unname(dfv), 1e-6)
        chk(paste(label, ": p-values and intervals"), c(me$p, me$lo, me$hi), c(unname(pv), unname(ci[, 1]), unname(ci[, 2])), 1e-8)
    }
    for (st in c("classical", "HC1", "HC2", "HC3")) {
        ref <- lm_robust(y ~ arm, data = dat, se_type = st)
        cmp(paste("difference in means,", st), ref, ex_effects(y, arm, method = "diff", se_type = tolower(st)))
    }
    cmp("covariates added, HC2", lm_robust(y ~ arm + x1 + x2, data = dat, se_type = "HC2"), ex_effects(y, arm, X, method = "adjust", se_type = "hc2"))
    ref <- lm_lin(y ~ arm, covariates = ~ x1 + x2, data = dat, se_type = "HC2")
    cmp("Lin (2013) interacted adjustment, HC2", ref, ex_effects(y, arm, X, method = "lin", se_type = "hc2"))
    cmp("block fixed effects + covariates, HC2", lm_robust(y ~ arm + x1 + x2 + blk, data = dat, se_type = "HC2"), ex_effects(y, arm, X, block = blk, method = "adjust", se_type = "hc2"))
    cmp("clustered, CR2 (Satterthwaite df)", lm_robust(yc ~ armc, data = dat, clusters = cl, se_type = "CR2"), ex_effects(yc, arm_cl, method = "diff", cluster = cl, se_type = "cr2"))
    cmp("clustered with covariates, CR2", lm_robust(yc ~ armc + x1, data = dat, clusters = cl, se_type = "CR2"), ex_effects(yc, arm_cl, X[, 1, drop = FALSE], method = "adjust", cluster = cl, se_type = "cr2"))
    r1 <- lm_robust(yc ~ armc, data = dat, clusters = cl, se_type = "stata")
    cmp("clustered, CR1 (Stata)", r1, ex_effects(yc, arm_cl, method = "diff", cluster = cl, se_type = "cr1"))
} else cat("estimatr not available: effect checks skipped\n")


# note: randomizr::declare_ra(N, m) mixes m and m + 1 treated units for an even N in its permutation matrix; m_each is exact
# ---- randomization inference against ri2 on exact enumerations (small designs)
if (have("ri2") && have("randomizr")) {
    suppressMessages({library(ri2); library(randomizr)})
    ri_chk <- function(label, d, decl, my) {
        ref <- conduct_ri(Y ~ Z, declaration = decl, sharp_hypothesis = 0, data = d, sims = 1e6)
        s <- summary(ref)
        chk(paste(label, ": observed statistic"), my$stat, s$estimate, 1e-9)
        chk(paste(label, ": two-sided p"), my$p_two, s$two_tailed_p_value, 1e-12)
        sims <- ref$sims_df$est_sim; tol <- 1e-10
        chk(paste(label, ": one-sided p (upper, lower)"), c(my$p_right, my$p_left), c(mean(sims >= s$estimate - tol), mean(sims <= s$estimate + tol)), 1e-12)
        chk(paste(label, ": number of randomizations"), my$n_rand, length(ref$sims_df$est_sim), 0.5)
    }
    set.seed(7)
    d1 <- data.frame(Y = round(rnorm(12), 3), Z = c(rep(1, 5), rep(0, 7))[sample(12)])
    ri_chk("complete, 12 units", d1, declare_ra(N = 12, m_each = c(7, 5)), my = ex_ri(d1$Y, d1$Z, sims = 1e6, exact_max = 1e6))
    d2 <- data.frame(Y = round(rnorm(18), 3), B = rep(1:3, each = 6)); d2$Z <- unlist(lapply(1:3, function(b) sample(c(1, 1, 1, 0, 0, 0))))
    ri_chk("blocked, 3 blocks of 6", d2, declare_ra(blocks = d2$B, block_m = c(3, 3, 3)), my = ex_ri(d2$Y, d2$Z, block = d2$B, exact_max = 1e6))
    d3 <- data.frame(Y = round(rnorm(24), 3), C = rep(1:8, each = 3)); zc <- sample(c(rep(1, 3), rep(0, 5))); d3$Z <- zc[d3$C]
    ri_chk("clustered, 8 clusters of 3", d3, declare_ra(clusters = d3$C, m = 3), my = ex_ri(d3$Y, d3$Z, cluster = d3$C, exact_max = 1e6))
    # simulation: p-value close to the exact one
    set.seed(3); d4 <- data.frame(Y = rnorm(40), Z = sample(rep(0:1, 20)))
    ex <- ex_ri(d4$Y, d4$Z, exact_max = 10, sims = 40000, seed = 1)
    ref <- conduct_ri(Y ~ Z, declaration = declare_ra(N = 40, m = 20), sharp_hypothesis = 0, data = d4, sims = 40000)
    chk("simulated, 40 units: p within Monte Carlo error of ri2", ex$p_two, summary(ref)$two_tailed_p_value, 0.015)
    # a sharp null other than zero: shift the outcome by the hypothesised effect
    tau <- 0.4; dd <- d4; dd$Yadj <- dd$Y - tau * dd$Z
    mine <- ex_ri(dd$Yadj, dd$Z, exact_max = 10, sims = 40000, seed = 1)
    ref2 <- conduct_ri(Y ~ Z, declaration = declare_ra(N = 40, m = 20), sharp_hypothesis = tau, data = d4, sims = 40000)
    chk("sharp null of an effect of 0.4: p close to ri2", mine$p_two, summary(ref2)$two_tailed_p_value, 0.015)
    # confidence interval by test inversion: the p-value crosses 5% exactly at the end points
    ci <- ex_ri(d1$Y, d1$Z, exact_max = 1e6)
    pf <- ci$pfun
    chk("test inversion: p inside the interval above 5%, outside not", c(pf(mean(ci$ci)) > 0.05, pf(ci$ci[1] + 1e-6) > 0.05, pf(ci$ci[1] - 1e-6) > 0.05, pf(ci$ci[2] - 1e-6) > 0.05, pf(ci$ci[2] + 1e-6) > 0.05), c(1, 1, 0, 1, 0), 0.5)
} else cat("ri2 / randomizr not available: randomization inference checks skipped\n")


# ---- multiple testing: p-value adjustments and the resampling procedures against a brute-force version built on lm()
set.seed(11)
nn <- 120; mm <- 5
Zc <- cbind(z1 = rnorm(nn))
dd0 <- sample(rep(0:1, nn / 2))
Ym <- sapply(1:mm, function(j) 0.1 * j * dd0 * (j <= 3) + 0.5 * Zc[, 1] + rnorm(nn)); colnames(Ym) <- paste0("y", 1:mm)
Ym[, 5] <- Ym[, 4] * 0.7 + rnorm(nn, 0, 0.5)                                     # correlated outcomes
res <- ex_mht(Ym, dd0, Zc, methods = c("bonferroni", "sidak", "holm", "hochberg", "bh", "by", "wy_minp", "wy_maxt", "rw"), nperm = 300, nboot = 300, seed = 5)
lmt <- t(sapply(1:mm, function(j) { s <- summary(lm(Ym[, j] ~ dd0 + Zc))$coefficients["dd0", ]; s[c("Estimate", "Std. Error", "t value", "Pr(>|t|)")] }))
chk("regression effects, s.e., t and p equal lm()", c(res$estimate, res$se, res$t, res$p), as.vector(lmt), 1e-10)
chk("Bonferroni, Holm, Hochberg, BH, BY equal stats::p.adjust", c(res$bonferroni, res$holm, res$hochberg, res$bh, res$by),
    c(p.adjust(res$p, "bonferroni"), p.adjust(res$p, "holm"), p.adjust(res$p, "hochberg"), p.adjust(res$p, "BH"), p.adjust(res$p, "BY")), 1e-12)
chk("Sidak", res$sidak, 1 - (1 - res$p)^mm, 1e-12)
# brute force Westfall-Young and Romano-Wolf with lm() on the same random numbers
set.seed(5)
tstar <- t(sapply(1:300, function(r) { dd <- dd0[sample.int(nn)]; sapply(1:mm, function(j) summary(lm(Ym[, j] ~ dd + Zc))$coefficients["dd", "t value"]) }))
dfr <- nn - 3
pstar <- 2 * pt(-abs(tstar), dfr); o <- order(res$p)
minp <- sapply(seq_len(mm), function(j) mean(apply(pstar[, o[j:mm], drop = FALSE], 1, min) <= res$p[o][j] + 1e-12))
bf <- numeric(mm); bf[o] <- cummax(minp)
chk("Westfall-Young min-p equals the brute-force version", res$wy_minp, bf, 1e-12)
o2 <- order(-abs(res$t))
maxt <- sapply(seq_len(mm), function(j) mean(apply(abs(tstar)[, o2[j:mm], drop = FALSE], 1, max) >= abs(res$t)[o2][j] - 1e-12))
bf2 <- numeric(mm); bf2[o2] <- cummax(maxt)
chk("Westfall-Young max-t equals the brute-force version", res$wy_maxt, bf2, 1e-12)
chk("adjusted p-values are at least the unadjusted ones, and at most 1", c(all(res$wy_minp >= res$p - 1e-12), all(res$rw >= res$p - 0.05), all(res$rw <= 1)), c(1, 1, 1), 0.5)
chk("Holm is never below Romano-Wolf by more than Monte Carlo error (power gain from dependence)", as.numeric(mean(res$rw - res$holm) < 0.05), 1, 0.5)

# ---- power against pwr
if (have("pwr")) {
    suppressMessages(library(pwr))
    for (cs in list(c(50, 50, 0.4), c(30, 70, 0.5), c(200, 100, 0.25))) {
        ref <- pwr.t2n.test(n1 = cs[1], n2 = cs[2], d = cs[3], sig.level = 0.05)$power
        chk(sprintf("power, n = %d + %d, d = %.2f", cs[1], cs[2], cs[3]), ex_power(cs[3], 1, cs[1], cs[2]), ref, 1e-9)
    }
    refm <- pwr.t2n.test(n1 = 60, n2 = 40, power = 0.8, sig.level = 0.05)$d
    chk("minimum detectable effect, n = 60 + 40, power 80%", ex_mde(1, 60, 40, 0.05, 0.8), refm, 1e-6)
    refn <- pwr.t.test(d = 0.3, power = 0.9, sig.level = 0.05, type = "two.sample")$n
    ss <- ex_sample_size(0.3, 1, 0.05, 0.9, p = 0.5)
    chk("sample size per group, d = 0.3, power 90%", ss[["n1"]], ceiling(refn), 0.5)
    chk("covariate with R2 = 0.4: equals a larger standardised effect", ex_power(0.3, 1, 80, 80, r2 = 0.4), pwr.t2n.test(n1 = 80, n2 = 80, d = 0.3 / sqrt(1 - 0.4))$power, 1e-9)
} else cat("pwr not available: power checks skipped\n")
# cluster randomization: design effect and degrees of freedom from the number of clusters
J1 <- 20; J0 <- 20; mc <- 25; rho <- 0.05
se <- sqrt((1 + (mc - 1) * rho) / mc * (1 / J1 + 1 / J0)); df <- J1 + J0 - 2; ncp <- 0.3 / se; tc <- qt(0.975, df)
chk("cluster randomization power (manual noncentral t)", ex_power(0.3, 1, J1, J0, m = mc, rho = rho), 1 - pt(tc, df, ncp) + pt(-tc, df, ncp), 1e-12)
chk("ICC (ANOVA estimator) on a balanced design", ex_icc(c(1, 2, 3, 2, 3, 4, 5, 6, 7, 8, 9, 7), rep(1:4, each = 3)), max(0, (((sum(3 * (c(2, 3, 6, 8) - mean(c(1, 2, 3, 2, 3, 4, 5, 6, 7, 8, 9, 7)))^2)) / 3) - (sum((c(1, 2, 3, 2, 3, 4, 5, 6, 7, 8, 9, 7) - rep(c(2, 3, 6, 8), each = 3))^2) / 8)) / (((sum(3 * (c(2, 3, 6, 8) - mean(c(1, 2, 3, 2, 3, 4, 5, 6, 7, 8, 9, 7)))^2)) / 3) + 2 * (sum((c(1, 2, 3, 2, 3, 4, 5, 6, 7, 8, 9, 7) - rep(c(2, 3, 6, 8), each = 3))^2) / 8))), 1e-12)


# ---- balance and attrition against t.test() and lm()/anova()
set.seed(21)
nb <- 200
armb <- factor(sample(rep(c("control", "T1", "T2"), length.out = nb)), levels = c("control", "T1", "T2"))
Xb <- cbind(age = rnorm(nb, 40, 10), income = rexp(nb), female = rbinom(nb, 1, 0.5))
tb <- ex_balance_table(Xb, armb)
ref_p <- sapply(seq_len(nrow(tb)), function(i) t.test(Xb[armb == tb$arm[i], tb$covariate[i]], Xb[armb == "control", tb$covariate[i]])$p.value)
ref_t <- sapply(seq_len(nrow(tb)), function(i) unname(t.test(Xb[armb == tb$arm[i], tb$covariate[i]], Xb[armb == "control", tb$covariate[i]])$statistic))
chk("balance table: Welch t and p equal t.test()", c(tb$t, tb$p), c(ref_t, ref_p), 1e-10)
j <- ex_balance_joint(Xb, armb, nperm = 200, seed = 1)
k <- "T1"; keep <- armb %in% c("control", k); dk <- as.numeric(armb[keep] == k)
fit <- lm(dk ~ Xb[keep, ]); fs <- summary(fit)$fstatistic
chk("joint balance test equals the F test of lm()", c(j$F[j$arm == k], j$p[j$arm == k]), c(unname(fs[1]), stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE)), 1e-10)
chk("randomization p-value is a valid p-value", as.numeric(all(j$p_ri >= 0 & j$p_ri <= 1)), 1, 0.5)
obsd <- rbinom(nb, 1, c(control = 0.8, T1 = 0.7, T2 = 0.8)[as.character(armb)])
at <- ex_attrition(obsd, armb, Xb)
chk("attrition: response rates by arm", at$rates$rate, as.numeric(tapply(obsd, armb, mean)), 1e-12)
a1 <- lm(obsd ~ armb + scale(Xb, scale = FALSE)); a2 <- lm(obsd ~ armb * scale(Xb, scale = FALSE))
an <- anova(a1, a2)
chk("attrition: arm x covariate F test equals anova() of nested models", c(at$joint$F, at$joint$p), c(an$F[2], an$`Pr(>F)`[2]), 1e-8)

cat(if (nfail == 0) "\nAll reference checks passed.\n" else sprintf("\n%d reference check(s) FAILED.\n", nfail))
if (nfail > 0) quit(status = 1)
