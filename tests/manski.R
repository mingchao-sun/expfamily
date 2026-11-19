# Manski worst-case bounds: extreme imputation of the unobserved potential outcomes (brute force), known results of the literature, and recovery of the true
# effect in simulations that satisfy the assumptions. Run from the module root:  Rscript tests/manski.R
for (f in c("R/common.R", "R/engine_lee.R", "R/engine_manski.R")) source(f)
nfail <- 0
ok <- function(cond, msg) { cat(sprintf("[%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", msg)); if (!isTRUE(cond)) nfail <<- nfail + 1 }
cmp <- function(label, a, b, tol = 1e-10) ok(all(abs(a - b) < tol), sprintf("%s (max diff %.1e)", label, max(abs(a - b))))

set.seed(1); n <- 600
d <- rbinom(n, 1, .4); y <- round(runif(n, 0, 10), 2); ymin <- 0; ymax <- 10
# ---- selection mode, no assumptions: impute the unobserved potential outcomes at the extremes
lo <- mean(ifelse(d == 1, y, ymin)) - mean(ifelse(d == 0, y, ymax)); hi <- mean(ifelse(d == 1, y, ymax)) - mean(ifelse(d == 0, y, ymin))
b <- ex_man_point(y, d, "selection", ymin, ymax)
cmp("no assumptions: bounds equal the extreme imputation", b["none", c("ate_lo", "ate_hi")], c(lo, hi))
ok(abs((b["none", "ate_hi"] - b["none", "ate_lo"]) - (ymax - ymin)) < 1e-10, "no assumptions: the width of the bounds on the ATE is exactly ymax - ymin")
# ---- MTR: Y(1) >= Y(0): impute the controls' Y(1) at least at their Y (up to ymax) and the treated's Y(0) between ymin and their Y
lo <- 0; hi <- mean(ifelse(d == 1, y, ymax)) - mean(ifelse(d == 0, y, ymin))
cmp("MTR: bounds equal the imputation (lower bound 0)", b["mtr", c("ate_lo", "ate_hi")], c(lo, hi))
# ---- MTS: E[Y(1)|D=0] <= E[Y|D=1], E[Y(0)|D=1] >= E[Y|D=0]
m1 <- mean(y[d == 1]); m0 <- mean(y[d == 0]); p1 <- mean(d)
cmp("MTS: upper bound is the difference in means", b["mts", "ate_hi"], m1 - m0)
cmp("MTS: lower bound", b["mts", "ate_lo"], m1 * p1 + ymin * (1 - p1) - (m0 * (1 - p1) + ymax * p1))
cmp("MTR and MTS together: bounds are [0, difference in means] (Manski and Pepper, 2000)", b["both", c("ate_lo", "ate_hi")], c(0, m1 - m0))
ok(all(b["both", "ate_hi"] <= b["mts", "ate_hi"] + 1e-12, b["both", "ate_hi"] <= b["mtr", "ate_hi"] + 1e-12, b["both", "ate_lo"] >= b["mts", "ate_lo"] - 1e-12, b["both", "ate_lo"] >= b["mtr", "ate_lo"] - 1e-12), "more assumptions never widen the bounds")
ok(all(b[c("none", "mtr", "mts"), "ate_lo"] <= b[c("none", "mtr", "mts"), "ate_hi"] + 1e-12), "lower bounds never exceed upper bounds without a contradiction")
ok((m1 < m0) == (b["both", "ate_lo"] > b["both", "ate_hi"]), sprintf("the two assumptions together are contradicted by the data exactly when the treated mean is below the control mean (%.2f against %.2f)", m1, m0))
ok(identical(ex_manski(y, d, "selection", ymin, ymax, nboot = 20)$empty, unname(b[, "ate_lo"] > b[, "ate_hi"] + 1e-12)), "the wrapper flags the assumption sets that the data contradict")
# ---- random mode with missing outcomes: extreme imputation within each arm
ym <- y; ym[runif(n) < ifelse(d == 1, 0.3, 0.1)] <- NA
r <- ex_man_point(ym, d, "random", ymin, ymax)
fill <- function(v, f) ifelse(is.na(v), f, v)
cmp("random assignment with missing outcomes: bounds equal the extreme imputation", r[1, c("ate_lo", "ate_hi")], c(mean(fill(ym[d == 1], ymin)) - mean(fill(ym[d == 0], ymax)), mean(fill(ym[d == 1], ymax)) - mean(fill(ym[d == 0], ymin))), 1e-10)
r0 <- ex_man_point(y, d, "random", ymin, ymax)
cmp("random assignment, nothing missing: the bounds collapse to the difference in means", r0[1, c("ate_lo", "ate_hi")], rep(m1 - m0, 2))
# ---- the Lee bounds lie inside the worst-case bounds when selection is monotone (same data, outcome missing, random assignment)
lee <- ex_lee(ym, d, nboot = 20)
ok(lee$lower >= r[1, "ate_lo"] - 1e-9 && lee$upper <= r[1, "ate_hi"] + 1e-9, sprintf("Lee bounds [%.2f, %.2f] lie inside the worst-case bounds [%.2f, %.2f]", lee$lower, lee$upper, r[1, "ate_lo"], r[1, "ate_hi"]))
# ---- simulations that satisfy the assumptions contain the true effect
sim <- function(seed, nn = 1500) {
    set.seed(seed); u <- rnorm(nn); y0 <- plogis(u); y1 <- pmin(1, y0 + runif(nn, 0, 0.3))        # MTR: Y(1) >= Y(0), outcomes in [0, 1]
    dd <- rbinom(nn, 1, plogis(0.8 * u))                                                           # MTS: units with high Y(0) and Y(1) select treatment
    list(y = ifelse(dd == 1, y1, y0), d = dd, ate = mean(y1 - y0))
}
sm <- sim(2); bb <- ex_man_point(sm$y, sm$d, "selection", 0, 1)
ok(all(bb[, "ate_lo"] <= sm$ate & sm$ate <= bb[, "ate_hi"]), sprintf("all four sets of bounds contain the true ATE %.3f (e.g. MTR+MTS: [%.3f, %.3f])", sm$ate, bb["both", "ate_lo"], bb["both", "ate_hi"]))
ok(bb["both", "ate_hi"] - bb["both", "ate_lo"] < 0.5 * (bb["none", "ate_hi"] - bb["none", "ate_lo"]), "MTR and MTS narrow the worst-case bounds")
cov <- replicate(60, { s <- sim(sample.int(1e6, 1), 800); e <- ex_manski(s$y, s$d, "selection", 0, 1, nboot = 100, seed = 1); c(e$ci_lo[4] <= s$ate && s$ate <= e$ci_hi[4], e$ci_lo[1] <= s$ate && s$ate <= e$ci_hi[1]) })
ok(mean(cov[1, ]) >= 0.9 && mean(cov[2, ]) >= 0.9, sprintf("95%% Imbens-Manski intervals contain the true ATE in %.0f%% (MTR+MTS) and %.0f%% (no assumptions) of 60 samples", 100 * mean(cov[1, ]), 100 * mean(cov[2, ])))
# ---- the wrapper: errors and options
e <- ex_manski(ym, d, "random", nboot = 50); ok(e$ymin == min(ym, na.rm = TRUE) && e$ymax == max(ym, na.rm = TRUE) && nrow(e$bounds) == 1, "the observed range is used when no range is given")
ok(inherits(try(ex_manski(y, d, "selection", ymin = 5, ymax = 2), silent = TRUE), "try-error"), "an empty range is refused")
ok(inherits(try(ex_manski(y, d, "selection", ymin = 1, ymax = 9), silent = TRUE), "try-error"), "outcomes outside the stated range are refused")
ok(inherits(try(ex_manski(y, rep(1, n), "selection"), silent = TRUE), "try-error"), "a single treatment group is refused")
cat(if (nfail == 0) "\nAll Manski bounds checks passed.\n" else sprintf("\n%d check(s) FAILED.\n", nfail))
if (nfail > 0) quit(status = 1)
