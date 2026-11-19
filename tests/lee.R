# Lee bounds: hand-computed example, direction symmetry, tightening, and recovery of a known effect in simulated data.
# Run from the module root:  Rscript tests/lee.R
source("R/common.R"); source("R/engine_lee.R")
nfail <- 0
ok <- function(cond, msg) { cat(sprintf("[%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", msg)); if (!isTRUE(cond)) nfail <<- nfail + 1 }

# Treated: 10 units, 8 observed with y = 1..8. Control: 10 units, 6 observed with y = 1..6.
# p1 = 0.8, p0 = 0.6, trimmed share q = 0.25, i.e. two of the eight treated outcomes. Keeping the lowest six gives mean 3.5, the highest six mean 5.5; control mean 3.5.
y <- c(1:8, NA, NA, 1:6, rep(NA, 4)); d <- rep(c(1, 0), each = 10)
r <- ex_lee_one(y, d, "up")
ok(all.equal(unname(r[c("lower", "upper", "p1", "p0", "q")]), c(0, 2, .8, .6, .25)) == TRUE, "hand example: bounds 0 and 2, trimming share 0.25")
# The same data with the roles swapped: the controls respond more, so the controls are trimmed and the effect changes sign
r2 <- ex_lee_one(y, 1 - d, "down")
ok(all.equal(unname(r2[c("lower", "upper")]), c(-2, 0)) == TRUE, "swapped groups: bounds -2 and 0")
e <- ex_lee(y, d, nboot = 50)
ok(e$dir == "up" && abs(e$naive - 1) < 1e-12 && abs(e$lower) < 1e-12 && abs(e$upper - 2) < 1e-12, "ex_lee: direction, naive difference and bounds")
ok(e$ci[1] <= e$lower && e$ci[2] >= e$upper, "the Imbens-Manski interval contains the bounds")
# No attrition difference: both bounds equal the difference in means
z <- c(1:8, 1:8); dd <- rep(c(1, 0), each = 8); z[c(2, 11)] <- NA
rr <- ex_lee_one(z, dd, "up"); ok(rr[["q"]] == 0 && abs(rr[["lower"]] - rr[["upper"]]) < 1e-12, "equal response rates: the bounds coincide")
# Imbens-Manski critical value: equals the one-sided normal value for a wide interval, the two-sided one for a point
ok(abs(ex_im_crit(100, 1, .95) - qnorm(.95)) < 1e-4 && abs(ex_im_crit(0, 1, .95) - qnorm(.975)) < 1e-6, "Imbens-Manski critical value limits")
# One cell reproduces the untightened bounds; many cells are never wider
set.seed(1); n <- 4000
x <- factor(sample(c("a", "b", "c"), n, TRUE)); u <- runif(n); dd <- rbinom(n, 1, .5)
# monotone selection: observed if u < base(x) + 0.15 d; potential outcome y0 depends on x and noise; true effect 1 for everybody
base <- c(a = .5, b = .6, c = .7)[as.character(x)]
s <- u < base + .15 * dd
y0 <- as.numeric(x) * 0.8 + rnorm(n) + 2 * (u < base)       # always-observed units are the ones with u < base
yy <- ifelse(s, y0 + dd, NA)
one <- factor(rep("all", n))
a <- ex_lee_est(yy, dd, "up"); b1 <- ex_lee_est(yy, dd, "up", one)
ok(all.equal(unname(a[c("lower", "upper")]), unname(b1[c("lower", "upper")])) == TRUE, "tightening with a single cell equals the plain bounds")
tb <- ex_lee_est(yy, dd, "up", x)
ok(tb[["upper"]] - tb[["lower"]] <= a[["upper"]] - a[["lower"]] + 1e-9, sprintf("tightening does not widen the bounds (%.3f against %.3f)", tb[["upper"]] - tb[["lower"]], a[["upper"]] - a[["lower"]]))
ok(tb[["lower"]] <= 1 && tb[["upper"]] >= 1 && a[["lower"]] <= 1 && a[["upper"]] >= 1, "both sets of bounds contain the true effect 1")
# Coverage of the true effect by the bounds and of the Imbens-Manski interval over repeated samples
cov <- replicate(60, {
    n <- 1500; u <- runif(n); dd <- rbinom(n, 1, .5); s <- u < .55 + .15 * dd
    y <- ifelse(s, 1 * dd + rnorm(n) + 2 * (u < .55), NA)
    f <- ex_lee(y, dd, nboot = 100, seed = sample.int(1e5, 1)); c(lo = f$lower, hi = f$upper, ci = f$ci[1] <= 1 && f$ci[2] >= 1)
})
ok(mean(cov["ci", ]) >= .88, sprintf("95%% Imbens-Manski interval covers the true effect in %.0f%% of 60 samples", 100 * mean(cov["ci", ])))
ok(mean(cov["lo", ]) <= 1 && mean(cov["hi", ]) >= 1, sprintf("the average estimated bounds (%.2f, %.2f) contain the true effect 1", mean(cov["lo", ]), mean(cov["hi", ])))
# Errors
ok(inherits(try(ex_lee(c(1, NA, 3, 4), c(1, 1, 0, 0), nboot = 10), silent = TRUE), "try-error"), "too few observed outcomes is refused")
ok(inherits(try(ex_lee(y, d, direction = "down", nboot = 10), silent = TRUE), "try-error"), "a direction contradicting the response rates is refused")
cat(if (nfail == 0) "\nAll Lee bounds checks passed.\n" else sprintf("\n%d check(s) FAILED.\n", nfail))
if (nfail > 0) quit(status = 1)
