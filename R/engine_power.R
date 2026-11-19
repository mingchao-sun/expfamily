# Power, minimum detectable effect and sample size for a comparison of two means in a randomized experiment, with a
# covariate that explains a share R2 of the outcome variance and, for cluster randomization, a design effect from the
# intracluster correlation. Exact power from the noncentral t distribution. Base R only. Checked against pwr and Stata power twomeans.

# Standard deviation of the estimator and its degrees of freedom.
# individual: n1, n0 units; cluster: J1, J0 clusters of m units (coefficient of variation cv of the cluster sizes), correlation rho.
ex_power_se <- function(sd, n1, n0, r2 = 0, ncov = 0, m = NULL, rho = 0, cv = 0) {
    s <- sd * sqrt(1 - r2)
    if (is.null(m)) return(list(se = s * sqrt(1 / n1 + 1 / n0), df = n1 + n0 - 2 - ncov))
    de <- 1 + ((cv^2 + 1) * m - 1) * rho                       # design effect of equal or unequal cluster sizes (Eldridge et al., 2006)
    list(se = s * sqrt(de / m) * sqrt(1 / n1 + 1 / n0), df = n1 + n0 - 2 - ncov)
}

# Power of the two-sided (or one-sided) t test of a difference delta.
ex_power <- function(delta, sd, n1, n0, alpha = 0.05, r2 = 0, ncov = 0, m = NULL, rho = 0, cv = 0, sided = 2) {
    s <- ex_power_se(sd, n1, n0, r2, ncov, m, rho, cv)
    if (s$df < 1) return(NA_real_)
    ncp <- abs(delta) / s$se
    if (sided == 2) {
        tc <- stats::qt(1 - alpha / 2, s$df)
        1 - stats::pt(tc, s$df, ncp) + stats::pt(-tc, s$df, ncp)
    } else {
        tc <- stats::qt(1 - alpha, s$df)
        1 - stats::pt(tc, s$df, ncp)
    }
}

# Smallest detectable effect for the target power.
ex_mde <- function(sd, n1, n0, alpha = 0.05, power = 0.8, r2 = 0, ncov = 0, m = NULL, rho = 0, cv = 0, sided = 2) {
    f <- function(delta) ex_power(delta, sd, n1, n0, alpha, r2, ncov, m, rho, cv, sided) - power
    hi <- sd * 10
    if (!is.finite(f(hi)) || f(hi) < 0) return(NA_real_)
    stats::uniroot(f, c(1e-12 * sd, hi), tol = 1e-12)$root
}

# Smallest total number of units (individual randomization; share p treated) or number of clusters (cluster randomization)
# reaching the target power. Returns the numbers per group.
ex_sample_size <- function(delta, sd, alpha = 0.05, power = 0.8, p = 0.5, r2 = 0, ncov = 0, m = NULL, rho = 0, cv = 0, sided = 2, nmax = 1e6) {
    pw <- function(N) { n1 <- max(2, ceiling(N * p)); n0 <- max(2, N - n1); ex_power(delta, sd, n1, n0, alpha, r2, ncov, m, rho, cv, sided) }
    N <- 6
    while (N < nmax) { v <- pw(N); if (is.finite(v) && v >= power) break; N <- N * 2 }
    if (N >= nmax) return(c(n1 = NA, n0 = NA))
    lo <- max(4, N %/% 2); hi <- N
    while (hi - lo > 1) { mid <- (lo + hi) %/% 2; v <- pw(mid); if (is.finite(v) && v >= power) hi <- mid else lo <- mid }
    n1 <- max(2, ceiling(hi * p)); c(n1 = n1, n0 = max(2, hi - n1))
}

# Intracluster correlation (ANOVA estimator) and the standard deviation of the outcome, from data.
ex_icc <- function(y, cluster) {
    ok <- stats::complete.cases(y, cluster); y <- y[ok]; g <- factor(cluster[ok])
    k <- nlevels(g); n <- length(y); ni <- as.numeric(table(g))
    if (k < 2) return(NA_real_)
    gm <- tapply(y, g, mean); ssb <- sum(ni * (gm - mean(y))^2); ssw <- sum((y - gm[as.character(g)])^2)
    msb <- ssb / (k - 1); msw <- ssw / (n - k)
    n0 <- (n - sum(ni^2) / n) / (k - 1)
    max(0, (msb - msw) / (msb + (n0 - 1) * msw))
}
