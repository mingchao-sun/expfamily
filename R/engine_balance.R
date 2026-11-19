# Balance of baseline covariates across the arms of a randomized experiment, a joint test of balance (with a
# randomization-inference p-value) and attrition checks (differential response by arm). Base R only.

# One row per covariate and non-control arm: means, standardised difference, Welch t test.
ex_balance_table <- function(X, arm) {
    arm <- droplevels(arm); ctl <- levels(arm)[1]; out <- list()
    for (k in levels(arm)[-1]) for (j in seq_len(ncol(X))) {
        x1 <- X[arm == k, j]; x0 <- X[arm == ctl, j]
        x1 <- x1[!is.na(x1)]; x0 <- x0[!is.na(x0)]
        m1 <- mean(x1); m0 <- mean(x0); v1 <- stats::var(x1); v0 <- stats::var(x0)
        se <- sqrt(v1 / length(x1) + v0 / length(x0))
        df <- se^4 / ((v1 / length(x1))^2 / (length(x1) - 1) + (v0 / length(x0))^2 / (length(x0) - 1))
        tt <- (m1 - m0) / se
        sdp <- sqrt((v1 + v0) / 2)
        out[[length(out) + 1]] <- data.frame(arm = k, covariate = colnames(X)[j], mean_control = m0, mean_arm = m1, sd_control = sqrt(v0), sd_arm = sqrt(v1),
                                             diff = m1 - m0, std_diff = if (sdp > 0) (m1 - m0) / sdp else NA_real_, t = tt, df = df, p = 2 * stats::pt(-abs(tt), df),
                                             n_control = length(x0), n_arm = length(x1), stringsAsFactors = FALSE)
    }
    do.call(rbind, out)
}

# Joint test that the covariates do not predict assignment: F test of the regression of the arm indicator on the covariates
# (units in that arm and the control), and the same statistic referred to its randomization distribution (treatment permuted within blocks).
ex_balance_joint <- function(X, arm, block = NULL, nperm = 2000, seed = 1234) {
    arm <- droplevels(arm); ctl <- levels(arm)[1]; out <- list()
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(seed)
    for (k in levels(arm)[-1]) {
        keep <- arm %in% c(k, ctl) & stats::complete.cases(X)
        d <- as.numeric(arm[keep] == k); Xk <- X[keep, , drop = FALSE]; bk <- if (is.null(block)) NULL else block[keep]
        keepc <- apply(Xk, 2, stats::sd) > 0; Xk <- Xk[, keepc, drop = FALSE]
        q <- qr(cbind(1, Xk), tol = 1e-9); p <- q$rank - 1
        if (p < 1 || length(d) - p - 1 < 2) { out[[length(out) + 1]] <- data.frame(arm = k, F = NA_real_, df1 = p, df2 = NA_real_, p = NA_real_, p_ri = NA_real_, n = length(d)); next }
        Q <- qr.Q(q)[, seq_len(q$rank), drop = FALSE]
        Fst <- function(dd) { dc <- dd - mean(dd); fit <- Q %*% crossprod(Q, dd); r2 <- sum((fit - mean(dd))^2) / sum(dc^2); (r2 / p) / ((1 - r2) / (length(dd) - p - 1)) }
        F0 <- Fst(d)
        bl <- if (is.null(bk)) list(seq_along(d)) else split(seq_along(d), factor(bk))
        Fs <- vapply(seq_len(nperm), function(r) { dd <- d; for (i in bl) if (length(i) > 1) dd[i] <- d[i][sample.int(length(i))]; Fst(dd) }, numeric(1))
        out[[length(out) + 1]] <- data.frame(arm = k, F = F0, df1 = p, df2 = length(d) - p - 1, p = stats::pf(F0, p, length(d) - p - 1, lower.tail = FALSE),
                                             p_ri = mean(Fs >= F0 - 1e-10), n = length(d), stringsAsFactors = FALSE)
    }
    do.call(rbind, out)
}

# Attrition: share of units with an observed outcome in every arm, the difference from the control with a Welch test, and an
# F test that the covariates predict response differently across arms (arm x covariate interactions).
ex_attrition <- function(observed, arm, X = NULL) {
    arm <- droplevels(arm); ctl <- levels(arm)[1]; ok <- !is.na(arm)
    obs <- as.numeric(observed)[ok]; arm <- arm[ok]
    tb <- ex_balance_table(matrix(obs, ncol = 1, dimnames = list(NULL, "response")), arm)
    rate <- tapply(obs, arm, mean); nn <- table(arm)
    joint <- NULL
    if (!is.null(X) && ncol(X) > 0) {
        X <- X[ok, , drop = FALSE]; cc <- stats::complete.cases(X)
        Xc <- sweep(X[cc, , drop = FALSE], 2, colMeans(X[cc, , drop = FALSE])); keepc <- apply(Xc, 2, stats::sd) > 0; Xc <- Xc[, keepc, drop = FALSE]
        a <- stats::model.matrix(~ arm[cc])[, -1, drop = FALSE]; y <- obs[cc]
        m0 <- cbind(1, a, Xc); inter <- do.call(cbind, lapply(seq_len(ncol(a)), function(k) Xc * a[, k]))
        m1 <- cbind(m0, inter)
        rss <- function(M) sum(stats::lm.fit(M, y)$residuals^2)
        r1 <- stats::lm.fit(m1, y); p1 <- r1$rank; p0 <- stats::lm.fit(m0, y)$rank
        if (p1 > p0 && length(y) - p1 > 1) {
            Fst <- ((rss(m0) - rss(m1)) / (p1 - p0)) / (rss(m1) / (length(y) - p1))
            joint <- data.frame(F = Fst, df1 = p1 - p0, df2 = length(y) - p1, p = stats::pf(Fst, p1 - p0, length(y) - p1, lower.tail = FALSE))
        }
    }
    list(rates = data.frame(arm = levels(arm), n = as.integer(nn), observed = as.integer(tapply(obs, arm, sum)), rate = as.numeric(rate), stringsAsFactors = FALSE),
         tests = tb[, c("arm", "diff", "t", "df", "p")], joint = joint)
}
