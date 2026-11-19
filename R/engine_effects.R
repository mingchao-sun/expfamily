# Treatment effects in randomized experiments: difference in means, regression with covariates (Lin, 2013), block
# fixed effects, several treatment arms, and heteroskedasticity- and cluster-robust standard errors including the
# small-sample CR2 variance with Satterthwaite degrees of freedom (Bell and McCaffrey, 2002; Pustejovsky and Tipton, 2018).
# Base R only. Checked against the R package estimatr (lm_robust, lm_lin) and Stata regress with vce(hc2), vce(hc3), vce(cluster).

ex_se_types <- c(classical = "Classical", hc1 = "Robust (HC1, as Stata)", hc2 = "Robust (HC2)", hc3 = "Robust (HC3)", cr1 = "Cluster (CR1, as Stata)", cr2 = "Cluster (CR2, Bell-McCaffrey)")

# Design matrix. arm: factor with the control group as its first level; X: numeric covariate matrix (or NULL);
# lin: interact the centred covariates with the arm dummies (Lin, 2013); block: factor (or NULL) for block fixed effects.
ex_design <- function(arm, X = NULL, lin = FALSE, block = NULL) {
    arm <- droplevels(arm)
    K <- nlevels(arm)
    A <- stats::model.matrix(~ arm)[, -1, drop = FALSE]
    colnames(A) <- levels(arm)[-1]
    parts <- list(`(Intercept)` = matrix(1, length(arm), 1), A)
    if (!is.null(X) && ncol(X) > 0) {
        Xc <- sweep(X, 2, colMeans(X))
        parts <- c(parts, list(Xc))
        if (lin) {
            for (k in seq_len(K - 1)) {
                I <- Xc * A[, k]
                colnames(I) <- paste0(levels(arm)[k + 1], ":", colnames(X))
                parts <- c(parts, list(I))
            }
        }
    }
    if (!is.null(block)) {
        B <- stats::model.matrix(~ droplevels(factor(block)))[, -1, drop = FALSE]
        if (ncol(B) > 0) parts <- c(parts, list(B))
    }
    M <- do.call(cbind, parts)
    # drop aliased columns (never the arm dummies)
    q <- qr(M, tol = 1e-9)
    keep <- sort(q$pivot[seq_len(q$rank)])
    arm_cols <- 2:K
    if (!all(arm_cols %in% keep)) stop("The treatment arms are collinear with the covariates or blocks.", call. = FALSE)
    list(M = M[, keep, drop = FALSE], arm_idx = match(arm_cols, keep), arm_names = levels(arm)[-1])
}

# Regression with the chosen variance estimator. Returns coefficients, standard errors, degrees of freedom for the arm dummies.
ex_ols <- function(y, M, se_type = "hc2", cluster = NULL, arm_idx = 2) {
    n <- length(y); p <- ncol(M)
    XtXi <- chol2inv(chol(crossprod(M)))
    b <- as.numeric(XtXi %*% crossprod(M, y))
    e <- as.numeric(y - M %*% b)
    h <- rowSums((M %*% XtXi) * M)
    dfres <- n - p
    clustered <- se_type %in% c("cr1", "cr2")
    if (clustered && is.null(cluster)) stop("Cluster-robust standard errors need a cluster variable.", call. = FALSE)
    df <- rep(dfres, length(arm_idx))
    if (!clustered) {
        w <- switch(se_type, classical = NULL, hc1 = e^2 * n / dfres, hc2 = e^2 / (1 - h), hc3 = e^2 / (1 - h)^2,
                    stop("Unknown standard error type.", call. = FALSE))
        if (any(!is.finite(w))) stop("A observation has leverage 1 (a group with a single observation); use HC1 or classical standard errors.", call. = FALSE)
        V <- if (is.null(w)) XtXi * sum(e^2) / dfres else XtXi %*% crossprod(M, M * w) %*% XtXi
    } else {
        g <- factor(cluster); G <- nlevels(g)
        if (G < 3) stop("Cluster-robust standard errors need at least three clusters.", call. = FALSE)
        idx <- split(seq_len(n), g)
        if (se_type == "cr1") {
            S <- matrix(0, p, p)
            for (i in idx) { s <- crossprod(M[i, , drop = FALSE], e[i]); S <- S + s %*% t(s) }
            V <- XtXi %*% S %*% XtXi * (G / (G - 1)) * ((n - 1) / dfres)
            df <- rep(G - 1, length(arm_idx))
        } else {
            if (n > 5000) stop("CR2 needs the full n x n projection; with more than 5,000 rows use CR1.", call. = FALSE)
            IH <- diag(n) - M %*% XtXi %*% t(M)
            invsqrt <- function(A) { ev <- eigen(A, symmetric = TRUE); if (any(ev$values < 1e-10)) stop("A cluster has no residual degrees of freedom for CR2; use CR1.", call. = FALSE); ev$vectors %*% (t(ev$vectors) / sqrt(ev$values)) }
            Aadj <- lapply(idx, function(i) invsqrt(IH[i, i, drop = FALSE]))
            S <- matrix(0, p, p)
            for (k in seq_along(idx)) { i <- idx[[k]]; s <- crossprod(M[i, , drop = FALSE], Aadj[[k]] %*% e[i]); S <- S + s %*% t(s) }
            V <- XtXi %*% S %*% XtXi
            # Satterthwaite degrees of freedom for each arm coefficient under the working model of independent homoskedastic errors
            for (j in seq_along(arm_idx)) {
                cvec <- numeric(p); cvec[arm_idx[j]] <- 1
                Bm <- matrix(0, n, G)
                for (k in seq_along(idx)) {
                    i <- idx[[k]]
                    a_g <- Aadj[[k]] %*% M[i, , drop = FALSE] %*% (XtXi %*% cvec)
                    Bm[, k] <- IH[, i, drop = FALSE] %*% a_g
                }
                BtB <- crossprod(Bm)
                df[j] <- sum(Bm^2)^2 / sum(BtB^2)
            }
        }
    }
    list(b = b, se = sqrt(diag(V)), V = V, df = df, e = e, n = n, p = p, G = if (clustered) nlevels(factor(cluster)) else NA_integer_, dfres = dfres)
}

# Effects of every non-control arm against the control arm. method: "diff" (arm only), "adjust" (covariates added),
# "lin" (covariates interacted with the arm). block: block fixed effects. Returns a data frame with one row per arm.
ex_effects <- function(y, arm, X = NULL, block = NULL, cluster = NULL, method = c("diff", "adjust", "lin"), se_type = "hc2", level = 0.95) {
    method <- match.arg(method)
    keep <- stats::complete.cases(y, arm, if (!is.null(X)) X, if (!is.null(block)) block, if (!is.null(cluster)) cluster)
    y <- y[keep]; arm <- droplevels(arm[keep])
    if (!is.null(X)) X <- X[keep, , drop = FALSE]; if (!is.null(block)) block <- block[keep]; if (!is.null(cluster)) cluster <- cluster[keep]
    if (nlevels(arm) < 2) stop("The treatment needs at least two groups.", call. = FALSE)
    if (min(table(arm)) < 2) stop("Every group needs at least two observations.", call. = FALSE)
    d <- ex_design(arm, if (method == "diff") NULL else X, lin = method == "lin", block = block)
    f <- ex_ols(y, d$M, se_type, cluster, d$arm_idx)
    est <- f$b[d$arm_idx]; se <- f$se[d$arm_idx]
    tt <- est / se
    crit <- stats::qt(1 - (1 - level) / 2, f$df)
    ctl <- levels(arm)[1]
    sd0 <- stats::sd(y[arm == ctl])
    data.frame(arm = d$arm_names, estimate = est, se = se, t = tt, df = f$df, p = 2 * stats::pt(-abs(tt), f$df),
               lo = est - crit * se, hi = est + crit * se, n_arm = as.integer(table(arm)[d$arm_names]), n_control = sum(arm == ctl),
               control_mean = mean(y[arm == ctl]), std_effect = est / sd0, stringsAsFactors = FALSE)
}
