# Multiple outcomes: adjustment of the p-values of the treatment effects for the number of outcomes tested.
# Bonferroni, Sidak, Holm, Hochberg, Benjamini-Hochberg and Benjamini-Yekutieli from the p-values; Westfall-Young
# step-down (min-p and max-t, by permuting the treatment) and Romano-Wolf step-down (studentised bootstrap), which use the
# dependence between the outcomes. Base R only. Compared with stats::p.adjust, a brute-force version with lm(), and Stata wyoung and rwolf.

ex_mht_methods <- c(bonferroni = "Bonferroni", sidak = "Sidak", holm = "Holm", hochberg = "Hochberg", bh = "Benjamini-Hochberg (FDR)", by = "Benjamini-Yekutieli (FDR)",
                    wy_minp = "Westfall-Young step-down (min-p)", wy_maxt = "Westfall-Young step-down (max-t)", rw = "Romano-Wolf step-down (bootstrap)")

# Treatment effect on each column of Y after removing the controls Z (matrix incl. intercept): by the Frisch-Waugh theorem the
# coefficient and the classical t statistic of the treatment in the regression of the outcome on treatment and controls.
ex_mht_stats <- function(Y, d, Qz) {
    dr <- d - Qz %*% crossprod(Qz, d)
    Yr <- Y - Qz %*% crossprod(Qz, Y)
    sdd <- sum(dr^2)
    cross <- as.numeric(crossprod(dr, Yr))
    theta <- cross / sdd
    rss <- colSums(Yr^2) - theta^2 * sdd
    df <- nrow(Y) - ncol(Qz) - 1
    se <- sqrt(pmax(rss, 0) / df / sdd)
    list(theta = theta, se = se, t = theta / se, df = df)
}

ex_mht_q <- function(Z) { q <- qr(Z, tol = 1e-9); qr.Q(q)[, seq_len(q$rank), drop = FALSE] }

# Z: matrix of controls (no intercept) or NULL; block: factor or NULL (block dummies are controls, and permutations stay within blocks).
ex_mht <- function(Y, d, Z = NULL, block = NULL, methods = c("bonferroni", "holm", "bh"), nperm = 2000, nboot = 1000, seed = 1234) {
    ok <- stats::complete.cases(Y, d, if (!is.null(Z)) Z, if (!is.null(block)) block)
    Y <- as.matrix(Y)[ok, , drop = FALSE]; d <- as.numeric(d[ok]); if (!is.null(Z)) Z <- as.matrix(Z)[ok, , drop = FALSE]; if (!is.null(block)) block <- block[ok]
    n <- nrow(Y); m <- ncol(Y)
    if (m < 2) stop("Select at least two outcomes.", call. = FALSE)
    if (!all(d %in% c(0, 1))) stop("The treatment must be 0/1.", call. = FALSE)
    if (sum(d) < 3 || sum(1 - d) < 3) stop("Each group needs at least three observations.", call. = FALSE)
    cols <- list(matrix(1, n, 1))
    if (!is.null(Z) && ncol(Z) > 0) cols <- c(cols, list(Z))
    if (!is.null(block)) { B <- stats::model.matrix(~ factor(block))[, -1, drop = FALSE]; if (ncol(B)) cols <- c(cols, list(B)) }
    Qz <- ex_mht_q(do.call(cbind, cols))
    st <- ex_mht_stats(Y, d, Qz)
    pval <- 2 * stats::pt(-abs(st$t), st$df)
    out <- data.frame(outcome = colnames(Y), estimate = st$theta, se = st$se, t = st$t, p = pval, stringsAsFactors = FALSE)
    adj <- list()
    for (mt in methods) {
        adj[[mt]] <- switch(mt,
            bonferroni = pmin(1, m * pval), sidak = 1 - (1 - pval)^m,
            holm = stats::p.adjust(pval, "holm"), hochberg = stats::p.adjust(pval, "hochberg"),
            bh = stats::p.adjust(pval, "BH"), by = stats::p.adjust(pval, "BY"), NULL)
    }
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(seed)
    if (any(c("wy_minp", "wy_maxt") %in% methods)) {
        tstar <- matrix(0, nperm, m)
        bl <- if (is.null(block)) list(seq_len(n)) else split(seq_len(n), factor(block))
        for (r in seq_len(nperm)) {
            dd <- d
            for (i in bl) if (length(i) > 1) dd[i] <- d[i][sample.int(length(i))]
            tstar[r, ] <- ex_mht_stats(Y, dd, Qz)$t
        }
        pstar <- 2 * stats::pt(-abs(tstar), st$df)
        if ("wy_minp" %in% methods) {
            o <- order(pval); ps <- pstar[, o, drop = FALSE]
            q <- ps; for (j in (m - 1):1) q[, j] <- pmin(ps[, j], q[, j + 1])           # successive minima from the least significant up
            a <- vapply(seq_len(m), function(j) mean(q[, j] <= pval[o][j] + 1e-12), numeric(1))
            a <- cummax(a); adj$wy_minp <- numeric(m); adj$wy_minp[o] <- a
        }
        if ("wy_maxt" %in% methods) {
            o <- order(-abs(st$t)); ts <- abs(tstar)[, o, drop = FALSE]
            q <- ts; for (j in (m - 1):1) q[, j] <- pmax(ts[, j], q[, j + 1])
            a <- vapply(seq_len(m), function(j) mean(q[, j] >= abs(st$t)[o][j] - 1e-12), numeric(1))
            a <- cummax(a); adj$wy_maxt <- numeric(m); adj$wy_maxt[o] <- a
        }
    }
    if ("rw" %in% methods) {
        tb <- matrix(0, nboot, m)
        for (r in seq_len(nboot)) {
            i <- sample.int(n, n, replace = TRUE)
            if (min(sum(d[i]), sum(1 - d[i])) < 2) { tb[r, ] <- NA; next }
            Zi <- do.call(cbind, lapply(cols, function(cc) cc[i, , drop = FALSE]))
            qi <- tryCatch(ex_mht_q(Zi), error = function(e) NULL)
            if (is.null(qi) || ncol(qi) >= n - 2) { tb[r, ] <- NA; next }
            s <- ex_mht_stats(Y[i, , drop = FALSE], d[i], qi)
            tb[r, ] <- (s$theta - st$theta) / s$se
        }
        tb <- tb[stats::complete.cases(tb), , drop = FALSE]
        o <- order(-abs(st$t)); ts <- abs(tb)[, o, drop = FALSE]
        q <- ts; for (j in (m - 1):1) q[, j] <- pmax(ts[, j], q[, j + 1])
        a <- vapply(seq_len(m), function(j) mean(q[, j] >= abs(st$t)[o][j] - 1e-12), numeric(1))
        a <- cummax(a); adj$rw <- numeric(m); adj$rw[o] <- a
    }
    for (mt in methods) out[[mt]] <- adj[[mt]]
    attr(out, "df") <- st$df
    out
}
