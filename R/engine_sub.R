# Subgroup effects and factorial designs, built on the regression with robust variance of engine_effects.R (ex_ols).
# Inference for single contrasts uses t with n - p residual degrees of freedom (G - 1 with clusters, CR1); joint tests use F.

ex_sub_se <- function(se, cluster) if (identical(se, "auto")) (if (is.null(cluster)) "hc2" else "cr1") else se

# Linear combinations L b of a fitted regression (ex_ols result): estimate, standard error, t, df, p and interval. L has one row per contrast.
ex_contrast <- function(fit, L, level = 0.95) {
    L <- matrix(L, ncol = length(fit$b))
    est <- as.numeric(L %*% fit$b); se <- sqrt(pmax(rowSums((L %*% fit$V) * L), 0))
    df <- if (!is.na(fit$G)) fit$G - 1 else fit$dfres
    tt <- est / se
    data.frame(estimate = est, se = se, t = tt, df = df, p = 2 * stats::pt(-abs(tt), df),
               lo = est - stats::qt(1 - (1 - level) / 2, df) * se, hi = est + stats::qt(1 - (1 - level) / 2, df) * se)
}

# Joint Wald (F) test that R b = 0: F = (R b)' (R V R')^-1 (R b) / q with q and n - p (G - 1 with clusters) degrees of freedom.
ex_wald <- function(fit, R) {
    R <- matrix(R, ncol = length(fit$b)); q <- nrow(R)
    rb <- as.numeric(R %*% fit$b); V <- R %*% fit$V %*% t(R)
    Fs <- tryCatch(as.numeric(t(rb) %*% solve(V, rb)) / q, error = function(e) NA_real_)
    df2 <- if (!is.na(fit$G)) fit$G - 1 else fit$dfres
    c(F = Fs, df1 = q, df2 = df2, p = if (is.finite(Fs)) stats::pf(Fs, q, df2, lower.tail = FALSE) else NA_real_)
}

# Design matrix: columns given as a list of numeric matrices/vectors (an intercept is added unless intercept = FALSE); drops constant and aliased columns.
ex_dm <- function(cols, intercept = TRUE) {
    M <- do.call(cbind, c(if (intercept) list(`(Intercept)` = 1), cols))
    qrM <- qr(M); if (qrM$rank < ncol(M)) M <- M[, sort(qrM$pivot[seq_len(qrM$rank)]), drop = FALSE]
    M
}
ex_dummies <- function(f) { f <- droplevels(f); if (nlevels(f) < 2) return(NULL); stats::model.matrix(~ f)[, -1, drop = FALSE] }

# ---------------------------------------------------------------------------------------------------------------------------
# Subgroup effects. arm: factor with two levels (control first); groups: named list of factors (the subgroup variables).
# method "diff" (difference in means in each subgroup) or "adjust" (covariates X and block fixed effects added in the subgroup).
# adjust: p-value adjustment across the subgroup effects: "none", "holm", "bonferroni", "bh". Interaction tests are adjusted across variables.
ex_subgroup <- function(y, arm, groups, X = NULL, block = NULL, cluster = NULL, method = c("diff", "adjust"), se_type = "auto", level = 0.95, adjust = "holm", min_n = 4) {
    method <- match.arg(method)
    se_type <- ex_sub_se(se_type, cluster)
    if (nlevels(arm) != 2) stop("Subgroup effects compare two groups; select the treatment and the control.", call. = FALSE)
    ok <- stats::complete.cases(y, arm, if (!is.null(X)) X, if (!is.null(block)) block, if (!is.null(cluster)) cluster, as.data.frame(groups))
    y <- y[ok]; arm <- droplevels(arm[ok]); groups <- lapply(groups, function(g) droplevels(g[ok]))
    if (!is.null(X)) X <- X[ok, , drop = FALSE]; if (!is.null(block)) block <- block[ok]; if (!is.null(cluster)) cluster <- cluster[ok]
    d <- as.numeric(arm == levels(arm)[2])
    xc <- if (method == "adjust") X else NULL
    overall <- ex_effects(y, arm, xc, block, cluster, if (method == "adjust") "adjust" else "diff", se_type, level)
    rows <- list(); ints <- list()
    for (v in names(groups)) {
        g <- groups[[v]]
        if (nlevels(g) < 2) stop(sprintf("The subgroup variable '%s' has a single value.", v), call. = FALSE)
        if (nlevels(g) > 12) stop(sprintf("The subgroup variable '%s' has more than 12 values.", v), call. = FALSE)
        for (lv in levels(g)) {
            s <- which(g == lv); n1 <- sum(d[s] == 1); n0 <- sum(d[s] == 0)
            r <- data.frame(variable = v, level = lv, n = length(s), n_treat = n1, n_control = n0, estimate = NA_real_, se = NA_real_, df = NA_real_, p = NA_real_, lo = NA_real_, hi = NA_real_, stringsAsFactors = FALSE)
            if (n1 >= min_n / 2 && n0 >= min_n / 2) {
                e <- tryCatch(ex_effects(y[s], arm[s], if (!is.null(xc)) xc[s, , drop = FALSE], if (!is.null(block)) droplevels(block[s]), if (!is.null(cluster)) cluster[s],
                                         if (method == "adjust") "adjust" else "diff", se_type, level), error = function(e) NULL)
                if (!is.null(e)) r[, c("estimate", "se", "df", "p", "lo", "hi")] <- e[1, c("estimate", "se", "df", "p", "lo", "hi")]
            }
            rows[[length(rows) + 1]] <- r
        }
        # interaction: treatment x subgroup in one regression
        G <- ex_dummies(g); GI <- d * G; colnames(GI) <- paste0("arm:", colnames(G))
        cols <- list(arm = d, G, GI)
        if (!is.null(xc)) cols <- c(cols, list(xc))
        if (!is.null(block)) cols <- c(cols, list(ex_dummies(block)))
        M <- ex_dm(cols)
        f <- ex_ols(y, M, se_type, cluster, 2)
        ia <- which(startsWith(colnames(M), "arm:"))
        w <- if (length(ia)) ex_wald(f, diag(ncol(M))[ia, , drop = FALSE]) else c(F = NA, df1 = NA, df2 = NA, p = NA)
        ints[[v]] <- data.frame(variable = v, F = w[["F"]], df1 = w[["df1"]], df2 = w[["df2"]], p = w[["p"]], stringsAsFactors = FALSE)
    }
    eff <- do.call(rbind, rows); int <- do.call(rbind, ints); rownames(int) <- NULL
    eff$p_adj <- if (adjust == "none") eff$p else stats::p.adjust(eff$p, c(holm = "holm", bonferroni = "bonferroni", bh = "BH")[[adjust]])
    int$p_adj <- if (adjust == "none") int$p else stats::p.adjust(int$p, c(holm = "holm", bonferroni = "bonferroni", bh = "BH")[[adjust]])
    list(overall = overall[1, ], effects = eff, interaction = int, se_type = se_type, arm_levels = levels(arm))
}

# ---------------------------------------------------------------------------------------------------------------------------
# Factorial design with two factors A and B (factors with control levels first). Saturated cell-means regression (covariates and block fixed
# effects added) and linear contrasts: simple effects, main effects (equal-weighted average of the simple effects over the other factor),
# interaction contrasts and their joint test; plus the effect of each factor ignoring the other.
ex_factorial <- function(y, A, B, X = NULL, block = NULL, cluster = NULL, se_type = "auto", level = 0.95) {
    se_type <- ex_sub_se(se_type, cluster)
    ok <- stats::complete.cases(y, A, B, if (!is.null(X)) X, if (!is.null(block)) block, if (!is.null(cluster)) cluster)
    y <- y[ok]; A <- droplevels(A[ok]); B <- droplevels(B[ok])
    if (!is.null(X)) X <- X[ok, , drop = FALSE]; if (!is.null(block)) block <- block[ok]; if (!is.null(cluster)) cluster <- cluster[ok]
    la <- levels(A); lb <- levels(B); na <- length(la); nb <- length(lb)
    if (na < 2 || nb < 2) stop("Each factor needs at least two levels.", call. = FALSE)
    if (na > 6 || nb > 6) stop("A factor has more than 6 levels.", call. = FALSE)
    cell <- interaction(A, B, drop = FALSE, sep = " x "); cl <- levels(cell)
    tab <- table(cell); if (any(tab < 2)) stop(sprintf("Every cell needs at least two observations; empty or tiny cells: %s.", paste(names(tab)[tab < 2], collapse = ", ")), call. = FALSE)
    Cm <- stats::model.matrix(~ 0 + cell); colnames(Cm) <- cl
    cols <- list(Cm)
    if (!is.null(X)) cols <- c(cols, list(X))
    if (!is.null(block)) cols <- c(cols, list(ex_dummies(block)))
    M <- ex_dm(cols, intercept = FALSE)
    f <- ex_ols(y, M, se_type, cluster, 1)
    P <- ncol(M); cidx <- function(a, b) match(paste(a, b, sep = " x "), colnames(M))
    L1 <- function(a, b, sgn = 1) { v <- numeric(P); v[cidx(a, b)] <- sgn; v }
    cellmeans <- data.frame(A = rep(la, times = nb), B = rep(lb, each = na), n = as.integer(tab[paste(rep(la, times = nb), rep(lb, each = na), sep = " x ")]),
                            mean = tapply(y, cell, mean)[paste(rep(la, times = nb), rep(lb, each = na), sep = " x ")], sd = tapply(y, cell, stats::sd)[paste(rep(la, times = nb), rep(lb, each = na), sep = " x ")])
    rownames(cellmeans) <- NULL
    out <- list(cells = cellmeans, se_type = se_type, levels = list(A = la, B = lb))
    # simple effects
    S <- list(); Ls <- list()
    for (b in lb) for (a in la[-1]) { S[[length(S) + 1]] <- data.frame(effect = sprintf("%s vs %s, when B = %s", a, la[1], b), stringsAsFactors = FALSE); Ls[[length(Ls) + 1]] <- L1(a, b) - L1(la[1], b) }
    for (a in la) for (b in lb[-1]) { S[[length(S) + 1]] <- data.frame(effect = sprintf("%s vs %s, when A = %s", b, lb[1], a), stringsAsFactors = FALSE); Ls[[length(Ls) + 1]] <- L1(a, b) - L1(a, lb[1]) }
    out$simple <- cbind(do.call(rbind, S), ex_contrast(f, do.call(rbind, Ls), level))
    # main effects: average of the simple effects over the other factor with equal weights
    Mn <- list(); Lm <- list()
    for (a in la[-1]) { Mn[[length(Mn) + 1]] <- data.frame(effect = sprintf("A: %s vs %s, averaged over B", a, la[1]), stringsAsFactors = FALSE); Lm[[length(Lm) + 1]] <- Reduce(`+`, lapply(lb, function(b) L1(a, b) - L1(la[1], b))) / nb }
    for (b in lb[-1]) { Mn[[length(Mn) + 1]] <- data.frame(effect = sprintf("B: %s vs %s, averaged over A", b, lb[1]), stringsAsFactors = FALSE); Lm[[length(Lm) + 1]] <- Reduce(`+`, lapply(la, function(a) L1(a, b) - L1(a, lb[1]))) / na }
    out$main <- cbind(do.call(rbind, Mn), ex_contrast(f, do.call(rbind, Lm), level))
    # interaction contrasts and their joint test
    It <- list(); Li <- list()
    for (a in la[-1]) for (b in lb[-1]) { It[[length(It) + 1]] <- data.frame(effect = sprintf("(%s vs %s) x (%s vs %s)", a, la[1], b, lb[1]), stringsAsFactors = FALSE); Li[[length(Li) + 1]] <- L1(a, b) - L1(la[1], b) - L1(a, lb[1]) + L1(la[1], lb[1]) }
    Lint <- do.call(rbind, Li)
    out$interaction <- cbind(do.call(rbind, It), ex_contrast(f, Lint, level))
    out$joint <- ex_wald(f, Lint)
    # effect of each factor ignoring the other (the comparison of all units by that factor alone)
    ign <- function(fac, nm) { r <- ex_effects(y, fac, X, block, cluster, if (is.null(X)) "diff" else "adjust", se_type, level); r$arm <- sprintf("%s: %s vs %s, ignoring the other factor", nm, r$arm, levels(fac)[1]); r }
    out$ignoring <- rbind(ign(A, "A"), ign(B, "B"))[, c("arm", "estimate", "se", "t", "df", "p", "lo", "hi")]
    names(out$ignoring)[1] <- "effect"
    out
}
