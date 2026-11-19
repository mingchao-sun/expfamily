# Randomization (Fisher) inference for a two-group comparison: p-values under the sharp null of no effect for any unit,
# for complete, blocked and cluster randomization, by exact enumeration of the randomizations or by simulation, and a
# confidence interval for a constant additive effect by test inversion. Base R only. Checked against ri2 and randomizr.
# Test statistics: difference in means, difference in mean ranks (Wilcoxon) and the studentised (Welch) t statistic.

ex_ri_stats <- c(diff = "Difference in means", rank = "Difference in mean ranks", tstat = "Studentised difference (Welch t)")

# Statistic of every column of Tmat (n x R matrix of 0/1 treatment indicators) for the outcome y.
ex_ri_stat <- function(y, Tmat, stat) {
    if (stat == "rank") y <- rank(y)
    n1 <- colSums(Tmat); n0 <- nrow(Tmat) - n1
    s1 <- colSums(Tmat * y); s0 <- sum(y) - s1
    m1 <- s1 / n1; m0 <- s0 / n0
    if (stat %in% c("diff", "rank")) return(m1 - m0)
    q1 <- colSums(Tmat * y^2); q0 <- sum(y^2) - q1
    v1 <- (q1 - n1 * m1^2) / (n1 - 1); v0 <- (q0 - n0 * m0^2) / (n0 - 1)
    (m1 - m0) / sqrt(v1 / n1 + v0 / n0)
}

# Randomizations of the assignment vector a (0/1 per assignment unit) within blocks.
# Returns an (assignment units x R) matrix and whether all randomizations were enumerated.
ex_ri_draws <- function(a, blocks, sims, exact_max, seed) {
    nu <- length(a)
    bl <- split(seq_len(nu), factor(blocks))
    m_b <- vapply(bl, function(i) sum(a[i]), numeric(1))
    N_b <- lengths(bl)
    if (any(m_b == 0 | m_b == N_b)) { bad <- TRUE } else bad <- FALSE
    total <- prod(choose(N_b, m_b))
    if (total <= exact_max) {
        combs <- lapply(seq_along(bl), function(b) {
            Nb <- N_b[b]; mb <- m_b[b]
            if (mb == 0 || mb == Nb) return(matrix(rep(as.numeric(mb == Nb), Nb), Nb, 1))
            cm <- utils::combn(Nb, mb)
            ind <- matrix(0, Nb, ncol(cm)); ind[cbind(as.vector(cm), rep(seq_len(ncol(cm)), each = mb))] <- 1
            ind
        })
        grid <- do.call(expand.grid, lapply(combs, function(m) seq_len(ncol(m))))
        out <- matrix(0, nu, nrow(grid))
        for (b in seq_along(bl)) out[bl[[b]], ] <- combs[[b]][, grid[[b]], drop = FALSE]
        return(list(draws = out, exact = TRUE, n = ncol(out)))
    }
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(seed)
    out <- matrix(0, nu, sims)
    for (r in seq_len(sims)) {
        v <- a
        for (i in bl) if (length(i) > 1) v[i] <- a[i][sample.int(length(i))]
        out[, r] <- v
    }
    list(draws = out, exact = FALSE, n = sims)
}

# y: outcome; treat: 0/1 per unit; block: optional factor per unit; cluster: optional cluster id per unit
# (treatment and block must be constant within clusters). Returns the observed statistic, p-values and, for the
# difference in means, the confidence interval of a constant additive effect.
ex_ri <- function(y, treat, block = NULL, cluster = NULL, stat = "diff", sims = 5000, exact_max = 20000, seed = 1234, level = 0.95, ci = TRUE, keep = 4000) {
    keepi <- stats::complete.cases(y, treat, if (!is.null(block)) block, if (!is.null(cluster)) cluster)
    y <- y[keepi]; treat <- treat[keepi]
    if (!is.null(block)) block <- block[keepi]; if (!is.null(cluster)) cluster <- cluster[keepi]
    if (!all(treat %in% c(0, 1))) stop("The treatment must be 0/1.", call. = FALSE)
    if (sum(treat) < 2 || sum(1 - treat) < 2) stop("Each group needs at least two units.", call. = FALSE)
    n <- length(y)
    if (is.null(cluster)) { cl <- seq_len(n); u_treat <- treat; u_block <- if (is.null(block)) rep(1, n) else block }
    else {
        cl <- match(cluster, unique(cluster))
        tc <- tapply(treat, cl, function(z) length(unique(z)))
        if (any(tc > 1)) stop("The treatment varies within a cluster; with clusters it must be assigned to whole clusters.", call. = FALSE)
        u_treat <- as.numeric(tapply(treat, cl, `[`, 1))
        if (is.null(block)) u_block <- rep(1, length(u_treat))
        else {
            bc <- tapply(as.character(block), cl, function(z) length(unique(z)))
            if (any(bc > 1)) stop("The block varies within a cluster.", call. = FALSE)
            u_block <- as.character(tapply(as.character(block), cl, `[`, 1))
        }
        if (sum(u_treat) < 2 || sum(1 - u_treat) < 2) stop("Each group needs at least two clusters.", call. = FALSE)
    }
    dr <- ex_ri_draws(u_treat, u_block, sims, exact_max, seed)
    obs <- ex_ri_stat(y, matrix(treat, ncol = 1), stat)
    # statistic over the randomizations, in chunks
    R <- ncol(dr$draws); T <- numeric(R)
    a_star <- numeric(R); b_star <- numeric(R)
    chunk <- max(1, floor(2e6 / n))
    for (s in seq(1, R, by = chunk)) {
        j <- s:min(R, s + chunk - 1)
        Tm <- dr$draws[cl, j, drop = FALSE]
        T[j] <- ex_ri_stat(y, Tm, stat)
        if (stat == "diff" && ci) { a_star[j] <- T[j]; b_star[j] <- ex_ri_stat(treat, Tm, "diff") }
    }
    tol <- 1e-10 * (1 + abs(obs))
    p2 <- mean(abs(T) >= abs(obs) - tol); pr <- mean(T >= obs - tol); pl <- mean(T <= obs + tol)
    res <- list(stat = obs, p_two = p2, p_right = pr, p_left = pl, n_rand = R, exact = dr$exact, mc_se = if (dr$exact) 0 else sqrt(p2 * (1 - p2) / R),
                n = n, n1 = sum(treat), n0 = sum(1 - treat), dist = if (R > keep) T[seq(1, R, length.out = keep)] else T, ci = NULL)
    if (stat == "diff" && ci) {
        alpha <- 1 - level
        pfun <- function(tau) mean(abs(a_star - tau * b_star) >= abs(obs - tau) - 1e-10 * (1 + abs(obs - tau)))
        se0 <- sqrt(stats::var(y[treat == 1]) / sum(treat) + stats::var(y[treat == 0]) / sum(1 - treat))
        width <- 12 * se0 + 1e-8
        edge <- function(dir) {
            tau_in <- obs
            if (pfun(tau_in) <= alpha) return(NA_real_)
            step <- width / 200
            tau_out <- NA_real_
            for (k in 1:200) { t <- obs + dir * k * step; if (pfun(t) <= alpha) { tau_out <- t; break }; tau_in <- t }
            if (is.na(tau_out)) return(dir * Inf)
            for (it in 1:80) { mid <- (tau_in + tau_out) / 2; if (pfun(mid) > alpha) tau_in <- mid else tau_out <- mid }
            tau_in
        }
        res$ci <- c(edge(-1), edge(1))
        res$pfun <- pfun
    }
    res
}
