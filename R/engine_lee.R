# Lee (2009) trimming bounds for the effect of a randomized treatment when the outcome is observed only for some units.
# Monotonicity: the treatment moves selection (having an observed outcome) in one direction for everybody. The group with the higher
# response rate contains units that would not have responded in the other group; trimming the share q of its observed outcomes
# from the top or the bottom gives the lower and upper bounds of the effect for the units observed under both assignments.

ex_lee_trim <- function(yg, q, side) {
    # mean of the group's observed outcomes after removing the share q of them: the largest ("top") or the smallest ("bottom").
    # Exact trimming: with m = q n, the m-th observation from the end is removed in part (weight 1 - frac(m)), as in Lee (2009) and Stata leebounds.
    if (q <= 0) return(mean(yg))
    ys <- sort(yg, decreasing = (side == "top")); n <- length(ys); m <- q * n; f <- floor(m)
    w <- c(rep(0, f), if (f < n) 1 - (m - f), rep(1, max(0, n - f - 1)))
    sum(w * ys) / sum(w)
}

# One set of bounds in a (cell of a) sample. y has NA where the outcome is not observed, d is 0/1; dir is "up" when the treatment
# raises the response rate (the treated are trimmed) and "down" when it lowers it (the controls are trimmed).
ex_lee_one <- function(y, d, dir) {
    s <- !is.na(y)
    n1 <- sum(d == 1); n0 <- sum(d == 0)
    p1 <- sum(s & d == 1) / n1; p0 <- sum(s & d == 0) / n0
    y1 <- y[s & d == 1]; y0 <- y[s & d == 0]
    if (length(y1) == 0 || length(y0) == 0) return(c(lower = NA, upper = NA, p1 = p1, p0 = p0, q = NA))
    if (dir == "up") {
        q <- max(0, (p1 - p0) / p1)
        lo <- ex_lee_trim(y1, q, "top") - mean(y0); hi <- ex_lee_trim(y1, q, "bottom") - mean(y0)
    } else {
        q <- max(0, (p0 - p1) / p0)
        lo <- mean(y1) - ex_lee_trim(y0, q, "bottom"); hi <- mean(y1) - ex_lee_trim(y0, q, "top")
    }
    c(lower = lo, upper = hi, p1 = p1, p0 = p0, q = q)
}

# Bounds with optional tightening: trimming is done within the cells of a categorical covariate and the cell bounds are averaged with
# the cell shares of the always-observed group (the observed units of the group with the lower response rate).
ex_lee_est <- function(y, d, dir, cell = NULL) {
    if (is.null(cell)) return(ex_lee_one(y, d, dir))
    base <- ex_lee_one(y, d, dir)
    ref <- if (dir == "up") 0 else 1                       # always-observed units are seen in the group with the lower response rate
    lv <- levels(cell); w <- numeric(length(lv)); b <- matrix(NA_real_, length(lv), 2)
    for (k in seq_along(lv)) {
        i <- which(cell == lv[k])
        w[k] <- sum(!is.na(y[i]) & d[i] == ref)
        if (sum(d[i] == 1) > 0 && sum(d[i] == 0) > 0) { r <- ex_lee_one(y[i], d[i], dir); b[k, ] <- r[c("lower", "upper")] }
    }
    ok <- is.finite(b[, 1]) & w > 0
    if (!any(ok)) return(c(lower = NA, upper = NA, base[c("p1", "p0", "q")]))
    w <- w[ok] / sum(w[ok])
    c(lower = sum(w * b[ok, 1]), upper = sum(w * b[ok, 2]), base[c("p1", "p0", "q")], cells = sum(ok), dropped = length(lv) - sum(ok))
}

# Critical value of the Imbens-Manski (2004) interval for a partially identified parameter [lower, upper].
ex_im_crit <- function(width, se, level) {
    if (!is.finite(se) || se <= 0) return(stats::qnorm(level))
    f <- function(cc) stats::pnorm(cc + width / se) - stats::pnorm(-cc) - level
    stats::uniroot(f, c(stats::qnorm(level) - 1e-9, stats::qnorm(1 - (1 - level) / 2) + 1e-9))$root
}

# y: outcome with NA where unobserved, d: 0/1 treatment, cell: optional factor for tightening.
ex_lee <- function(y, d, cell = NULL, direction = c("auto", "up", "down"), level = 0.95, nboot = 1000, seed = 1234) {
    direction <- match.arg(direction)
    ok <- !is.na(d) & (if (!is.null(cell)) !is.na(cell) else TRUE)
    y <- y[ok]; d <- d[ok]; if (!is.null(cell)) cell <- droplevels(cell[ok])
    if (sum(d == 1) < 2 || sum(d == 0) < 2) stop("Each group needs at least two units.", call. = FALSE)
    s <- !is.na(y)
    p1 <- mean(s[d == 1]); p0 <- mean(s[d == 0])
    if (sum(s[d == 1]) < 2 || sum(s[d == 0]) < 2) stop("Each group needs at least two units with an observed outcome.", call. = FALSE)
    dir <- if (direction == "auto") (if (p1 >= p0) "up" else "down") else direction
    if ((dir == "up" && p1 < p0) || (dir == "down" && p1 > p0)) stop("The chosen direction of selection contradicts the observed response rates: the group that is trimmed must have the higher response rate.", call. = FALSE)
    est <- ex_lee_est(y, d, dir, cell)
    naive <- mean(y[s & d == 1]) - mean(y[s & d == 0])
    set.seed(seed)
    i1 <- which(d == 1); i0 <- which(d == 0)
    bs <- matrix(NA_real_, nboot, 2)
    for (b in seq_len(nboot)) {
        ii <- c(i1[sample.int(length(i1), replace = TRUE)], i0[sample.int(length(i0), replace = TRUE)])
        r <- tryCatch(ex_lee_est(y[ii], d[ii], dir, if (!is.null(cell)) factor(cell[ii], levels = levels(cell)) else NULL), error = function(e) c(lower = NA_real_, upper = NA_real_))
        bs[b, ] <- r[c("lower", "upper")]
    }
    se <- c(stats::sd(bs[, 1], na.rm = TRUE), stats::sd(bs[, 2], na.rm = TRUE))
    cc <- ex_im_crit(est[["upper"]] - est[["lower"]], max(se), level)
    list(lower = est[["lower"]], upper = est[["upper"]], se_lower = se[1], se_upper = se[2],
         ci = c(est[["lower"]] - cc * se[1], est[["upper"]] + cc * se[2]), crit = cc,
         p1 = p1, p0 = p0, q = est[["q"]], dir = dir, naive = naive,
         n1 = sum(d == 1), n0 = sum(d == 0), o1 = sum(s & d == 1), o0 = sum(s & d == 0),
         cells = if (is.null(cell)) NA else est[["cells"]], dropped = if (is.null(cell)) NA else est[["dropped"]])
}
