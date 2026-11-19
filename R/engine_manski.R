# Worst-case (Manski) bounds on the average treatment effect, with monotone treatment response and monotone treatment selection
# (Manski, 1990; Manski and Pepper, 2000). The outcome is bounded: Y in [ymin, ymax].
#   "random":    treatment assigned at random, the outcome may be missing for some units. For each arm the mean outcome of the units
#                observed is combined with the lowest or the highest possible value for the missing ones.
#   "selection": treatment not random, the outcome is observed for everybody. Y(1) is observed only for the treated and Y(0) only for the
#                controls; the other potential outcome is anywhere in [ymin, ymax] unless an assumption restricts it:
#                MTR  monotone treatment response: Y(1) >= Y(0) for every unit;
#                MTS  monotone treatment selection: E[Y(d) | D = 1] >= E[Y(d) | D = 0] (the treated would do better under either treatment).

ex_man_rows <- c(none = "No assumptions (worst case)", mtr = "Monotone treatment response (effect is never negative)", mts = "Monotone treatment selection (the treated do better anyway)", both = "Both assumptions")

# One set of bounds. Returns a matrix with a row per assumption set and the columns e1_lo, e1_hi, e0_lo, e0_hi, ate_lo, ate_hi.
ex_man_point <- function(y, d, mode = c("random", "selection"), ymin, ymax) {
    mode <- match.arg(mode)
    if (mode == "random") {
        one <- function(v) { s <- !is.na(y[v]); ps <- mean(s); m <- if (any(s)) mean(y[v][s]) else NA_real_; c(lo = m * ps + ymin * (1 - ps), hi = m * ps + ymax * (1 - ps)) }
        b1 <- one(d == 1); b0 <- one(d == 0)
        return(matrix(c(b1[["lo"]], b1[["hi"]], b0[["lo"]], b0[["hi"]], b1[["lo"]] - b0[["hi"]], b1[["hi"]] - b0[["lo"]]), 1, 6,
                      dimnames = list("none", c("e1_lo", "e1_hi", "e0_lo", "e0_hi", "ate_lo", "ate_hi"))))
    }
    p1 <- mean(d == 1); p0 <- 1 - p1; m1 <- mean(y[d == 1]); m0 <- mean(y[d == 0])
    # worst-case intervals, then the restrictions of the assumptions intersect them
    e1 <- c(m1 * p1 + ymin * p0, m1 * p1 + ymax * p0); e0 <- c(m0 * p0 + ymin * p1, m0 * p0 + ymax * p1)
    mtr1 <- c(m1 * p1 + m0 * p0, m1 * p1 + ymax * p0); mtr0 <- c(m0 * p0 + ymin * p1, m0 * p0 + m1 * p1)      # Y(1) >= Y(0): the controls' Y(1) is at least their Y, the treated's Y(0) at most their Y
    mts1 <- c(m1 * p1 + ymin * p0, m1);                 mts0 <- c(m0, m0 * p0 + ymax * p1)                     # E[Y(1) | D = 0] <= m1 and E[Y(0) | D = 1] >= m0
    meet <- function(a, b) c(max(a[1], b[1]), min(a[2], b[2]))
    sets <- list(none = list(e1, e0), mtr = list(meet(e1, mtr1), meet(e0, mtr0)), mts = list(meet(e1, mts1), meet(e0, mts0)),
                 both = list(meet(meet(e1, mtr1), mts1), meet(meet(e0, mtr0), mts0)))
    out <- t(vapply(names(sets), function(k) { a <- sets[[k]][[1]]; b <- sets[[k]][[2]]
        lo <- a[1] - b[2]; hi <- a[2] - b[1]
        if (k %in% c("mtr", "both")) lo <- max(lo, 0)                                           # Y(1) >= Y(0) for every unit: the average effect is not negative
        c(a, b, lo, hi) }, numeric(6)))
    colnames(out) <- c("e1_lo", "e1_hi", "e0_lo", "e0_hi", "ate_lo", "ate_hi")
    out
}

# y numeric (NA = not observed, allowed in "random" mode), d 0/1. ymin / ymax: the bounds of the outcome (default: the observed range).
ex_manski <- function(y, d, mode = c("random", "selection"), ymin = NA_real_, ymax = NA_real_, level = 0.95, nboot = 1000, seed = 1234) {
    mode <- match.arg(mode)
    ok <- !is.na(d) & (mode == "random" | !is.na(y)); y <- y[ok]; d <- d[ok]
    if (sum(d == 1) < 2 || sum(d == 0) < 2) stop("Each group needs at least two units.", call. = FALSE)
    obs <- y[!is.na(y)]
    if (length(obs) < 4) stop("There are too few observed outcomes.", call. = FALSE)
    if (!is.finite(ymin)) ymin <- min(obs)
    if (!is.finite(ymax)) ymax <- max(obs)
    if (ymin >= ymax) stop("The lowest possible outcome must be below the highest possible outcome.", call. = FALSE)
    if (any(obs < ymin) || any(obs > ymax)) stop("Some outcomes lie outside the stated range of the outcome.", call. = FALSE)
    if (mode == "random" && any(vapply(0:1, function(g) sum(!is.na(y[d == g])) < 2, TRUE))) stop("Each group needs at least two observed outcomes.", call. = FALSE)
    est <- ex_man_point(y, d, mode, ymin, ymax)
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(seed)
    n <- length(y); i1 <- which(d == 1); i0 <- which(d == 0)
    bs <- array(NA_real_, c(nboot, nrow(est), 2))
    for (b in seq_len(nboot)) {
        ii <- if (mode == "random") c(i1[sample.int(length(i1), replace = TRUE)], i0[sample.int(length(i0), replace = TRUE)]) else sample.int(n, replace = TRUE)
        if (mode == "selection" && (sum(d[ii] == 1) < 1 || sum(d[ii] == 0) < 1)) next
        r <- tryCatch(ex_man_point(y[ii], d[ii], mode, ymin, ymax), error = function(e) NULL)
        if (!is.null(r)) bs[b, , ] <- r[, c("ate_lo", "ate_hi")]
    }
    se <- apply(bs, c(2, 3), stats::sd, na.rm = TRUE)
    ci <- t(vapply(seq_len(nrow(est)), function(k) {
        if (est[k, "ate_lo"] > est[k, "ate_hi"] + 1e-12) return(c(NA_real_, NA_real_))              # the assumptions contradict the data: no interval
        cc <- ex_im_crit(est[k, "ate_hi"] - est[k, "ate_lo"], max(se[k, ]), level)
        c(est[k, "ate_lo"] - cc * se[k, 1], est[k, "ate_hi"] + cc * se[k, 2]) }, numeric(2)))
    s <- !is.na(y)
    m1 <- mean(y[d == 1 & s]); m0 <- mean(y[d == 0 & s])
    v1 <- stats::var(y[d == 1 & s]) / sum(d == 1 & s); v0 <- stats::var(y[d == 0 & s]) / sum(d == 0 & s)
    list(bounds = est, empty = unname(est[, "ate_lo"] > est[, "ate_hi"] + 1e-12), se_lo = se[, 1], se_hi = se[, 2], ci_lo = ci[, 1], ci_hi = ci[, 2], mode = mode, ymin = ymin, ymax = ymax,
         naive = m1 - m0, naive_se = sqrt(v1 + v0), n = length(y), n1 = sum(d == 1), n0 = sum(d == 0), miss1 = mean(!s[d == 1]), miss0 = mean(!s[d == 0]), p_treated = mean(d == 1))
}
