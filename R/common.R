# Helpers shared by the experiment analyses.

ex_fmt <- function(x, d = 3) formatC(x, format = "f", digits = d, drop0trailing = TRUE)

ex_guard <- function(expr) tryCatch(expr, error = function(e) jmvcore::reject(conditionMessage(e), code = ""))

ex_show_text <- function(results, opts, key) {
    if (isTRUE(opts$showGuide)) results$guide$setContent(ex_guide_html(key))
    if (isTRUE(opts$showHelp)) results$helpText$setContent(ex_notes_html(key))
}

# Treatment variable as a factor whose first level is the control group (the stated value, else the smallest value or first level).
ex_arm <- function(x, control = "") {
    f <- if (is.factor(x)) droplevels(x) else factor(x, levels = sort(unique(x[!is.na(x)])))
    lv <- levels(f)
    if (length(lv) < 2) stop("The treatment needs at least two groups.", call. = FALSE)
    if (length(lv) > 12) stop("The treatment has more than 12 groups.", call. = FALSE)
    control <- trimws(control %||% "")
    if (nzchar(control)) {
        if (!control %in% lv) stop(sprintf("The control group value '%s' is not among the treatment values (%s).", control, paste(lv, collapse = ", ")), call. = FALSE)
        f <- stats::relevel(f, ref = control)
    }
    f
}
`%||%` <- function(a, b) if (is.null(a)) b else a

ex_num <- function(v, label) {
    if (is.factor(v)) stop(sprintf("The %s must be a numeric variable (set its type to Continuous in the Data tab).", label), call. = FALSE)
    out <- as.numeric(v)
    if (any(is.infinite(out), na.rm = TRUE)) stop(sprintf("The %s has infinite values.", label), call. = FALSE)
    out
}

# Covariates as a numeric matrix (nominal variables become dummies; missing values kept as NA; constant columns dropped).
ex_covmat <- function(data, covs) {
    if (length(covs) == 0) return(NULL)
    df <- data[, covs, drop = FALSE]
    for (v in covs) if (is.factor(df[[v]])) df[[v]] <- droplevels(df[[v]]) else df[[v]] <- ex_num(df[[v]], paste("covariate", v))
    mm <- stats::model.matrix(stats::reformulate(paste0("`", covs, "`")), stats::model.frame(stats::reformulate(paste0("`", covs, "`")), df, na.action = stats::na.pass))[, -1, drop = FALSE]
    mm[, apply(mm, 2, function(z) stats::sd(z, na.rm = TRUE)) > 0, drop = FALSE]
}

# Rows with every needed variable present; returns outcome, arm, covariates, block and cluster for those rows.
ex_prepare <- function(data, outcome, treat, control, covs = NULL, block = NULL, cluster = NULL) {
    if (is.null(outcome) || is.null(treat)) stop("Select an outcome and a treatment.", call. = FALSE)
    if (length(intersect(covs, c(outcome, treat, block, cluster)))) stop("A variable can sit in one box only.", call. = FALSE)
    arm <- ex_arm(data[[treat]], control)
    y <- ex_num(data[[outcome]], "outcome")
    X <- ex_covmat(data, covs)
    bl <- if (!is.null(block)) factor(data[[block]]) else NULL
    cl <- if (!is.null(cluster)) factor(data[[cluster]]) else NULL
    ok <- stats::complete.cases(y, arm, if (!is.null(X)) X, bl, cl)
    n_drop <- sum(!ok)
    if (sum(ok) < 10) stop("Fewer than 10 complete observations.", call. = FALSE)
    arm <- droplevels(arm[ok])
    if (nlevels(arm) < 2) stop("The treatment needs at least two groups among the complete observations.", call. = FALSE)
    list(y = y[ok], arm = arm, X = if (!is.null(X)) X[ok, , drop = FALSE] else NULL, block = if (!is.null(bl)) droplevels(bl[ok]) else NULL,
         cluster = if (!is.null(cl)) droplevels(cl[ok]) else NULL, n = sum(ok), n_drop = n_drop)
}
