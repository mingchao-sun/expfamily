# jamovi analysis class: power, minimum detectable effect and sample size of a randomized experiment.

expowerClass <- R6::R6Class(
    "expowerClass",
    inherit = expowerBase,
    private = list(

        .run = function() {
            o <- self$options
            ex_show_text(self$results, o, "expower")
            notes <- character(0)
            sd <- o$sd; r2 <- o$r2; ncov <- o$ncov; rho <- o$rho
            cluster_design <- identical(o$design, "cluster")
            if (isTRUE(o$useData)) {
                if (is.null(o$dataOutcome)) jmvcore::reject("Select an outcome to take the standard deviation from the data, or untick the option.", code = "")
                y <- ex_guard(ex_num(self$data[[o$dataOutcome]], "outcome"))
                sd <- stats::sd(y, na.rm = TRUE)
                if (length(o$dataCovs) > 0) {
                    X <- ex_guard(ex_covmat(self$data, o$dataCovs)); ok <- stats::complete.cases(y, X)
                    f <- stats::lm.fit(cbind(1, X[ok, , drop = FALSE]), y[ok])
                    r2 <- 1 - sum(f$residuals^2) / sum((y[ok] - mean(y[ok]))^2); ncov <- f$rank - 1
                }
                if (!is.null(o$dataCluster)) { rho <- ex_icc(y, self$data[[o$dataCluster]]); notes <- c(notes, "The intracluster correlation is the ANOVA estimate of the outcome in the data.") }
            }
            if (!is.finite(sd) || sd <= 0) jmvcore::reject("The standard deviation must be positive.", code = "")
            sided <- if (identical(o$sided, "one")) 1 else 2
            ctr <- if (cluster_design) list(m = o$msize, rho = rho, cv = o$cv) else list(m = NULL, rho = 0, cv = 0)
            J <- if (cluster_design) o$clusters else o$ntotal
            n1 <- max(2, ceiling(J * o$share)); n0 <- max(2, J - n1)
            unit <- if (cluster_design) "clusters" else "units"
            args <- function(d, a1, a0, pw = NULL) list(delta = d, sd = sd, n1 = a1, n0 = a0, alpha = o$alpha, r2 = r2, ncov = ncov, m = ctr$m, rho = ctr$rho, cv = ctr$cv, sided = sided)
            inp <- self$results$inputs
            rows <- list(c("Standard deviation of the outcome", sd), c("Share of variance explained by covariates", r2), c("Significance level", o$alpha), c("Share assigned to treatment", o$share))
            if (cluster_design) rows <- c(rows, list(c("Units per cluster", o$msize), c("Intracluster correlation", rho), c("Coefficient of variation of cluster sizes", o$cv)))
            for (i in seq_along(rows)) inp$addRow(rowKey = i, values = list(item = rows[[i]][1], value = as.numeric(rows[[i]][2])))
            rs <- self$results$res
            add <- function(i, item, value) rs$addRow(rowKey = i, values = list(item = item, value = value))
            if (o$mode == "power") {
                pw <- do.call(ex_power, args(o$effect, n1, n0))
                add(1, sprintf("Power to detect %s with %d %s (%d treated, %d control)", ex_fmt(o$effect, 3), n1 + n0, unit, n1, n0), pw)
                add(2, "Effect in standard deviations", o$effect / sd)
            } else if (o$mode == "mde") {
                m <- do.call(ex_mde, c(list(sd = sd, n1 = n1, n0 = n0, alpha = o$alpha, power = o$target, r2 = r2, ncov = ncov, m = ctr$m, rho = ctr$rho, cv = ctr$cv, sided = sided)))
                if (!is.finite(m)) jmvcore::reject("No effect within ten standard deviations reaches the target power with this sample.", code = "")
                add(1, sprintf("Smallest effect detected with power %s with %d %s", ex_fmt(o$target, 2), n1 + n0, unit), m)
                add(2, "In standard deviations", m / sd)
            } else {
                ss <- ex_sample_size(o$effect, sd, o$alpha, o$target, o$share, r2, ncov, ctr$m, ctr$rho, ctr$cv, sided)
                if (anyNA(ss)) jmvcore::reject("The target power cannot be reached with a feasible sample.", code = "")
                add(1, sprintf("%s needed (power %s for an effect of %s)", if (cluster_design) "Clusters" else "Units", ex_fmt(o$target, 2), ex_fmt(o$effect, 3)), sum(ss))
                add(2, paste("Treated", unit), ss[["n1"]]); add(3, paste("Control", unit), ss[["n0"]])
                if (cluster_design) add(4, "Units in total", sum(ss) * o$msize)
                n1 <- ss[["n1"]]; n0 <- ss[["n0"]]
            }
            if (cluster_design) {
                de <- 1 + ((ctr$cv^2 + 1) * ctr$m - 1) * ctr$rho
                add(10, "Design effect (inflation of the variance)", de)
                notes <- c(notes, "Cluster randomization: the standard error uses the design effect 1 + ((CV^2 + 1) m - 1) rho (Eldridge et al., 2006) and the t distribution with the number of clusters minus two degrees of freedom.")
            }
            if (ncov > 0 && r2 > 0) notes <- c(notes, sprintf("Covariates: the residual standard deviation is reduced by the factor %s and %d degrees of freedom are used.", ex_fmt(sqrt(1 - r2), 3), ncov))
            self$results$notes$setContent(if (length(notes)) paste0("<p>", paste(notes, collapse = " "), "</p>") else "")
            if (isTRUE(o$plot)) {
                eff <- if (o$mode == "mde") add_mde <- do.call(ex_mde, list(sd = sd, n1 = n1, n0 = n0, alpha = o$alpha, power = o$target, r2 = r2, ncov = ncov, m = ctr$m, rho = ctr$rho, cv = ctr$cv, sided = sided)) else o$effect
                Ns <- unique(round(seq(max(6, (n1 + n0) * 0.2), (n1 + n0) * 3, length.out = 60)))
                pw <- vapply(Ns, function(N) { a1 <- max(2, ceiling(N * o$share)); do.call(ex_power, args(eff, a1, max(2, N - a1))) }, numeric(1))
                self$results$powPlot$setState(list(N = Ns, power = pw, now = n1 + n0, target = o$target, unit = unit, effect = eff))
            }
        },

        .plotPower = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            df <- data.frame(N = st$N, power = st$power)
            g <- ggplot2::ggplot(df, ggplot2::aes(x = N, y = power)) + ggtheme +
                ggplot2::geom_hline(yintercept = st$target, linetype = "dashed", colour = "grey40") +
                ggplot2::geom_line(linewidth = 0.9, colour = "#6b9de8") +
                ggplot2::geom_vline(xintercept = st$now, colour = "#e6ac40", linewidth = 0.8) +
                ggplot2::scale_y_continuous(limits = c(0, 1)) +
                ggplot2::labs(x = paste("Total number of", st$unit), y = "Power", caption = sprintf("Effect %s; orange line: the sample of the calculation", signif(st$effect, 3)))
            print(g)
            TRUE
        }
    )
)
