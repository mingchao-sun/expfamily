# jamovi analysis class: Lee (2009) trimming bounds for the effect of a treatment when outcomes are missing for some units.

exleeClass <- R6::R6Class(
    "exleeClass",
    inherit = exleeBase,
    private = list(

        .run = function() {
            o <- self$options
            ex_show_text(self$results, o, "exlee")
            if (is.null(o$outcome) || is.null(o$treat)) {
                self$results$notes$setContent("<p>Select the outcome (with missing values for units that were not followed up) and the treatment variable to begin.</p>")
                for (nm in c("sel", "res")) self$results[[nm]]$setVisible(FALSE)
                return()
            }
            if (length(intersect(o$tight, c(o$treat, o$outcome))) || identical(o$treat, o$outcome)) jmvcore::reject("A variable can sit in one box only.", code = "")
            arm <- ex_guard(ex_arm(self$data[[o$treat]], o$control))
            ctl <- levels(arm)[1]
            trt <- trimws(o$armTreat)
            if (!nzchar(trt)) trt <- levels(arm)[2]
            if (!trt %in% levels(arm) || identical(trt, ctl)) jmvcore::reject(sprintf("The treatment group value must be one of the non-control values (%s).", paste(levels(arm)[-1], collapse = ", ")), code = "")
            y <- ex_guard(ex_num(self$data[[o$outcome]], "outcome"))
            cell <- NULL
            if (length(o$tight) > 0) {
                parts <- lapply(o$tight, function(v) factor(self$data[[v]]))
                cell <- if (length(parts) == 1) parts[[1]] else interaction(parts, drop = TRUE)
            }
            in2 <- arm %in% c(ctl, trt) & !is.na(arm)
            n_cell_na <- if (!is.null(cell)) sum(in2 & is.na(cell)) else 0
            keep <- in2 & (if (!is.null(cell)) !is.na(cell) else TRUE)
            d <- as.numeric(arm[keep] == trt)
            private$.checkpoint()
            res <- ex_guard(ex_lee(y[keep], d, if (!is.null(cell)) droplevels(cell[keep]) else NULL, o$direction, o$level / 100, o$nboot, o$seed))
            s <- !is.na(y[keep])
            sb <- self$results$sel
            for (g in 1:2) {
                m <- d == (g == 2)
                sb$addRow(rowKey = g, values = list(arm = if (g == 1) paste(ctl, "(control)") else trt, n = sum(m), obs = sum(m & s), rate = mean(s[m]), mean = mean(y[keep][m & s])))
            }
            rb <- self$results$res
            rb$addRow(rowKey = 1, values = list(what = "Observed outcomes only (no correction)", lower = res$naive, upper = res$naive, sel = NaN, seu = NaN, lo = NaN, hi = NaN))
            rb$addRow(rowKey = 2, values = list(what = sprintf("Lee bounds%s", if (!is.null(cell)) " (tightened)" else ""), lower = res$lower, upper = res$upper, sel = res$se_lower, seu = res$se_upper, lo = res$ci[1], hi = res$ci[2]))
            rb$setNote("a", sprintf("Bounds on the effect of %s compared with %s for the units whose outcome is observed under either assignment. The interval is the Imbens-Manski %s%% confidence interval for the effect (critical value %s); standard errors are bootstrap standard errors of the bounds (%s samples within groups).",
                                    trt, ctl, ex_fmt(o$level, 1), ex_fmt(res$crit, 3), format(o$nboot, big.mark = ",")))
            notes <- character(0)
            grp <- if (res$dir == "up") trt else ctl
            oth <- if (res$dir == "up") ctl else trt
            if (res$q > 0) notes <- c(notes, sprintf("Response rates are %s%% in %s and %s%% in %s, so the share %s of the observed outcomes of %s is trimmed from the top (lower bound) or the bottom (upper bound) of their distribution.",
                                                     ex_fmt(100 * (if (res$dir == "up") res$p1 else res$p0), 1), grp, ex_fmt(100 * (if (res$dir == "up") res$p0 else res$p1), 1), oth, ex_fmt(res$q, 3), grp))
            else notes <- c(notes, "The response rates are equal, so no trimming is needed: both bounds equal the difference in means of the observed outcomes.")
            if (identical(o$direction, "auto")) notes <- c(notes, sprintf("The direction of the effect on response (%s) was taken from the observed response rates.", if (res$dir == "up") "treatment raises response" else "treatment lowers response"))
            if (!is.null(cell)) notes <- c(notes, sprintf("Tightened with %d cell(s) of %s; the cell bounds are averaged with the shares of the units observed in %s.%s", res$cells, paste(o$tight, collapse = " by "), oth,
                                                         if (res$dropped > 0) sprintf(" %d cell(s) without units in both groups were left out.", res$dropped) else ""))
            if (n_cell_na > 0) notes <- c(notes, sprintf("%d unit(s) with a missing tightening covariate were dropped.", n_cell_na))
            if (res$upper - res$lower > 4 * max(res$se_lower, res$se_upper)) notes <- c(notes, "The bounds are wide relative to their sampling error: attrition, not sampling variation, limits what can be learned about the effect.")
            self$results$notes$setContent(paste0("<p>", paste(notes, collapse = " "), "</p>"))
        }
    )
)
