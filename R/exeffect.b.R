# jamovi analysis class: treatment effects in a randomized experiment.

exeffectClass <- R6::R6Class(
    "exeffectClass",
    inherit = exeffectBase,
    private = list(

        .run = function() {
            o <- self$options
            ex_show_text(self$results, o, "exeffect")
            if (is.null(o$outcome) || is.null(o$treat)) {
                self$results$notes$setContent("<p>Select an outcome and the treatment variable (the arms) to begin.</p>")
                return()
            }
            pp <- ex_guard(ex_prepare(self$data, o$outcome, o$treat, o$control, o$covs, o$block, o$cluster))
            notes <- character(0)
            se <- if (identical(o$se, "auto")) (if (is.null(pp$cluster)) "hc2" else "cr2") else o$se
            if (se %in% c("cr1", "cr2") && is.null(pp$cluster)) jmvcore::reject("Cluster-robust standard errors need a cluster variable.", code = "")
            if (!is.null(pp$cluster) && !se %in% c("cr1", "cr2"))
                notes <- c(notes, "A cluster variable is given but the chosen standard errors do not use it; choose CR1 or CR2 (or Automatic) for cluster randomization.")
            if (identical(o$method, "diff") && length(o$covs) > 0)
                notes <- c(notes, "Covariates are selected but the estimator is the difference in means; choose regression adjustment or Lin to use them.")
            if (o$method %in% c("adjust", "lin") && length(o$covs) == 0)
                notes <- c(notes, "No covariates are selected, so the estimate is the difference in means.")
            res <- ex_guard(ex_effects(pp$y, pp$arm, pp$X, pp$block, pp$cluster, o$method, se, level = o$level / 100))
            dt <- self$results$desc
            for (k in seq_len(nlevels(pp$arm))) {
                g <- levels(pp$arm)[k]; yy <- pp$y[pp$arm == g]
                dt$addRow(rowKey = k, values = list(arm = if (k == 1) paste(g, "(control)") else g, n = length(yy), mean = mean(yy), sd = stats::sd(yy)))
            }
            et <- self$results$est
            for (i in seq_len(nrow(res)))
                et$addRow(rowKey = i, values = list(arm = res$arm[i], estimate = res$estimate[i], se = res$se[i], t = res$t[i], df = res$df[i], p = res$p[i], lo = res$lo[i], hi = res$hi[i], std = res$std_effect[i]))
            et$setNote("a", paste0(ex_se_types[[se]], ". Confidence intervals and p-values use the t distribution",
                                   if (se == "cr2") " with Satterthwaite degrees of freedom" else if (se %in% c("cr1")) " with the number of clusters minus one" else " with the residual degrees of freedom",
                                   ". ", if (!is.null(pp$block)) "Block fixed effects are included. " else "", "The last column divides the effect by the standard deviation of the control group."))
            if (pp$n_drop > 0) notes <- c(notes, sprintf("%d observation(s) with missing values were dropped.", pp$n_drop))
            if (!is.null(pp$cluster)) notes <- c(notes, sprintf("%d clusters.", nlevels(pp$cluster)))
            self$results$notes$setContent(if (length(notes)) paste0("<p>", paste(notes, collapse = " "), "</p>") else "")
        }
    )
)
