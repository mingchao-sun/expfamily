# jamovi analysis class: balance of baseline covariates, joint test and attrition.

exbalanceClass <- R6::R6Class(
    "exbalanceClass",
    inherit = exbalanceBase,
    private = list(

        .run = function() {
            o <- self$options
            ex_show_text(self$results, o, "exbalance")
            if (is.null(o$treat) || length(o$covs) == 0) {
                self$results$notes$setContent("<p>Select the treatment variable and at least one baseline covariate to begin. Add the outcome to check attrition.</p>")
                for (nm in c("att", "attj")) self$results[[nm]]$setVisible(FALSE)
                return()
            }
            if (length(intersect(o$covs, c(o$treat, o$block, o$outcome)))) jmvcore::reject("A variable can sit in one box only.", code = "")
            arm <- ex_guard(ex_arm(self$data[[o$treat]], o$control))
            X <- ex_guard(ex_covmat(self$data, o$covs))
            bl <- if (!is.null(o$block)) factor(self$data[[o$block]]) else NULL
            ok <- !is.na(arm) & (if (!is.null(bl)) !is.na(bl) else TRUE)
            n_drop <- sum(!ok)
            arm <- droplevels(arm[ok]); X <- X[ok, , drop = FALSE]; if (!is.null(bl)) bl <- droplevels(bl[ok])
            if (nlevels(arm) < 2) jmvcore::reject("The treatment needs at least two groups.", code = "")
            private$.checkpoint()
            tb <- ex_balance_table(X, arm)
            bt <- self$results$bal
            for (i in seq_len(nrow(tb)))
                bt$addRow(rowKey = i, values = list(covariate = tb$covariate[i], arm = tb$arm[i], m0 = tb$mean_control[i], m1 = tb$mean_arm[i], diff = tb$diff[i], std = tb$std_diff[i], p = tb$p[i]))
            nflag <- sum(abs(tb$std_diff) > o$flag, na.rm = TRUE)
            bt$setNote("a", sprintf("Standardised difference: difference in means over the pooled standard deviation. %d of %d comparisons exceed %s in absolute value; with random assignment about 5%% of the p-values fall below .05 by chance.", nflag, nrow(tb), ex_fmt(o$flag, 2)))
            jt <- ex_guard(ex_balance_joint(X, arm, bl, o$nperm, o$seed))
            jb <- self$results$joint
            for (i in seq_len(nrow(jt))) jb$addRow(rowKey = i, values = list(arm = jt$arm[i], F = jt$F[i], df1 = jt$df1[i], df2 = jt$df2[i], p = jt$p[i], pri = jt$p_ri[i], n = jt$n[i]))
            jb$setNote("a", sprintf("F test that the covariates jointly predict assignment to the group (against the control); the randomization p-value permutes the assignment%s (%s permutations).",
                                    if (!is.null(bl)) " within blocks" else "", format(o$nperm, big.mark = ",")))
            notes <- character(0)
            if (n_drop > 0) notes <- c(notes, sprintf("%d observation(s) without a treatment value were dropped.", n_drop))
            if (anyNA(X)) notes <- c(notes, "Covariates with missing values: each comparison uses the available values; the joint test uses complete cases.")
            if (!is.null(o$outcome)) {
                yv <- ex_guard(ex_num(self$data[[o$outcome]], "outcome"))[ok]
                at <- ex_guard(ex_attrition(!is.na(yv), arm, X))
                ab <- self$results$att
                diffs <- c(NA, at$tests$diff); pv <- c(NA, at$tests$p)
                for (i in seq_len(nrow(at$rates)))
                    ab$addRow(rowKey = i, values = list(arm = if (i == 1) paste(at$rates$arm[i], "(control)") else at$rates$arm[i], n = at$rates$n[i], obs = at$rates$observed[i], rate = at$rates$rate[i],
                                                        diff = if (is.na(diffs[i])) NaN else diffs[i], p = if (is.na(pv[i])) NaN else pv[i]))
                ab$setNote("a", "Response rate: share of units with an observed outcome. Differential attrition (different rates by group) can bias the effect estimates even when assignment was random.")
                if (!is.null(at$joint)) self$results$attj$addRow(rowKey = 1, values = list(F = at$joint$F, df1 = at$joint$df1, df2 = at$joint$df2, p = at$joint$p))
                else self$results$attj$setVisible(FALSE)
            } else for (nm in c("att", "attj")) self$results[[nm]]$setVisible(FALSE)
            self$results$notes$setContent(if (length(notes)) paste0("<p>", paste(notes, collapse = " "), "</p>") else "")
        }
    )
)
