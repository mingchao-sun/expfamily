# jamovi analysis class: factorial design with two crossed treatments.

exfactClass <- R6::R6Class(
    "exfactClass",
    inherit = exfactBase,
    private = list(

        .run = function() {
            o <- self$options
            ex_show_text(self$results, o, "exfact")
            if (is.null(o$outcome) || is.null(o$factorA) || is.null(o$factorB)) {
                self$results$notes$setContent("<p>Select the outcome and the two treatment factors to begin. Every combination of the levels of the two factors needs units.</p>")
                for (nm in c("cells", "main", "simple", "inter", "ign")) self$results[[nm]]$setVisible(FALSE)
                return()
            }
            if (anyDuplicated(c(o$outcome, o$factorA, o$factorB, o$covs, o$block, o$cluster))) jmvcore::reject("A variable can sit in one box only.", code = "")
            A <- ex_guard(ex_arm(self$data[[o$factorA]], o$controlA)); B <- ex_guard(ex_arm(self$data[[o$factorB]], o$controlB))
            y <- ex_guard(ex_num(self$data[[o$outcome]], "outcome"))
            X <- ex_guard(ex_covmat(self$data, o$covs))
            bl <- if (!is.null(o$block)) factor(self$data[[o$block]]) else NULL
            cl <- if (!is.null(o$cluster)) self$data[[o$cluster]] else NULL
            se <- o$se
            if (se == "cr1" && is.null(cl)) jmvcore::reject("Cluster-robust standard errors need a cluster variable.", code = "")
            keep <- stats::complete.cases(y, A, B, if (!is.null(X)) X, if (!is.null(bl)) bl, if (!is.null(cl)) cl)
            n_drop <- length(y) - sum(keep)
            private$.checkpoint()
            res <- ex_guard(ex_factorial(y[keep], droplevels(A[keep]), droplevels(B[keep]), if (!is.null(X)) X[keep, , drop = FALSE], if (!is.null(bl)) droplevels(bl[keep]), if (!is.null(cl)) cl[keep], se, o$level / 100))
            cb <- self$results$cells
            for (i in seq_len(nrow(res$cells))) cb$addRow(rowKey = i, values = list(A = res$cells$A[i], B = res$cells$B[i], n = res$cells$n[i], mean = res$cells$mean[i], sd = res$cells$sd[i]))
            fill <- function(tbl, df) for (i in seq_len(nrow(df))) tbl$addRow(rowKey = i, values = list(lab = df$effect[i], estimate = df$estimate[i], se = df$se[i], t = df$t[i], df = df$df[i], p = df$p[i], lo = df$lo[i], hi = df$hi[i]))
            fill(self$results$main, res$main); fill(self$results$simple, res$simple); fill(self$results$inter, res$interaction); fill(self$results$ign, res$ignoring)
            la <- res$levels$A; lb <- res$levels$B
            self$results$main$setNote("a", sprintf("The effect of each level of one factor against its control level, averaged over the levels of the other factor with equal weights. Control levels: A = %s, B = %s.", la[1], lb[1]))
            self$results$simple$setNote("a", "The effect of one factor inside each level of the other factor, from the cell means.")
            jt <- res$joint
            self$results$inter$setNote("a", sprintf("The difference between the simple effects of a factor at two levels of the other. Joint test that all interactions are zero: F(%d, %s) = %s, p %s.", jt[["df1"]], ex_num_fmt(jt[["df2"]]), ex_fmt(jt[["F"]], 3), if (is.finite(jt[["p"]]) && jt[["p"]] < 0.001) "< .001" else ex_fmt(jt[["p"]], 3)))
            self$results$ign$setNote("a", "The comparison of all units at the control level of a factor with all units at another level, whatever the other factor: it is the plain difference in means (with the covariates and blocks if given) and equals the average of the simple effects weighted by the shares of the other factor's levels. It is valid in a design where both factors were randomized independently.")
            notes <- sprintf("%d units in %d cells. Standard errors: %s; interval and tests with n minus the number of parameters degrees of freedom (clusters minus one with clustering).", sum(res$cells$n), nrow(res$cells), ex_se_types[[res$se_type]])
            if (jt[["p"]] < 0.05 && is.finite(jt[["p"]])) notes <- c(notes, "The interaction is significant: the effect of one factor depends on the other, so look at the simple effects rather than the main effects.")
            if (n_drop > 0) notes <- c(notes, sprintf("%d observation(s) with missing values were dropped.", n_drop))
            self$results$notes$setContent(paste0("<p>", paste(notes, collapse = " "), "</p>"))
        }
    )
)

ex_num_fmt <- function(x) format(round(x, 1), nsmall = 0)
