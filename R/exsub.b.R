# jamovi analysis class: treatment effects by subgroup, interaction tests and adjusted p-values.

exsubClass <- R6::R6Class(
    "exsubClass",
    inherit = exsubBase,
    private = list(

        .run = function() {
            o <- self$options
            ex_show_text(self$results, o, "exsub")
            if (is.null(o$outcome) || is.null(o$treat) || length(o$subgroups) == 0) {
                self$results$notes$setContent("<p>Select the outcome, the treatment variable (two groups) and at least one subgroup variable to begin.</p>")
                for (nm in c("overall", "eff", "inter")) self$results[[nm]]$setVisible(FALSE)
                return()
            }
            if (anyDuplicated(c(o$outcome, o$treat, o$subgroups, o$covs, o$block, o$cluster))) jmvcore::reject("A variable can sit in one box only.", code = "")
            arm <- ex_guard(ex_arm(self$data[[o$treat]], o$control))
            ctl <- levels(arm)[1]
            trt <- trimws(o$armTreat)
            if (!nzchar(trt)) trt <- levels(arm)[2]
            if (!trt %in% levels(arm) || identical(trt, ctl)) jmvcore::reject(sprintf("The treatment group value must be one of the non-control values (%s).", paste(levels(arm)[-1], collapse = ", ")), code = "")
            y <- ex_guard(ex_num(self$data[[o$outcome]], "outcome"))
            groups <- stats::setNames(lapply(o$subgroups, function(v) factor(self$data[[v]])), o$subgroups)
            X <- ex_guard(ex_covmat(self$data, o$covs))
            bl <- if (!is.null(o$block)) factor(self$data[[o$block]]) else NULL
            cl <- if (!is.null(o$cluster)) self$data[[o$cluster]] else NULL
            in2 <- arm %in% c(ctl, trt) & !is.na(arm)
            keep <- in2 & stats::complete.cases(y, if (!is.null(X)) X, if (!is.null(bl)) bl, if (!is.null(cl)) cl, as.data.frame(groups))
            n_drop <- sum(in2) - sum(keep)
            a2 <- factor(as.character(arm[keep]), levels = c(ctl, trt))
            se <- o$se
            if (se == "cr1" && is.null(cl)) jmvcore::reject("Cluster-robust standard errors need a cluster variable.", code = "")
            private$.checkpoint()
            res <- ex_guard(ex_subgroup(y[keep], a2, lapply(groups, function(g) droplevels(g[keep])), if (!is.null(X)) X[keep, , drop = FALSE], if (!is.null(bl)) droplevels(bl[keep]), if (!is.null(cl)) cl[keep],
                                        o$method, se, o$level / 100, o$adjust))
            ov <- res$overall
            self$results$overall$addRow(rowKey = 1, values = list(what = sprintf("%s compared with %s", trt, ctl), n = sum(keep), estimate = ov$estimate, se = ov$se, p = ov$p, lo = ov$lo, hi = ov$hi))
            eb <- self$results$eff
            e <- res$effects
            for (i in seq_len(nrow(e)))
                eb$addRow(rowKey = i, values = list(variable = e$variable[i], level = e$level[i], n = e$n[i], n1 = e$n_treat[i], n0 = e$n_control[i], estimate = if (is.na(e$estimate[i])) NaN else e$estimate[i],
                                                    se = if (is.na(e$se[i])) NaN else e$se[i], p = if (is.na(e$p[i])) NaN else e$p[i], padj = if (is.na(e$p_adj[i])) NaN else e$p_adj[i],
                                                    lo = if (is.na(e$lo[i])) NaN else e$lo[i], hi = if (is.na(e$hi[i])) NaN else e$hi[i]))
            adjtxt <- c(holm = "Holm", bonferroni = "Bonferroni", bh = "Benjamini-Hochberg", none = "no")[[o$adjust]]
            eb$setNote("a", sprintf("Adjusted p: %s adjustment over the %d subgroup effects. Subgroups with too few treated or control units are left empty.", adjtxt, nrow(e)))
            ib <- self$results$inter; it <- res$interaction
            for (i in seq_len(nrow(it))) ib$addRow(rowKey = i, values = list(variable = it$variable[i], F = it$F[i], df1 = it$df1[i], df2 = it$df2[i], p = it$p[i], padj = it$p_adj[i]))
            ib$setNote("a", sprintf("Wald F test, in one regression of the outcome on the treatment, the subgroup variable and their interaction%s; it asks whether the effect differs between the subgroups, which is not the same as one subgroup effect being significant and another not.",
                                    if (o$method == "adjust") " (with the covariates and block effects)" else ""))
            if (isTRUE(o$plot)) {
                ok <- is.finite(e$estimate)
                self$results$subPlot$setState(list(label = paste0(e$variable[ok], ": ", e$level[ok]), est = e$estimate[ok], lo = e$lo[ok], hi = e$hi[ok], all = c(ov$estimate, ov$lo, ov$hi)))
            }
            notes <- sprintf("%d units: %d treated with '%s' and %d control with '%s'. Standard errors: %s. Subgroup effects are exploratory unless the subgroups were specified in advance; the more subgroups, the more chance findings.", sum(keep), sum(a2 == trt), trt, sum(a2 == ctl), ctl, ex_se_types[[res$se_type]])
            if (n_drop > 0) notes <- c(notes, sprintf("%d observation(s) with missing values were dropped.", n_drop))
            self$results$notes$setContent(paste0("<p>", paste(notes, collapse = " "), "</p>"))
        },

        .plotSub = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            df <- data.frame(label = factor(st$label, levels = rev(st$label)), est = st$est, lo = st$lo, hi = st$hi)
            g <- ggplot2::ggplot(df, ggplot2::aes(x = est, y = label)) + ggtheme +
                ggplot2::annotate("rect", ymin = -Inf, ymax = Inf, xmin = st$all[2], xmax = st$all[3], fill = "#e6ac40", alpha = 0.18) +
                ggplot2::geom_vline(xintercept = st$all[1], colour = "#e6ac40", linewidth = 0.8) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
                ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi), height = 0.2, colour = "#6b9de8", linewidth = 0.8) +
                ggplot2::geom_point(size = 2.8, colour = "#6b9de8") +
                ggplot2::labs(x = "Treatment effect", y = NULL, caption = "Orange: effect in the whole sample with its interval; dashed: no effect")
            print(g)
            TRUE
        }
    )
)
