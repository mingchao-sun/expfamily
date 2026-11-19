# jamovi analysis class: several outcomes, adjusted p-values.

exmhtClass <- R6::R6Class(
    "exmhtClass",
    inherit = exmhtBase,
    private = list(

        .run = function() {
            o <- self$options
            ex_show_text(self$results, o, "exmht")
            if (length(o$outcomes) < 2 || is.null(o$treat)) {
                self$results$notes$setContent("<p>Select at least two outcomes and the treatment variable to begin.</p>")
                return()
            }
            if (length(intersect(o$outcomes, c(o$treat, o$covs, o$block)))) jmvcore::reject("A variable can sit in one box only.", code = "")
            arm <- ex_guard(ex_arm(self$data[[o$treat]], o$control))
            ctl <- levels(arm)[1]
            trt <- trimws(o$armTreat)
            if (!nzchar(trt)) trt <- levels(arm)[2]
            if (!trt %in% levels(arm) || identical(trt, ctl)) jmvcore::reject(sprintf("The treatment group value must be one of the non-control values (%s).", paste(levels(arm)[-1], collapse = ", ")), code = "")
            Y <- vapply(o$outcomes, function(v) ex_guard(ex_num(self$data[[v]], paste("outcome", v))), numeric(nrow(self$data)))
            colnames(Y) <- o$outcomes
            X <- ex_guard(ex_covmat(self$data, o$covs))
            bl <- if (!is.null(o$block)) factor(self$data[[o$block]]) else NULL
            keep <- arm %in% c(ctl, trt) & stats::complete.cases(Y, arm, if (!is.null(X)) X, bl)
            n_drop <- sum(arm %in% c(ctl, trt)) - sum(keep)
            d <- as.numeric(arm[keep] == trt)
            meths <- c(bonf = "bonferroni", sidak = "sidak", holm = "holm", hochberg = "hochberg", bh = "bh", by = "by", wymin = "wy_minp", wymax = "wy_maxt", rw = "rw")
            sel <- meths[vapply(names(meths), function(k) isTRUE(o[[k]]), TRUE)]
            if (length(sel) == 0) jmvcore::reject("Select at least one adjustment method.", code = "")
            private$.checkpoint()
            res <- ex_guard(ex_mht(Y[keep, , drop = FALSE], d, if (!is.null(X)) X[keep, , drop = FALSE] else NULL, if (!is.null(bl)) droplevels(bl[keep]) else NULL,
                                   unname(sel), nperm = o$nperm, nboot = o$nboot, seed = o$seed))
            tb <- self$results$res
            for (i in seq_len(nrow(res))) {
                v <- list(outcome = res$outcome[i], estimate = res$estimate[i], se = res$se[i], p = res$p[i])
                for (k in names(sel)) v[[paste0("a_", k)]] <- res[[sel[[k]]]][i]
                tb$addRow(rowKey = i, values = v)
            }
            nsig <- vapply(c(list(unadjusted = res$p), stats::setNames(lapply(sel, function(m) res[[m]]), names(sel))), function(p) sum(p < o$alpha), numeric(1))
            tb$setNote("a", paste0("Effect of ", trt, " compared with ", ctl, " (", sum(d == 1), " and ", sum(d == 0), " units), with a classical t test per outcome", if (length(o$covs) > 0) " adjusted for the covariates" else "",
                                   ". Adjusted p-values control the family-wise error rate (Bonferroni, Sidak, Holm, Hochberg, Westfall-Young, Romano-Wolf) or the false discovery rate (Benjamini-Hochberg and Yekutieli). ",
                                   "Outcomes with p below ", ex_fmt(o$alpha, 3), ": ", paste(sprintf("%s %d", c("unadjusted", unname(ex_mht_labels[names(sel)])), nsig), collapse = ", "), "."))
            notes <- character(0)
            if (n_drop > 0) notes <- c(notes, sprintf("%d observation(s) with missing values were dropped.", n_drop))
            if (any(c("wymin", "wymax") %in% names(sel))) notes <- c(notes, sprintf("Westfall-Young: %s permutations of the treatment%s.", format(o$nperm, big.mark = ","), if (!is.null(bl)) " within blocks" else ""))
            if ("rw" %in% names(sel)) notes <- c(notes, sprintf("Romano-Wolf: %s bootstrap samples.", format(o$nboot, big.mark = ",")))
            self$results$notes$setContent(if (length(notes)) paste0("<p>", paste(notes, collapse = " "), "</p>") else "")
        }
    )
)

ex_mht_labels <- c(bonf = "Bonferroni", sidak = "Sidak", holm = "Holm", hochberg = "Hochberg", bh = "BH", by = "BY", wymin = "WY min-p", wymax = "WY max-t", rw = "Romano-Wolf")
