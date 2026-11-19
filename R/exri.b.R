# jamovi analysis class: randomization (Fisher) inference.

exriClass <- R6::R6Class(
    "exriClass",
    inherit = exriBase,
    private = list(

        .run = function() {
            o <- self$options
            ex_show_text(self$results, o, "exri")
            if (is.null(o$outcome) || is.null(o$treat)) {
                self$results$notes$setContent("<p>Select an outcome and the treatment variable to begin.</p>")
                return()
            }
            pp <- ex_guard(ex_prepare(self$data, o$outcome, o$treat, o$control, NULL, o$block, o$cluster))
            notes <- character(0)
            ctl <- levels(pp$arm)[1]
            tb <- self$results$res
            plot_state <- list()
            for (k in levels(pp$arm)[-1]) {
                keep <- pp$arm %in% c(ctl, k)
                d <- as.numeric(pp$arm[keep] == k)
                y <- pp$y[keep]; bl <- if (!is.null(pp$block)) droplevels(pp$block[keep]) else NULL; cl <- if (!is.null(pp$cluster)) droplevels(pp$cluster[keep]) else NULL
                private$.checkpoint()
                yadj <- y - o$tau * d
                want_ci <- isTRUE(o$ci) && identical(o$stat, "diff")
                r <- ex_guard(ex_ri(yadj, d, bl, cl, o$stat, o$sims, o$exactMax, o$seed, o$level / 100, ci = want_ci && o$tau == 0))
                ci <- NULL
                if (want_ci)                                   # the interval does not depend on the hypothesised effect: it uses the outcome itself
                    ci <- if (o$tau == 0) r$ci else ex_guard(ex_ri(y, d, bl, cl, "diff", o$sims, o$exactMax, o$seed, o$level / 100, ci = TRUE))$ci
                tb$addRow(rowKey = k, values = list(arm = paste(k, "vs", ctl), stat = r$stat, p2 = r$p_two, pr = r$p_right, pl = r$p_left, nrand = r$n_rand,
                                                    how = if (r$exact) "Exact (all randomizations)" else sprintf("Simulated (MC s.e. %s)", ex_fmt(r$mc_se, 4)),
                                                    lo = if (is.null(ci)) NaN else ci[1], hi = if (is.null(ci)) NaN else ci[2]))
                plot_state[[k]] <- list(dist = r$dist, obs = r$stat, label = paste(k, "vs", ctl))
            }
            tb$setNote("a", paste0("Statistic: ", tolower(ex_ri_stats[[o$stat]]), if (o$tau != 0) sprintf(", after subtracting the hypothesised effect %s from the treated units", ex_fmt(o$tau, 3)) else "", ". ",
                                   "p-values are the share of randomizations with a statistic at least as extreme as the observed one (two-sided: in absolute value). ",
                                   if (!is.null(pp$cluster)) "Whole clusters are assigned; statistics are computed on the units. " else "", if (!is.null(pp$block)) "Randomizations permute treatment within blocks. " else ""))
            if (isTRUE(o$ci) && identical(o$stat, "diff"))
                notes <- c(notes, sprintf("The confidence interval is the set of constant effects not rejected at the %s%% level (test inversion); it assumes the same effect for every unit.", ex_fmt(o$level, 1)))
            if (isTRUE(o$ci) && !identical(o$stat, "diff")) notes <- c(notes, "The confidence interval is available for the difference in means only.")
            if (pp$n_drop > 0) notes <- c(notes, sprintf("%d observation(s) with missing values were dropped.", pp$n_drop))
            self$results$notes$setContent(if (length(notes)) paste0("<p>", paste(notes, collapse = " "), "</p>") else "")
            if (isTRUE(o$plot)) self$results$riPlot$setState(plot_state)
        },

        .plotRi = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st) || length(st) == 0) return(FALSE)
            df <- do.call(rbind, lapply(st, function(s) data.frame(value = s$dist, label = s$label)))
            ob <- do.call(rbind, lapply(st, function(s) data.frame(obs = s$obs, label = s$label)))
            g <- ggplot2::ggplot(df, ggplot2::aes(x = value)) + ggtheme +
                ggplot2::geom_histogram(bins = 40, fill = "#6b9de8", colour = "white", linewidth = 0.2) +
                ggplot2::geom_vline(data = ob, ggplot2::aes(xintercept = obs), colour = "#e6ac40", linewidth = 1.1) +
                ggplot2::labs(x = "Statistic under the sharp null (orange line: observed)", y = "Randomizations")
            if (length(st) > 1) g <- g + ggplot2::facet_wrap(~ label, scales = "free")
            print(g)
            TRUE
        }
    )
)
