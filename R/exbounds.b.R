# jamovi analysis class: worst-case (Manski) bounds on the average treatment effect.

exboundsClass <- R6::R6Class(
    "exboundsClass",
    inherit = exboundsBase,
    private = list(

        .run = function() {
            o <- self$options
            ex_show_text(self$results, o, "exbounds")
            if (is.null(o$outcome) || is.null(o$treat)) {
                self$results$notes$setContent("<p>Select the outcome and the treatment variable (two groups) to begin.</p>")
                for (nm in c("ref", "res", "means")) self$results[[nm]]$setVisible(FALSE)
                return()
            }
            if (identical(o$outcome, o$treat)) jmvcore::reject("A variable can sit in one box only.", code = "")
            arm <- ex_guard(ex_arm(self$data[[o$treat]], o$control))
            ctl <- levels(arm)[1]
            trt <- trimws(o$armTreat)
            if (!nzchar(trt)) trt <- levels(arm)[2]
            if (!trt %in% levels(arm) || identical(trt, ctl)) jmvcore::reject(sprintf("The treatment group value must be one of the non-control values (%s).", paste(levels(arm)[-1], collapse = ", ")), code = "")
            y <- ex_guard(ex_num(self$data[[o$outcome]], "outcome"))
            num <- function(txt, label) { t <- trimws(txt); if (!nzchar(t)) return(NA_real_); v <- suppressWarnings(as.numeric(t)); if (is.na(v)) jmvcore::reject(sprintf("%s must be a number.", label), code = ""); v }
            ymin <- num(o$ymin, "The lowest possible outcome"); ymax <- num(o$ymax, "The highest possible outcome")
            in2 <- arm %in% c(ctl, trt) & !is.na(arm)
            d <- as.numeric(arm[in2] == trt); yy <- y[in2]
            n_na <- sum(is.na(yy))
            private$.checkpoint()
            r <- ex_guard(ex_manski(yy, d, o$mode, ymin, ymax, o$level / 100, o$nboot, o$seed))
            rb <- self$results$ref
            add <- function(i, what, value) rb$addRow(rowKey = i, values = list(what = what, value = value))
            add(1, sprintf("Difference in mean outcomes, %s minus %s (observed units)", trt, ctl), r$naive)
            add(2, "Std. error of that difference", r$naive_se)
            add(3, "Range of the outcome used: lowest", r$ymin); add(4, "Range of the outcome used: highest", r$ymax)
            if (r$mode == "random") { add(5, sprintf("Share of outcomes missing, %s", trt), r$miss1); add(6, sprintf("Share of outcomes missing, %s", ctl), r$miss0) }
            else add(5, "Share of units that received the treatment", r$p_treated)
            labs <- ex_man_rows[rownames(r$bounds)]
            if (r$mode == "random") labs <- "Random assignment only; missing outcomes anywhere in the range"
            tb <- self$results$res; mb <- self$results$means
            for (i in seq_len(nrow(r$bounds))) {
                b <- r$bounds[i, ]; emp <- r$empty[i]
                tb$addRow(rowKey = i, values = list(what = unname(labs[i]), lower = if (emp) NaN else b[["ate_lo"]], upper = if (emp) NaN else b[["ate_hi"]], width = if (emp) NaN else b[["ate_hi"]] - b[["ate_lo"]],
                                                    sel = if (emp) NaN else r$se_lo[i], seu = if (emp) NaN else r$se_hi[i], lo = if (emp) NaN else r$ci_lo[i], hi = if (emp) NaN else r$ci_hi[i]))
                mb$addRow(rowKey = i, values = list(what = unname(labs[i]), e1lo = b[["e1_lo"]], e1hi = b[["e1_hi"]], e0lo = b[["e0_lo"]], e0hi = b[["e0_hi"]]))
            }
            tb$setNote("a", sprintf("Bounds from the extreme values of the unobserved outcomes; the interval is the Imbens-Manski %s%% interval for the effect (bootstrap standard errors of the bounds, %s samples).", ex_fmt(o$level, 1), format(o$nboot, big.mark = ",")))
            notes <- character(0)
            if (r$mode == "random") notes <- c(notes, sprintf("Treatment is assumed to have been assigned at random, so each arm's observed units describe the arm up to the missing outcomes; those are allowed to be anywhere between %s and %s. For tighter bounds under a monotone effect on response use the analysis Bounds under Attrition (Lee).", ex_fmt(r$ymin, 3), ex_fmt(r$ymax, 3)))
            else notes <- c(notes, "Treatment is not assumed random: the outcome that a unit would have had under the other treatment is anywhere in the range unless an assumption restricts it. Monotone treatment response: the treatment never lowers a unit's outcome. Monotone treatment selection: the units that took the treatment would have done at least as well, on average, under either treatment.")
            if (any(r$empty)) notes <- c(notes, "At least one set of assumptions is contradicted by the data (it gives a lower bound above the upper bound, for example because the treated do worse than the controls although selection is assumed to favour them); its row is left empty.")
            if (is.na(ymin) || is.na(ymax)) notes <- c(notes, "The range of the outcome is the observed range, which is a property of the sample: state the possible range (for example 0 and 1 for a yes or no outcome) when it is known, since the width of the bounds is proportional to it.")
            if (r$mode == "selection" && n_na > 0) notes <- c(notes, sprintf("%d unit(s) with a missing outcome were dropped.", n_na))
            self$results$notes$setContent(paste0("<p>", paste(notes, collapse = " "), "</p>"))
            if (isTRUE(o$plot)) self$results$bndPlot$setState(list(label = unname(labs), lo = r$bounds[, "ate_lo"], hi = r$bounds[, "ate_hi"], cilo = r$ci_lo, cihi = r$ci_hi, empty = r$empty, naive = r$naive))
        },

        .plotBounds = function(image, ggtheme, theme, ...) {
            st <- image$state
            if (is.null(st)) return(FALSE)
            df <- data.frame(label = factor(st$label, levels = rev(st$label)), lo = unname(st$lo), hi = unname(st$hi), cilo = st$cilo, cihi = st$cihi)[!st$empty, , drop = FALSE]
            if (!nrow(df)) return(FALSE)
            g <- ggplot2::ggplot(df, ggplot2::aes(y = label)) + ggtheme +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") + ggplot2::geom_vline(xintercept = st$naive, colour = "#e6ac40", linewidth = 0.8) +
                ggplot2::geom_errorbarh(ggplot2::aes(xmin = cilo, xmax = cihi), height = 0.25, colour = "grey60") +
                ggplot2::geom_segment(ggplot2::aes(x = lo, xend = hi, yend = label), colour = "#6b9de8", linewidth = 2.4) +
                ggplot2::scale_y_discrete(labels = function(x) vapply(x, function(z) paste(strwrap(z, 34), collapse = "\n"), "")) +
                ggplot2::labs(x = "Average treatment effect", y = NULL, caption = "Blue: bounds; grey: Imbens-Manski interval; orange: difference in observed means; dashed: no effect")
            print(g)
            TRUE
        }
    )
)
