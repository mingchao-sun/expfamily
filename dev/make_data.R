# Generates the three sample datasets and prints the values quoted in the guides. Run from the module root:  Rscript dev/make_data.R
for (f in list.files("R", "^engine_.*[.]R$", full.names = TRUE)) source(f)
set.seed(20261121)
out <- "data/"
# ---- individually randomized trial in 6 blocks, 3 arms, 4 outcomes, differential attrition
n <- 600
block <- rep(paste0("B", 1:6), each = n / 6)
arm <- unlist(lapply(1:6, function(b) sample(rep(c("control", "T1", "T2"), length.out = n / 6))))
age <- round(rnorm(n, 40, 12)); female <- rbinom(n, 1, 0.5); baseline <- round(rnorm(n), 3); income <- round(rlnorm(n, 3, 0.6), 2)
common <- rnorm(n)
eff <- list(y1 = c(T1 = 0.30, T2 = 0.50), y2 = c(T1 = 0.20, T2 = 0.40), y3 = c(T1 = 0.05, T2 = 0.00), y4 = c(T1 = 0, T2 = 0))
Y <- sapply(names(eff), function(v) round(0.5 * baseline + 0.1 * (age - 40) / 12 + 0.4 * as.integer(factor(block)) / 6 + ifelse(arm == "control", 0, eff[[v]][arm]) + 0.6 * common + 0.8 * rnorm(n), 3))
pmiss <- plogis(qlogis(c(control = 0.08, T1 = 0.10, T2 = 0.25)[arm]) - 0.5 * baseline * (arm == "T2"))
endline <- Y[, "y1"]; endline[runif(n) < pmiss] <- NA
trial <- data.frame(id = seq_len(n), block = block, arm = arm, age = age, female = female, baseline = baseline, income = income, Y, endline = endline)
write.csv(trial, paste0(out, "sim_trial.csv"), row.names = FALSE, na = "")
# ---- cluster randomized trial: 40 schools of 25 students
J <- 40; m <- 25
school <- rep(seq_len(J), each = m)
grp <- sample(rep(c("control", "treated"), J / 2))[school]
bl <- round(rnorm(J * m), 3); u <- rnorm(J, 0, sqrt(0.11 / 0.89))[school]
yc <- round(0.25 * (grp == "treated") + 0.5 * bl + u + rnorm(J * m), 3)
write.csv(data.frame(student = seq_len(J * m), school = school, group = grp, baseline = bl, y = yc), paste0(out, "sim_cluster_trial.csv"), row.names = FALSE)
# ---- small experiment for exact randomization inference
small <- data.frame(id = 1:12, group = c(rep("treated", 6), rep("control", 6)), y = c(7.1, 5.4, 8.3, 6.2, 9.0, 4.8, 5.9, 3.7, 6.5, 4.1, 5.2, 6.8))
write.csv(small, paste0(out, "sim_small_trial.csv"), row.names = FALSE)

# ---- values for the guides (the same calls the analyses make)
tr <- read.csv(paste0(out, "sim_trial.csv"), na.strings = "")
armf <- factor(tr$arm, levels = c("control", "T1", "T2"))
cat("== Treatment Effects, y1, difference in means (HC2)\n"); e1 <- ex_effects(tr$y1, armf, method = "diff", se_type = "hc2"); print(cbind(arm = e1$arm, round(e1[, c("estimate", "se", "p", "lo", "hi", "std_effect")], 4)))
Xc <- as.matrix(tr[, c("baseline", "age", "female")])
cat("== Lin adjustment with baseline, age, female and block fixed effects (HC2)\n"); e2 <- ex_effects(tr$y1, armf, Xc, block = factor(tr$block), method = "lin", se_type = "hc2"); print(cbind(arm = e2$arm, round(e2[, c("estimate", "se", "p")], 4)))
cat("== cluster trial, CR2\n"); cl <- read.csv(paste0(out, "sim_cluster_trial.csv")); gf <- factor(cl$group, levels = c("control", "treated"))
print(round(ex_effects(cl$y, gf, cluster = cl$school, method = "diff", se_type = "cr2")[, c("estimate", "se", "df", "p", "lo", "hi")], 4))
print(round(ex_effects(cl$y, gf, as.matrix(cl[, "baseline", drop = FALSE]), cluster = cl$school, method = "adjust", se_type = "cr2")[, c("estimate", "se", "df", "p")], 4))
print(round(ex_effects(cl$y, gf, method = "diff", se_type = "hc2")[, c("estimate", "se", "p")], 4))
cat("== randomization inference, small trial\n"); r <- ex_ri(small$y, as.numeric(small$group == "treated"), exact_max = 1e6)
cat(sprintf("stat %.4f p two %.4f right %.4f left %.4f n %d exact %s CI [%.3f, %.3f]\n", r$stat, r$p_two, r$p_right, r$p_left, r$n_rand, r$exact, r$ci[1], r$ci[2]))
rr <- ex_ri(small$y, as.numeric(small$group == "treated"), stat = "rank", exact_max = 1e6); cat(sprintf("rank stat %.4f p two %.4f\n", rr$stat, rr$p_two))
cat("== multiple outcomes: T2 vs control, covariate baseline, seed 1234\n")
k <- tr$arm %in% c("control", "T2")
m <- ex_mht(as.matrix(tr[k, paste0("y", 1:4)]), as.numeric(tr$arm[k] == "T2"), as.matrix(tr[k, "baseline", drop = FALSE]), methods = c("bonferroni", "holm", "bh", "wy_minp", "rw"), nperm = 2000, nboot = 1000, seed = 1234)
print(cbind(outcome = m$outcome, round(m[, c("estimate", "se", "p", "bonferroni", "holm", "bh", "wy_minp", "rw")], 4)))
cat("== power\n")
cat(sprintf("power d=0.3, 100+100: %.4f\n", ex_power(0.3, 1, 100, 100)))
cat(sprintf("MDE 100+100, 80%%: %.4f\n", ex_mde(1, 100, 100)))
cat("sample size d=0.3 80%:", ex_sample_size(0.3, 1), "\n")
cat(sprintf("cluster power 20+20 clusters, m=25, rho=0.05, effect 0.25: %.4f\n", ex_power(0.25, 1, 20, 20, m = 25, rho = 0.05)))
cat("clusters needed, effect 0.25, 80%:", ex_sample_size(0.25, 1, m = 25, rho = 0.05), "\n")
cat(sprintf("ICC of the cluster trial outcome: %.4f, sd %.4f\n", ex_icc(cl$y, cl$school), sd(cl$y)))
cat("== balance and attrition\n")
b <- ex_balance_table(Xc, armf); print(cbind(b[, c("arm", "covariate")], round(b[, c("mean_control", "mean_arm", "diff", "std_diff", "p")], 4)))
j <- ex_balance_joint(Xc, armf, factor(tr$block), 2000, 1234); print(cbind(arm = j$arm, round(j[, -1], 4)))
at <- ex_attrition(!is.na(tr$endline), armf, Xc); print(at$rates); print(cbind(arm = at$tests$arm, round(at$tests[, -1], 4))); print(round(at$joint, 4))
