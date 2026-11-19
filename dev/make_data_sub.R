# Sample data of the subgroup and factorial analyses. Run from the module root:  Rscript dev/make_data_sub.R
set.seed(20261118)
# ---- subgroup trial: two arms; the effect is larger for women and does not depend on age group or region
n <- 800
gender <- sample(c("man", "woman"), n, TRUE); agegroup <- sample(c("under 40", "40 and over"), n, TRUE, c(.55, .45))
region <- sample(c("north", "south", "east"), n, TRUE, c(.4, .35, .25)); baseline <- round(rnorm(n), 3)
arm <- sample(c("control", "treated"), n, TRUE)
tau <- 0.15 + 0.6 * (gender == "woman")
y <- round(0.5 * baseline + 0.2 * (region == "east") + (arm == "treated") * tau + rnorm(n), 3)
write.csv(data.frame(id = seq_len(n), arm, gender, agegroup, region, baseline, y), "data/sim_subgroup_trial.csv", row.names = FALSE)
cat("true effect: men 0.15, women 0.75, average", round(mean(tau), 3), "\n")
# ---- factorial trial: a message (none, info, reminder) crossed with a cash incentive (no, yes); the incentive works better with the reminder
set.seed(20261119)
m <- 900
message <- sample(c("none", "info", "reminder"), m, TRUE); incentive <- sample(c("no", "yes"), m, TRUE)
age <- round(rnorm(m, 38, 9)); base <- round(rnorm(m), 3)
mu <- 10 + 0.8 * base + 0.03 * (age - 38)
effect <- 0.4 * (message == "info") + 0.6 * (message == "reminder") + 0.5 * (incentive == "yes") + 0.7 * (message == "reminder") * (incentive == "yes")
savings <- round(mu + effect + rnorm(m, 0, 1.5), 3)
write.csv(data.frame(id = seq_len(m), message, incentive, age, baseline = base, savings), "data/sim_factorial_trial.csv", row.names = FALSE)
cat("true effects: info 0.4, reminder 0.6, incentive 0.5, interaction reminder x incentive 0.7\n")
# ---- selection into a programme (observational): binary outcome, monotone response and monotone selection hold by construction
set.seed(20261120)
k <- 1500
u <- rnorm(k)
e0 <- rnorm(k, 0, 0.8); y0 <- as.integer(u + e0 > 0.2)
y1 <- pmax(y0, as.integer(u + rnorm(k, 0, 0.8) > -0.3))                                  # Y(1) >= Y(0) for every unit
dprog <- rbinom(k, 1, plogis(-0.2 + 0.8 * u))                                              # people with a high u join more often
employed <- ifelse(dprog == 1, y1, y0)
write.csv(data.frame(id = seq_len(k), program = ifelse(dprog == 1, "yes", "no"), employed), "data/sim_selection_program.csv", row.names = FALSE)
cat("true ATE", round(mean(y1 - y0), 3), " ATT", round(mean((y1 - y0)[dprog == 1]), 3), " share in the programme", round(mean(dprog), 3), "\n")
