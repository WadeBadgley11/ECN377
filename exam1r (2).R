# =====================================================================
#  ECN 377  -  EXAM I  PLUG-IN R SCRIPT   (open notes, open R)
#
#  HOW TO USE
#   1. Run BLOCK 0 once at the start (select it, Cmd+Enter / Ctrl+Enter).
#   2. Find the block that matches the question (see the list below).
#   3. Change ONLY the numbers on the lines marked  # <- PLUG IN
#   4. Select the whole block and run it. Answers print rounded to
#      2 decimals, with the exact value next to them.
#   Every block comes filled in with a real quiz / problem-set question,
#   and the correct answers are listed in its header so you can check.
#   "NA" in the output = you didn't give that input; just ignore it.
#
#  WHICH BLOCK?
#   A  Wooldridge dataset regression (Problem Set 7 type)
#   B  Small list of x and y numbers (slope, intercept, predict, residual)
#   C  Given a fitted line (fitted value, residual, SSR for points)
#   D  SST / SSE / SSR / R^2 (give any two)
#   E  Slope or intercept from summary numbers (cov, var, means, cor, sd)
#   F  Linear function: value of y, change in y (one or two x's)
#   G  Sums, means, percent change, percentage points
#   H  Probability table: E, Var, Cov, Cor, E[Y | X = x], Var(Y | X = x)
#   I  Properties: E[aX+b], Var(aX+b), Var(aX+bY), Cov, Cor ...
#   J  Sample rescaling: var/sd/cov/cor of (a*x + b)
#   K  Sum and mean rules (n, xbar, + c)
#   L  Overall mean from group means (E[Y] from E[Y | X])
#   M  OLS property shortcuts (missing residual, ybar from line, R^2 from cor)
#   N  Logs and exponentials
#  13  Concept answers      14  Concept notes by study-guide topic
# =====================================================================


# =====================================================================
# BLOCK 0  -  SETUP  (run once)
# =====================================================================
rm(list = ls())
library(wooldridge)          # if missing: install.packages("wooldridge")

# ans(): prints an answer rounded to 2 decimals, plus the exact value
ans <- function(label, value) {
  value <- unname(value)
  if (length(value) == 0 || all(is.na(value))) {
    cat(sprintf("%-34s NA\n", label))
  } else {
    cat(sprintf("%-34s %s   (exact: %s)\n", label,
                paste(format(round(value, 2), nsmall = 2), collapse = ", "),
                paste(signif(value, 6), collapse = ", ")))
  }
}


# =====================================================================
# BLOCK A  -  WOOLDRIDGE DATASET REGRESSION   (Problem Set 7 type)
#   Filled in with PS7: wage on educ in wage1.
#   Correct answers: n 526 | mean y 5.90 | sd x 2.77 | slope 0.54 |
#   intercept -0.90 | predict at 14: 6.67 | change for +2: 1.08 |
#   SSR 5980.68 | R^2 0.16 | mean y when educ == 16: 8.04
# =====================================================================
data("wage1")                # <- PLUG IN dataset name (also on next line)
D      <- wage1              # <- PLUG IN same dataset name
yname  <- "wage"             # <- PLUG IN y variable (left of ~), in quotes
xname  <- "educ"             # <- PLUG IN x variable, in quotes
x0     <- 14                 # <- PLUG IN x value to predict at
dx     <- 2                  # <- PLUG IN change in x
gvar   <- "educ"             # <- PLUG IN variable for a group mean
gval   <- 16                 # <- PLUG IN its value (group: gvar == gval)
row    <- 1                  # <- PLUG IN which observation's residual
# ---- run (don't change) ----
y <- D[[yname]]; x <- D[[xname]]
reg <- lm(y ~ x)
b0 <- reg$coefficients[1]; b1 <- reg$coefficients[2]
SSR <- sum(reg$residuals^2)
SST <- sum((y - mean(y))^2)
SSE <- sum((reg$fitted.values - mean(y))^2)
ans("n (nrow)", nrow(D))
ans("mean of y", mean(y));            ans("mean of x", mean(x))
ans("sd of y", sd(y));                ans("sd of x", sd(x))
ans("var of y", var(y));              ans("var of x", var(x))
ans("cov(x, y)", cov(x, y));          ans("cor(x, y)", cor(x, y))
ans("slope b1", b1);                  ans("intercept b0", b0)
ans("predicted y at x0", b0 + b1 * x0)
ans("change in y for dx", b1 * dx)
ans("SSR", SSR); ans("SST", SST); ans("SSE", SSE)
ans("R^2", SSE / SST)
ans("residual of chosen row", reg$residuals[row])
ans("mean of y in group", mean(y[D[[gvar]] == gval]))
ans("how many in group", sum(D[[gvar]] == gval))
# Other datasets used: bwght (bwght ~ cigs), ceosal1 (salary ~ roe),
# gpa1 (colGPA ~ hsGPA), wage2 (wage ~ IQ), hprice1 (price ~ sqrft).
# Run ?wage1 (or any name) to see what the variables mean and their units.


# =====================================================================
# BLOCK B  -  SMALL LIST OF x AND y NUMBERS   (Quizzes 3, 5, 6)
#   Filled in with Quiz 5 Q7: x = (8,6,1), y = (8,12,10), predict at 9.
#   Correct: slope -0.15 | intercept 10.77 | predict at 9: 9.38
# =====================================================================
x  <- c(8, 6, 1)             # <- PLUG IN x values
y  <- c(8, 12, 10)           # <- PLUG IN y values (same order)
x0 <- 9                      # <- PLUG IN x to predict at
dx <- 1                      # <- PLUG IN change in x (for change in y)
px <- 6; py <- 12            # <- PLUG IN a point (x, y) to get its residual
# ---- run (don't change) ----
fit <- lm(y ~ x)
b0 <- fit$coefficients[1]; b1 <- fit$coefficients[2]
ans("mean x", mean(x));  ans("mean y", mean(y))
ans("sample var x", var(x)); ans("sample sd x", sd(x))
ans("sample var y", var(y)); ans("sample sd y", sd(y))
ans("sample cov(x, y)", cov(x, y)); ans("sample cor(x, y)", cor(x, y))
ans("slope b1", b1);  ans("intercept b0", b0)
ans("predicted y at x0", b0 + b1 * x0)
ans("change in y for dx", b1 * dx)
ans("residual of point (px, py)", py - (b0 + b1 * px))
ans("all fitted values", fit$fitted.values)
ans("all residuals", fit$residuals)
ans("SSR", sum(fit$residuals^2))
ans("SST", sum((y - mean(y))^2))
ans("R^2", summary(fit)$r.squared)


# =====================================================================
# BLOCK C  -  GIVEN A FITTED LINE   (Quizzes 5, 6)
#   Filled in with Quiz 6: yhat = 3 + 0.6x, person x = 8, y = 18
#   and SSR for yhat = 2 + 0.4x at (10,2), (6,15), (7,5).
#   Correct: fitted 7.8 | residual 10.2 | SSR 128.4
# =====================================================================
b0 <- 3; b1 <- 6/10          # <- PLUG IN the line (fractions like 6/10 are fine)
x0 <- 8; y0 <- 18            # <- PLUG IN one person's x and ACTUAL y
# For SSR over several points (can be a different line):
L0 <- 2; L1 <- 4/10          # <- PLUG IN line for the SSR question
xs <- c(10, 6, 7)            # <- PLUG IN the points' x values
ys <- c(2, 15, 5)            # <- PLUG IN the points' y values
# ---- run (don't change) ----
ans("fitted value at x0", b0 + b1 * x0)
ans("residual (actual - fitted)", y0 - (b0 + b1 * x0))
ans("residuals of the points", ys - (L0 + L1 * xs))
ans("SSR of the points", sum((ys - (L0 + L1 * xs))^2))
# negative residual = OVER-predicted, positive = UNDER-predicted


# =====================================================================
# BLOCK D  -  SST / SSE / SSR / R^2   (give any two; put NA for unknowns)
#   Filled in with Quiz 6 Q11: SST = 294, SSR = 29.
#   Correct: SSE 265 | R^2 0.90
# =====================================================================
SST <- 294                   # <- PLUG IN (or NA)
SSE <- NA                    # <- PLUG IN (or NA)
SSR <- 29                    # <- PLUG IN (or NA)
R2  <- NA                    # <- PLUG IN (or NA)
# ---- run (don't change) ----
if (is.na(SST) && !is.na(SSE) && !is.na(SSR)) SST <- SSE + SSR
if (is.na(SST) && !is.na(R2) && !is.na(SSE))  SST <- SSE / R2
if (is.na(SST) && !is.na(R2) && !is.na(SSR))  SST <- SSR / (1 - R2)
if (is.na(SSE) && !is.na(SSR)) SSE <- SST - SSR
if (is.na(SSE) && !is.na(R2))  SSE <- R2 * SST
if (is.na(SSR)) SSR <- SST - SSE
R2 <- SSE / SST
ans("SST", SST); ans("SSE", SSE); ans("SSR", SSR); ans("R^2", R2)


# =====================================================================
# BLOCK E  -  SLOPE / INTERCEPT FROM SUMMARY NUMBERS   (Quiz 5)
#   Filled in with Quiz 5: cov = -11, var(x) = 2; ybar = 12, xbar = 9.
#   Correct: slope -5.5 | intercept (with this slope) 61.5
#   (Quiz 5 Q5 gave slope 7/10 directly -> set b1_given <- 7/10 -> 5.7)
# =====================================================================
covxy    <- -11              # <- PLUG IN sample cov(x, y)   (or NA)
varx     <- 2                # <- PLUG IN sample var(x)      (or NA)
corxy    <- NA               # <- PLUG IN cor(x, y)          (or NA)
sdx <- NA; sdy <- NA         # <- PLUG IN sd(x), sd(y)       (or NA)
xbar <- 9; ybar <- 12        # <- PLUG IN means              (or NA)
b1_given <- NA               # <- PLUG IN slope if the question GIVES it
# ---- run (don't change) ----
b1 <- if (!is.na(b1_given)) b1_given else if (!is.na(covxy)) covxy / varx else corxy * sdy / sdx
ans("slope = cov/var (or cor*sdy/sdx)", b1)
ans("intercept = ybar - b1*xbar", ybar - b1 * xbar)


# =====================================================================
# BLOCK F  -  LINEAR FUNCTION: VALUE OF y, CHANGE IN y   (Quiz 2, 5)
#   Filled in with Quiz 2: y = 4 + 2x1 + 8x2, dx1 = -1, dx2 = 4.
#   Correct: change in y 30
# =====================================================================
b0 <- 4                      # <- PLUG IN intercept (only used for a VALUE)
b1 <- 2                      # <- PLUG IN slope on x1
b2 <- 8                      # <- PLUG IN slope on x2 (0 if only one x)
x1 <- 0; x2 <- 0             # <- PLUG IN x values (for a predicted VALUE)
dx1 <- -1; dx2 <- 4          # <- PLUG IN changes (dx2 <- 0 if only one x)
# ---- run (don't change) ----
ans("value of y at x1, x2", b0 + b1 * x1 + b2 * x2)
ans("change in y (no intercept)", b1 * dx1 + b2 * dx2)


# =====================================================================
# BLOCK G  -  SUMS, MEANS, PERCENT CHANGE, PERCENTAGE POINTS   (Quiz 1, 2)
#   Filled in with Quiz 2 Q8 (48 -> 63) and Quiz 1 numbers.
#   Correct: % change 31.25 | sum 15 | mean 5
# =====================================================================
old <- 48; new <- 63         # <- PLUG IN old and new values
nums <- c(8, 3, 4)           # <- PLUG IN a list of numbers
# ---- run (don't change) ----
ans("percent change", 100 * (new - old) / old)
ans("point change (new - old)", new - old)
ans("sum", sum(nums)); ans("mean", mean(nums)); ans("n", length(nums))
ans("sample var", var(nums)); ans("sample sd", sd(nums))


# =====================================================================
# BLOCK H  -  PROBABILITY TABLE   (Quizzes 3, 4, 5)  POPULATION, not sample
#   Type each ROW of the table in order. One variable only? set yv <- NA.
#   Filled in with Quiz 5 Q10/Q11 style table (x,y,P):
#   (6,5,.2), (1,0,.3), (1,8,.5).
#   Correct: E[X] 2 | E[Y] 5 | E[XY] 10 | Cov 0 | E[Y|X=1] 5
# =====================================================================
xv <- c(6, 1, 1)             # <- PLUG IN x column
yv <- c(5, 0, 8)             # <- PLUG IN y column (or NA)
p  <- c(0.2, 0.3, 0.5)       # <- PLUG IN probability column
xc <- 1                      # <- PLUG IN x value for E[Y | X = xc]
# ---- run (don't change) ----
ans("sum of p (must be 1)", sum(p))
EX <- sum(xv * p); EX2 <- sum(xv^2 * p); VX <- EX2 - EX^2
ans("E[X]", EX); ans("E[X^2]", EX2); ans("Var(X)", VX); ans("sd(X)", sqrt(VX))
if (!all(is.na(yv))) {
  EY <- sum(yv * p); EY2 <- sum(yv^2 * p); VY <- EY2 - EY^2
  EXY <- sum(xv * yv * p); CXY <- EXY - EX * EY
  ans("E[Y]", EY); ans("E[Y^2]", EY2); ans("Var(Y)", VY); ans("sd(Y)", sqrt(VY))
  ans("E[XY]", EXY); ans("Cov(X, Y)", CXY); ans("Cor(X, Y)", CXY / sqrt(VX * VY))
  pc <- p[xv == xc] / sum(p[xv == xc])
  ans("P(X = xc)", sum(p[xv == xc]))
  ans("E[Y | X = xc]", sum(yv[xv == xc] * pc))
  ans("Var(Y | X = xc)", sum(yv[xv == xc]^2 * pc) - sum(yv[xv == xc] * pc)^2)
}
# Conditional-only question (Y values and probs GIVEN X = x)? put them in
# xv and p with yv <- NA, and read E[X] as E[Y | X = x].


# =====================================================================
# BLOCK I  -  PROPERTIES   (Quiz 4 + properties practice)
#   Plug in what the question gives; put NA for anything not given.
#   Filled in with Quiz 4 Q10: Var(X)=5, Var(Y)=4, Cov=2, a=3, b=3.
#   Correct: Var(aX + bY) 117
# =====================================================================
EX   <- NA                   # <- PLUG IN E[X]
EY   <- NA                   # <- PLUG IN E[Y]
VX   <- 5                    # <- PLUG IN Var(X)   (or give sdX instead)
VY   <- 4                    # <- PLUG IN Var(Y)   (or give sdY instead)
sdX  <- NA; sdY <- NA        # <- PLUG IN sd(X), sd(Y) if given instead of Var
CXY  <- 2                    # <- PLUG IN Cov(X, Y)  (0 if independent)
EX2  <- NA                   # <- PLUG IN E[X^2]
EXY  <- NA                   # <- PLUG IN E[XY]
a    <- 3                    # <- PLUG IN a  (multiplies X)
b    <- 3                    # <- PLUG IN b  (multiplies Y, or the constant in aX + b)
cc   <- 1                    # <- PLUG IN c  (multiplies Y in Cov(aX + b, cY + d))
# ---- run (don't change) ----
if (is.na(VX) && !is.na(sdX)) VX <- sdX^2
if (is.na(VY) && !is.na(sdY)) VY <- sdY^2
if (is.na(VX) && !is.na(EX2) && !is.na(EX)) VX <- EX2 - EX^2
if (is.na(CXY) && !is.na(EXY)) CXY <- EXY - EX * EY
ans("E[aX + b]", a * EX + b)
ans("E[aX + bY]", a * EX + b * EY)
ans("Var(X) (from E[X^2] if given)", VX)
ans("sd(X)", sqrt(VX))
ans("E[X^2] = Var + (E[X])^2", VX + EX^2)
ans("Var(aX + b)  (b drops out)", a^2 * VX)
ans("sd(aX + b) = |a| sd(X)", abs(a) * sqrt(VX))
ans("Var(aX + bY)", a^2 * VX + b^2 * VY + 2 * a * b * CXY)
ans("Var(X + Y)", VX + VY + 2 * CXY)
ans("Var(X - Y)", VX + VY - 2 * CXY)
ans("sd(X + Y)", sqrt(VX + VY + 2 * CXY))
ans("Cov(X, Y) (from E[XY] if given)", CXY)
ans("Cov(aX + b, cY + d) = a*c*Cov", a * cc * CXY)
ans("Cor(X, Y)", CXY / sqrt(VX * VY))
# Reverse question: Var(X+Y) given, find Cov -> (VarXplusY - VX - VY) / 2
VarXplusY <- NA              # <- PLUG IN Var(X + Y) if given
ans("Cov from Var(X + Y)", (VarXplusY - VX - VY) / 2)


# =====================================================================
# BLOCK J  -  SAMPLE RESCALING: (a*x + b)   (properties practice)
#   Filled in with: var(x) = 4, cov(x,y) = 10, cor = 0.6, a = 3.
#   Correct: var 36 | sd 6 | cov 30 | cor 0.6
# =====================================================================
varx  <- 4                   # <- PLUG IN sample var(x)  (or NA and give sdx)
sdx   <- NA                  # <- PLUG IN sample sd(x)   (or NA)
covxy <- 10                  # <- PLUG IN cov(x, y)      (or NA)
corxy <- 0.6                 # <- PLUG IN cor(x, y)      (or NA)
a     <- 3                   # <- PLUG IN the multiplier (the + b never matters)
# ---- run (don't change) ----
if (is.na(varx) && !is.na(sdx)) varx <- sdx^2
ans("var(a*x + b) = a^2 var", a^2 * varx)
ans("sd(a*x + b) = |a| sd", abs(a) * sqrt(varx))
ans("cov(a*x + b, y) = a cov", a * covxy)
ans("cor(a*x + b, y) (sign of a)", sign(a) * corxy)
# If x is replaced by a*x in a regression: slope is DIVIDED by a, R^2 unchanged.


# =====================================================================
# BLOCK K  -  SUM AND MEAN RULES   (properties practice)
#   Filled in with: n = 5, xbar = 6, add c = 2; mean(2x - 1) style.
#   Correct: sum 30 | sum(x + 2) 40 | mean(a*x + b) 11
# =====================================================================
n    <- 5                    # <- PLUG IN n
xbar <- 6                    # <- PLUG IN sample mean (or NA and give sumx)
sumx <- NA                   # <- PLUG IN sum of x (or NA)
cadd <- 2                    # <- PLUG IN constant added to every x
a <- 2; b <- -1              # <- PLUG IN for mean(a*x + b)
# ---- run (don't change) ----
if (is.na(sumx)) sumx <- n * xbar
if (is.na(xbar)) xbar <- sumx / n
ans("sum of x = n * xbar", sumx)
ans("sum of (x + c) = sum + n*c", sumx + n * cadd)
ans("mean(a*x + b)", a * xbar + b)
ans("sum of (x - xbar)", 0)


# =====================================================================
# BLOCK L  -  OVERALL MEAN FROM GROUP MEANS   (E[Y] from E[Y | X])
#   Filled in with: E[Y|X=0] = 10, E[Y|X=1] = 16, P(X=1) = 0.4.
#   Correct: E[Y] 12.4
# =====================================================================
gmeans <- c(10, 16)          # <- PLUG IN each group's mean E[Y | X = x]
gprobs <- c(0.6, 0.4)        # <- PLUG IN each group's probability (sum to 1)
# ---- run (don't change) ----
ans("sum of probs (must be 1)", sum(gprobs))
ans("E[Y] = sum(P * group mean)", sum(gmeans * gprobs))
# Zero conditional mean: E[Y | X = x] = b0 + b1 * x  -> use BLOCK F.


# =====================================================================
# BLOCK M  -  OLS PROPERTY SHORTCUTS
#   Filled in with: residuals 1.5, -2, 0.5, 3 known; line 5 + 2x, xbar 3;
#   cor = -0.4.  Correct: missing residual -3 | ybar 11 | R^2 0.16
# =====================================================================
known_resid <- c(1.5, -2, 0.5, 3)   # <- PLUG IN the residuals you know
lb0 <- 5; lb1 <- 2; lxbar <- 3      # <- PLUG IN line and xbar (for ybar)
r   <- -0.4                         # <- PLUG IN sample correlation (for R^2)
# ---- run (don't change) ----
ans("missing residual", -sum(known_resid))
ans("ybar = b0 + b1 * xbar", lb0 + lb1 * lxbar)
ans("R^2 = cor^2 (simple regression)", r^2)


# =====================================================================
# BLOCK N  -  LOGS AND EXPONENTIALS   (log() in R = natural log)
# =====================================================================
v <- 8                       # <- PLUG IN a number
ans("ln(v)", log(v)); ans("e^v", exp(v)); ans("ln(e^v)", log(exp(v)))


# =====================================================================
# 13  CONCEPT ANSWERS  (multiple choice / true-false)
# =====================================================================
# Econometrics = data + economics + STATISTICS
# Ceteris paribus = holding all other factors fixed
# Ice cream & drownings correlated -> a CONFOUNDER (summer) drives both
# Correlation does NOT prove causation (educ-wage correlation != causal) -> FALSE
# 526 workers in 1976 = CROSS-SECTION; one unit over time = TIME SERIES;
#   same units over time = PANEL
# Whole group we care about = POPULATION; we use the SAMPLE to learn about it -> TRUE
# summary() gives min, mean, max, etc. of every variable
# Sample variance divides by n - 1
# Correlation is always between -1 and 1, has no units, same sign as covariance
# E[X^2] = sum of each SQUARED value times its probability (NOT (E[X])^2)
# Var(X) = E[X^2] - (E[X])^2
# E[Y | X = x] = average of Y among observations with X = x
# Independent -> Cov = 0, but Cov = 0 does NOT imply independent
# Var(X + 10) = Var(X): adding a constant never changes spread -> TRUE
# Var(X - Y) = Var(X) - Var(Y) -> FALSE (variances add; only the Cov term flips)
# sd(X + Y) = sd(X) + sd(Y) -> FALSE (add variances, then square root)
# A variance can be negative -> FALSE
# Rescaling x changes the slope but NOT R^2 or the correlation
# In y = b0 + b1 x, the intercept does NOT affect the change dy -> FALSE
# In y = b0 + b1 X1 + b2 X2, b1 = effect of X1 holding X2 fixed
# In Y = b0 + b1 X + U, b1 = ceteris paribus effect of X on Y
#   U = all unobserved factors (e.g. ability in a wage equation)
# Zero conditional mean E[U | X] = 0: the average of U does not depend on X
#   implies E[Y | X] = b0 + b1 X;  FAILS when something in U is related to X
# OLS slope = cov(x, y) / var(x);  intercept = ybar - b1 * xbar
# OLS minimizes the SUM OF SQUARED RESIDUALS
# Residual = actual - fitted;  negative residual = OVER-predicted,
#   positive residual = UNDER-predicted
# Sum of OLS residuals is always 0;  cov(x, residuals) = 0;
#   (xbar, ybar) is always on the OLS line
# SST = SSE + SSR;  R^2 = SSE/SST = 1 - SSR/SST, between 0 and 1
# R^2 = share of the sample variation in y explained by x
# In simple regression R^2 = (sample correlation)^2
# Low R^2 is common in economics (many unobserved factors);
#   it does NOT by itself mean the slope is wrong
# reg$coefficients[1] = intercept, reg$coefficients[2] = slope


# =====================================================================
# 14  CONCEPTS BY STUDY-GUIDE TOPIC  (Exam I study guide, in order)
#     Sources: study guide, Quizzes 1-6, Problem Set 7
# =====================================================================

# ---------------------------------------------------------------------
# 14.1  FOUNDATIONS  (Days 1-3)
# ---------------------------------------------------------------------
# - Econometrics uses DATA to learn about ECONOMIC relationships;
#   it combines data, economics and STATISTICS.
# - Correlation = two variables move together. Causation = changing one
#   causes the other to change. Correlation alone does NOT prove causation
#   (educ-wage correlation does not prove educ causes higher wages).
# - Confounding variable = a third factor driving BOTH variables.
#   Ice cream sales and drownings rise together because of summer.
# - Ceteris paribus = holding all other factors fixed.
# - Cross-sectional data = many units at ONE time (wage1: 526 workers, 1976).
#   Time series = ONE unit over many periods (GDP each year).
#   Panel = the SAME units followed over time (300 firms, 2018-2023).
# - Population = the whole group we care about (can't observe it all).
#   Sample = the part we observe; we use it to learn about the population.
# - sum_{i=1}^{n} x_i = x_1 + ... + x_n.  Sample mean xbar = sum(x_i) / n.
# - Linear function y = b0 + b1 x: intercept b0 = y when x = 0;
#   slope b1 = change in y when x rises by 1.
# - Change: dy = b1 * dx;  two variables: dy = b1*dx1 + b2*dx2.
#   The intercept DROPS OUT of any change.
# - In y = b0 + b1 X1 + b2 X2, b1 = effect of X1 holding X2 fixed.
# - Percent change = 100*(new - old)/old. Percentage-point change = new - old
#   (for variables already in %). 4% -> 6% = 2 points = 50% increase.

# ---------------------------------------------------------------------
# 14.2  DESCRIBING DATA  (Day 4)
# ---------------------------------------------------------------------
# - Sample variance divides the sum of squared deviations by n - 1.
# - Sample sd = sqrt(sample variance); measures spread in ORIGINAL units.
# - Sign of covariance/correlation: + = move together, - = move opposite,
#   0 = no linear relationship.
# - Correlation always lies between -1 and 1 (+/-1 = points exactly on a line).
# - Correlation has NO units: cor = cov / (sd_x * sd_y), so units cancel.
#   Changing units (inches -> cm) does not change it. Covariance has units.

# ---------------------------------------------------------------------
# 14.3  PROBABILITY  (Days 5-7)
# ---------------------------------------------------------------------
# - ln and exp undo each other: ln(e^x) = x, e^(ln a) = a, ln(1) = 0,
#   ln(ab) = ln a + ln b, ln(a^k) = k ln a.  R: log() = ln, exp().
# - Random variable = value determined by chance; its distribution lists
#   every value and its probability (probabilities sum to 1).
# - E[X] = sum of each value times its probability.
# - E[X^2] = sum of each SQUARED value times its probability (NOT (E[X])^2).
# - Var(X) = E[X^2] - (E[X])^2;  sd(X) = sqrt(Var(X)).
# - E[aX + b] = a E[X] + b;  E[aX + bY] = a E[X] + b E[Y] (always).
# - Var(aX + b) = a^2 Var(X) (b disappears).
# - Var(aX + bY) = a^2 Var(X) + b^2 Var(Y) + 2ab Cov(X,Y); the Cov term is
#   there because X and Y can move together or opposite.
# - Population Cov(X,Y) = E[XY] - E[X]E[Y];  Cor = Cov / (sd(X) sd(Y)).
# - Independence: knowing X tells you nothing about Y; P(x,y) = P(x)P(y).
#   Independent -> Cov = 0. But Cov = 0 does NOT imply independence
#   (X = -1, 0, 1 and Y = X^2: Cov = 0, yet Y depends on X).
# - E[Y | X = x] = the average of Y within the group X = x.
#   From a table: keep the X = x rows, divide their probs by P(X = x), then sum(y * p).
# - Conditional variance Var(Y | X = x) = the spread of Y within that group.
# - Distributions to know: normal, t, F.

# ---------------------------------------------------------------------
# 14.4  SIMPLE LINEAR REGRESSION  (Days 8-10)
# ---------------------------------------------------------------------
# - Y = b0 + b1 X + U:  Y = dependent (explained) variable,
#   X = explanatory variable, U = error term = ALL unobserved factors
#   affecting Y (e.g. ability, family background in a wage equation).
# - b1 = the ceteris paribus effect of X on Y (change in Y per one-unit
#   rise in X, holding U fixed).
# - Zero conditional mean E[U | X] = 0: the average of U does NOT depend
#   on X (not "U is always 0"). It implies E[Y | X] = b0 + b1 X.
# - It FAILS when something in U is related to X (ability is in U and is
#   correlated with education).
# - "Least squares": OLS picks b0-hat, b1-hat to minimize the SUM OF
#   SQUARED RESIDUALS.
# - b1-hat = cov(x, y) / var(x);  b0-hat = ybar - b1-hat * xbar.
#   The slope has the same sign as cov(x, y).
# - Fitted value y-hat = b0-hat + b1-hat x;  residual u-hat = y - y-hat.
# - Negative residual = OVER-predicted;  positive residual = UNDER-predicted.
# - Predicted change: d(y-hat) = b1-hat * dx.
# - wage1: wage = -0.90 + 0.54 educ -> each extra year of education raises
#   predicted hourly wage by $0.54.
# - ceosal1: salary = 963.19 + 18.50 roe (salary in $1000s) -> +1 point of
#   roe raises predicted salary by $18,500.

# ---------------------------------------------------------------------
# 14.5  GOODNESS OF FIT  (Days 11-12)
# ---------------------------------------------------------------------
# - Algebraic properties of OLS: (1) residuals sum to zero, (2) x and the
#   residuals are uncorrelated, (3) (xbar, ybar) lies on the OLS line.
# - SST = sum (y - ybar)^2 (total);  SSE = sum (y-hat - ybar)^2 (explained);
#   SSR = sum u-hat^2 (unexplained).   SST = SSE + SSR.
# - R^2 = SSE/SST = 1 - SSR/SST, between 0 and 1;
#   = (sample correlation)^2 in simple regression.
# - R^2 = fraction of the sample variation in y explained by x
#   (wage1: 0.16 -> educ explains 16% of the variation in wages).
# - Low R^2 is common in economics because many unobserved factors affect
#   outcomes; it does NOT by itself mean the slope is wrong or useless.

# ---------------------------------------------------------------------
# 14.6  R SKILLS
# ---------------------------------------------------------------------
# - c() makes a vector; sum() adds; length() counts items (n).
# - library(wooldridge) then data("wage1") loads a dataset.
# - wage1$wage picks a column; nrow(wage1) = sample size.
# - summary() gives min, mean, max, etc. of every variable.
# - mean(), var(), sd(), cov(), cor() = sample statistics (n - 1).
# - reg <- lm(y ~ x, data = DATA)  (y goes left of ~).
# - reg$coefficients[1] = intercept, [2] = slope; $fitted.values = y-hats;
#   $residuals = u-hats;  summary(reg)$r.squared = R^2.
# - Save results with <- and reuse them so answers keep every decimal.
