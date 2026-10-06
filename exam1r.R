# =====================================================================
#  ECN 377  -  EXAM I  R CHEAT SHEET  (open notes, open R)
#  Each section = one question type:
#     TEMPLATE  -> the code to copy; change the names/numbers
#     EXAMPLE   -> a real question, worked, answer written after "# ->"
#  Run a line: cursor on it + Cmd+Enter (Mac) / Ctrl+Enter (PC)
#  Round the FINAL answer to 2 decimals. Never retype rounded numbers.
#
#  CONTENTS
#   0  Setup + helper functions (run this block first)
#   1  Describe a dataset (nrow, mean, sd, var, cov, cor, summary)
#   2  Sums, means, percent change, percentage points
#   3  Linear functions: predict y, change in y
#   4  Sample stats from a small list of numbers
#   5  Probability tables: E[X], E[X^2], Var, Cov, Cor
#   6  Properties: sums, rescaling, E, Var, sd, Cov, Cor, E[Y|X],
#      OLS properties, logs   (6A-6I)
#   7  OLS by hand from small data (slope, intercept, predict)
#   8  Given a fitted line: fitted value, residual, SSR, SSE, R^2
#   9  OLS from summary numbers (cov, var, means, cor, sd)
#  10  Wooldridge data regression  (Problem Set 7 template)
#  11  Subsets / conditional means in data
#  12  Check the OLS properties
#  13  Concept answers (multiple choice / true-false facts)
# =====================================================================


# =====================================================================
# 0  SETUP + HELPER FUNCTIONS   (run this whole block first)
# =====================================================================
rm(list = ls())
library(wooldridge)          # if missing: install.packages("wooldridge")

# ols(x, y): prints everything about a simple regression of y on x.
# Optional x0 = a value of x to predict at.
ols <- function(x, y, x0 = NULL) {
  fit <- lm(y ~ x)
  b0  <- unname(fit$coefficients[1])
  b1  <- unname(fit$coefficients[2])
  SSR <- sum(fit$residuals^2)
  SST <- sum((y - mean(y))^2)
  SSE <- sum((fit$fitted.values - mean(y))^2)
  cat("intercept b0 =", round(b0, 4), "\n")
  cat("slope     b1 =", round(b1, 4), "\n")
  cat("SST =", round(SST, 4), " SSE =", round(SSE, 4), " SSR =", round(SSR, 4), "\n")
  cat("R^2 =", round(SSE / SST, 4), "\n")
  cat("fitted    :", round(fit$fitted.values, 4), "\n")
  cat("residuals :", round(fit$residuals, 4), "\n")
  if (!is.null(x0)) cat("prediction at x =", x0, ":", round(b0 + b1 * x0, 4), "\n")
  invisible(fit)
}

# ptable(x, y, p): prints every population quantity from a joint table.
# Enter x, y, p in the SAME row order as the table. y is optional.
ptable <- function(x, y = NULL, p) {
  cat("sum of p =", sum(p), "(must be 1)\n")
  EX <- sum(x * p); EXsq <- sum(x^2 * p); VX <- EXsq - EX^2
  cat("E[X] =", round(EX, 4), " E[X^2] =", round(EXsq, 4),
      " Var(X) =", round(VX, 4), " sd(X) =", round(sqrt(VX), 4), "\n")
  if (!is.null(y)) {
    EY <- sum(y * p); EYsq <- sum(y^2 * p); VY <- EYsq - EY^2
    EXY <- sum(x * y * p); CXY <- EXY - EX * EY
    cat("E[Y] =", round(EY, 4), " E[Y^2] =", round(EYsq, 4),
        " Var(Y) =", round(VY, 4), " sd(Y) =", round(sqrt(VY), 4), "\n")
    cat("E[XY] =", round(EXY, 4), " Cov(X,Y) =", round(CXY, 4),
        " Cor(X,Y) =", round(CXY / sqrt(VX * VY), 4), "\n")
  }
}


# =====================================================================
# 1  DESCRIBE A DATASET
# =====================================================================
# TEMPLATE
#   data("DATASET")
#   nrow(DATASET)                     # number of observations (n)
#   ncol(DATASET)                     # number of variables
#   ls(DATASET)                       # variable names
#   summary(DATASET)                  # min, quartiles, mean, max of every variable
#   mean(DATASET$X); median(DATASET$X); var(DATASET$X); sd(DATASET$X)
#   min(DATASET$X);  max(DATASET$X);   sum(DATASET$X)
#   cov(DATASET$X, DATASET$Y);  cor(DATASET$X, DATASET$Y)

# EXAMPLE (wage1)
data("wage1")
nrow(wage1)                          # How many workers?               -> 526
ncol(wage1)                          # How many variables?             -> 24
mean(wage1$wage)                     # Mean hourly wage?               -> 5.90
median(wage1$wage)                   # Median wage?                    -> 4.65
sd(wage1$educ)                       # SD of education?                -> 2.77
var(wage1$educ)                      # Variance of education?          -> 7.67
max(wage1$educ)                      # Most years of education?        -> 18
cov(wage1$wage, wage1$educ)          # Sample covariance?              -> 4.15
cor(wage1$wage, wage1$educ)          # Sample correlation?             -> 0.41
mean(wage1$female)                   # Share of workers who are female -> 0.48
# (mean of a 0/1 variable = proportion)
sum(wage1$educ == 12)                # How many have exactly 12 years? -> 198


# =====================================================================
# 2  SUMS, MEANS, PERCENT CHANGE, PERCENTAGE POINTS   (Quizzes 1-2)
# =====================================================================
# TEMPLATE
#   sum(c(a, b, c));  mean(c(a, b, c))
#   100 * (new - old) / old           # percent change
#   new - old                         # percentage-point change (for rates)

# EXAMPLES
sum(c(8, 3, 4))                      # 8 + 3 + 4?                       -> 15
mean(c(2, 5, 7, 10))                 # Mean of 2, 5, 7, 10?             -> 6
200 / 10                             # 10 numbers sum to 200. Mean?     -> 20
mean(c(9, 1, 11))                    # Sample mean of x = (9,1,11)?     -> 7
100 * (63 - 48) / 48                 # % change, 48 -> 63?              -> 31.25
6 - 4                                # Rate 4% -> 6%: points?           -> 2 percentage points
100 * (6 - 4) / 4                    # ... and percent increase?        -> 50%
x <- c(1, 2, 3); sum(2 * x)          # Sum of 2*x_i for x = (1,2,3)?    -> 12


# =====================================================================
# 3  LINEAR FUNCTIONS: PREDICT y, CHANGE IN y   (Quiz 2)
# =====================================================================
# TEMPLATE
#   b0 + b1 * x                       # value of y at x
#   b1 * dx                           # change in y: INTERCEPT DROPS OUT
#   b1 * dx1 + b2 * dx2               # change with two variables

# EXAMPLES
14 + 2 * 2                           # y = 14 + 2x, y at x = 2?            -> 18
7 * 9                                # b0=14, b1=7, dx=9. Change in y?     -> 63
2 * (-1) + 8 * 4                     # b1=2, b2=8, dx1=-1, dx2=4. dy?      -> 30
0.5 * 9                              # wage slope 5/10, educ up 9. dwage?  -> 4.5
13/10 + 5/10 * 25/10                 # colGPA = 13/10 + 5/10*hsGPA at 2.5  -> 2.55
9/10 * 4                             # slope 9/10, x up 4. dy?             -> 3.6


# =====================================================================
# 4  SAMPLE STATS FROM A SMALL LIST OF NUMBERS   (Quiz 3)
#    (var, sd, cov already divide by n - 1)
# =====================================================================
# TEMPLATE
#   x <- c(...); y <- c(...)
#   mean(x); var(x); sd(x); cov(x, y); cor(x, y)
#   by formula: sum((x - mean(x))^2) / (length(x) - 1)

# EXAMPLES
var(c(2, 5, 2))                      # Sample variance of (2,5,2)?      -> 3
var(c(2, 8, 9))                      # Sample variance of (2,8,9)?      -> 14.33
sd(c(6, 1, 7))                       # Sample sd of (6,1,7)?            -> 3.21
sd(c(0, 8, 7))                       # Sample sd of (0,8,7)?            -> 4.36
cov(c(6, 4, 0), c(0, 6, 6))          # Sample cov, x=(6,4,0), y=(0,6,6) -> -8
cov(c(2, 6, 3), c(3, 6, 6))          # Sample cov, x=(2,6,3), y=(3,6,6) -> 2.5
cor(c(8, 7, 3), c(7, 7, 2))          # Sample cor, x=(8,7,3), y=(7,7,2) -> 0.98
x <- c(2, 5, 2)
sum((x - mean(x))^2) / (length(x) - 1)   # same variance by formula     -> 3


# =====================================================================
# 5  PROBABILITY TABLES   (Quizzes 3-5)  -- population, NOT sample
# =====================================================================
# TEMPLATE
#   xvec <- c(...); yvec <- c(...); probvec <- c(...)   # same row order!
#   ptable(xvec, yvec, probvec)       # prints everything at once
#   ...or by hand:
#   EX   <- sum(xvec * probvec)
#   EXsq <- sum(xvec^2 * probvec)
#   EY   <- sum(yvec * probvec)
#   EXY  <- sum(xvec * yvec * probvec)
#   varX <- EXsq - EX^2;  sdX <- sqrt(varX)
#   covXY <- EXY - EX * EY
#   corXY <- covXY / (sqrt(varX) * sqrt(varY))

# EXAMPLE A: table   x y  P          Find E[X] and Cov(X,Y)
#                    3 6 0.2
#                    1 2 0.3
#                    2 2 0.5
xvec <- c(3, 1, 2); yvec <- c(6, 2, 2); probvec <- c(0.2, 0.3, 0.5)
sum(xvec * probvec)                                  # E[X]          -> 1.9
ptable(xvec, yvec, probvec)                          # all at once

# EXAMPLE B: table (6,5,.2), (1,0,.3), (1,8,.5). Find Cov(X,Y)
xvec <- c(6, 1, 1); yvec <- c(5, 0, 8); probvec <- c(0.2, 0.3, 0.5)
EX  <- sum(xvec * probvec)                           # -> 2
EY  <- sum(yvec * probvec)                           # -> 5
EXY <- sum(xvec * yvec * probvec)                    # -> 10
EXY - EX * EY                                        # Cov           -> 0

# EXAMPLE C: pairs (2,1), (1,2), (3,1), each prob 1/3. Cov(X,Y)?
ptable(c(2, 1, 3), c(1, 2, 1), c(1/3, 1/3, 1/3))     # Cov           -> -0.33

# EXAMPLE D: X = 9, 7, 4 with probs 0.2, 0.5, 0.3. Find E[X^2]
sum(c(9, 7, 4)^2 * c(0.2, 0.5, 0.3))                 # E[X^2]        -> 45.5

# EXAMPLE E: fair coin, 4 on heads, 7 on tails. E[X]?
sum(c(4, 7) * c(1/2, 1/2))                           #               -> 5.5

# EXAMPLE F: X = 9 w.p. 3/10, 4 w.p. 7/10. E[X]?
sum(c(9, 4) * c(3/10, 7/10))                         #               -> 5.5

# EXAMPLE G (conditional expectation): given X = x, Y = 5, 5, 0 with
#            probs 0.2, 0.3, 0.5. Find E[Y | X = x]
sum(c(5, 5, 0) * c(0.2, 0.3, 0.5))                   #               -> 2.5


# =====================================================================
# 6  PROPERTIES   (Quiz 4 + Properties practice)   -- just plug in
#    If a question gives you E[X], Var(X), Cov, a, b ... and NO table or
#    data, it is a properties question. Find the rule, plug in.
# =====================================================================

# ---------------------------------------------------------------------
# 6A  SUMS AND MEANS
#   sum(x_i + c)   = sum(x_i) + n*c        (c is added n times!)
#   sum(c * x_i)   = c * sum(x_i)
#   sum(x_i)       = n * xbar
#   mean(a*x + b)  = a * xbar + b
#   sum(x_i - xbar) = 0                    (always, for any data)
# ---------------------------------------------------------------------
4 * 5                                # n=4, xbar=5. sum(x)?                -> 20
20 + 4 * 3                           # sum(x)=20, n=4. sum(x_i + 3)?       -> 32  (not 23)
5 * 6 + 5 * 2                        # n=5, xbar=6. sum(x_i + 2)?          -> 40  (not 32)
2 * 5 - 1                            # xbar=5. mean(2x - 1)?               -> 9
x <- c(4, 9, 2, 5); sum(x - mean(x)) # deviations from the mean sum to     -> 0

# ---------------------------------------------------------------------
# 6B  RESCALING SAMPLE DATA  (a number times x, plus a constant)
#   var(a*x + b)   = a^2 * var(x)          (b drops out, a is SQUARED)
#   sd(a*x + b)    = |a| * sd(x)           (absolute value, NOT squared)
#   cov(a*x + b, y) = a * cov(x, y)        (a comes out once)
#   cov(a*x, c*y)  = a * c * cov(x, y)
#   cor(a*x + b, y) = cor(x, y) if a > 0,  -cor(x, y) if a < 0
#   Adding b moves every value the same amount -> spread does not change.
#   Correlation has NO units -> rescaling never changes its size.
# ---------------------------------------------------------------------
3^2 * 4                              # var(x)=4. var(3x + 7)?              -> 36
(-2)^2 * 9                           # var(x)=9. var(-2x + 4)?             -> 36
abs(-0.5) * 4                        # sd(x)=4. sd(-0.5x)?                 -> 2
2 * 10                               # cov(x,y)=10. cov(2x + 5, y)?        -> 20
2.54 * 30                            # cov=30, x times 2.54. new cov?      -> 76.2
-0.6                                 # cor=0.6. cor(-3x + 1, y)?           -> -0.6 (sign flips)
0.7                                  # cor=0.7. cor(5x - 3, y)?            -> 0.7  (unchanged)
# Prove it with any data:
x <- c(3, 7, 8, 12, 5); y <- c(10, 14, 13, 20, 11)
var(3 * x + 7) / var(x)              # always a^2                          -> 9
sd(-0.5 * x) / sd(x)                 # always |a|                          -> 0.5
cor(-3 * x + 1, y); -cor(x, y)       # same number                         -> -0.96

# ---------------------------------------------------------------------
# 6C  EXPECTED VALUE
#   E[b]          = b                      (a constant's average is itself)
#   E[aX + b]     = a*E[X] + b
#   E[aX + bY]    = a*E[X] + b*E[Y]        (ALWAYS, related or not)
#   E[XY]         = E[X]*E[Y]  ONLY if X, Y independent (Cov = 0)
# ---------------------------------------------------------------------
-2 * 6 + 10                          # E[X]=6, a=-2, b=10. E[aX+b]?        -> -2
10 - 2 * 4                           # E[X]=4. E[10 - 2X]?                 -> 2
3 * 4 - 2                            # E[X]=4. E[3X - 2]?                  -> 10
4 * 3 + (-1) * (-2)                  # E[X]=3,E[Y]=-2,a=4,b=-1. E[aX+bY]?  -> 14
1 * 5 + 5 * 2                        # E[X]=5,E[Y]=2,a=1,b=5. E[aX+bY]?    -> 15
2 * 3                                # independent, E[X]=2,E[Y]=3. E[XY]?  -> 6

# ---------------------------------------------------------------------
# 6D  VARIANCE AND STANDARD DEVIATION
#   Var(b)        = 0                      (a constant never varies)
#   Var(aX + b)   = a^2 * Var(X)           sd(aX + b) = |a| * sd(X)
#   Var(X)        = E[X^2] - (E[X])^2      (never negative)
#   E[X^2]        = Var(X) + (E[X])^2      (same formula, rearranged)
#   sd(X)         = sqrt(Var(X))
#   Var(aX + bY)  = a^2 Var(X) + b^2 Var(Y) + 2ab Cov(X,Y)
#   Var(X + Y)    = Var(X) + Var(Y) + 2Cov(X,Y)
#   Var(X - Y)    = Var(X) + Var(Y) - 2Cov(X,Y)   (variances still ADD)
#   Independent   -> Cov = 0 -> Var(X + Y) = Var(X) + Var(Y)
#   sd's do NOT add: go through the variances, then sqrt
# ---------------------------------------------------------------------
0                                    # Var(7)?                             -> 0
2^2 * 7                              # Var(X)=7, a=2. Var(aX + b)?         -> 28
3^2 * 9                              # Var(X)=9. Var(3X + b)?              -> 81
(-2)^2 * 6                           # Var(X)=6. Var(-2X + 10)?            -> 24
4^2 * 25                             # Var(Q)=25. Var(4Q - 50)?            -> 400
abs(-3) * 5                          # sd(X)=5, a=-3. sd(aX + b)?          -> 15
72 - 6^2                             # E[X]=6, E[X^2]=72. Var(X)?          -> 36
30 - 5^2                             # E[X]=5, E[X^2]=30. Var(X)?          -> 5
sqrt(25 - 4^2)                       # E[X]=4, E[X^2]=25. sd(X)?           -> 3
4 + 3^2                              # Var(X)=4, E[X]=3. E[X^2]?           -> 13
6 + (-2)^2                           # Var(X)=6, E[X]=-2. E[X^2]?          -> 10
4 + 9 + 2 * 1 * 1 * 3                # Var 4, 9, Cov 3. Var(X + Y)?        -> 19
4 + 9 - 2 * 3                        # Var 4, 9, Cov 3. Var(X - Y)?        -> 7
4 + 9 - 2 * (-2)                     # Var 4, 9, Cov -2. Var(X - Y)?       -> 17
3^2*5 + 3^2*4 + 2*3*3*2              # Var 5, 4, Cov 2, a=b=3. Var(aX+bY)? -> 117
2^2*4 + 1^2*9 + 2*2*1*(-3)           # Var 4, 9, Cov -3. Var(2X + Y)?      -> 13
.5^2*16 + .5^2*36 + 2*.5*.5*6        # Var 16, 36, Cov 6, a=b=.5           -> 16
3 + 7                                # independent, Var 3, 7. Var(X - Y)?  -> 10
sqrt(3^2 + 4^2)                      # independent, sd 3, 4. sd(X + Y)?    -> 5 (NOT 7)
(14 - 5 - 5) / 2                     # Var 5, 5, Var(X+Y)=14. Cov?         -> 2

# ---------------------------------------------------------------------
# 6E  COVARIANCE
#   Cov(X, Y)            = E[XY] - E[X]*E[Y]
#   Cov(X, X)            = Var(X)
#   Cov(aX + b, cY + d)  = a * c * Cov(X, Y)   (constants drop, multipliers out)
#   Independent -> Cov = 0,  but Cov = 0 does NOT mean independent
# ---------------------------------------------------------------------
9 - 1 * 1                            # E[XY]=9, E[X]=E[Y]=1. Cov?          -> 8
12 - 2 * 5                           # E[XY]=12, E[X]=2, E[Y]=5. Cov?      -> 2  (not independent)
6                                    # Var(X)=6. Cov(X, X)?                -> 6
2 * (-1) * 3                         # Cov=3. Cov(2X + 1, -Y + 4)?         -> -6
3 * 2 * 5                            # Cov=5. Cov(3X - 1, 2Y + 7)?         -> 30
# Cov = 0 but NOT independent: X = -1, 0, 1 (prob 1/3 each), Y = X^2
ptable(c(-1, 0, 1), c(1, 0, 1), c(1/3, 1/3, 1/3))   # Cov -> 0, yet Y depends on X

# ---------------------------------------------------------------------
# 6F  CORRELATION
#   Cor(X, Y) = Cov(X, Y) / (sd(X) * sd(Y))   (divide by sd's, NOT variances)
#   always between -1 and 1, no units, same sign as Cov
# ---------------------------------------------------------------------
0 / (3 * 2)                          # Cov=0, sd 3 and 2. Cor?             -> 0
-8 / (2 * 8)                         # Cov=-8, sd 2 and 8. Cor?            -> -0.5
4 / (sqrt(4) * sqrt(16))             # Cov=4, VARIANCES 4 and 16. Cor?     -> 0.5
-6 / (2 * 5)                         # Cov=-6, sd 2 and 5. Cor?            -> -0.6

# ---------------------------------------------------------------------
# 6G  CONDITIONAL EXPECTATION
#   E[Y | X = x] = average of Y within the X = x group
#   E[Y] = sum of P(X = x) * E[Y | X = x]   (weight by group size)
#   Zero conditional mean E[U | X] = 0  ->  E[Y | X] = b0 + b1*X
# ---------------------------------------------------------------------
0.75 * 10 + 0.25 * 16                # E[Y|X=0]=10, E[Y|X=1]=16, P(X=1)=.25 -> 11.5 (not 13)
0.6 * 10 + 0.4 * 16                  # same, P(X=1)=.4. E[Y]?              -> 12.4
0.4 * 30 + 0.6 * 20                  # group means 30 & 20, 40% in first   -> 24
2 + 0.5 * 6                          # ZCM, b0=2, b1=0.5. E[Y | X=6]?      -> 5

# ---------------------------------------------------------------------
# 6H  OLS PROPERTIES (as plug-in questions)
#   (xbar, ybar) is always on the OLS line  -> yhat at xbar = ybar
#   residuals always sum to 0               -> missing residual = -(sum of others)
#   b1 = cov/var = cor * sd(y) / sd(x)      (slope has the sign of cov)
#   R^2 = cor^2 in simple regression        (never negative)
#   R^2 = SSE/SST,  SSR = SST * (1 - R^2)
#   mean of fitted values = ybar
#   x times a  -> slope divided by a, R^2 UNCHANGED
# ---------------------------------------------------------------------
5 + 2 * 3                            # yhat = 5 + 2x, xbar = 3. ybar?      -> 11
11                                   # xbar=4, ybar=11. yhat at x=4?       -> 11 (no slope needed)
-(1.5 - 2 + 0.5 + 3)                 # 4 residuals 1.5,-2,.5,3. 5th?       -> -3
-(2 - 1 + 3)                         # 3 residuals 2,-1,3. 4th?            -> -4
0.8 * 10 / 4                         # cor=.8, sd(y)=10, sd(x)=4. slope?   -> 2
(-0.4)^2                             # cor=-.4. R^2?                       -> 0.16
400 * (1 - 0.30)                     # SST=400, R^2=.30. SSR?              -> 280
# Prove the rescaling rule:
x <- c(1, 3, 4, 6, 8); y <- c(2, 5, 4, 9, 10)
lm(y ~ x)$coefficients[2]; lm(y ~ I(2 * x))$coefficients[2]   # slope halves -> 1.20, 0.60
summary(lm(y ~ x))$r.squared; summary(lm(y ~ I(2 * x)))$r.squared  # same   -> 0.91, 0.91

# ---------------------------------------------------------------------
# 6I  LOGS AND EXPONENTIALS   (log() in R = natural log, ln)
#   ln(e^x) = x,  e^(ln a) = a,  ln(1) = 0,  e^0 = 1
#   ln(a*b) = ln a + ln b,  ln(a/b) = ln a - ln b,  ln(a^k) = k*ln a
#   ln(a + b) is NOT ln a + ln b
# ---------------------------------------------------------------------
log(exp(4)) + log(1)                 # ln(e^4) + ln(1)?                    -> 4
log(8); 3 * log(2)                   # ln(8) = 3 ln(2)                     -> 2.08
log(2 * 8); log(2) + log(8)          # ln(ab) = ln a + ln b                -> 2.77
exp(log(5))                          # e^(ln 5)?                           -> 5


# =====================================================================
# 7  OLS BY HAND FROM SMALL DATA   (Quizzes 5-6)
# =====================================================================
# TEMPLATE
#   x <- c(...); y <- c(...)
#   ols(x, y, x0 = VALUE)             # prints slope, intercept, prediction, SSR, R^2
#   ...or by hand:
#   b1 <- cov(x, y) / var(x)          # slope
#   b0 <- mean(y) - b1 * mean(x)      # intercept
#   b0 + b1 * x0                      # prediction

# EXAMPLE A: x = (1,5,5), y = (11,11,12). OLS slope?
x <- c(1, 5, 5); y <- c(11, 11, 12)
cov(x, y) / var(x)                                   # slope  -> 0.125 (Canvas: 0.12)

# EXAMPLE B: x = (1,3,2), y = (1,5,12). OLS intercept?
x <- c(1, 3, 2); y <- c(1, 5, 12)
b1 <- cov(x, y) / var(x)                             # -> 2
mean(y) - b1 * mean(x)                               # intercept -> 2

# EXAMPLE C: x = (8,8,6), y = (1,9,9). Predict y at x = 8
ols(c(8, 8, 6), c(1, 9, 9), x0 = 8)                  # prediction -> 5

# EXAMPLE D: x = (3,5,3), y = (8,0,9). Slope?
cov(c(3, 5, 3), c(8, 0, 9)) / var(c(3, 5, 3))        # -> -4.25

# EXAMPLE E: x = (8,6,1), y = (8,12,10). Predict at x = 9
ols(c(8, 6, 1), c(8, 12, 10), x0 = 9)                # prediction -> 9.38


# =====================================================================
# 8  GIVEN A FITTED LINE: FITTED VALUE, RESIDUAL, SSR, SSE, R^2   (Quiz 6)
# =====================================================================
# TEMPLATE
#   yhat <- b0 + b1 * x               # fitted value
#   y - yhat                          # residual (ACTUAL minus FITTED)
#   sum((y - (b0 + b1 * x))^2)        # SSR for listed points
#   SST - SSR                         # SSE
#   SSE / SST   or   1 - SSR / SST    # R^2

# EXAMPLES
7 + 5/10 * 5                         # yhat = 7 + (5/10)x at x = 5            -> 9.5
18 - (3 + 6/10 * 8)                  # yhat = 3 + (6/10)x, x=8, y=18. resid?  -> 10.2
x <- c(10, 6, 7); y <- c(2, 15, 5)   # yhat = 2 + (4/10)x, points (10,2),(6,15),(7,5)
sum((y - (2 + 4/10 * x))^2)          # SSR?                                    -> 128.4
294 - 29                             # SST = 294, SSR = 29. SSE?               -> 265
1 - 29 / 294                         # ... and R^2?                            -> 0.90
100 / 500                            # SST = 500, SSE = 100. R^2?              -> 0.2
# Residual SIGN: positive -> under-predicted (actual above line)
#                negative -> over-predicted  (actual below line)


# =====================================================================
# 9  OLS FROM SUMMARY NUMBERS   (Quiz 5)
# =====================================================================
# TEMPLATE
#   b1 = cov(x,y) / var(x)
#   b0 = ybar - b1 * xbar
#   b1 = cor(x,y) * sd(y) / sd(x)

# EXAMPLES
-11 / 2                              # cov = -11, var(x) = 2. Slope?      -> -5.5 (keep the sign!)
12 - 7/10 * 9                        # ybar=12, xbar=9, b1=7/10. b0?      -> 5.7
0.5 * 4 / 2                          # cor=0.5, sd(y)=4, sd(x)=2. Slope?  -> 1


# =====================================================================
# 10  WOOLDRIDGE DATA REGRESSION   (Problem Set 7 = THE exam template)
# =====================================================================
# TEMPLATE  (replace DATA, Y, X)
#   data("DATA")
#   reg <- lm(Y ~ X, data = DATA)
#   b0  <- reg$coefficients[1]        # intercept
#   b1  <- reg$coefficients[2]        # slope
#   b0 + b1 * VALUE                   # predicted Y at X = VALUE
#   b1 * CHANGE                       # predicted change in Y
#   SSR <- sum(reg$residuals^2)
#   SST <- sum((DATA$Y - mean(DATA$Y))^2)
#   SSE <- sum((reg$fitted.values - mean(DATA$Y))^2)
#   1 - SSR / SST                     # R^2
#   summary(reg)$r.squared            # R^2 check
#   summary(reg)                      # full output table

# EXAMPLE A: Problem Set 7 (wage1, bwght)
data("wage1"); data("bwght")
nrow(wage1)                                          # Q1  -> 526
mean(wage1$wage)                                     # Q2  -> 5.90
sd(wage1$educ)                                       # Q3  -> 2.77
reg <- lm(wage ~ educ, data = wage1)
b1 <- reg$coefficients[2]; b1                        # Q4  -> 0.54
b0 <- reg$coefficients[1]; b0                        # Q5  -> -0.90
b0 + b1 * 14                                         # Q6  -> 6.67
b1 * 2                                               # Q7  -> 1.08
SSR <- sum(reg$residuals^2); SSR                     # Q8  -> 5980.68
SST <- sum((wage1$wage - mean(wage1$wage))^2)
1 - SSR / SST                                        # Q9  -> 0.16
summary(reg)$r.squared                               #        check 0.16
reg2 <- lm(bwght ~ cigs, data = bwght)
reg2$coefficients[2] * 5                             # Q10 -> -2.57
mean(wage1$wage[wage1$educ == 16])                   # Q11 -> 8.04

# EXAMPLE B: CEO salary (ceosal1), salary in $1000s, roe in %
data("ceosal1")
regA <- lm(salary ~ roe, data = ceosal1)
regA$coefficients                                    # b0 -> 963.19, b1 -> 18.50
regA$coefficients[1] + regA$coefficients[2] * 30     # predicted salary at roe=30 -> 1518.23
regA$coefficients[2] * 10                            # change for roe +10 -> 185.01 ($185,012)
summary(regA)$r.squared                              # R^2 -> 0.01
regA$residuals[1]                                    # residual of 1st CEO -> -129.06 (over-predicted)

# EXAMPLE C: college GPA (gpa1)
data("gpa1")
regB <- lm(colGPA ~ hsGPA, data = gpa1)
regB$coefficients                                    # b0 -> 1.42, b1 -> 0.48
regB$coefficients[1] + regB$coefficients[2] * 3.5    # predicted colGPA at 3.5 -> 3.10
summary(regB)$r.squared                              # R^2 -> 0.17
cor(gpa1$colGPA, gpa1$hsGPA)^2                       # R^2 = cor^2 in SLR -> 0.17

# Using the $ form instead of data = (same answer):
lm(wage1$wage ~ wage1$educ)$coefficients             # -> -0.90, 0.54


# =====================================================================
# 11  SUBSETS / CONDITIONAL MEANS IN DATA
# =====================================================================
# TEMPLATE
#   mean(DATA$Y[DATA$X == VALUE])     # mean of Y for one group
#   mean(DATA$Y[DATA$X > VALUE])      # mean of Y above a cutoff
#   sum(DATA$X == VALUE)              # how many in that group
#   var(DATA$Y[DATA$X == VALUE])      # conditional (sample) variance

# EXAMPLES (wage1)
mean(wage1$wage[wage1$educ == 16])                   # E[wage | educ = 16]   -> 8.04
mean(wage1$wage[wage1$educ == 12])                   # E[wage | educ = 12]   -> 5.37
mean(wage1$wage[wage1$female == 1])                  # mean wage of women     -> 4.59
mean(wage1$wage[wage1$female == 0])                  # mean wage of men       -> 7.10
sum(wage1$educ == 16)                                # workers with 16 years  -> 68
var(wage1$wage[wage1$educ == 12])                    # Var(wage | educ = 12)  -> 9.57


# =====================================================================
# 12  CHECK THE OLS PROPERTIES
# =====================================================================
x <- c(2, 4, 6, 8); y <- c(3, 7, 8, 12)
fit <- lm(y ~ x)
sum(fit$residuals)                   # residuals sum to 0      -> 0 (5.55e-17 = 0)
cor(x, fit$residuals)                # x uncorrelated w/ resid -> 0
fit$coefficients[1] + fit$coefficients[2] * mean(x)   # line passes (xbar, ybar) -> 7.5
mean(y)                                               #                          -> 7.5
SST <- sum((y - mean(y))^2); SSE <- sum((fit$fitted.values - mean(y))^2)
SSR <- sum(fit$residuals^2)
SST; SSE + SSR                       # SST = SSE + SSR         -> 41 = 41
SSE / SST                            # R^2                     -> 0.96
cor(x, y)^2                          # = R^2 in simple regression -> 0.96


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