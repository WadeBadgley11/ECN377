# =====================================================================
# ECN 377 - Exam I practice script  (Dr. Loh, Wed 10/7)
# Covers: Problem Set 7 + Quizzes 1-6 style problems + new practice
# How to use: put your cursor on a line and press Cmd+Enter (Mac) or
# Ctrl+Enter (PC) to run it. Answers print in the Console.
# Round every answer to the hundredths (2 decimals).
# Try each problem yourself BEFORE running the solution lines.
# =====================================================================

rm(list = ls())          # clear environment
library(wooldridge)      # install once first:  install.packages("wooldridge")


# =====================================================================
# PART 1 - PROBLEM SET 7, fully worked  (the exam template)
# =====================================================================
data("wage1")
data("bwght")

# Q1 How many workers?
nrow(wage1)                                    # 526

# Q2 Sample mean of wage
mean(wage1$wage)                               # 5.90

# Q3 Sample sd of educ
sd(wage1$educ)                                 # 2.77

# Q4 OLS slope of wage on educ
reg <- lm(wage ~ educ, data = wage1)
b1 <- reg$coefficients[2]                      # [2] = slope
b1                                             # 0.54

# Q5 OLS intercept
b0 <- reg$coefficients[1]                      # [1] = intercept
b0                                             # -0.90

# Q6 Predicted wage at educ = 14  (use saved b0, b1 -- do NOT retype rounded numbers)
b0 + b1 * 14                                   # 6.67

# Q7 Change in predicted wage when educ rises by 2  (intercept drops out)
b1 * 2                                         # 1.08

# Q8 SSR = sum of squared residuals
SSR <- sum(reg$residuals^2)
SSR                                            # 5980.68

# Q9 R-squared = 1 - SSR/SST
SST <- sum((wage1$wage - mean(wage1$wage))^2)
1 - SSR / SST                                  # 0.16
summary(reg)$r.squared                         # check: 0.1648 -> 0.16

# Q10 bwght on cigs: change in birth weight for 5 more cigs/day
reg2 <- lm(bwght ~ cigs, data = bwght)
reg2$coefficients[2] * 5                       # -2.57

# Q11 (challenge) mean wage among workers with exactly 16 years of educ
mean(wage1$wage[wage1$educ == 16])             # 8.04


# =====================================================================
# PART 2 - PRACTICE SET A: CEO salary and return on equity (ceosal1)
# salary = CEO salary in THOUSANDS of dollars; roe = return on equity, in %
# =====================================================================
data("ceosal1")

# A1 How many CEOs are in the sample?
nrow(ceosal1)

# A2 Sample mean of salary (in $1000s)
mean(ceosal1$salary)

# A3 Sample standard deviation of roe
sd(ceosal1$roe)

# A4 Sample correlation between salary and roe
cor(ceosal1$salary, ceosal1$roe)

# A5 Regress salary on roe. What is the OLS slope?
regA <- lm(salary ~ roe, data = ceosal1)
bA1 <- regA$coefficients[2]
bA1

# A6 What is the OLS intercept?
bA0 <- regA$coefficients[1]
bA0

# A7 Check: slope = sample cov / sample var of x
cov(ceosal1$salary, ceosal1$roe) / var(ceosal1$roe)

# A8 Predicted salary when roe = 30
bA0 + bA1 * 30

# A9 Predicted CHANGE in salary when roe rises by 10 points
bA1 * 10

# A10 SSR
SSR_A <- sum(regA$residuals^2)
SSR_A

# A11 SST and SSE, and check SST = SSE + SSR
SST_A <- sum((ceosal1$salary - mean(ceosal1$salary))^2)
SSE_A <- sum((regA$fitted.values - mean(ceosal1$salary))^2)
SST_A
SSE_A
SSE_A + SSR_A                                  # equals SST_A

# A12 R-squared (decimal)
1 - SSR_A / SST_A
summary(regA)$r.squared

# A13 Residual for the FIRST CEO in the data:  u_hat = actual - fitted
ceosal1$salary[1] - regA$fitted.values[1]
regA$residuals[1]                              # same thing

# A14 Mean salary among CEOs with roe above 20 (challenge-style)
mean(ceosal1$salary[ceosal1$roe > 20])


# =====================================================================
# PART 3 - PRACTICE SET B: college GPA and high school GPA (gpa1)
# =====================================================================
data("gpa1")

# B1 Sample size
nrow(gpa1)

# B2 Mean colGPA, sample variance of hsGPA
mean(gpa1$colGPA)
var(gpa1$hsGPA)

# B3 Regress colGPA on hsGPA: slope and intercept
regB <- lm(colGPA ~ hsGPA, data = gpa1)
bB0 <- regB$coefficients[1]
bB1 <- regB$coefficients[2]
bB1
bB0

# B4 Predicted colGPA for hsGPA = 3.5
bB0 + bB1 * 3.5

# B5 Predicted change in colGPA if hsGPA rises by 0.5
bB1 * 0.5

# B6 R-squared
summary(regB)$r.squared

# B7 In simple regression, R^2 = (sample correlation)^2. Check:
cor(gpa1$colGPA, gpa1$hsGPA)^2

# B8 Now regress colGPA on ACT. Predicted change for ACT up 4 points?
regB2 <- lm(colGPA ~ ACT, data = gpa1)
regB2$coefficients[2] * 4


# =====================================================================
# PART 4 - PRACTICE SET C: more datasets, same moves
# =====================================================================
# C1-C3: vote1.  voteA = % of vote for candidate A, shareA = A's % share of campaign spending
data("vote1")
regC <- lm(voteA ~ shareA, data = vote1)
regC$coefficients[2]                           # C1 slope
regC$coefficients[1] + regC$coefficients[2] * 60   # C2 predicted voteA at shareA = 60
summary(regC)$r.squared                        # C3 R-squared

# C4-C6: meap93.  math10 = % of students passing 10th grade math,
#        lnchprg = % of students eligible for school lunch program (poverty proxy)
data("meap93")
regD <- lm(math10 ~ lnchprg, data = meap93)
regD$coefficients[2]                           # C4 slope (negative!)
regD$coefficients[2] * 10                      # C5 change in math10 for lnchprg up 10 points
summary(regD)$r.squared                        # C6 R-squared

# C7-C8: sleep75.  sleep = minutes of sleep per week, totwrk = minutes of work per week
data("sleep75")
regE <- lm(sleep ~ totwrk, data = sleep75)
regE$coefficients[2]                           # C7 slope
regE$coefficients[2] * 120                     # C8 change in sleep for 2 more hours (120 min) of work


# =====================================================================
# PART 5 - OLS BY HAND WITH SMALL DATA (Quiz 5 & 6 style)
# Type the numbers in with c(), then use lm() or the formulas.
# =====================================================================

# H1 (Quiz 6 Q4) x = (1,5,5), y = (11,11,12). OLS slope?
x <- c(1, 5, 5); y <- c(11, 11, 12)
cov(x, y) / var(x)                             # formula: slope = cov(x,y)/var(x)
lm(y ~ x)$coefficients[2]                      # same with lm()

# H2 (Quiz 6 Q5) x = (1,3,2), y = (1,5,12). OLS intercept?
x <- c(1, 3, 2); y <- c(1, 5, 12)
b1h <- cov(x, y) / var(x)
mean(y) - b1h * mean(x)                        # formula: b0 = ybar - b1 * xbar
lm(y ~ x)$coefficients[1]

# H3 (Quiz 6 Q6) x = (8,8,6), y = (1,9,9). Fit OLS, predict y at x = 8
x <- c(8, 8, 6); y <- c(1, 9, 9)
fit <- lm(y ~ x)
fit$coefficients[1] + fit$coefficients[2] * 8

# H4 (Quiz 5 Q6) x = (3,5,3), y = (8,0,9). OLS slope?
x <- c(3, 5, 3); y <- c(8, 0, 9)
cov(x, y) / var(x)

# H5 (Quiz 5 Q7) x = (8,6,1), y = (8,12,10). Predict y at x = 9
x <- c(8, 6, 1); y <- c(8, 12, 10)
fit <- lm(y ~ x)
fit$coefficients[1] + fit$coefficients[2] * 9

# H6 (Quiz 6 Q8) yhat = 3 + 0.6x. Person has x = 8, actual y = 18. Residual?
18 - (3 + 0.6 * 8)                             # u_hat = y - yhat

# H7 (Quiz 6 Q10) yhat = 2 + 0.4x, points (10,2), (6,15), (7,5). SSR?
x <- c(10, 6, 7); y <- c(2, 15, 5)
yhat <- 2 + 0.4 * x
sum((y - yhat)^2)

# H8 (Quiz 6 Q11) SST = 294, SSR = 29. SSE?
294 - 29                                       # SSE = SST - SSR

# H9 NEW: x = (2,4,6,8), y = (3,7,8,12). Slope, intercept, prediction at x = 5, R^2
x <- c(2, 4, 6, 8); y <- c(3, 7, 8, 12)
fit <- lm(y ~ x)
fit$coefficients                               # [1] intercept, [2] slope
fit$coefficients[1] + fit$coefficients[2] * 5
summary(fit)$r.squared

# H10 NEW: same data. Verify the 3 algebraic properties of OLS
sum(fit$residuals)                             # = 0 (tiny number like 1e-16 means 0)
cor(x, fit$residuals)                          # = 0
fit$coefficients[1] + fit$coefficients[2] * mean(x)   # = mean(y): (xbar, ybar) is on the line
mean(y)

# H11 NEW: sample variance, sd, covariance, correlation of small vectors
x <- c(4, 9, 2, 5); y <- c(10, 3, 8, 7)
var(x); sd(x); cov(x, y); cor(x, y)


# =====================================================================
# PART 6 - PROBABILITY TABLES (Quiz 4 & 5 style, population quantities)
# Enter the table as three vectors in the SAME order, then use sum().
# =====================================================================

# P1 (Quiz 5 Q10) x,y,p: (3,6,.2), (1,2,.3), (2,2,.5). E[X]?
xvec <- c(3, 1, 2); yvec <- c(6, 2, 2); probvec <- c(0.2, 0.3, 0.5)
sum(xvec * probvec)

# P2 (Quiz 5 Q11) x,y,p: (6,5,.2), (1,0,.3), (1,8,.5). Cov(X,Y)?
xvec <- c(6, 1, 1); yvec <- c(5, 0, 8); probvec <- c(0.2, 0.3, 0.5)
EX  <- sum(xvec * probvec)
EY  <- sum(yvec * probvec)
EXY <- sum(xvec * yvec * probvec)
EXY - EX * EY                                  # Cov = E[XY] - E[X]E[Y]

# P3 (Quiz 4 Q8) pairs (2,1), (1,2), (3,1), each prob 1/3. Cov(X,Y)?
xvec <- c(2, 1, 3); yvec <- c(1, 2, 1); probvec <- c(1/3, 1/3, 1/3)
EX <- sum(xvec * probvec); EY <- sum(yvec * probvec); EXY <- sum(xvec * yvec * probvec)
EXY - EX * EY

# P4 NEW full workup. x,y,p: (1,4,.1), (2,6,.4), (3,5,.3), (4,9,.2)
xvec <- c(1, 2, 3, 4); yvec <- c(4, 6, 5, 9); probvec <- c(0.1, 0.4, 0.3, 0.2)
sum(probvec)                                   # always check: must equal 1
EX   <- sum(xvec * probvec)
EY   <- sum(yvec * probvec)
EXsq <- sum(xvec^2 * probvec)
EYsq <- sum(yvec^2 * probvec)
EXY  <- sum(xvec * yvec * probvec)
varX <- EXsq - EX^2
varY <- EYsq - EY^2
covXY <- EXY - EX * EY
corXY <- covXY / (sqrt(varX) * sqrt(varY))
EX; EY; varX; sqrt(varX); covXY; corXY

# P5 NEW conditional expectation: E[Y | X = 2] when, given X = 2,
#    Y takes values 10, 20, 40 with conditional probs 0.5, 0.3, 0.2
sum(c(10, 20, 40) * c(0.5, 0.3, 0.2))


# =====================================================================
# PART 7 - PROPERTIES (no data needed, just plug in)
# =====================================================================
# E[aX + b]        = a*E[X] + b
# E[aX + bY]       = a*E[X] + b*E[Y]
# Var(aX + b)      = a^2 * Var(X)                    (b disappears!)
# Var(aX + bY)     = a^2 Var(X) + b^2 Var(Y) + 2ab Cov(X,Y)
# Var(X)           = E[X^2] - (E[X])^2
# Cov(X,Y)         = E[XY] - E[X]E[Y]
# Cor(X,Y)         = Cov(X,Y) / (sd(X) sd(Y))

3^2 * 9                                        # Quiz 4 Q6: Var(3X + b), Var(X) = 9
9 - 1 * 1                                      # Quiz 4 Q7: Cov, E[XY]=9, E[X]=E[Y]=1
3^2 * 5 + 3^2 * 4 + 2 * 3 * 3 * 2              # Quiz 4 Q10: Var(3X + 3Y)
-11 / 2                                        # Quiz 5 Q4: slope = cov/var = -11/2
5/10 * 9                                       # Quiz 5 Q9: delta wage = b1 * delta educ
1.3 + 0.5 * 2.5                                # Quiz 5 Q8: 13/10 + 5/10 * 25/10

# NEW
2 * 7 - 4                                      # E[2X - 4] when E[X] = 7
(-2)^2 * 6                                     # Var(-2X + 10) when Var(X) = 6
2^2 * 4 + 1^2 * 9 + 2 * 2 * 1 * (-3)           # Var(2X + Y): Var(X)=4, Var(Y)=9, Cov=-3
50 - 4^2                                       # Var(X) when E[X^2] = 50, E[X] = 4
6 / (3 * 4)                                    # Cor when Cov = 6, sd(X) = 3, sd(Y) = 4
100 * (63 - 48) / 48                           # % change from 48 to 63
6 - 4                                          # 4% -> 6% is 2 percentage points ...
100 * (6 - 4) / 4                              # ... which is a 50% increase


# =====================================================================
# TOOLKIT - copy into your own exam script (open notes, open R)
# =====================================================================
# n:            nrow(data)                     length(x)
# summary:      summary(data)                  mean(), var(), sd(), cov(), cor()
# regression:   reg <- lm(y ~ x, data = data)
# slope:        reg$coefficients[2]            intercept: reg$coefficients[1]
# prediction:   b0 + b1 * x0                   change: b1 * delta_x
# fitted/resid: reg$fitted.values              reg$residuals
# SSR:          sum(reg$residuals^2)
# SST:          sum((y - mean(y))^2)
# SSE:          sum((reg$fitted.values - mean(y))^2)
# R^2:          1 - SSR/SST   or   SSE/SST   or   summary(reg)$r.squared
# subset mean:  mean(data$y[data$x == value])
# by hand:      b1 = cov(x,y)/var(x);  b0 = mean(y) - b1*mean(x)