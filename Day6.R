## >>> GET & SAVE (details in the R guide) -------------------------
## GET, git mode  -> run in the Console:
##   download.file("https://raw.githubusercontent.com/isaaccloh/ECON377/main/377_2026/D06/D06_starter.R", "D06.R")
## GET, easy mode -> copy this file from github.com/isaaccloh/ECON377 (377_2026/D06) into a new script
## SAVE your work -> commit + push D06.R to your own econ377 repo (or upload it on github.com)
## ----------------------------------------------------------------

## ===== 1. Properties of expectation:  E[aX + bY] = a*E[X] + b*E[Y] =====
EX <- 2
EY <- 3
4*EX + 5*EY - 3     # E[4X + 5Y - 3] = 4(2) + 5(3) - 3 = 20

## ===== 2. Variance & standard deviation:  Var(X) = E[X^2] - E[X]^2 =====
x <- c(0, 1)          # the values           -- the two faces
p <- c(1/2, 1/2)      # their probabilities  -- must sum to 1
EX  <- sum(x * p)     # E[X]  = 0*(1/2) + 1*(1/2) = 0.5
EX2 <- sum(x^2 * p)   # E[X^2] = 0^2*(1/2) + 1^2*(1/2) = 0.5
VarX <- EX2 - EX^2    # Var(X) = 0.5 - 0.25 = 0.25
sdX  <- sqrt(VarX)    # sd(X) = sqrt(0.25) = 0.5

## ===== 3. Covariance:  Cov(X,Y) = E[XY] - E[X]*E[Y] =====
xj <- c(0, 0, 1, 1)         # x across pairs (0,0) (0,1) (1,0) (1,1)
yj <- c(0, 1, 0, 1)         # y across those same pairs
pj <- c(1/4, 1/4, 1/4, 1/4) # probability of each pair
EXY <- sum(xj * yj * pj)    # E[XY] = 0+0+0+1*(1/4) = 0.25
CovXY <- EXY - EX*EY        # Cov = 0.25 - 0.5*0.5 = 0  (independent!)

## ===== 4. Correlation:  Cor(X,Y) = Cov(X,Y) / ( sd(X) * sd(Y) ) =====
CovXY / (0.5 * 0.5)   # Cor(X,Y) = 0 / 0.25 = 0

## ================= YOUR TURN =========================
## X takes values 1, 2, 3 with probabilities 0.2, 0.5, 0.3.
x <- c(1, 2, 3); p <- c(0.2, 0.5, 0.3)
EX   <- sum(x * p)        # (a) E[X]
EX2  <- sum(x^2 * p)      # (b) E[X^2]
VarX <- EX2 - EX^2        # (c) Var(X) = E[X^2] - E[X]^2

