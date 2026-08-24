## >>> GET & SAVE (details in the R guide) -------------------------
## GET, git mode  -> run in the Console:
##   download.file("https://raw.githubusercontent.com/isaaccloh/ECON377/main/377_2026/D02/D02_starter.R", "D02.R")
## GET, easy mode -> copy this file from github.com/isaaccloh/ECON377 (377_2026/D02) into a new script
## SAVE your work -> commit + push D02.R to your own econ377 repo (or upload it on github.com)
## ----------------------------------------------------------------

## ECN 377 - Day 2  |  first R session: the mean, real data, a first line.

## (1) The mean, from our own numbers  -- we fill this in together
## step 1: put our class poll numbers in a vector -- use c()
poll <- c(0,0,0,5)
## step 2: add them all up -- use sum()
sum(poll)
## step 3: how many are there? (n) -- use length()
length(poll)
## step 4: the mean = sum / length ... or just mean()
mean(poll)

## (2) Real data: dataset$variable
## R convention: dataset$variable picks ONE variable (column) out of a dataset --
##   e.g. wage1$wage = the "wage" column of wage1, wage1$educ = the "educ" column.
install.packages("wooldridge")

library(wooldridge); data("wage1")   # 526 workers, 1976
summary(wage1)                        # snapshot of every variable
mean(wage1$wage)                          # TODO: fill in wage1$wage       -- center
sd(wage1$wage)                        # spread
hist(wage1$wage)                      # shape
cor(wage1$educ, wage1$wage)           # do they move together? (not causation!)

## ================= PROBLEMS (your turn) =========================
mean_wage <- ______           # (a) the mean wage        (hint: mean(wage1$wage))
## (b) Is wage1 cross-section, time-series, or panel?   ANSWER (comment):