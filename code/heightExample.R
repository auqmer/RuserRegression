#************************************************************************
# Title: height example 
# Author: William Murrah
# Description: simulating and modeling height as an introduction to
#              regression modeling in R.
# Created: Friday, 25 June 2021
# R version: R version 4.1.0 (2021-05-18)
# Project(working) directory: /Users/wmm0017/Projects/QMER/RuserRegression
#************************************************************************
library(texreg)
# Let's assume that the average height of women in the U.S. is 64 inches
# with a standard deviation of 2 inches.

mu_wm <- 64
sigma_wm <- 2
mu_mn <- 70
sigma_mn <- 3

N <- 1e6

set.seed(1234)
pop_wm_ht <- rnorm(n = N, mean = mu_wm, sd = sigma_wm)
pop_mn_ht <- rnorm(n = N, mean = mu_mn, sd = sigma_mn)
mean(pop_wm_ht)
sd(pop_wm_ht)


hist(pop_wm_ht, breaks = "fd")
abline(v = mean(pop_wm_ht), col = "red")
samp200 <- sample(x = pop_wm_ht, size = 200, replace = FALSE)
hist(samp200, breaks = "fd")

samp20 <- sample(x = pop_wm_ht, size = 20, replace = FALSE)
hist(samp20)
mean(samp20)
sd(samp20)

samp8 <- sample(x = pop_wm_ht, size = 8, replace = FALSE)
hist(samp8)
mean(samp8)
sd(samp8)


mean(samp200)
sd(samp200)
wm_ht <- samp200
emptyModel <- lm(wm_ht ~ 1)
summary(emptyModel)

pop_devs <- pop_wm_ht - mean(pop_wm_ht)

hist(pop_wm_ht, breaks = "fd")
abline(v = mean(pop_wm_ht), col = "red")

hist(pop_devs, breaks = "fd")
abline(v = 0, col = "red")

sqrt(mean(pop_devs^2))
var(pop_wm_ht)
sd(pop_wm_ht)

hist(pop_mn_ht, breaks = "fd")


ht <- c(pop_wm_ht, pop_mn_ht)
gender <- rep(c("female", "male"), each = N)

htdat <- data.frame(ht = ht, gender = factor(gender))

hist(htdat$ht, breaks = "fd")


sampht200 <- htdat[sample(x = nrow(htdat), 200), ]
hist(samp200, breaks = "fd")

mod0 <- lm(ht ~ 1, data = htdat)
summary(mod0)
mod1 <- lm(ht ~ gender, data = htdat)
summary(mod1)

mod0 <- lm(ht ~ 1, data = sampht200)
summary(mod0)

mod1 <- lm(ht ~ gender, data = sampht200)
summary(mod1)

anova(mod0, mod1)

aov1 <- aov(ht ~ gender, data = sampht200)
summary(aov1)
anova(mod1)
