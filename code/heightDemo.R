#------------------------------------------------------------------------------
# Title: Basic Linear Model in R
# Author: William Murrah
# Description: Simulate heights to demontrate a simple model.
#------------------------------------------------------------------------------

mu_wm <- 64
sigma_wm <- 2
N <- 1e6

set.seed(1234)
pop_wm_ht <- rnorm(n = N, mean = mu_wm, sd = sigma_wm)

# Examine distribution of women's heights
hist(pop_wm_ht, breaks = "fd")
abline(v = mean(pop_wm_ht), col = "red")

mean(pop_wm_ht)
sd(pop_wm_ht)

set.seed(5678)
samp200 <- sample(x = pop_wm_ht, size = 200, replace = FALSE)
samp20 <- sample(x = pop_wm_ht, size = 20, replace = FALSE)

mean(samp200)
mean(samp20)
sd(samp200)
sd(samp20)

hist(samp200, breaks = "fd")
hist(samp20, breaks = "fd")


M <- mean(samp20)
S <- sd(samp20)
ht <- samp20


wmod0 <- lm(ht ~ 1)
summary(wmod0)

htdat <- data.frame(ht = ht)
htdat$resids <- htdat$ht - mean(htdat$ht)
htdat$predvals <- predict(wmod0)
htdat$obsvals <- htdat$predvals + htdat$resids

mu_mn <- 70
sigma_mn <- 3
set.seed(1234)
pop_mn_ht <- rnorm(n = N, mean = mu_mn, sd = sigma_mn)

ht <- c(pop_wm_ht, pop_mn_ht)
gender <- rep(c("female", "male"), each = N)

htdat <- data.frame(ht = ht, gender = factor(gender))

hist(htdat$ht, breaks = "fd")


sampht20 <- htdat[sample(x = nrow(htdat), 20), ]


mod0 <- lm(ht ~ 1, data = sampht20)
summary(mod0)
mod1 <- lm(ht ~ gender, data = sampht20)
summary(mod1)
