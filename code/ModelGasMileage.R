# Title: Model Gas Mileage 
# Author: William Murrah
# Description: Model to predict gas mileage using Motor Trends data

library(psych)
library(texreg)
data("mtcars")

# create categorical variables
mtcars$cyl <- factor(x = mtcars$cyl, labels = c("4_cyl", "6_cyl", "8_cyl"))
mtcars$vs <- factor(x = mtcars$vs, labels = c("V-shaped", "straight"))
mtcars$am <- factor(x = mtcars$am, labels = c("automatic", "manual"))

# subset analytic variables
cars <- subset(mtcars, select = c("mpg", "cyl", "hp", "wt", "vs", "am"))

pairs(cars)

pairs.panels(cars)

describe(cars)

mod0 <- lm(mpg ~ 1, data = cars)
summary(mod0)

modwt <- lm(mpg ~ wt, data = cars)
summary(modwt)

hist(cars$mpg)
abline(v = mean(mtcars$mpg), col = "red")

plot(resid(modwt) ~ predict(modwt))
abline(h=0)

anova(mod0, modwt)

modhp <- lm(mpg ~ hp, data = cars)
modwthp <- lm(mpg ~ wt + hp, data = cars)
modwthp_2 <- lm(mpg ~ poly(wt, 2,raw=TRUE)+ hp, data = cars)
summary(modhp)
summary(modwthp)
summary(modwthp_2)
screenreg(list(mod0, modwt, modhp, modwthp))

plot(resid(modwthp) ~ predict(modwthp))
abline(h = 0)

plot(resid(modwthp) ~ wt, data = cars)
abline(h=0)
plot(resid(modwthp) ~ hp, data = cars)
abline(h=0)
plot(resid(modwthp_2) ~ predict(modwthp_2))
abline(h=0)

hist(resid(modwthp))
hist(resid(modwthp_2))

modam <- lm(mpg ~ am, cars)
summary(modam)

modcyl <- lm(mpg ~ cyl, data = cars)
summary(modcyl)

modwha <- lm(mpg ~ poly(wt,2,raw=TRUE) + hp + am, data = cars)
summary(modwha)
