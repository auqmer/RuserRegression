# Title: Model Gas Mileage 
# Author: William Murrah
# Description: Model to predict gas mileage using Motor Trends data

library(psych)

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

summary(modhp)
summary(modwthp)
