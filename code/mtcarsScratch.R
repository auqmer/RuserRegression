library(psych)
data(mtcars)
str(mtcars)

mtcars <- transform(mtcars,
                    cyl = factor(cyl, labels = c("4cyl", "6cyl", "8cyl")),
                    vs = factor(vs, labels = c("V-shaped", "straight")),
                    am = factor(am, labels =c("automatic", "manual")))

cars <- mtcars[ , c("mpg", "cyl", "hp", "wt", "vs", "am")]
pairs(cars)

pairs.panels(cars)

describe(cars)

mod0 <- lm(mpg ~ 1, data = cars)
summary(mod0)
modwt <- lm(mpg ~ wt, cars)
summary(modwt)
modwt2 <- lm(mpg ~ poly(wt, 2, raw=TRUE), cars)
anova(modwt, modwt2)
modwt3 <- lm(mpg ~ poly(wt, 3, raw=TRUE), cars)
anova(modwt2, modwt3)
plot(modwt)
plot(modwt2)
hist(resid(modwt))
hist(resid(modwt2))

modwt2cyl <- lm(mpg ~ poly(wt,2,raw=TRUE) + cyl, data = cars)
summary(modwtcyl)
modwtcyl <- lm(mpg ~ wt + cyl, data = cars)
anova(modwt2cyl, modwtcyl)

plot(rstudent(modwt2cyl) ~ predict(modwt2cyl))
abline(h = 0)


modwt_cyl <- lm(mpg ~ wt*cyl, data = cars)
summary(modwt_cyl)
anova(modwt_cyl)

plot(rstudent(modwt_cyl) ~ predict(modwt_cyl))
abline(h=0)
