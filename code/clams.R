#************************************************************************
# Title: Clams lm example: assumptions
# Author: William Murrah
# Description: demonstration of assumption violation from Zuur et al 2009
# Created: Friday, 09 July 2021
# R version: R version 4.1.0 (2021-05-18)
# Project(working) directory: /Users/wmm0017/Projects/QMER/RuserRegression
#************************************************************************
library(psych)
library(ggplot2)
path <- paste0("~/Projects/Courses/ERMA_Multilevel_Modeling/",
                    "data/ZuurDataMixedModelling/Clams.txt")

clams <- read.table(path, header = TRUE)[ ,1:3]
rm(path)

# Make lowercase names for easier typing
names(clams) <- tolower(names(clams))

# Explore data
pairs(clams)

clams$lnlength <- log(clams$length)
clams$lnafd <- log(clams$afd)
clams$month <- factor(clams$month,
                      levels = c(9, 11, 12, 2, 3, 4),
                      labels = c("Sept.", "Nov.", "Dec.",
                                 "Feb.", "March", "April"))

pairs.panels(clams)

coplot(lnafd ~ lnlength | month, data = clams)

ggplot(clams, aes(x = lnlength, y = lnafd)) + geom_point() +
  facet_wrap(~ month)

describe(clams)

fullmodel <- lm(lnafd ~ lnlength*month, data = clams)

summary(fullmodel)
anova(fullmodel)

drop1(fullmodel, test = "F")

plot(resid(fullmodel) ~ predict(fullmodel))
abline(h = 0, col = "red")
hist(resid(fullmodel))

plot(resid(fullmodel) ~ clams$month)
par()
