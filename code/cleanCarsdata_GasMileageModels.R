#************************************************************************
# Title: Clean Cars data for analysis
# Author: William Murrah
# Description: Create analytic data set for Gas Mileage Models
# Created: Friday, 23 July 2021
# R version: R version 4.1.0 (2021-05-18)
# Project(working) directory: /Users/wmm0017/Projects/QMER/RuserRegression
#************************************************************************

library(psych)
library(texreg)
data("mtcars")

# create categorical variables
mtcars$cyl <- factor(x = mtcars$cyl, labels = c("4_cyl", "6_cyl", "8_cyl"))
mtcars$vs <- factor(x = mtcars$vs, labels = c("V-shaped", "straight"))
mtcars$am <- factor(x = mtcars$am, labels = c("automatic", "manual"))

# subset analytic variables
cars <- subset(mtcars, select = c("mpg", "cyl", "hp", "wt", "vs", "am"))

rm(mtcars)
