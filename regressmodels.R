# Simple regression models example

# linear regresion
data(mtcars)
n <- length(mtcars$mpg)
alpha <- 0.05
limod <- lm(mpg ~ am, data = mtcars)
coef(summary(limod))

pe <- coef(summary(limod))["am", "Estimate"]
se <- coef(summary(limod))["am", "Std. Error"]
tstat <- qt(1 - alpha/2, n - 2)  # n - 2 for model with intercept and slope
pe + c(-1, 1) * (se * tstat)

# multiple regression
bestfit <- lm(mpg ~ wt + qsec + am, data = mtcars)
coef(summary(bestfit))

pe <- coef(summary(bestfit))["am", "Estimate"]
se <- coef(summary(bestfit))["am", "Std. Error"]
tstat <- qt(1 - alpha/2, n - 2)  # n - 2 for model with intercept and slope
pe + c(-1, 1) * (se * tstat)

# Nested model
fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- update(fit1, mpg ~ wt + qsec)
fit3 <- update(fit2, mpg ~ wt + qsec + am)
anova(fit1, fit2, fit3)

mtcars_vars <- mtcars[, c(1, 6, 7, 9)]
mar.orig <- par()$mar  # save the original values 
par(mar = c(1, 1, 1, 1))  # set your new values 
pairs(mtcars_vars, panel = panel.smooth, col = 9 + mtcars$wt)

par(mar = mar.orig)  # put the original values back 
cor(mtcars_vars)

library(ggplot2)
library(gridExtra)
mpg_dist <- qplot(mtcars_vars$mpg, fill = I("red"))
wt_dist <- qplot(mtcars_vars$wt, fill = I("lightblue"))
qsec_dist <- qplot(mtcars_vars$qsec, fill = I("purple"))
am_dist <- qplot(mtcars_vars$am, fill = I("green"))
grid.arrange(mpg_dist, wt_dist, qsec_dist, am_dist, ncol = 2)
