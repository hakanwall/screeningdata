# Fit screening data to regression models 
# total PGSI points is regressed against the predictors: latent class, gender and age group
# several different models are fitted and evaluated 

library(dplyr)
library(tidyr)
library(MASS)
library(foreign)
library(sandwich)

# To set cluster 3 as reference cluster for the regression analyses

sub_1_plus_analyses$cluster <- relevel(sub_1_plus_analyses$cluster, c("3"))


# Fit different poisson regression models

fit.p.1 <- glm(total~cluster, data = sub_1_plus_analyses, family = "poisson")
fit.p.2 <- glm(total~cluster + Gender, data = sub_1_plus_analyses, family = "poisson")
fit.p.3 <- glm(total~cluster + Gender + Age, data = sub_1_plus_analyses, family = "poisson")
fit.p.4 <- glm(total~cluster*Gender + Age, data = sub_1_plus_analyses, family = "poisson")


# Fit with quasi-poisson since there is overdipersion in the model

fit.quasi <- glm(total~cluster*Gender + Age, data = sub_1_plus_analyses, family =quasipoisson)

summary(fit.quasi)
# Get coef
coef_quasi <- exp(fit.quasi$coefficients)
# Calculate 95 % CI
CI_quasi <- exp(confint.default(fit.quasi))


# calculate robust standard errors for model fit.p due to an over-dispersed error term

#cov.m1 <- vcovHC(fit.p.4, type = "HC0")

#std.err <- sqrt(diag(cov.m1))

#q.val <- qnorm(0.975)

#r.est.p <- cbind(
#  Estimate = exp(coef(fit.p.4))
#  , "Robust SE" = std.err
#  , z = coef(fit.p.4)/std.err
#  , "Pr(>|z|) "= 2 * pnorm(abs(coef(fit.p.4)/std.err), lower.tail = FALSE)
#  , LL = exp(coef(fit.p.4) - q.val  * std.err)
#  , UL = exp(coef(fit.p.4) + q.val  * std.err)
#)

#r.est.p


