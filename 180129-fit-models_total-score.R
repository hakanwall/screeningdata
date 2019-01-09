# Fit screening data to regression models 
# total PGSI points is regressed against the predictors: latent class, gender and age group
# several different models are fitted and evaluated 

library(dplyr)
library(tidyr)
library(MASS)
library(foreign)


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

#test all types of gambling in a Poisson regression model 

tmp_data <- dplyr::select(sub_1_plus_analyses, total, casino_online:horsebetting)

model_all_types <- glm(total~., data=tmp_data, family = "poisson")

summary(model_all_types)

exp(model_all_types$coefficients)

# Fit with quasi-poisson since there is overdipersion in the model

model_all_types <- glm(total~., data=tmp_data, family = quasipoisson)

summary(model_all_types)

exp(model_all_types$coefficients)


