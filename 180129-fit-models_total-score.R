# Fit screening data to regression models 
# total PGSI points is regressed against the predictors: latent class, gender and age group
# several different models are fitted and evaluated 

library(dplyr)
library(tidyr)
library(MASS)
library(foreign)


# To set cluster 5 as reference cluster for the regression analyses


sub_1_plus_analyses <- data_omitted_dupl

sub_1_plus_analyses <- sub_1_plus_analyses %>% filter(na.omit(total))

levels(sub_1_plus_analyses$Age) <- c(levels(sub_1_plus_analyses$Age),"No Answer")
sub_1_plus_analyses$Age[is.na(sub_1_plus_analyses$Age)] <- "No Answer"
sub_1_plus_analyses$Age[sub_1_plus_analyses$Age == "Gotlands län"] <- "No Answer"


sub_1_plus_analyses <- sub_1_plus_analyses %>% filter(Age != "No Answer")



#
fit.p.1 <- glm(total~cluster, data = sub_1_plus_analyses, family = "poisson")
fit.p.2 <- glm(total~cluster + Gender, data = sub_1_plus_analyses, family = "poisson")
fit.p.3 <- glm(total~cluster + Gender + Age, data = sub_1_plus_analyses, family = "poisson")
fit.p.4 <- glm(total~cluster*Gender + Age, data = sub_1_plus_analyses, family = "poisson")

summary(fit.p.1)
# Fit with quasi-poisson since there is overdipersion in the model

# unadjusted

fit.quasi_cluster <- glm(total~cluster, data = sub_1_plus_analyses, family =quasipoisson)

summary(fit.quasi_cluster)

# Get coef
coef_quasi_cluster <- exp(fit.quasi_cluster$coefficients);coef_quasi_cluster
# Calculate 95 % CI
CI_quasi_cluster <- exp(confint.default(fit.quasi_cluster));CI_quasi_cluster

round(coef_quasi_cluster[1]*coef_quasi_cluster[7],1)
round(coef_quasi_cluster[1]*CI_quasi_cluster[7,2],1)


# adjusted for age

fit.quasi_cluster.adj <- glm(total~cluster + Age, data = sub_1_plus_analyses, family =quasipoisson)

summary(fit.quasi_cluster.adj)
# Get coef
coef_quasi_cluster.adj <- exp(fit.quasi_cluster.adj$coefficients);coef_quasi_cluster.adj
  # Calculate 95 % CI
CI_quasi_cluster.adj <- exp(confint.default(fit.quasi_cluster.adj));CI_quasi_cluster.adj

round(coef_quasi_cluster.adj[1]*coef_quasi_cluster.adj[7],1)
round(coef_quasi_cluster.adj[1]*CI_quasi_cluster.adj[7,2],1)


# Add gender as an interaction
#
#fit.quasi <- glm(total~cluster*Gender, data = sub_1_plus_analyses, family =quasipoisson)

#summary(fit.quasi)
# Get coef
#coef_quasi <- exp(fit.quasi$coefficients);coef_quasi
# 95 % CI
#CI_quasi <- exp(confint.default(fit.quasi));CI_quasi

# Get coefs and 95% CIs 

#cluster 1, males
# coef
#round(coef_quasi[1]*coef_quasi[8],1)

#CI 
#round(coef_quasi[1]*CI_quasi[8,1],1)
#round(coef_quasi[1]*CI_quasi[8,2],1)

#cluster 2, females
#round(coef_quasi[1]*coef_quasi[2],1)

#CI 
#round(coef_quasi[1]*CI_quasi[2,1],1)
#round(coef_quasi[1]*CI_quasi[2,2],1)

#cluster 2, males
#round(coef_quasi[1]*coef_quasi[2]*coef_quasi[9]*coef_quasi[8],1)

#CI 
#round(coef_quasi[1]*coef_quasi[2]*CI_quasi[9,1]*coef_quasi[8],1)
#round(coef_quasi[1]*coef_quasi[2]*CI_quasi[9,2]*coef_quasi[8],1)

#cluster 3, females
#round(coef_quasi[1]*coef_quasi[3],1)

#CI 
#round(coef_quasi[1]*CI_quasi[3,1],1)
#round(coef_quasi[1]*CI_quasi[3,2],1)

#cluster 3, males
#round(coef_quasi[1]*coef_quasi[3]*coef_quasi[10]*coef_quasi[8],1)

#CI 
#round(coef_quasi[1]*coef_quasi[3]*CI_quasi[10,1]*coef_quasi[8],1)
#round(coef_quasi[1]*coef_quasi[3]*CI_quasi[10,2]*coef_quasi[8],1)

#cluster 4, females
#round(coef_quasi[1]*coef_quasi[4],1)

#CI 
#round(coef_quasi[1]*CI_quasi[4,1],1)
#round(coef_quasi[1]*CI_quasi[4,2],1)

#cluster 4, males
#round(coef_quasi[1]*coef_quasi[4]*coef_quasi[11]*coef_quasi[8],1)

#CI 
#round(coef_quasi[1]*coef_quasi[4]*CI_quasi[11,1]*coef_quasi[8],1)
#round(coef_quasi[1]*coef_quasi[4]*CI_quasi[11,2]*coef_quasi[8],1)

#cluster 5, females
#round(coef_quasi[1]*coef_quasi[5],1)

#CI 
#round(coef_quasi[1]*CI_quasi[5,1],1)
#round(coef_quasi[1]*CI_quasi[5,2],1)

#cluster 5, males
#round(coef_quasi[1]*coef_quasi[5]*coef_quasi[12]*coef_quasi[8],1)

#CI 
#round(coef_quasi[1]*coef_quasi[5]*CI_quasi[12,1]*coef_quasi[8],1)
#round(coef_quasi[1]*coef_quasi[5]*CI_quasi[12,2]*coef_quasi[8],1)

#cluster 6, females
#round(coef_quasi[1]*coef_quasi[6],1)

#CI 
#round(coef_quasi[1]*CI_quasi[6,1],1)
#round(coef_quasi[1]*CI_quasi[6,2],1)

#cluster 6, males
#round(coef_quasi[1]*coef_quasi[6]*coef_quasi[13]*coef_quasi[8],1)

#CI 
#round(coef_quasi[1]*coef_quasi[6]*CI_quasi[13,1]*coef_quasi[8],1)
#round(coef_quasi[1]*coef_quasi[6]*CI_quasi[13,2]*coef_quasi[8],1)

#cluster 7, females
#round(coef_quasi[1]*coef_quasi[7],1)

#CI 
#round(coef_quasi[1]*CI_quasi[7,1],1)
#round(coef_quasi[1]*CI_quasi[7,2],1)

#cluster 7, males
#round(coef_quasi[1]*coef_quasi[7]*coef_quasi[14]*coef_quasi[8],1)

#CI 
#round(coef_quasi[1]*coef_quasi[7]*CI_quasi[14,1]*coef_quasi[8],1)
#round(coef_quasi[1]*coef_quasi[7]*CI_quasi[14,2]*coef_quasi[8],1)



# Adjust for age

#fit.quasi.adj <- glm(total~cluster*Gender + Age, data = sub_1_plus_analyses, family =quasipoisson)

#summary(fit.quasi.adj)
# Get coef
#coef_quasi.adj <- exp(fit.quasi.adj$coefficients);coef_quasi.adj
# Calculate 95 % CI
#CI_quasi.adj <- exp(confint.default(fit.quasi.adj));CI_quasi.adj

# Get coefs and 95% CIs

#Online casino gamblers, males
#coef
round(coef_quasi.adj[1]*coef_quasi.adj[8],1)
#CI
round(coef_quasi.adj[1]*CI_quasi.adj[8,1],1)
round(coef_quasi.adj[1]*CI_quasi.adj[8,2],1)

#Cluster 2, females
coef_quasi.adj[1]*coef_quasi.adj[2]
#CI
coef_quasi.adj[1]*CI_quasi.adj[2,1]
coef_quasi.adj[1]*CI_quasi.adj[2,2]

#Cluster 2, males
coef_quasi.adj[1]*coef_quasi.adj[2]*coef_quasi.adj[17]*coef_quasi.adj[8]
#CI
coef_quasi.adj[1]*coef_quasi.adj[2]*CI_quasi.adj[17,1]*coef_quasi.adj[8]
coef_quasi.adj[1]*coef_quasi.adj[2]*CI_quasi.adj[17,2]*coef_quasi.adj[8]


#Cluster 3, females
coef_quasi.adj[1]*coef_quasi.adj[3]
#CI
coef_quasi.adj[1]*CI_quasi.adj[3,1]
coef_quasi.adj[1]*CI_quasi.adj[3,2]

#Cluster 3, males
coef_quasi.adj[1]*coef_quasi.adj[3]*coef_quasi.adj[18]*coef_quasi.adj[8]
#CI
coef_quasi.adj[1]*coef_quasi.adj[3]*CI_quasi.adj[18,1]*coef_quasi.adj[8]
coef_quasi.adj[1]*coef_quasi.adj[3]*CI_quasi.adj[18,2]*coef_quasi.adj[8]

#Cluster 4, females
coef_quasi.adj[1]*coef_quasi.adj[4]
#CI
coef_quasi.adj[1]*CI_quasi.adj[4,1]
coef_quasi.adj[1]*CI_quasi.adj[4,2]

#Cluster 4, males
coef_quasi.adj[1]*coef_quasi.adj[4]*coef_quasi.adj[19]*coef_quasi.adj[8]
#CI
coef_quasi.adj[1]*coef_quasi.adj[4]*CI_quasi.adj[19,1]*coef_quasi.adj[8]
coef_quasi.adj[1]*coef_quasi.adj[4]*CI_quasi.adj[19,2]*coef_quasi.adj[8]

#Cluster 5, females
coef_quasi.adj[1]*coef_quasi.adj[5]
#CI
coef_quasi.adj[1]*CI_quasi.adj[5,1]
coef_quasi.adj[1]*CI_quasi.adj[5,2]

#Cluster 5, males
coef_quasi.adj[1]*coef_quasi.adj[5]*coef_quasi.adj[20]*coef_quasi.adj[8]
#CI
coef_quasi.adj[1]*coef_quasi.adj[5]*CI_quasi.adj[20,1]*coef_quasi.adj[8]
coef_quasi.adj[1]*coef_quasi.adj[5]*CI_quasi.adj[20,2]*coef_quasi.adj[8]

#Cluster 6, females
coef_quasi.adj[1]*coef_quasi.adj[6]
#CI
coef_quasi.adj[1]*CI_quasi.adj[6,1]
coef_quasi.adj[1]*CI_quasi.adj[6,2]

#Cluster 6, males
coef_quasi.adj[1]*coef_quasi.adj[6]*coef_quasi.adj[21]*coef_quasi.adj[8]
#CI
coef_quasi.adj[1]*coef_quasi.adj[6]*CI_quasi.adj[21,1]*coef_quasi.adj[8]
coef_quasi.adj[1]*coef_quasi.adj[6]*CI_quasi.adj[21,2]*coef_quasi.adj[8]

#Cluster 7, females
coef_quasi.adj[1]*coef_quasi.adj[7]
#CI
coef_quasi.adj[1]*CI_quasi.adj[7,1]
coef_quasi.adj[1]*CI_quasi.adj[7,2]

#Cluster 6, males
coef_quasi.adj[1]*coef_quasi.adj[7]*coef_quasi.adj[22]*coef_quasi.adj[8]
#CI
coef_quasi.adj[1]*coef_quasi.adj[7]*CI_quasi.adj[22,1]*coef_quasi.adj[8]
coef_quasi.adj[1]*coef_quasi.adj[7]*CI_quasi.adj[22,2]*coef_quasi.adj[8]


# Test involvement effect by type of gambling, i.e. subset those participating in a certain type of gambling and explore effect on PGSI by adding an
# extra type of gambling as a chategorical variable.

#Casino online
casino_online_sub <- sub_1_plus_analyses %>% filter(casino_online == 1) %>% mutate(num_games = factor(if_else(n >= 7,7,n)))

#Regress num_games against PGSI-score (adjusing for involvement)

fit.quasi.co <- glm(total~num_games, data = casino_online_sub, family =quasipoisson)

summary(fit.quasi.co)

# Get coef
coef_quasi.co <- exp(fit.quasi.co$coefficients);coef_quasi.co
# Calculate 95 % CI
CI_quasi.co <- exp(confint.default(fit.quasi.co));CI_quasi.co


#EGM
egm_sub <- sub_1_plus_analyses %>% filter(egm == 1) %>% mutate(num_games = factor(if_else(n >= 7,7,n)))

#Regress num_games against PGSI-score (adjusing for involvement)

fit.quasi.egm <- glm(total~num_games, data = egm_sub, family =quasipoisson)

summary(fit.quasi.egm)

# Get coef
coef_quasi.egm <- exp(fit.quasi.egm$coefficients);coef_quasi.egm
# Calculate 95 % CI
CI_quasi.egm <- exp(confint.default(fit.quasi.egm));CI_quasi.egm


#Horses
horsebetting_sub <- sub_1_plus_analyses %>% filter(horsebetting == 1) %>% mutate(num_games = factor(if_else(n >= 7,7,n)))
#Regress num_games against PGSI-score (adjusing for involvement)

fit.quasi.horse <- glm(total~num_games, data = horsebetting_sub, family =quasipoisson)

summary(fit.quasi.horse)

# Get coef
coef_quasi.horse <- exp(fit.quasi.horse$coefficients);coef_quasi.horse
# Calculate 95 % CI
CI_quasi.horse <- exp(confint.default(fit.quasi.horse));CI_quasi.horse


#Online sportsbetting
sportsbetting_online_sub <- sub_1_plus_analyses %>% filter(sportbetting_online == 1)  %>% mutate(num_games = factor(if_else(n >= 7,7,n)))
#Regress num_games against PGSI-score (adjusing for involvement)

fit.quasi.sportsbetting_online <- glm(total~num_games, data = sportsbetting_online_sub, family =quasipoisson)

summary(fit.quasi.sportsbetting_online)

# Get coef
coef_quasi.sportsbetting_online <- exp(fit.quasi.sportsbetting_online$coefficients);coef_quasi.sportsbetting_online
# Calculate 95 % CI
CI_quasi.sportsbetting_online <- exp(confint.default(fit.quasi.sportsbetting_online));CI_quasi.sportsbetting_online


#lotteries
lotteries_sub <- sub_1_plus_analyses %>% filter(lottery_type == 1) %>% mutate(num_games = factor(if_else(n >= 7,7,n)))

#Regress num_games against PGSI-score (adjusing for involvement)

fit.quasi.lotteries <- glm(total~num_games, data = lotteries_sub, family =quasipoisson)

summary(fit.quasi.lotteries)

# Get coef
coef_quasi.lotteries <- exp(fit.quasi.lotteries$coefficients);coef_quasi.lotteries
# Calculate 95 % CI
CI_quasi.lotteries <- exp(confint.default(fit.quasi.lotteries));CI_quasi.lotteries


#Landbased sportsbetting
#sportsbetting_land_sub <- sub_1_plus_analyses %>% filter(sportbetting_land == 1) %>% mutate(lottery_type = if_else(lotteries == 1 | keno_type == 1, 1, 0)) %>% select(-lotteries, -keno_type, - sportbetting_land, -cluster, -num_games) %>% 
#  mutate(num_games = rowSums(.[17:25])+1) %>% mutate(num_games = factor(if_else(num_games >= 7,7,num_games)))

sportsbetting_land_sub <- sub_1_plus_analyses %>% filter(sportbetting_land == 1)  %>% mutate(num_games = factor(if_else(n >= 7,7,n)))


#Regress num_games against PGSI-score (adjusing for involvement)

fit.quasi.sportsbetting_land <- glm(total~num_games, data = sportsbetting_land_sub, family =quasipoisson)

summary(fit.quasi.sportsbetting_land)

# Get coef
coef_quasi.sportsbetting_land <- exp(fit.quasi.sportsbetting_land$coefficients);coef_quasi.sportsbetting_land
# Calculate 95 % CI
CI_quasi.sportsbetting_land <- exp(confint.default(fit.quasi.sportsbetting_land));CI_quasi.sportsbetting_land

#Landbased poker
poker_land_sub <- sub_1_plus_analyses %>% filter(poker_landbased == 1) %>% mutate(num_games = factor(if_else(n >= 7,7,n)))

#Regress num_games against PGSI-score (adjusing for involvement)

fit.quasi.poker_land <- glm(total~num_games, data = poker_land_sub, family =quasipoisson)

summary(fit.quasi.poker_land)

coef_quasi.poker_land <- exp(fit.quasi.poker_land$coefficients);coef_quasi.poker_land
# Calculate 95 % CI
CI_quasi.poker_land <- exp(confint.default(fit.quasi.poker_land));CI_quasi.poker_land

# Poker online
poker_online_sub <- sub_1_plus_analyses %>% filter(poker_online == 1) %>% mutate(num_games = factor(if_else(n >= 7,7,n)))

#Regress num_games against PGSI-score (adjusing for involvement)

fit.quasi.poker_online <- glm(total~num_games, data = poker_online_sub, family =quasipoisson)

summary(fit.quasi.poker_online)

# Get coef
coef_quasi.poker_online <- exp(fit.quasi.poker_online$coefficients);coef_quasi.poker_online
# Calculate 95 % CI
CI_quasi.online <- exp(confint.default(fit.quasi.poker_online));CI_quasi.online

#Landbased casino (CC)
casino_land_sub <- sub_1_plus_analyses %>% filter(casino_landbased == 1) %>% mutate(num_games = factor(if_else(n >= 7,7,n)))
#Regress num_games against PGSI-score (adjusing for involvement)

fit.quasi.casino_land <- glm(total~num_games, data = casino_land_sub, family =quasipoisson)

summary(fit.quasi.casino_land)

# Get coef
coef_quasi.casino_land <- exp(fit.quasi.casino_land$coefficients);coef_quasi.casino_land
# Calculate 95 % CI
CI_quasi.casino_land <- exp(confint.default(fit.quasi.casino_land));CI_quasi.casino_land

# Bingo
bingo_sub <- sub_1_plus_analyses %>% filter(bingo == 1)  %>% mutate(num_games = factor(if_else(n >= 7,7,n)))

#Regress num_games against PGSI-score (adjusing for involvement)

fit.quasi.bingo<- glm(total~num_games, data = bingo_sub, family =quasipoisson)

summary(fit.quasi.bingo)

# Get coef
coef_quasi.bingo <- exp(fit.quasi.bingo$coefficients);coef_quasi.bingo
# Calculate 95 % CI
CI_quasi.bingo <- exp(confint.default(fit.quasi.bingo));CI_quasi.bingo


# The average effect
total <- sub_1_plus_analyses %>% mutate(num_games = factor(if_else(n >= 7,7,n)))
fit.model_total <- glm(total~num_games, data = total, family =quasipoisson)
summary(fit.model_total)

# Get coef
coef_quasi.total <- exp(fit.model_total$coefficients);coef_quasi.total
# Calculate 95 % CI
CI_quasi.total <- exp(confint.default(fit.model_total));CI_quasi.total



#test all types of gambling in a Poisson regression model 
tnp_analyses <- all_data
tnp_analyses$total <- sub_1_plus_analyses$total
tnp_analyses$gender <- sub_1_plus_analyses$Gender

model_all_types <- glm(total~., data=tnp_analyses, family = "poisson")

summary(model_all_types)

exp(model_all_types$coefficients)

# Fit with quasi-poisson since there is overdipersion in the model

model_all_types <- glm(total~., data=tnp_analyses, family = quasipoisson)

summary(model_all_types)

exp(model_all_types$coefficients)

exp(confint.default(model_all_types))

# Fit with each type of game * gender separately while controlling for the number of other types of games played 

#subset data for the analyses
#tmp_data_n <- dplyr::select(sub_1_plus_analyses, total, Gender, casino_online:horsebetting,n)
tmp_data_n <- dplyr::select(tmp_data_omitted, total, Gender, casino_online:horsebetting,n)
# Online casino games
model_casino_online <- glm(total~casino_online*Gender + (n - casino_online), data=tmp_data_n, family = quasipoisson)
summary(model_casino_online)
exp(model_casino_online$coefficients)

# Landbased sportsbetting
model_sportbetting_land <- glm(total~sportbetting_land + (n - sportbetting_land), data=tmp_data_n, family = quasipoisson)
summary(model_sportbetting_land)
exp(model_sportbetting_land$coefficients)

# Keno-type games
model_keno_type <- glm(total~keno_type + (n - keno_type), data=tmp_data_n, family = quasipoisson)
summary(model_keno_type)
exp(model_keno_type$coefficients)

# Online poker games
model_poker_online <- glm(total~poker_online + (n - poker_online), data=tmp_data_n, family = quasipoisson)
summary(model_poker_online)
exp(model_poker_online$coefficients)

# Online sportsbetting
model_sportbetting_online <- glm(total~sportbetting_online + (n - sportbetting_online), data=tmp_data_n, family = quasipoisson)
summary(model_sportbetting_online)
exp(model_sportbetting_online$coefficients)

# Lotteries
model_lotteries <- glm(total~lotteries + (n - lotteries), data=tmp_data_n, family = quasipoisson)
summary(model_lotteries)
exp(model_lotteries$coefficients)

# Landbased poker
model_poker_landbased <- glm(total~poker_landbased + (n - poker_landbased), data=tmp_data_n, family = quasipoisson)
summary(model_poker_landbased)
exp(model_poker_landbased$coefficients)

# Landbased casino games
model_casino_landbased <- glm(total~casino_landbased + (n - casino_landbased), data=tmp_data_n, family = quasipoisson)
summary(model_casino_landbased)
exp(model_casino_landbased$coefficients)

# Bingo
model_bingo <- glm(total~bingo + (n - bingo), data=tmp_data_n, family = quasipoisson)
summary(model_bingo)
exp(model_bingo$coefficients)

# EGMs
model_egm <- glm(total~egm + (n - egm), data=tmp_data_n, family = quasipoisson)
summary(model_egm)
exp(model_egm$coefficients)

# Horesebetting
model_horsebetting <- glm(total~horsebetting + (n - horsebetting), data=tmp_data_n, family = quasipoisson)
summary(model_horsebetting)
exp(model_horsebetting$coefficients)





# Subset those gambling on online casino games and test the effect of adding other games

sub_online_casino <- sub_1_plus_analyses %>%
  filter(casino_online == 1) %>%
    dplyr::select(total, sportbetting_land:horsebetting)

model_casino_online_only <- glm(total~., data=sub_online_casino, family = quasipoisson)
summary(model_casino_online_only)
exp(model_casino_online_only$coefficients)

# Subset those NOT gambling on online casino games and test the effect of adding other games

sub_not_online_casino <- sub_1_plus_analyses %>%
  filter(casino_online == 0) %>%
  dplyr::select(total, sportbetting_land:horsebetting)

model_not_casino_online <- glm(total~., data=sub_not_online_casino, family = quasipoisson)
summary(model_not_casino_online)
exp(model_not_casino_online$coefficients)


# Subset horsebettors

sub_horsebettors <- sub_1_plus_analyses %>%
  filter(horsebetting == 1) %>%
  dplyr::select(total, Gender, casino_online:egm)

model_horsebettors <- glm(total~., data=sub_horsebettors, family = quasipoisson)
summary(model_horsebettors)
exp(model_horsebettors$coefficients)


# Subset online sportsbettors

sub_online_sportsbetting <- sub_1_plus_analyses %>%
  filter(sportbetting_online == 1) %>%
  dplyr::select(total, Gender, casino_online:poker_online,lotteries:horsebetting)

model_online_sportsbetting <- glm(total~., data=sub_online_sportsbetting, family = quasipoisson)
summary(model_online_sportsbetting)
exp(model_online_sportsbetting$coefficients)



