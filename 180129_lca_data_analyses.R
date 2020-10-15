# Data analyses screening data
# 2018-01-22
# hakan.wall@ki.se

library(dplyr)
library(MASS)
library(cbird)
library(foreign)
library(sandwich)
library(parallel)
library(cbird)
library(poLCA)
# Subtype using cbrid with 3 mixture components and no penalization
all_3_2_omitted <- cbird(as.matrix(all_data), 3, 2, mc.cores = 4)
all_3_3_omitted <- cbird(as.matrix(all_data), 3, 3, mc.cores = 4)
all_3_4_omitted <- cbird(as.matrix(all_data), 3, 4, mc.cores = 4)
all_3_5_omitted <- cbird(as.matrix(all_data), 3, 5, mc.cores = 4)
all_3_6_omitted <- cbird(as.matrix(all_data), 3, 6, mc.cores = 8)
all_3_7_omitted <- cbird(as.matrix(all_data), 3, 7, mc.cores = 4)
all_3_8_omitted <- cbird(as.matrix(all_data), 3, 8, mc.cores = 4)

#penal = 0.001
p1_3_3 <- cbird(as.matrix(all_data), 3, 3, mc.cores = 4, lambda = 0.001)
p1_3_4 <- cbird(as.matrix(all_data), 3, 4, mc.cores = 4, lambda = 0.001)
p1_3_5 <- cbird(as.matrix(all_data), 3, 5, mc.cores = 4, lambda = 0.001)
p1_3_6 <- cbird(as.matrix(all_data), 3, 6, mc.cores = 8, lambda = 0.001)
p1_3_7 <- cbird(as.matrix(all_data), 3, 7, mc.cores = 4, lambda = 0.001)
p1_3_8 <- cbird(as.matrix(all_data), 3, 8, mc.cores = 4, lambda = 0.001)
#penal = 0.005
p5_3_5 <- cbird(as.matrix(all_data), 3, 5, mc.cores = 4, lambda = 0.005)
p5_3_6 <- cbird(as.matrix(all_data), 3, 6, mc.cores = 8, lambda = 0.005)
p5_3_7 <- cbird(as.matrix(all_data), 3, 7, mc.cores = 4, lambda = 0.005)
p5_3_8 <- cbird(as.matrix(all_data), 3, 8, mc.cores = 4, lambda = 0.005)

# To ensure correct latent classes run poLCA ----------------
# poLCA demands entries > 0, add 1 to each obs in matrix. Run with 2 to 8 latent classes (clusters) ---------------------

d_ <- as.matrix(all_data) + 1

# to run poLCA, variables have to be defined as below ----------------
f <- cbind(casino_online,sportbetting_land,poker_online,sportbetting_online,poker_landbased,casino_landbased,bingo,egm,horsebetting,lottery_type) ~ 1

lca_2 <- poLCA(f,as.data.frame(d_),2, maxiter = 10000)
lca_3 <- poLCA(f,as.data.frame(d_),3, maxiter = 10000)
lca_4 <- poLCA(f,as.data.frame(d_),4, maxiter = 10000)
lca_5 <- poLCA(f,as.data.frame(d_),5, maxiter = 10000)
lca_6 <- poLCA(f,as.data.frame(d_),6, maxiter = 10000)
lca_7 <- poLCA(f,as.data.frame(d_),7, maxiter = 10000)
lca_8 <- poLCA(f,as.data.frame(d_),8, maxiter = 10000)
lca_9 <- poLCA(f,as.data.frame(d_),9, maxiter = 10000)
lca_10 <- poLCA(f,as.data.frame(d_),10, maxiter = 10000)


#Is done to create a cluster column in the analyses data set

sub_1_plus_analyses$cluster <- as.factor(all_3_7_omitted$cluster)

# View the distribution of each type of gambling based on cluster
aggregate(all_data,by=list(lca_8$predclass),FUN=mean)*100


bic_tibble <- tibble(cluster = c(3,4,5,6,7,8), value=c(all_3_3_omitted$bic,all_3_4_omitted$bic,all_3_5_omitted$bic,all_3_6_omitted$bic,all_3_7_omitted$bic, all_3_8_omitted$bic))
