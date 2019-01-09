# Data analyses screening data
# 2018-01-22
# hakan.wall@ki.se

library(dplyr)
library(MASS)
library(cbird)
library(foreign)
library(sandwich)
library(parallel)

# Subtype using cbrid with 3 mixture components and no penalization

all_2_3 <- cbird(as.matrix(all_data), 3, 2, mc.cores = 2)
all_3_3 <- cbird(as.matrix(all_data), 3, 3, mc.cores = 2)
all_3_5 <- cbird(as.matrix(all_data), 3, 5, mc.cores = 2)
all_3_4 <- cbird(as.matrix(all_data), 3, 4, mc.cores = 2)
all_3_6 <- cbird(as.matrix(all_data), 3, 6, mc.cores = 2)
all_3_7 <- cbird(as.matrix(all_data), 3, 7, mc.cores = 2)
all_3_8 <- cbird(as.matrix(all_data), 3, 8, mc.cores = 2)



# To ensure correct latent classes run poLCA ----------------
# poLCA demands entries > 0, add 1 to each obs in matrix. Run with 2 to 8 latent classes (clusters) ---------------------

#d_ <- as.matrix(all_data) + 1

# to run poLCA, variables have to be defined as below ----------------
#f <- cbind(casino_online,sportbetting_land,sportbetting_online,casino_landbased,bingo,egm,horsebetting,poker,lottery_type) ~ 1

#lca_2 <- poLCA(f,as.data.frame(d_),2, maxiter = 10000)
#lca_3 <- poLCA(f,as.data.frame(d_),3, maxiter = 10000)
#lca_4 <- poLCA(f,as.data.frame(d_),4, maxiter = 10000)
#lca_5 <- poLCA(f,as.data.frame(d_),5, maxiter = 10000)
#lca_6 <- poLCA(f,as.data.frame(d_),6, maxiter = 10000)
#lca_7 <- poLCA(f,as.data.frame(d_),7, maxiter = 10000)
#lca_8 <- poLCA(f,as.data.frame(d_),8, maxiter = 10000)


#Is done to create a cluster column in the analyses data set
# Use the 5 cluster solution

sub_1_plus_analyses$cluster <- as.factor(all_3_5$cluster)

# View the distribution of each type of gambling based on cluster
aggregate(all_data,by=list(all_3_5$cluster),FUN=mean)*100





