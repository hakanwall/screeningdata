##
# Plots for screening data
##
library(dplyr)
library(tidyr)
library(ggplot2)


# Plot types of gambling by gender, aggregate types of gambling by gender ---------
# a is just a temporary variable used  ---------
types_of_gambling <- dplyr::select(sub_1_plus, casino_online:other)
a <- aggregate(types_of_gambling,by=list(sub_1_plus$Gender),FUN=mean)*100

# this is done to arrange data nicely
a <- a %>%
  gather(type_gambling, value, casino_online:other, Group.1)
a$gender <- c("Female","Male")

# is done to remove unwanted rows
a <- a[1:24,]

# Done to provide informative labels
a$type_gambling[a$type_gambling == "casino_online"] <- "Online casino games"
a$type_gambling[a$type_gambling == "sportbetting_land"] <- "Landbased sports betting"
a$type_gambling[a$type_gambling == "keno_type"] <- "Keno-type games"
a$type_gambling[a$type_gambling == "poker_online"] <- "Online poker"
a$type_gambling[a$type_gambling == "sportbetting_online"] <- "Online sports betting"
a$type_gambling[a$type_gambling == "lotteries"] <- "Lotteries"
a$type_gambling[a$type_gambling == "poker_landbased"] <- "Landbased poker"
a$type_gambling[a$type_gambling == "casino_landbased"] <- "Landbased casino games"
a$type_gambling[a$type_gambling == "bingo"] <- "Bingo"
a$type_gambling[a$type_gambling == "egm"] <- "EGM outside casino"
a$type_gambling[a$type_gambling == "horsebetting"] <- "Horse betting"
a$type_gambling[a$type_gambling == "other"] <- "Other types of gambling*"

# is done to provide whole numbers when printing value in geom_text() -------------
a$value <- round(a$value)

positions <- c("Other types of gambling*","Online casino games","Online sports betting","Horse betting","Landbased sports betting","Online poker","EGM outside casino","Bingo","Landbased casino games","Lotteries","Landbased poker","Keno-type games")

a %>%
  ggplot(aes(type_gambling, value/100)) + geom_bar(aes(fill = relevel(as.factor(gender), c("Female"))), colour="black",
                                               width = 0.5, position = position_dodge(width=0.8), stat="identity") +  
  theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size=14)  , axis.text.y = element_text(size=14), axis.text.x = element_text(size=14), axis.title = element_text(size=14)) +  #,axis.title.x=element_blank()) + 
  geom_text(aes(label = paste("", value), group = relevel(as.factor(a$gender), c("Female"))), position = position_dodge(width=0.8), hjust=-0.2, size =5) +
  labs(x="", y="Proportion", title = "") +
  scale_x_discrete(limits = positions) +
  #scale_fill_brewer(palette="RdGy") +
  scale_fill_manual(values = c("Male"= "#e6550d", "Female" ="white")) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()

ggsave("figs/types_of_gambling-by-gender-ppt.png", device ="png", width = 12, height= 8, units = c("cm"))
ggsave("figs/types_of_gambling-by-gender-word-.eps", device ="eps", width =12, height= 8, units = c("in"))

# Plot variable loadings of the different clusters 
clust_aggr <- aggregate(all_data,by=list(all_3_5$cluster),FUN=mean)*100

clust_aggr <- clust_aggr %>%
  gather(type_gambling, value, casino_online:lottery_type, Group.1)

clust_aggr <- clust_aggr[1:50,]
clust_aggr$cluster <- c(1,2,3,4,5)


clust_aggr$type_gambling[clust_aggr$type_gambling == "casino_online"] <- "Online casino games"
clust_aggr$type_gambling[clust_aggr$type_gambling == "sportbetting_land"] <- "Landbased sports betting"
clust_aggr$type_gambling[clust_aggr$type_gambling == "sportbetting_online"] <- "Online sports betting"
clust_aggr$type_gambling[clust_aggr$type_gambling == "bingo"] <- "Bingo"
clust_aggr$type_gambling[clust_aggr$type_gambling == "horsebetting"] <- "Horse betting"
clust_aggr$type_gambling[clust_aggr$type_gambling == "lottery_type"] <- "Lottery-type"
clust_aggr$type_gambling[clust_aggr$type_gambling == "casino_landbased"] <- "Landbased casino games"
clust_aggr$type_gambling[clust_aggr$type_gambling == "poker"] <- "Poker"
clust_aggr$type_gambling[clust_aggr$type_gambling == "egm"] <- "EGM"
clust_aggr$type_gambling[clust_aggr$type_gambling == "poker_landbased"] <- "Landbased poker"
clust_aggr$type_gambling[clust_aggr$type_gambling == "poker_online"] <- "Online poker"

clust_aggr$value <- round(clust_aggr$value)
clust_aggr %>%
  ggplot(aes(cluster,value/100, fill=type_gambling)) + geom_bar(width = 0.4, position = position_dodge(width=0.7), stat="identity", color="black") +  
  theme(legend.position=c("bottom"), legend.title = element_blank()) + 
  #legend.title=element_blank()) + 
  scale_fill_brewer(palette="RdGy") +
  geom_text(aes(label = ifelse(value >9,value,""), group = type_gambling), position = position_dodge(0.7), vjust = -0.5, check_overlap = F) +
  #geom_text_repel(aes(label=value, group = type_gambling)) +
  #geom_label(aes(label=type_gambling), position ="identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(y="Proportion", x="Cluster")    
  #coord_flip()
# In ppt-format
ggsave("figs/types_of_gambling-by-cluster_ppt.png", device ="png", width = 15, height= 10, units = c("cm"))
# For Word
ggsave("figs/types_of_gambling-by-cluster-word.eps", device ="eps", width = 7.5, height= 5, units = c("in"))


# Mean PGSI by gender per cluster

gend_clust <- sub_1_plus_analyses %>% 
  filter(!is.na(Gender), !is.na(Age), !is.na(total))  %>% 
  group_by(Gender, cluster_5) %>% 
  summarise(mean_pgsi = mean(total),
            num_games = mean(n),
            antal = n())

gend_clust$Gender[gend_clust$Gender == 0] <- c("Female")
gend_clust$Gender[gend_clust$Gender == 1] <- c("Male")



gend_clust$mean_pgsi <- round(gend_clust$mean_pgsi, 1)

gend_clust %>%
  ggplot(aes(cluster,mean_pgsi)) + geom_bar(aes(fill = Gender), color="black",
                                             width = 0.5, position = position_dodge(width=0.5), stat="identity") +  
  theme(legend.position="bottom", legend.title = element_blank()) + 
  #scale_fill_brewer(palette="RdGy") +
  scale_fill_manual(values = c("Male"= "#e6550d", "Female" ="white")) +
  labs(y="PGSI score", x="Cluster") +
  geom_text(aes(label = mean_pgsi, group = Gender), position = position_dodge(0.5), vjust = -0.5)

gend_clust %>%
  #filter(!is.na(Age)) %>%
  ggplot(aes(cluster, mean_pgsi, color=Gender, group=Gender)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits=c(0,30)) +
  scale_fill_brewer(palette="RdGy") +
  geom_text(aes(label = mean_pgsi),  position = position_dodge(0.5), vjust = -0.5, check_overlap = F )

# plot density curves by cluster

sub_a$Gender[sub_a$Gender == "Female"] <- 0
sub_a$Gender[sub_a$Gender == "Male"] <- 1
sub_a  %>%
  ggplot(aes(total)) + 
  #geom_bar(aes(color=Gender)) +
  geom_density() +
  facet_grid(~cluster)



# save for ppt
ggsave("figs/cluster-by_gender_pgsi-ppt.png", device ="png", width = 15, height= 10, units = c("cm"))

# save for Word
ggsave("figs//cluster-by_gender_pgsi-word.eps", device ="eps", width = 7.5, height= 5, units = c("in"))


# Gender dist per cluster

gend_dist_clust <- sub_1_plus_analyses %>% 
  filter(!is.na(Gender))  %>% 
  group_by(Gender, cluster) %>% 
  summarise(antal = n())

gend_dist_clust$Gender[gend_dist_clust$Gender == 0] <- c("Female")
gend_dist_clust$Gender[gend_dist_clust$Gender == 1] <- c("Male")

gend_dist_clust$cluster <- c("casino gamblers","horse bettors","sports bettors","multi-type-gamblers")
gend_dist_clust$cluster[gend_dist_clust$cluster == "2"] <- c("casino gamblers")
gend_dist_clust$cluster[gend_dist_clust$cluster == "3"] <- c("sports bettors")
gend_dist_clust$cluster[gend_dist_clust$cluster == "4"] <- c("multi-type-gamblers")



male <- .7782147 *length(sub_1_plus_analyses$total)
female <- .22375*length(sub_1_plus_analyses$total)

gend_dist_clust <- gend_dist_clust %>%
  mutate(prop = if_else(Gender == "Female", antal/female,antal/male))

pos_ <- c("multi-type-gamblers","sports bettors","casino gamblers","horse bettors")

gend_dist_clust %>%
  ggplot(aes(cluster,prop*100)) + geom_bar(aes(fill = Gender), color="black",
                                            width = 0.5, position = position_dodge(width=0.5), stat="identity") +  
  theme(legend.position="bottom", legend.title = element_blank()) + 
  labs(y="Proportion", x="Cluster", title = "") +
  scale_y_continuous() +
  scale_x_discrete(limits = pos_) +
  scale_fill_manual(values = c("Male"= "#e6550d", "Female" ="white")) +
  #scale_fill_brewer(palette="RdGy") +
  geom_text(aes(label = round(prop*100), group = Gender), position = position_dodge(0.5), hjust = -0.5) +
  coord_flip()

# save for ppt
ggsave("figs/cluster-by_gender_prop-ppt.png", device ="png", width = 15, height= 10, units = c("cm"))

# save for Word
ggsave("figs//cluster-by_gender_prop-word.eps", device ="eps", width = 7.5, height= 5, units = c("in"))



# Plot Bayesian Information Criterion for both cluster models to determine the appropriate number of latent classes. ---------

#bic_cb <- c(clust_cb_2_3$bic, clust_cb_3_3$bic,clust_cb_3_4$bic,clust_cb_3_5$bic,clust_cb_3_6$bic,clust_cb_3_7$bic,clust_cb_3_8$bic)
bic_cb <- c(all_2_3$bic, all_3_3$bic,all_3_4$bic,all_3_5$bic,all_3_6$bic,all_3_7$bic)
#bic_lca <- c(lca_2_$bic, lca_3_$bic, lca_4_$bic, lca_5_$bic, lca_6_$bic, lca_7_$bic, lca_8_$bic) 
#num_cluster <- c(rep(c(2,3,4,5,6,7,8),2))
num_cluster <- c(2,3,4,5,6,7)

#group  <- c(rep("CLUSBIRD",7),rep("poLCA",7))
b <- data.frame(bic_cb,bic_lca)
#c_plot <- as.data.frame(cbind(group, value=round(append(bic_cb,bic_lca)), num_cluster))
c_plot <- as.data.frame(cbind(value=round(bic_cb), num_cluster))
c_plot$value <- as.integer(as.character(c_plot$value))
c_plot$num_cluster <- as.integer(as.character(c_plot$num_cluster))

ggplot(data=c_plot, aes(x=num_cluster, y=value)) + 
  geom_line(size=1.5) +
  geom_point(size=3) + 
  ylim(34000,37000) + 
  theme(plot.title = element_text(size=20, hjust = 0.5), text = element_text(size=12)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  labs(title="BIC values for cluster algorithm", x = "Clusters", y="BIC")

# Save plot for word-format -------
ggsave(file="figs/scree_plot_cbird-word.eps", device ="eps", width = 7.5, height= 5, units = c("in"))

# ------------ /End ------


ggplot(data=gend_clust, aes(x=cluster, y=mean_pgsi, group=Gender, color=Gender)) +
  geom_line(aes(linetype=Gender), size=1.5) +
  scale_shape_manual(name="Gender of gambler",
                    values=c(22,21)) +
  geom_point(size=3, fill="white") +
  theme_bw() +
  theme(legend.position=c(.7, .4))          




num_m <- sum(sub_1_plus$Gender == 1)
num_f <- sum(sub_1_plus$Gender == 0)
tot_num <- length(sub_1_plus$Gender)
gend_clust <- mutate(gend_clust, andel=ifelse(Gender=="female", num_f/tot_num, num_m/tot_num))

gend_clust %>%
  ggplot(aes(cluster,andel)) + geom_bar(aes(fill = Gender), 
                                         width = 0.5, position = position_dodge(width=0.5), stat="identity") +  
  theme(legend.position="top", legend.title = 
          element_blank(),axis.title.x=element_blank()) + 
  labs(y="Proportion", x="", title = "Gender proportion per gambler group") + 
  scale_y_continuous(labels = scales::percent) 
ggsave("Dropbox/presentation Oslo/cluster_gender_prop.png", device ="png", width = 12, height= 8, units = c("cm"))

#Plot Age distribution per gender

tmp_age <- dplyr::select(sub_1_plus, Age, Gender)

tmp_age <- tmp_age %>%
  filter(!is.na(Age)) %>%
  group_by(Gender, Age) %>%
    summarise(n =n())

tmp_age <- as.data.frame(tmp_age)

tmp_age$Age <- as.character(tmp_age$Age)
 
# Create propotion column

male <- round(0.78*5395)
female= round(0.22*5395)

tmp_age <- tmp_age %>% mutate(prop = if_else(Gender == 0, n/female, n/male))
tmp_age$prop <- round(tmp_age$prop, 2)


tmp_age$Gender <- as.character(tmp_age$Gender)
tmp_age$Gender[tmp_age$Gender == "0"] <- "Female"
tmp_age$Gender[tmp_age$Gender == "1"] <- "Male"
tmp_age$Age[tmp_age$Age == ">65"] <- ">64"
positions <- c("<18","18-24","25-44","45-64",">64")

tmp_age %>%
  ggplot(aes(Age, prop)) + geom_bar(aes(group = relevel(as.factor(Gender), c("Male")), fill= relevel(as.factor(Gender), c("Female"))),  width = 0.5, position = position_dodge(width=0.5), stat="identity", color="black") + 
  geom_text(aes(label = prop*100, group = relevel(as.factor(Gender), c("Male"))), position = position_dodge(0.6), vjust = -0.5) +
  
  scale_x_discrete(limits = positions) +
  labs(y="Proportion", x="Age group") +
  theme(legend.position="bottom", legend.title = element_blank(), legend.text.align = 1) + 
  scale_fill_manual(values = c("Male"= "#e6550d", "Female" ="white")) +
  #scale_fill_brewer(palette="RdGy") +
  scale_y_continuous(labels = scales::percent) 

# In ppt-format
ggsave("figs/age_group_by_gender-ppt.png", device ="png", width = 20, height= 13, units = c("cm"))
# For Word
ggsave("figs/age_group_by_gender-word.eps", device ="eps", width = 7.5, height= 5, units = c("in"))




# --- / END -------------

a <- mutate(a, Andel = Num/colS)



a %>%
  ggplot(aes(Age,Andel)) + geom_bar(aes(fill = "red"), 
                                    width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="top", legend.title = 
          element_blank(),axis.title.x=element_blank()) + 
  labs(y="Andel", x="", title = "Åldersfördelning") +
  guides(fill=FALSE)

ggsave("Dropbox/presentation Oslo/age_prop.png", device ="png", width = 12, height= 8, units = c("cm"))


#Plot pgsi dist with denstiy curve
tab_pgsi <- as.data.frame(table(p_all$total));tab_pgsi
sumP <- sum(tab_pgsi$Freq)
tab_pgsi <- mutate(tab_pgsi, Andel=Freq/sumP)

colnames(tab_pgsi)[1:2] <- c("Value","Antal")

tab_pgsi %>%
  ggplot(aes(Value,Andel)) + geom_bar(aes(fill="red"), 
                                      width = 0.5, position = position_dodge(width=0.5), stat="identity") +  
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="top", legend.title = 
          element_blank(),axis.title.x=element_blank()) + 
  labs(y="Andel", x="", title = "Fördelning PGSI") +
  guides(fill=FALSE)

p_all %>%
  ggplot(aes(total)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(0, 27, by = 1), 
                 col="red", 
                 fill="red", 
                 alpha = .4) + 
  geom_density(col="red") + 
  labs(title="Histogram for PGSI") +
  labs(x="Värde", y="Density")
ggsave("Dropbox/presentation Oslo/pgsi_prop.png", device ="png", width = 12, height= 8, units = c("cm"))



ggplot(data=sub_1_plus, aes(x=cbird, y=total, group=Gender)) +
  geom_line() +
  geom_point()



ggplot(data=sub_1_plus, aes(x=casino_online, y=total, group=cbird, fill=cbird)) +
  geom_point()


## Plot BIC-value for clusters

bic_cb <- c(clust_cb_2_3_$bic, clust_cb_3_3_$bic,clust_cb_3_4_$bic,clust_cb_3_5_$bic,clust_cb_3_6_$bic,clust_cb_3_7_$bic,clust_cb_3_8_$bic)
bic_lca <- c(lca_2_$bic, lca_3_$bic, lca_4_$bic, lca_5_$bic, lca_6_$bic, lca_7_$bic, lca_8_$bic) 

num_cluster <- c(rep(c(2,3,4,5,6,7,8),2))
group  <- c(rep("CLUSBIRD",7),rep("poLCA",7))
b <- data.frame(bic_cb,bic_lca)
c_plot <- as.data.frame(cbind(group, value=round(append(bic_cb,bic_lca)), num_cluster))
c_plot$value <- as.integer(as.character(c_plot$value))
c_plot$num_cluster <- as.integer(as.character(c_plot$num_cluster))


ggplot(data=c_plot, aes(x=num_cluster, y=value, group = group, colour=group)) + 
  geom_line(size=1.5) +
  geom_point(size=3) + 
  ylim(30000,35000) + 
  theme(plot.title = element_text(size=20, hjust = 0.5), text = element_text(size=12)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  labs(title="BIC values for CLUSBIRD and poLCA", x = "Clusters", y="BIC", colour="Algorithm")
  

### plot speltyper stödlinjen och LI
data_ <- data.frame(speltyp = rep(c("Lotterier","Sportspel land","Sportspel online","Trav eller galopp","Bingo","Poker","Casino"),2), grupp = c(rep(c("Stödlinjen"),7),rep(c("Befolkningen"),7)), andel=c(0.11,0.14,0.28,0.15,0.05,0.16,0.73,0.7,0.08,0.13,0.33,0.11,0.05,0.07))
positions <- c("Bingo","Lotterier","Sportspel land","Trav eller galopp","Poker","Sportspel online","Casino")
data_ %>%
  ggplot(aes(speltyp,andel)) + geom_bar(aes(fill = grupp), 
                                          width = 0.5, position = position_dodge(width=0.5), stat="identity") +  
  theme(legend.position="top", legend.title = 
          element_blank(),axis.title.x=element_blank()) + 
  scale_x_discrete(limits = positions) +
  labs(y="Andel", x="", title = "Speltyp")+
  scale_y_continuous(labels = scales::percent) +
  coord_flip()


a %>%
  ggplot(aes(type_gambling, value)) + geom_bar(aes(fill = gender), 
                                               width = 0.4, position = position_dodge(width=0.5), stat="identity") +  
  theme(legend.position="top", legend.title = 
          element_blank(),axis.title.x=element_blank()) + 
  labs(y="Andel", x="", title = "Types of gambling") +
  coord_flip()


qqplot(data=data, aes(x=speltyp, y=värde, group=grupp, colour=grupp)) + 
  geom_bar()

###
sub_1_plus$cluster <- clust_cb_3_4$cluster

sub_1_plus %>%
    group_by(cluster, Gender) %>%
      summarise(PGSI = mean(total),
                casino_online = mean(casino_online),
                num_games = mean(n),
                n= n())



######
aaa <- select()
aaa
data_ <- as.matrix(mydata_) +1

pol_ <- poLCA(cbind(casino_online, sportbetting_land, sportbetting_online, bingo, horsebetting, lottery, poker, casino_land)~1, data=as.data.frame(d_), 6)

t1 <- lca(as.matrix(mydata_), k = 4)
disma <- hamming.distance(z) 
hr <- hclust(as.dist(disma)) 
plot(hr)
clust_hc <- cutree(hr, k=3) 
plot(as.dendrogram(hr), edgePar=list(col=3, lwd=4), horiz=T)



x1 <- c(rep(c(0,1),8))
x2 <- c(rep(c(0,0,1,1),4))
x3 <- c(rep(0,8),rep(1,8)) 

f <- cbind(x1,x2,x3) ~ 1
z <- cbind(x1,x2,x3)
z <- z+1

pol_ <- poLCA(f, as.data.frame(z), 3)  




t_lca <- lca(as.matrix(z),2, matchdata = TRUE)
t_lca$matching
t_lca$bic
t_cb_4 <- cbird(z, 4, 3) 
t_cb$cluster
t_cb$bic

disma <- hamming.distance(z) 
hr <- hclust(as.dist(disma)) 
clust_hc <- cutree(hr, k=4) 

aggregate(z,by=list(t_cb$cluster),FUN=mean)*100




aggregate(mydata,by=list(clust_cb_6$cluster),FUN=mean)*100
(aggregate(z,by=list(t_lca$matching),FUN=mean)*100)-100
aggregate(data_,by=list(pol_$predclass),FUN=mean)*100

z_ <- as.data.frame(z)
sub_1_plus <- mutate(sub_1_plus, cb=clust_cb_4$cluster, lca=t_lca$matching, pol = pol_$predclass)

sub_1_plus %>%
    group_by(pol, Gender) %>%
      summarise(PGSI = mean(total),
                GAMES = mean(num_games),
                n = n())
  

set.seed(1)
y <- matrix(rbinom(100 * 20, 1, 0.5), 100, 10)
z
out <- cbird(Y, 2, 3)


colSums(z_$V1:z_$V10)

mydata_ <- select(mydata_, casino_online)


#########INGVAR###########
library(poLCA)
d <- read.csv(file = "../User/Box Sync/data files/niu2.csv", sep=",", header = T)
head(d)
d <- dplyr::select(d, -respid)
d_ <- dplyr::filter(d, d_q2 != ".c", d_q2 != ".b", d_q3 != ".c", d_q3 != ".b",d_q5 != ".c", d_q5 != ".b",d_q6 != ".c", d_q6 != ".b",d_q8a != ".c", d_q8a != ".b",
              d_q8b != ".c", d_q8b != ".b",d_q9 != ".c", d_q9 != ".b",d_q10 != ".c", d_q10 != ".b",d_q11 != ".c", d_q11 != ".b",d_q12 != ".c", d_q12 != ".b",
              d_q13 != ".c", d_q13 != ".b",d_q30 != ".c", d_q30 != ".b",d2_audc != ".c", d2_audc != ".b") 


for(i in 1:13){
  d_[,i] <- as.integer(as.character(d_[,i]))
}

f_ <- cbind(d_q2,d_q3,d_q5,d_q6,d_q8a,d_q8b,d_q9,d_q10,d_q11,d_q12,d_q13,d_q30,d2_audc)~1



str(d_)

tmp <- d_
d_ <- as.matrix(mydata_) + 1


for(i in 1:7){
  c_ <- poLCA(f_,d_,i)
  print(paste(i,c_$bic, sep=" "))
} 


#c_ <- cbird(as.matrix(d_),3,3)

aggregate(tmp,by=list(c_5$cluster),FUN=mean)*100

tmp <- mutate(tmp, cluster = c_5$cluster)

tmp %>%
  group_by(cluster) %>%
    summarise(n=n())

tmp$respid <- d_$respid


sub_1_plus %>%
  group_by(lotteries == 1, num_games ==1) %>%
  summarise(PGSI = mean(total),
            co = mean(casino_online),
            ss_online= mean(sportbetting_online),
            ss_o = mean(sportbetting_land),
            Bingo = mean(bingo),
            Vegas = mean(egm),
            #CC = mean(casino_landbased),
            #lot = mean(lotteries),
            num = mean(Keno_type),
            p_online = mean(poker_online),
            p_cc = mean(poker_landbased),
            ATG = mean(horsebetting),
            Mean_N = mean(n),
            n= n())
##try clustering with kmodes
library(klaR)
cl <- kmodes(mydata_,6)








