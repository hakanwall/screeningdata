  ##
# Plots for screening data
##
library(tidyverse)
library(ggrepel)
library(ggplot2)

# Plot types of gambling by gender, aggregate types of gambling by gender ---------
# a is just a temporary variable used  ---------
types_of_gambling <- dplyr::select(sub_1_plus, casino_online:other)
a <- aggregate(types_of_gambling,by=list(sub_1_plus$Gender),FUN=mean)*100

8# this is done to arrange data nicely
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
ggsave("figs/types_of_gambling-by-gender-word.eps", device ="eps", width =12, height= 8, units = c("in"))

# ALt plot for type of game by gender

a %>%
  ggplot(aes(type_gambling, value/100, group=gender, color=gender)) +
  geom_line(aes(linetype=gender, color=gender, size=gender))+
  geom_point() +
  theme(legend.position="bottom") +
  scale_color_manual(values=c('#7570b3','#1b9e77','#d95f02')) + #,'#7570b3','#e7298a','#66a61e'))+
  scale_size_manual(values=c(1,1,1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(limits = positions) +
  labs(y="Probability", x="Type of gambling") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title=element_blank()) +
  geom_label_repel(aes(label = if_else(value >= 10, round(value),c()), group = gender), show.legend = FALSE) 

ggsave("figs/types_of_gambling-by-gender-word-alt.png", plot=last_plot(), device = png(), width = 10, height = 5, units = c("in"), dpi="print")


# Plot variable loadings of the different clusters 
clust_aggr <- aggregate(all_data,by=list(all_3_6$cluster),FUN=mean)*100

clust_aggr <- clust_aggr %>%
  gather(type_gambling, value, casino_online:lottery_type, Group.1)

clust_aggr <- clust_aggr[1:60,]
clust_aggr$cluster <- c("online casino gamblers","casino games gamblers","online sports bettors","multi-type gamblers","casino and sports betting","horse and lottery players")


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
  ggplot(aes(cluster,value/100, fill=type_gambling)) + geom_bar(width = 0.6, position = position_dodge(width=0.8), stat="identity", color="black") +  
  scale_fill_brewer(palette="RdGy") +
  geom_text(aes(label = ifelse(value >9,value,""), group = type_gambling), position = position_dodge(0.8), vjust = -0.5, check_overlap = T) +
  scale_x_discrete(limits = rev(pos_)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y="Proportion", x="Cluster") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position=c("bottom"), legend.title = element_blank()) 
  #coord_flip()
# In ppt-format
ggsave("figs/types_of_gambling-by-cluster_ppt.png", device ="png", width = 15, height= 10, units = c("cm"))
# For Word
ggsave(filename = "figs/types_of_gambling-by-cluster-flip-word", plot=last_plot(), device = png(), width = 10, height = 5, units = c("in"), dpi="print")
ggsave("figs/types_of_gambling-by-cluster-flip-word.eps", device ="eps", width = 8, height= 5.33, units = c("in"))


# Plot clusters differently...  type of game on x-axis and group by cluster

tmmp_ <- clust_aggr %>%
filter(cluster == "online casino gamblers" | cluster == "online sports bettors" | cluster == "horse and lottery players")
  
tmmp_ %>%  
ggplot(aes(type_gambling, value/100, group=cluster, color=cluster)) +
  geom_line(aes(linetype=cluster, color=cluster, size=cluster))+
  geom_point() +
  scale_color_manual(values=c('#1b9e77','#d95f02','#7570b3')) + #,'#e7298a','#66a61e','#000000'))+
  scale_size_manual(values=c(1, 1, 1)) + # , 1, 1,1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(limits = c("Bingo","Horse betting","Lottery-type","Landbased poker","EGM","Landbased casino games","Landbased sports betting","Online casino games","Online sports betting","Online poker")) +
  labs(y="Probability", x="") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right", legend.title=element_blank()) +
  geom_label_repel(aes(label = ifelse(value > 9 & value <= 100, round(value),""), group = cluster), show.legend = FALSE) 



ggsave(filename = "figs/types_of_gambling-by-cluster_top_3.png", plot=last_plot(), device = png(), width = 7.5, height = 3.75, units = c("in"), dpi="print")


# Plot result from regression model

sub_1_plus_analyses %>%
  filter(!is.na(total)) %>%
    group_by(cluster, Gender) %>%
      summarize(total = mean(total),
                num_games = mean(n),
                num = n()) %>%
  ggplot(aes(cluster, total, group=Gender, color=Gender)) + geom_point() + geom_line(size=1) + 
  theme_minimal() +
  scale_color_manual(values=c('#1b9e77','#d95f02','#000000')) + #,'#7570b3','#e7298a','#66a61e'))+
  scale_size_manual(values=c(1.5,1.5)) + 
  scale_y_continuous(limits = c(0,27)) +
  geom_label_repel(aes(label = round(total,1), group = Gender)) + 
  # add horisontal line displaying mean PGSI
  geom_hline(yintercept = 15.2, linetype="dashed") 

ggsave("figs/cluster_by_gender_total_score-word.eps", device ="eps", width = 8, height= 5.33, units = c("in"))



  # Mean PGSI by gender per cluster

gend_clust <- sub_1_plus_analyses %>% 
  filter(!is.na(Gender), !is.na(Age), !is.na(total))  %>% 
  group_by(Gender, cluster) %>% 
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
  filter(!is.na(Gender), !is.na(total))  %>% 
  group_by(Gender, cluster) %>% 
  summarise(mean_PGSI = mean(total),
            antal = n())

gend_dist_clust$Gender[gend_dist_clust$Gender == 0] <- c("Female")
gend_dist_clust$Gender[gend_dist_clust$Gender == 1] <- c("Male")

gend_dist_clust$cluster <- c("multi-type-gamblers","online sportsbettors","online casino gamblers","casino and sportsbettors","not casino gamblers")
#gend_dist_clust$cluster[gend_dist_clust$cluster == "2"] <- c("casino gamblers")
#gend_dist_clust$cluster[gend_dist_clust$cluster == "3"] <- c("sports bettors")
#gend_dist_clust$cluster[gend_dist_clust$cluster == "4"] <- c("multi-type-gamblers")

male <- mean(sub_1_plus_analyses$Gender) *length(sub_1_plus_analyses$Gender)
female <- (1-mean(sub_1_plus_analyses$Gender))*length(sub_1_plus_analyses$Gender)

gend_dist_clust <- gend_dist_clust %>%
  mutate(prop = if_else(Gender == "Female", antal/female,antal/male))

pos_ <- c("multi-type-gamblers","online sportsbettors","online casino gamblers","casino and sportsbettors","not casino gamblers")

gend_dist_clust %>%
  ggplot(aes(cluster,prop*100)) + geom_bar(aes(fill = Gender), color="black",
                                            width = 0.5, position = position_dodge(width=0.5), stat="identity") +  
  theme(legend.position="bottom", legend.title = element_blank()) + 
  labs(y="Proportion", x="Cluster", title = "") +
  scale_y_continuous() +
  scale_x_discrete(limits = rev(pos_)) +
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
tmp_age <- tmp_age[1:10,]

tmp_age$Age <- as.character(tmp_age$Age)
 
# Create propotion column

num <- length(sub_1_plus$Gender)
male <- round(mean(sub_1_plus$Gender)*num)
female= round((1-mean(sub_1_plus$Gender))*num)

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


# Plot regression model output
# Calculate mean PGSI per cluster
cluster_1_female <- coef_quasi[1];cluster_1_female
cluster_1_male <- coef_quasi[1]*coef_quasi[7];cluster_1_male
cluster_2_female <- coef_quasi[1]*coef_quasi[2];cluster_2_female 
cluster_2_male  <- coef_quasi[1]*coef_quasi[2]*coef_quasi[7]*coef_quasi[12];cluster_2_male
cluster_3_female <- coef_quasi[1]*coef_quasi[3];cluster_3_female
cluster_3_male <- coef_quasi[1]*coef_quasi[3]*coef_quasi[7]*coef_quasi[13];cluster_3_male
cluster_4_female <- coef_quasi[1]*coef_quasi[4];cluster_4_female 
cluster_4_male <- coef_quasi[1]*coef_quasi[4]*coef_quasi[7]*coef_quasi[14];cluster_4_male
cluster_5_female <- coef_quasi[1]*coef_quasi[5];cluster_5_female
cluster_5_male <- coef_quasi[1]*coef_quasi[5]*coef_quasi[7]*coef_quasi[15];cluster_5_male
cluster_6_female <- coef_quasi[1]*coef_quasi[6];cluster_6_female
cluster_6_male <- coef_quasi[1]*coef_quasi[6]*coef_quasi[7]*coef_quasi[16];cluster_6_male


#Create data frame containing cluster, value and gender
clust_gend <- data.frame(cluster = c("1","2","3","4",
                                     "5","6","1","2","3","4",
                                     "5","6","1","2","3","4",
                                     "5","6"), value = c(coef_quasi_cluster[1],coef_quasi_cluster[1]*coef_quasi_cluster[2],coef_quasi_cluster[1]*coef_quasi_cluster[3],
                                                         coef_quasi_cluster[1]*coef_quasi_cluster[4],coef_quasi_cluster[1]*coef_quasi_cluster[5],coef_quasi_cluster[1]*coef_quasi_cluster[6],
                                                         cluster_1_female,cluster_2_female,cluster_3_female,
                                                         cluster_4_female,cluster_5_female,cluster_6_female,
                                                         cluster_1_male,cluster_2_male,cluster_3_male,cluster_4_male,
                                                         cluster_5_male,cluster_6_male),
                         gender=c(rep("combined",6),rep("female",6),rep("male",6)))

#Plot 

clust_gend$gender <- as.character(clust_gend$gender)

clust_gend %>%
  ggplot(aes(cluster, value, group=gender, color=gender)) + geom_point() + geom_line(aes(linetype=gender, size=gender)) +
  scale_color_manual(values=c('#7570b3','#1b9e77','#d95f02')) + 
  labs(y="PGSI score", x="") +
  scale_size_manual(values=c(1, 1, 1)) +
  scale_y_continuous(limits = c(8,27), breaks = c(8,10,12.5,15,17.5,20,22.5,25,27)) +
  scale_x_discrete(limits = c("Online casino gamblers","Multi casino gamblers","Online sports bettors","Multi-type gamblers","Sports and casino gamblers","Lottery and horse gamblers")) +
  geom_label_repel(aes(label = if_else(gender == "total", paste(round(value,1), if_else(cluster == "Online sports bettors" | cluster == "Multi-type gamblers" | cluster == "Lottery and horse gamblers","*","")), 
                                                          paste(round(value,1), if_else(cluster == "Online casino gamblers","**","")))), show.legend = FALSE, segment.size = 0.2) +
  #scale_size_manual(values=c(1,1.5))  +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10), axis.text.y = element_text(size=10), legend.title=element_blank(), legend.text = element_text(size=10))

ggsave("figs/PGSI_by_cluster_times_gender.png", plot=last_plot(), device = png(), width = 7.5, height = 5, units = c("in"), dpi="print")



## Plot invovlement effect by type of game

## Build a tibble containing type of game, num other games and PGSI total score

inv_tibble <- tibble(game = c(rep(c("Online casino games"),7),rep(c("EGM"),7),rep(c("Online sports betting"),7),rep(c("Land-based sports betting"),7),rep("Online poker",7), rep("Land-based poker",7),
                    rep("Lotteries",7),rep("Bingo",7),rep("Horse betting",7),rep("Land-based casino",7),rep("Total",7)), 
                    num_games = rep(c("0","1","2","3","4","5","6 or more"),11),
       value = c(17,16.1,16.7,16.2,17.2,17.7,18.7,
                 16.1,16.6,16.2,17,17.1,17.8,18.9,
                 14,14.2,15.7,15.2,16.6,17.8,18.6,
                 12.4,14.5,15.2,15.1,17.1,18.1,19,
                 14.2,16.2,16.6,16.1,16.5,17.3,18.8,
                 11.5,12.7,15.1,15.8,16.9,17.1,19,
                 5.6,10.1,14.1,13.7,16.2,16.4,18.6,
                 10,13.1,15.1,14.8,15.9,18.4,19.2,
                 8.6,11.6,12.6,13.9,17,17.2,19.1,
                 12.7,14.7,16.3,18.1,17.7,19.4,18.8,
                 15.4,14.8,15.6,15.6,16.9,17.6,18.6)) 


inv_tibble %>% ggplot(aes(num_games,value, group=game, color=game, label=game)) + geom_line(aes(linetype=game, color=game), size=1.5) +
  #geom_point(aes(shape=game), size=4) +
  scale_x_discrete(name = "Number of other games", limits = c("0","1","2","3","4","5","≥6"), labels = c("≥6" = "6 or more"), expand = c(0.3,0,0,0)) +
  scale_y_continuous(name= "PGSI score", limits = c(5,20), breaks = c(5,7.5,10,12.5,15,17.5,20), position = "right") + 
  scale_linetype_manual(values=c("twodash","dotted","solid","longdash","dotdash","dashed","twodash","dotted","solid","longdash","solid"))+
  scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#666666','#a65628','#f781bf','#999999','#000000','#000000')) +
  geom_text_repel(
      data = subset(inv_tibble, num_games == "0"),
      nudge_x      = -0.15,
      direction    = "y",
      hjust        = 1,
      segment.size = 0.2,
      size=5
    ) +  
  geom_point(aes(shape=game), size = 4) +
  theme_minimal() + 
  theme(legend.position = "none", text = element_text(size=14), axis.text.x = element_text(color="#000000", size=14),
        axis.text.y = element_text(color="#000000", size=14))
    
 
  
  ggsave("../screeningdata/figs/involvement_600dpi.png", last_plot(), width = 12, height = 8,
          dpi = 600)

  
  # Plot prob of participating in a certain type of gambling as a function of cluster  

a <- aggregate(all_data,by=list(all_3_7_omitted$cluster),FUN=mean)
a <- a %>% rename(group = Group.1)
a <- a %>% gather(group, value) %>% mutate(cluster = rep(c("online casino","online sports/casino","casino/betting/poker","horse/lottery","online sports","casino/EGM","diverse"),10)) %>% rename(game_type = group)
#a <- a %>% gather(group, value) %>% mutate(cluster = rep(c("1","2","3","4","5"),10)) %>% rename(game_type = group)
a %>% ggplot(aes(game_type,value, group=cluster, color=cluster)) + geom_line(aes(linetype=cluster, color=cluster), size=1.5) +
  scale_x_discrete(name = "", limits = c("bingo","horsebetting","lottery_type","poker_landbased","egm","casino_landbased","sportbetting_land","casino_online","sportbetting_online","poker_online"),
                   labels = c("bingo" =  "bingo", "horsebetting" = "horse betting", "lottery_type" = "lotteries", "poker_landbased" ="land-based poker","egm"="EGM","casino_landbased" ="land-based casino",
                              "sportbetting_land" = "land-based sports betting", "casino_online" = "online casino games", "sportbetting_online" = "online sports betting", "poker_online" = "online poker")) +
  scale_y_continuous(name= "Probability of participation", limits = c(0,1), breaks = c(0,.25,.50,.75,1), position = "left", labels = scales::percent) + 
  scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#666666','#a65628')) +
  geom_point(aes(shape=cluster), size = 4) +
  theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(size=14), legend.text = element_text(size=14), axis.text.x = element_text(color="#000000", size=14,angle = 45, hjust = 1),
        axis.text.y = element_text(color="#000000", size=14))


ggsave("figs/custers.png", last_plot(), width = 12, height = 8,
       dpi = 300)

ggsave("../screeningdata/figs/cluster_600dpi.png", last_plot(), width = 12, height = 8, dpi = 600)




