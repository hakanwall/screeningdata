# R-script analyzing screening-data from the Swedish national gambling helpline 
#
# Date: 2018-12-16-xx-xx
# Author: Håkan Wall
# @: hakan.wall@ki.se AND/OR hakan.wall74@gmail.com
#
library(parallel)
library(psych)
library(nnet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cbird)
library(poLCA)
library(markdown)

#Get master csv file containing screening result

path <- c("/../Users/User/Downloads/stats-7 2/pgsi.csv")
master <- read.csv(path, sep=",", header=F, skip=1651, col.names = c("x","total","item1","item2","item3","item4","item5","item6","item7","item8","item9", "Date", "Gender", "Age","Reason", "County", "Ok_research","game1","game2","game3","game4","game5","game6","game7","game8","game9","game10","game11","game12"))

# Do som data-cleaning
# set PGSI-item (V4) as integer
# drop the x-column since fills no function
master$item3 <- as.integer(as.character(master$item3))
master$x <- NULL
#Set Date as Date-type
master$Date <- as.Date(master$Date)
#Set Age as character
master$Age <- as.character(master$Age)

# Iterate through type of gambling; if type present in row, set corresponding response column to 1 else 0. 0 also indicating no entry at all.

master <- mutate(master, casino_online = if_else(game1 == "Online casino" | game2 == "Online casino" | game3 == "Online casino" | game4 == "Online casino" | game5 == "Online casino" | game6 == "Online casino" | game7 == "Online casino" | game8 == "Online casino" | game9 == "Online casino" | game10 == "Online casino" | game11 == "Online casino" | game12 == "Online casino",1,0))
master <- mutate(master, sportbetting_land = if_else(game1 == "Sportspel ombud" | game2 == "Sportspel ombud" | game3 == "Sportspel ombud" | game4 == "Sportspel ombud" | game5 == "Sportspel ombud" | game6 == "Sportspel ombud" | game7 == "Sportspel ombud" | game8 == "Sportspel ombud" | game9 == "Sportspel ombud" | game10 == "Sportspel ombud" | game11 == "Sportspel ombud" | game12 == "Sportspel ombud",1,0))
master <- mutate(master, keno_type = if_else(game1 == "Nummerspel" | game2 == "Nummerspel" | game3 == "Nummerspel" | game4 == "Nummerspel" | game5 == "Nummerspel" | game6 == "Nummerspel" | game7 == "Nummerspel" | game8 == "Nummerspel" | game9 == "Nummerspel" | game10 == "Nummerspel" | game11 == "Nummerspel" | game12 == "Nummerspel",1,0))
master <- mutate(master, poker_online = if_else(game1 == "Poker online" | game2 == "Poker online" | game3 == "Poker online" | game4 == "Poker online" | game5 == "Poker online" | game6 == "Poker online" | game7 == "Poker online" | game8 == "Poker online" | game9 == "Poker online" | game10 == "Poker online" | game11 == "Poker online" | game12 == "Poker online",1,0))
master <- mutate(master, sportbetting_online = if_else(game1 == "Sportspel online" | game2 == "Sportspel online" | game3 == "Sportspel online" | game4 == "Sportspel online" | game5 == "Sportspel online" | game6 == "Sportspel online" | game7 == "Sportspel online" | game8 == "Sportspel online" | game9 == "Sportspel online" | game10 == "Sportspel online" | game11 == "Sportspel online" | game12 == "Sportspel online",1,0))
master <- mutate(master, lotteries = if_else(game1 == "Lotterier" | game2 == "Lotterier" | game3 == "Lotterier" | game4 == "Lotterier" | game5 == "Lotterier" | game6 == "Lotterier" | game7 == "Lotterier" | game8 == "Lotterier" | game9 == "Lotterier" | game10 == "Lotterier" | game11 == "Lotterier" | game12 == "Lotterier",1,0))
master <- mutate(master, poker_landbased = if_else(game1 == "Poker live" | game2 == "Poker live" | game3 == "Poker live" | game4 == "Poker live" | game5 == "Poker live" | game6 == "Poker live" | game7 == "Poker live" | game8 == "Poker live" | game9 == "Poker live" | game10 == "Poker live" | game11 == "Poker live" | game12 == "Poker live",1,0))
master <- mutate(master, casino_landbased = if_else(game1 == "Casino Cosmopol" | game2 == "Casino Cosmopol" | game3 == "Casino Cosmopol" | game4 == "Casino Cosmopol" | game5 == "Casino Cosmopol" | game6 == "Casino Cosmopol" | game7 == "Casino Cosmopol" | game8 == "Casino Cosmopol" | game9 == "Casino Cosmopol" | game10 == "Casino Cosmopol" | game11 == "Casino Cosmopol" | game12 == "Casino Cosmopol",1,0))
master <- mutate(master, bingo = if_else(game1 == "Bingo" | game2 == "Bingo" | game3 == "Bingo" | game4 == "Bingo" | game5 == "Bingo" | game6 == "Bingo" | game7 == "Bingo" | game8 == "Bingo" | game9 == "Bingo" | game10 == "Bingo" | game11 == "Bingo" | game12 == "Bingo",1,0))
master <- mutate(master, egm = if_else(game1 == "Spelmaskiner" | game2 == "Spelmaskiner" | game3 == "Spelmaskiner" | game4 == "Spelmaskiner" | game5 == "Spelmaskiner" | game6 == "Spelmaskiner" | game7 == "Spelmaskiner" | game8 == "Spelmaskiner" | game9 == "Spelmaskiner" | game10 == "Spelmaskiner" | game11 == "Spelmaskiner" | game12 == "Spelmaskiner",1,0))
master <- mutate(master, horsebetting = if_else(game1 == "Hästar" | game2 == "Hästar" | game3 == "Hästar" | game4 == "Hästar" | game5 == "Hästar" | game6 == "Hästar" | game7 == "Hästar" | game8 == "Hästar" | game9 == "Hästar" | game10 == "Hästar" | game11 == "Hästar" | game12 == "Hästar",1,0))
master <- mutate(master, other = if_else(game1 == "Annat" | game2 == "Annat" | game3 == "Annat" | game4 == "Annat" | game5 == "Annat" | game6 == "Annat" | game7 == "Annat" | game8 == "Annat" | game9 == "Annat" | game10 == "Annat" | game11 == "Annat" | game12 == "Annat",1,0))


#save master file to tmp obj incase of FAIL.
# tmp <- master

#set gender column as integer; 1 = male, 0 = female, and 2 = not_entered 

master$Gender <- as.integer(master$Gender) - 1

#drop game* columns.
master <- dplyr::select(master, -starts_with("game"))

#create column displaying number of games played summing columns containing binary (1 or 0) entries for type of gambling -----------
master$num_games <- rowSums(master[,17:28])

# To have functional age levels, rearrange age groups 
master$Age <- as.character(master$Age)


master$Age[master$Age== "<15" | master$Age== "15-17"] <- c("<18")
master$Age[master$Age== "25-34" | master$Age== "35-44"] <- c("25-44")
master$Age[master$Age== "45-54" | master$Age== "55-64"] <- c("45-64")
master$Age[master$Age== "65-74" | master$Age== ">74"] <- c(">65")

#Set wrong entries as NA 
master$Age[master$Age== "null" | master$Age== "Stockholms län" | master$Age== "Välj..." | master$Age =="Södermanlands län" | master$Age =="Jönköpings län" | master$Age =="Choose"] <- ""
master$County <- as.character(master$County)
master$County[master$County == "Välj..." | master$County == "15-17" | master$County == "18-24" | master$County == "25-34" | master$County == "35-44" | master$County == "Choose" | master$County == "null" | master$County == "NA" | is.na(master$County)] <- ""

master$Age[master$Age == ""] <- NA
master$County[master$County == ""] <- NA
master$Age <- as.factor(master$Age)
#set 25-44 as ref level for analyses
master$Age <- relevel(master$Age, "25-44")

#Save master to csv if session crashes
#write.csv(master, file = "Box Sync/data files/screeningdata/data files/master.csv", sep=",")
# master_


# Subset only those gambling ≥ 1 type of games or more AND doing the test for fun AND Gender != NULL ---------------

sub_1_plus <- filter(master, num_games > 0, Reason == "knowmore", Gender < 2, Ok_research == "true") 

#subset those screening before and after 2017-06-28 -------------

sub_after <- filter(sub_1_plus, Date >= "2017-06-28")

sub_before <- filter(sub_1_plus, Date < "2017-06-28")

#check if num_games or total PGSI points differ before and after altering the question regarding type games played
wilcox.test(sub_before$num_games, sub_after$num_games, paired = F)
wilcox.test(sub_before$total, sub_after$total, paired = F)


#subset those doing the test for 'fun' and 'knowmore'

sub_fun <- filter(master, num_games > 0, Reason == "fun", Gender < 2, Ok_research == "true") 
sub_knowmore <- filter(sub_1_plus, Reason =="knowmore")

# Check if "knowmore" and "for fun" groups differ regarding number of games played and total PGSI score

wilcox.test(sub_fun$num_games, sub_knowmore$num_games, paired = F)
wilcox.test(sub_fun$total, sub_knowmore$total, paired = F)

#Drop erroneous Age levels
sub_1_plus <- sub_1_plus %>% 
  filter(Age !="25-44" | Age != "18-24" | Age != "45-64" | Age != "<18" | Age != ">65") %>% 
  droplevels()
#manually drop Gotlands län    
droplevels(sub_1_plus$Age, exclude = "Gotlands l\303\244n")


# Create a new variable called mydata for identifying clusters and collapse identical types of gambling: lotteries AND Keno_type

# Try with all types of gambling

all_data <- dplyr::select(sub_1_plus, casino_online:horsebetting)
all_data <- all_data %>%
  mutate(lottery_type = if_else(lotteries | keno_type,1,0))
all_data <- all_data %>%  
  dplyr::select(-lotteries,-keno_type)

all_data$n <- rowSums(all_data[,1:10])

#set new number of games played 

sub_1_plus$n <- all_data$n

# Remove those scoring less than 1 games 
all_data <- dplyr::filter(all_data, n > 0)
all_data <- dplyr::select(all_data, -n)

# Create data set for analyses 

sub_1_plus_analyses <- dplyr::filter(sub_1_plus, n > 0)







