# The Analysis of the data found and combined

#Read into the file the .csv files from the working directory
rm(list=ls()) 
library(readxl)
library(tidyverse)
library(lme4)
library("broom")
library(cluster)
library(factoextra)
require(neuralnet)
library(party)
setwd( "/Users/denisebradford/Documents/nflstatistics") 

#Read all of the datasets that we need
total<-read.csv("total_def_data.csv", header = TRUE)

#Only read these in if necessary to pull data that has been deleted in this program
season_data<-read.csv('seasontotals_1318.csv', header = TRUE)
combine_data<-read.csv('combine_all_data.csv', header = TRUE)

load("sheets unloaded.RData")
players$nameFull <- paste(abbreviate(word(players$nameFull, 1, 1), 1, strict = TRUE), word(players$nameFull, 2, -1), sep = ".")
names(players)[names(players) == "nameFull"] <- "player_name"
a <- merge(season2017_def, players, all=TRUE) 
b <- merge(season2018_def, players, all=TRUE)
ab <- merge(a,b, all=TRUE)
ab_basic<-ab[which(ab$position == c("DE", "DT", "MLB", "DB", "SS", "SAF", "ILB", "LB", "CB","DL","OLB","LS","FS","NT")),]
ab_basic<-ab_basic[which(ab_basic$playerId > 20000000),]


#Let's Drop unnecessary columns that we don't need to determine the best Defensive Team

keep.cols <-c("player_name", "nameFirst", "nameLast","season", "pass_defended","total_intercept","total_fumble_forced",
              "total_assist_tackle", "total_sack", "total_solo_tackle",  "position","posteam", "posteam_type", "defteam",
              "total_pos_time_in_minutes_per_game")
finaldata<-ab_basic %>%
  select(one_of(keep.cols)) 

write.csv(finaldata, 'best_data.csv')

#START HERE when restart
finaldata<-read.csv('best_data.csv', header = TRUE)

#break up the datasets so that we can get the best clustering in the position type 
#Defensive Backs (Safeties and Corner backs)
defensive_backs <- finaldata %>%  #finaldata
  filter(position %in% c('DB','S')) %>%
  mutate_if(is.numeric, funs(replace_na(., 0))) %>%
  mutate(season_fantasy_points = 1*total_solo_tackle + 1*pass_defended + 3*total_intercept + 3*total_fumble_forced + 0.5*total_assist_tackle + 2*total_sack)

defensive_backs <- filter(defensive_backs, season > 0)


defensive_backs <- defensive_backs %>%
  group_by(player_name, nameFirst, nameLast, season, position, defteam) %>%
  summarise_all(funs(mean)) %>%
  select(-one_of(c("posteam", "posteam_type", "X"))) #,"College", "Height", "Drafted (tm/rnd/yr)", "Pos","Player","School"

defensive_backs_2018 <- defensive_backs %>%
  filter(season == 2018) 

  defensive_backs_2018$best = if_else((defensive_backs_2018$season_fantasy_points >= quantile(defensive_backs$season_fantasy_points,.8,.8)
                        & defensive_backs_2018$total_pos_time_in_minutes_per_game <= quantile(defensive_backs$total_pos_time_in_minutes_per_game,.8,.8)), 1,0)


apply(defensive_backs_2018,2,function(x) sum(is.na(x)))

#scale data
scaleddata<-scale(defensive_backs_2018[,c(7:15)])

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- as.data.frame(lapply(defensive_backs_2018[ ,c(7:14)], normalize))

defensive_backs_2018[ ,c(7:14)] = maxmindf[ ,c(1:8)] 

#Develop a training dataset
# Training and Test Data
trainset <- defensive_backs_2018[1:111, ]
testset <- defensive_backs_2018[112:223, ]

# fit neural network
nn=neuralnet(best ~ total_solo_tackle + pass_defended + total_intercept + total_fumble_forced + total_assist_tackle + total_sack,
             data=trainset, hidden=c(3,3), linear.output=F, threshold=0.01)
nn$result.matrix
plot(nn)

#Test the resulting output
temp_test <- subset(testset, select = c("total_solo_tackle","pass_defended","total_intercept",
                                        "total_fumble_forced","total_assist_tackle","total_sack"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testset$best, prediction = nn.results$net.result)

roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

pred <- predict(nn, testset)
table(pred[, 1] > 0.5)

res <- compute(nn, defensive_backs_2018[,-c(1:6,15)])
summary(res)
#Ultimately, we yield an 68.75% (77/112) accuracy rate in determining whether a DB is the best or not.

#Predicting the best from combine Data
combine_backs <- combinedata_2019 %>%  #finaldata
  filter(Pos %in% c('DB','S','CB','FS','SS')) %>%
  select(-one_of(c("AV", "College", "Height"))) 

#Defensive End and Defensive Tackle
defensive_end <- finaldata %>% 
  filter(position %in% c('DE','DT','NT')) %>%
  mutate_if(is.numeric, funs(replace_na(., 0))) %>%
  mutate(season_fantasy_points = 1*total_solo_tackle + 1*pass_defended + 3*total_intercept + 3*total_fumble_forced + 0.5*total_assist_tackle + 2*total_sack)

defensive_end <- filter(defensive_end, season > 0)


defensive_end <- defensive_end %>%
  group_by(player_name, nameFirst, nameLast, season, position, defteam) %>%
  summarise_all(funs(mean)) %>%
  select(-one_of(c("posteam", "posteam_type", "X"))) 

defensive_end_2018 <- defensive_end %>%
  filter(season == 2018) 

defensive_end_2018$best = if_else((defensive_end_2018$season_fantasy_points >= quantile(defensive_end$season_fantasy_points,.8,.8)
                                     & defensive_end_2018$total_pos_time_in_minutes_per_game <= quantile(defensive_end$total_pos_time_in_minutes_per_game,.8,.8)), 1,0)


apply(defensive_end_2018,2,function(x) sum(is.na(x)))

#scale data
scaleddata<-scale(defensive_end_2018[,c(7:15)])

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- as.data.frame(lapply(defensive_end_2018[ ,c(7:12)], normalize))

defensive_end_2018[ ,c(7:12)] = maxmindf[ ,c(1:6)] 

#Develop a training dataset
# Training and Test Data
trainset <- defensive_end_2018[1:114, ]
testset <- defensive_end_2018[115:228, ]

# fit neural network for logistic regression approach
nn=neuralnet(best ~ total_solo_tackle + pass_defended + total_intercept + total_fumble_forced + total_assist_tackle + total_sack,
             data=trainset, hidden=c(3,3), linear.output=TRUE, threshold=0.01)
nn$result.matrix
plot(nn)


#Test the resulting output
temp_test <- subset(testset, select = c("total_solo_tackle","pass_defended","total_intercept",
                                        "total_fumble_forced","total_assist_tackle","total_sack" 
                                        ))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testset$best, prediction = nn.results$net.result)
#results <- data.frame(actual = testset$season_fantasy_points, prediction = nn.results$net.result)

roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

pred <- predict(nn, testset)
table(pred[, 1] > 0.5)

#Ultimately, we yield an 64.04% (73/114) accuracy rate in determining whether a DE is the best or not.


#Linebacker Dataset
linebacker <- finaldata %>% 
  filter(position %in% c('LB','ILB','OLB')) %>%
  mutate_if(is.numeric, funs(replace_na(., 0))) %>%
  mutate(season_fantasy_points = 1*total_solo_tackle + 1*pass_defended + 3*total_intercept + 3*total_fumble_forced + 0.5*total_assist_tackle + 2*total_sack)

linebacker <- filter(linebacker, season > 0)


linebacker <- linebacker %>%
  group_by(player_name, nameFirst, nameLast, season, position, defteam) %>%
  summarise_all(funs(mean)) %>%
  select(-one_of(c("posteam", "posteam_type", "X"))) 

linebacker_2018 <- linebacker %>%
  filter(season == 2018) 

linebacker_2018$best = if_else((linebacker_2018$season_fantasy_points >= quantile(linebacker$season_fantasy_points,.8,.8)
                                     & linebacker_2018$total_pos_time_in_minutes_per_game <= quantile(linebacker$total_pos_time_in_minutes_per_game,.8,.8)), 1,0)


apply(linebacker_2018,2,function(x) sum(is.na(x)))

#scale data
scaleddata<-scale(linebacker_2018[,c(7:15)])

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- as.data.frame(lapply(linebacker_2018[ ,c(7:14)], normalize))

linebacker_2018[ ,c(7:14)] = maxmindf[ ,c(1:8)] 

#Develop a training dataset
# Training and Test Data
trainset <- linebacker_2018[1:111, ]
testset <- linebacker_2018[112:222, ]

# fit neural network
nn=neuralnet(best ~ total_solo_tackle + pass_defended + total_intercept + total_fumble_forced + total_assist_tackle + total_sack,
             data=trainset, hidden=3, linear.output=T, threshold=0.01)
nn$result.matrix
plot(nn)

#Test the resulting output
temp_test <- subset(testset, select = c("total_solo_tackle","pass_defended","total_intercept",
                                        "total_fumble_forced","total_assist_tackle","total_sack"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testset$best, prediction = nn.results$net.result)

roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

pred <- predict(nn, testset)
table(pred[, 1] > 0.5)
#Ultimately, we yield an 74.74% (83/111) accuracy rate in determining whether a LB is the best or not.

