library(caTools)
library(party)
library(randomForest)
setwd( "/Users/denisebradford/Documents/nflstatistics") 

combinedata_2019<-read_xls('combinedata_2019.xls', col_names = TRUE)
combinedata_2018<-read_xls('combinedata_2018.xls', col_names = TRUE)
combinedata_2017<-read_xls('combinedata_2017.xls', col_names = TRUE)
combinedata_2016<-read_xls('combinedata_2016.xls', col_names = TRUE)
combinedata_2015<-read_xls('combinedata_2015.xls', col_names = TRUE)
combinedata_2014<-read_xls('combinedata_2014.xls', col_names = TRUE)
combinedata_2013<-read_xls('combinedata_2013.xls', col_names = TRUE)
combinedata_2012<-read_xls('combinedata_2012.xls', col_names = TRUE)
combinedata_2011<-read_xls('combinedata_2011.xls', col_names = TRUE)
combinedata_2010<-read_xls('combinedata_2010.xls', col_names = TRUE)
combinedata_2009<-read_xls('combinedata_2009.xls', col_names = TRUE)
combinedata_2008<-read_xls('combinedata_2008.xls', col_names = TRUE)
combinedata_2007<-read_xls('combinedata_2007.xls', col_names = TRUE)
combinedata_2006<-read_xls('combinedata_2006.xls', col_names = TRUE)
combinedata_2005<-read_xls('combinedata_2005.xls', col_names = TRUE)

all_combine_data<-rbind(combinedata_2005,combinedata_2006,combinedata_2007,combinedata_2008,combinedata_2009,
                        combinedata_2010,combinedata_2011,combinedata_2012,combinedata_2013,combinedata_2014,
                        combinedata_2015,combinedata_2016,combinedata_2017,combinedata_2018,combinedata_2019)

all_combine_data$Height=substring(all_combine_data$Height, 6)

all_combine_data$nameFirst<-word(all_combine_data$Player, 1, 1) #, 1, strict = TRUE) 
all_combine_data$nameLast<-word(all_combine_data$Player, 2, -1)

a<-merge(finaldata,all_combine_data, all = T)
b<-a[which(!is.na(a$`Drafted (tm/rnd/yr)`)),]

keep.cols <-c("player_name", "nameFirst", "nameLast","season", "pass_defended","total_intercept","total_fumble_forced",
              "total_assist_tackle", "total_sack", "total_solo_tackle",  "position","posteam", "posteam_type", "defteam",
              "total_pos_time_in_minutes_per_game","Age","Height","Wt","40YD","Vertical",
              "BenchReps","Broad Jump","3Cone","Shuttle")
a<-a %>%
  select(one_of(keep.cols)) 

#Random Forest Analysis
dim(a)
summary(a)

a_backs <- a %>%  #finaldata
  filter(position %in% c('DB','S')) %>%
  mutate_if(is.numeric, funs(replace_na(., 0))) %>%
  mutate(season_fantasy_points = 1*total_solo_tackle + 1*pass_defended + 3*total_intercept + 3*total_fumble_forced + 0.5*total_assist_tackle + 2*total_sack)

a_backs <- filter(a_backs, season > 0)

a_backs <- a_backs %>%
  group_by(player_name, nameFirst, nameLast, season, position, defteam) %>%
  summarise_all(funs(mean)) %>%
  select(-one_of(c("posteam", "posteam_type", "X", "College"))) #,"College", "Height", "Drafted (tm/rnd/yr)", "Pos","Player","School"

a_backs_2018 <- a_backs %>%
  filter(season == 2018) 

a_backs_2018$best = if_else((a_backs_2018$season_fantasy_points >= quantile(a_backs$season_fantasy_points,.8,.8)
                       & a_backs_2018$total_pos_time_in_minutes_per_game <= quantile(a_backs$total_pos_time_in_minutes_per_game,.8,.8)), 0,1)



sapply(a_backs_2018, class)
summary(a_backs_2018)

a_backs_2018 <- transform(
  a_backs_2018,
  player_name=as.factor(player_name),
  nameFirst=as.factor(nameFirst),
  nameLast=as.factor(nameLast),
  season=as.integer(season),
  position=as.factor(position),
  defteam=as.factor(defteam),
  pass_defended=as.integer(pass_defended),
  total_intercept=as.integer(total_intercept),
  total_fumble_forced=as.integer(total_fumble_forced),
  total_assist_tackle=as.integer(total_assist_tackle),
  total_sack=as.integer(total_sack),
  total_solo_tackle=as.numeric(total_solo_tackle),
  total_pos_time_in_minutes_per_game=as.numeric(total_pos_time_in_minutes_per_game),
  Wt=as.numeric(Wt),
  X40YD =as.numeric(X40YD),
  Vertical=as.numeric(Vertical),
  BenchReps=as.numeric(BenchReps),
  Broad.Jump=as.numeric(Broad.Jump),
  X3Cone=as.numeric(X3Cone),
  Shuttle=as.numeric(Shuttle),
  season_fantasy_points=as.numeric(season_fantasy_points),
  best=as.factor(best)
)

colSums(is.na(a_backs_2018))

a_backs_2018 <- a_backs_2018 %>% select(-one_of(c("Player","Pos","School", "Height")))
colSums(is.na(a_backs_2018))



sample = sample.split(a_backs_2018$best, SplitRatio = 3/4)
train = subset(a_backs_2018, sample == TRUE)
test  = subset(a_backs_2018, sample == FALSE)
dim(train)
dim(test)

set.seed(131)
rf <- randomForest(
  best ~ X40YD + Vertical + BenchReps + Broad.Jump + X3Cone + Shuttle,
  data=train, na.action=na.omit
)

rf

plot(rf)
varImpPlot(rf)

pred = predict(rf, newdata=test[-23])

cm = table(test[,23] , pred)
cm #About a 67% correct

plot(margin(rf,test$best))

getTree(rf, 1, labelVar=TRUE)

#Predicting the best from combine Data
combine_backs <- combinedata_2019 %>%  #finaldata
  filter(Pos %in% c('DB','S','CB','FS','SS')) %>%
  select(-one_of(c("AV", "College", "Height"))) 

sapply(combine_backs, class)
summary(combine_backs)

combine_backs <- transform(
combine_backs,
Player=as.factor(Player),
Wt=as.numeric(Wt),
X40YD =as.numeric(`40YD`),
Vertical=as.numeric(Vertical),
BenchReps=as.numeric(BenchReps),
Broad.Jump=as.numeric(`Broad Jump`),
X3Cone=as.numeric(`3Cone`),
Shuttle=as.numeric(Shuttle))

pred = predict(rf, newdata=combine_backs)

cm = table(combine_backs[,1], pred)
cm 

cforest(best ~ ., data=combine_backs, controls=cforest_control(mtry=2, mincriterion=0))


#Defensive End and Defensive Tackle
a_end <- a %>% 
  filter(position %in% c('DE','DT','NT')) %>%
  mutate_if(is.numeric, funs(replace_na(., 0))) %>%
  mutate(season_fantasy_points = 1*total_solo_tackle + 1*pass_defended + 3*total_intercept + 3*total_fumble_forced + 0.5*total_assist_tackle + 2*total_sack)

a_end <- filter(a_end, season > 0)


a_end <- a_end %>%
  group_by(player_name, nameFirst, nameLast, season, position, defteam) %>%
  summarise_all(funs(mean)) %>%
  select(-one_of(c("posteam", "posteam_type", "X"))) 

a_end_2018 <- a_end %>%
  filter(season == 2018) 

a_end_2018$best = if_else((a_end_2018$season_fantasy_points >= quantile(a_end$season_fantasy_points,.8,.8)
                             & a_end_2018$total_pos_time_in_minutes_per_game <= quantile(a_end$total_pos_time_in_minutes_per_game,.8,.8)), 0,1)

sapply(a_end_2018, class)
summary(a_end_2018)

a_end_2018 <- transform(
  a_end_2018,
  player_name=as.factor(player_name),
  nameFirst=as.factor(nameFirst),
  nameLast=as.factor(nameLast),
  season=as.integer(season),
  position=as.factor(position),
  defteam=as.factor(defteam),
  pass_defended=as.integer(pass_defended),
  total_intercept=as.integer(total_intercept),
  total_fumble_forced=as.integer(total_fumble_forced),
  total_assist_tackle=as.integer(total_assist_tackle),
  total_sack=as.integer(total_sack),
  total_solo_tackle=as.numeric(total_solo_tackle),
  total_pos_time_in_minutes_per_game=as.numeric(total_pos_time_in_minutes_per_game),
  Wt=as.numeric(Wt),
  X40YD =as.numeric(X40YD),
  Vertical=as.numeric(Vertical),
  BenchReps=as.numeric(BenchReps),
  Broad.Jump=as.numeric(Broad.Jump),
  X3Cone=as.numeric(X3Cone),
  Shuttle=as.numeric(Shuttle),
  season_fantasy_points=as.numeric(season_fantasy_points),
  best=as.factor(best)
)

colSums(is.na(a_end_2018))

a_end_2018 <- a_end_2018 %>% select(-one_of(c("Player","Pos","School", "Height")))
colSums(is.na(a_end_2018))

sample2 = sample.split(a_end_2018$best, SplitRatio = 3/4)
train2 = subset(a_end_2018, sample == TRUE)
test2  = subset(a_end_2018, sample == FALSE)
dim(train2)
dim(test2)

set.seed(1112)
rf2 <- randomForest(
  best ~ X40YD + Vertical + BenchReps + Broad.Jump + X3Cone + Shuttle,
  data=train2, na.action=na.omit
)

rf2

plot(rf2)
varImpPlot(rf2)

pred2 = predict(rf2, newdata=test2[-23])

cm2 = table(test2[,23] , pred2)
cm2 #About a 62% correct

plot(margin(rf2,test2$best))

getTree(rf2, 1, labelVar=TRUE)

#Linebacker Dataset
a_line <- a %>% 
  filter(position %in% c('LB','ILB','OLB')) %>%
  mutate_if(is.numeric, funs(replace_na(., 0))) %>%
  mutate(season_fantasy_points = 1*total_solo_tackle + 1*pass_defended + 3*total_intercept + 3*total_fumble_forced + 0.5*total_assist_tackle + 2*total_sack)

a_line <- filter(a_line, season > 0)


a_line <- a_line %>%
  group_by(player_name, nameFirst, nameLast, season, position, defteam) %>%
  summarise_all(funs(mean)) %>%
  select(-one_of(c("posteam", "posteam_type", "X"))) 

a_line_2018 <- a_line %>%
  filter(season == 2018) 

a_line_2018$best = if_else((a_line_2018$season_fantasy_points >= quantile(a_line$season_fantasy_points,.8,.8)
                             & a_line_2018$total_pos_time_in_minutes_per_game <= quantile(a_line$total_pos_time_in_minutes_per_game,.8,.8)), 0,1)

sapply(a_line_2018, class)
summary(a_line_2018)

a_line_2018 <- transform(
  a_line_2018,
  player_name=as.factor(player_name),
  nameFirst=as.factor(nameFirst),
  nameLast=as.factor(nameLast),
  season=as.integer(season),
  position=as.factor(position),
  defteam=as.factor(defteam),
  pass_defended=as.integer(pass_defended),
  total_intercept=as.integer(total_intercept),
  total_fumble_forced=as.integer(total_fumble_forced),
  total_assist_tackle=as.integer(total_assist_tackle),
  total_sack=as.integer(total_sack),
  total_solo_tackle=as.numeric(total_solo_tackle),
  total_pos_time_in_minutes_per_game=as.numeric(total_pos_time_in_minutes_per_game),
  Wt=as.numeric(Wt),
  `40YD` =as.numeric(`40YD`),
  Vertical=as.numeric(Vertical),
  BenchReps=as.numeric(BenchReps),
  `Broad Jump`=as.numeric(`Broad Jump`),
  `3Cone`=as.numeric(`3Cone`),
  Shuttle=as.numeric(Shuttle),
  season_fantasy_points=as.numeric(season_fantasy_points),
  best=as.factor(best)
)

colSums(is.na(a_line_2018))

a_line_2018 <- a_line_2018 %>% select(-one_of(c("Player","Pos","School", "Height")))
colSums(is.na(a_line_2018))

sample3 = sample.split(a_line_2018$best, SplitRatio = 3/4)
train3 = subset(a_line_2018, sample == TRUE)
test3  = subset(a_line_2018, sample == FALSE)
dim(train3)
dim(test3)

set.seed(321)
rf3 <- randomForest(
  best ~ X40YD + Vertical + BenchReps + Broad.Jump + X3Cone + Shuttle,
  data=train3, na.action=na.omit
)

rf3

plot(rf3)
varImpPlot(rf3)

pred3 = predict(rf3, newdata=test3[-23])

cm3 = table(test3[,23] , pred3)
cm3 #About a 71% correct

plot(margin(rf3,test3$best))

getTree(rf3, 1, labelVar=TRUE)
