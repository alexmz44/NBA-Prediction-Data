setwd("C:/Users/alexa/OneDrive/Documents/ECON MASTERS/ECON511A_ECONOMETRICS")
NBA_Games <- read.csv(file ="games.csv", header = T)
head(NBA_Games)
NBA.df <- data.frame(NBA_Games)

head(NBA.df)
## renaming all of the names by their code
NBA.df[NBA.df == "1610612737"] <- "ATL"
NBA.df[NBA.df == "1610612738"] <- "BOS"
NBA.df[NBA.df == "1610612740"] <- "NO"
NBA.df[NBA.df == "1610612741"] <- "CHI"
NBA.df[NBA.df == "1610612742"] <- "DAL"
NBA.df[NBA.df == "1610612743"] <- "DEN"
NBA.df[NBA.df == "1610612745"] <- "HOU"
NBA.df[NBA.df == "1610612746"] <- "LAC"
NBA.df[NBA.df == "1610612747"] <- "LAL"
NBA.df[NBA.df == "1610612748"] <- "MIA"
NBA.df[NBA.df == "1610612749"] <- "MIL"
NBA.df[NBA.df == "1610612750"] <- "MIN"
NBA.df[NBA.df == "1610612751"] <- "BKN"
NBA.df[NBA.df == "1610612752"] <- "NYK"
NBA.df[NBA.df == "1610612753"] <- "ORL"
NBA.df[NBA.df == "1610612754"] <- "IND"
NBA.df[NBA.df == "1610612755"] <- "PHI"
NBA.df[NBA.df == "1610612756"] <- "PHX"
NBA.df[NBA.df == "1610612757"] <- "POR"
NBA.df[NBA.df == "1610612758"] <- "SAC"
NBA.df[NBA.df == "1610612759"] <- "SAS"
NBA.df[NBA.df == "1610612760"] <- "OKC"
NBA.df[NBA.df == "1610612761"] <- "TOR"
NBA.df[NBA.df == "1610612762"] <- "UTA"
NBA.df[NBA.df == "1610612763"] <- "MEM"
NBA.df[NBA.df == "1610612764"] <- "WAS"
NBA.df[NBA.df == "1610612765"] <- "DET"
NBA.df[NBA.df == "1610612766"] <- "CHA"
NBA.df[NBA.df == "1610612739"] <- "CLE"
NBA.df[NBA.df == "1610612744"] <- "GSW"

head(NBA.df)
tail(NBA.df)
library(tidyverse)
install.packages("dplyr")
games$SEASON

####### This is a MLR that is made with the averages of the home
####### teams only to see what may affect a team's winning percentage
########

library(dplyr)
### I figured that this was a classification problem
games_home <- NBA.df %>%
  group_by(SEASON, TEAM_ID_home) %>%
  summarise(avg_points = mean(PTS_home, na.rm = T),
            home_team_wins = HOME_TEAM_WINS,
            avg_fgpct_home = mean(FG_PCT_home, na.rm = T),
            avg_ftpct_home = mean(FT_PCT_home, na.rm = T),
            avg_fg3pct_home = mean(FG3_PCT_home, na.rm = T),
            avg_ast_home = mean(AST_home, na.rm = T),
            avg_reb_home = mean(REB_home, na.rm = T),
            observations = n())
games_home
head(games_home)
games.home.df <- data.frame(games_home)

# I want to see the plots of each of the variables
# they look like they all have pretty even distributions
games.home.df %>% 
  select(-home_team_wins)%>%
  keep(is.numeric)%>%
  gather()%>%
  ggplot()+
  geom_histogram(mapping = aes(x= value , fill = key), color = "black")+
  facet_wrap(~key, scales = "free")
## summary of all the variables 
summary(games.home.df)
games.home.df <- games.home.df %>% select(-observations)
# partition the dataset 
set.seed(1234)
sample_set <- sample(nrow(games.home.df), .75*round(nrow(games.home.df)), replace = F)
home_games_train<- games.home.df[sample_set,]
home_games_test<- games.home.df[-sample_set,]

# check the class distribution of the data to see that is is all the same 
# all distributions are about the same and the majority / minority class distribution are pretty close
# we must be wary of this bias and skewed model results
round(prop.table(table(select(games.home.df, home_team_wins), exclude = NULL)),4) * 100
round(prop.table(table(select(home_games_train, home_team_wins), exclude = NULL)),4) * 100
round(prop.table(table(select(home_games_test, home_team_wins), exclude = NULL)),4) * 100


# train the model 
home_model <- home_games_train %>%
  glm(formula = home_team_wins ~., family = binomial)
# summary of model
summary(home_model)


# Predictions against the test data 
home_pred1 <- predict(home_model, home_games_test, type = 'response')
head(home_pred1)

# create optimal cutoff point
library(InformationValue)
ideal_cutoff <- optimalCutoff(
  actuals = home_games_test$home_team_wins,
  predictedScores = home_pred1,
  optimiseFor = "Both"
)

ideal_cutoff

home_pred1 <- ifelse(home_pred1 >= ideal_cutoff,1 ,0)

head(home_pred1)

# confusion matrix
home_pred1.table <- table(home_games_test$home_team_wins, home_pred1)
sum(diag(home_pred1))/nrow(home_games_test)

#predictive accuracy of about 48%%


















Y_winprob <- games.df$win_prob
X1_avgfgpct <- games.df$avg_fgpct_home
X2_avgftpct <- games.df$avg_ftpct_home
X3_avgfg3pct <- games.df$avg_fg3pct_home
X4_avgreb <- games.df$avg_reb_home
X5_avgpts <- games.df$avg_points

NBA_Home.lm <- lm(Y_winprob ~ X1_avgfgpct + X2_avgftpct + X3_avgfg3pct  +X4_avgreb + X5_avgpts data = games.df)
NBA_Home.lm




######using some diagnostics to make sure that this 
######is a good model fit for the data

plot(resid(NBA_Home.lm) ~  fitted(NBA_Home.lm))

qqnorm(resid(NBA_Home.lm), main = "")











#########################################################################################################################################
####### Regression Model on the away team probability to win

library(dplyr)
games2 <- NBA.df %>%
  group_by(SEASON, TEAM_ID_away) %>%
  summarise(avga_points = mean(PTS_away, na.rm = T),
            win_probaway = 1 -  mean(HOME_TEAM_WINS),
            avg_fgpct_away = mean(FG_PCT_away, na.rm = T),
            avg_ftpct_away = mean(FT_PCT_away, na.rm = T),
            avg_fg3pct_away = mean(FG3_PCT_away, na.rm = T),
            avg_ast_away = mean(AST_away, na.rm = T),
            avg_reb_away = mean(REB_away, na.rm = T),
            observations = n())
games2.df <- data.frame(games2)
games2.df
YA_winprob <- games2.df$win_probaway
X1A_avgfgpct <- games2.df$avg_fgpct_away
X2A_avgftpct <- games2.df$avg_ftpct_away
X3A_avgfg3pct <- games2.df$avg_fg3pct_away
X4A_avgreb <- games2.df$avg_reb_away
X5A_avgpts <- games2.df$avga_points

NBA_Away.lm <- lm(YA_winprob ~ X1A_avgfgpct + X2A_avgftpct + X3A_avgfg3pct +X4A_avgreb + X5A_avgpts)
NBA_Away.lm
summary(NBA_Away.lm)

plot(resid(NBA_Away.lm) ~  fitted(NBA_Away.lm))

qqnorm(resid(NBA_Away.lm), main = "")

################################################################################################
### finding the averges 
mean(games.df$win_prob)

mean(games2.df$win_prob)
##########################################################################################


bind_rows(
  games %>%
    mutate(won = (HOME_TEAM_WINS == 1)) %>%
    select(1:3, 6, won, contains("HOME"), contains("home")) %>%
    rename_with(~str_remove(., "_home"), contains("home")) %>%
    mutate(home = TRUE),
  games %>%
    mutate(won = (HOME_TEAM_WINS != 1)) %>%
    select(1:3, 6, won, contains("AWAY"), contains("away")) %>%
    rename_with(~str_remove(., "_away"), contains("away")) %>%
    mutate(home = FALSE)
  )
  



newdata.df = data.frame(X1_avgfgpct= mean(X1_avgfgpct), X2_avgftpct = mean(X2_avgftpct), 
                        X3_avgfg3pct = mean(X3_avgfg3pct), X4_avgreb = mean(X4_avgreb), 
                        X5_avgpts = mean(X5_avgpts))
predict.lm(NBA_Away.lm, newdata = newdata.df, se.fit= T,
           interval = 'prediction') 

newdata.dfA = data.frame(X1A_avgfgpct= mean(X1A_avgfgpct), X2A_avgftpct = mean(X2A_avgftpct), 
                         X3A_avgfg3pct = mean(X3A_avgfg3pct), X4A_avgreb = mean(X4A_avgreb), 
                         X5A_avgpts = mean(X5A_avgpts))


newdata.df1 = data.frame(X1_avgfgpct= .55, X2_avgftpct = .9, 
                        X3_avgfg3pct = .2, X4_avgreb = 35, 
                        X5_avgpts = 118)

predict.lm(NBA_Home.lm,newdata.df) ### avg confidence interval home
predict(NBA_Away.lm,newdata.dfA, interval ="conf", level = .975) ### avg confidence away

predict.lm(NBA_Home.lm,newdata.df1, interval = "pred") ### avg confidence interval home


mean(X4_avgreb)
mean(X5A_avgpts)
    

summary(NBA_Away.lm)   
summary(NBA_Home.lm)
