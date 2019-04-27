library(DBI)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)


EuropeanSoccer <- dbConnect(RSQLite::SQLite(), "EuropeanSoccer.sqlite")

tables<-dbListTables(EuropeanSoccer)
Leagues <- dbReadTable(EuropeanSoccer,"League")
#dbListFields(EuropeanSoccer,"Match")

#res <-  (dbGetQuery(EuropeanSoccer, "SELECT * FROM Match WHERE goal > 100"))

MatchTable <- tbl_df(dbGetQuery(EuropeanSoccer, "SELECT * FROM Match"))

ByLeagueID <- MatchTable %>%
  select(league_id,date,home_team_goal,away_team_goal) %>% group_by(league_id)

ByLeagueID$total_goals <- ByLeagueID$home_team_goal + ByLeagueID$away_team_goal


GoalsPerLeague  <- summarise(ByLeagueID, avg_goals_per_league = mean(total_goals))
SumGoals <- summarise(ByLeagueID, total_goals=sum(total_goals))
#by_id <- group_by(Match, league_id, add=FALSE)

names(Leagues) <- c("id", "league_id","league_name")
EuropeanLeagues <- merge(GoalsPerLeague, Leagues, by="league_id")

EuropeanLeagues$id <- NULL

AttractiveLeagues <- (EuropeanLeagues[c(2,4,5,10),c("avg_goals_per_league", "league_name")])

p <-ggplot(AttractiveLeagues, aes(league_name, avg_goals_per_league))
p +geom_bar(stat = "identity")

#1.b



ByLeagueID$EUidentifier<- ifelse(ByLeagueID$league_id == c(1729)
                                 | ByLeagueID$league_id == c(7809)
                                 | ByLeagueID$league_id == c(10257)
                                 |ByLeagueID$league_id== c(21518),
                                 ByLeagueID$EUidentifier<- 1,ByLeagueID$EUidentifier<- 0)

ByLeagueID <- ByLeagueID %>%
  select(league_id,date, home_team_goal, away_team_goal, total_goals, EUidentifier) %>% group_by(EUidentifier)
Compare<- ByLeagueID %>% summarise(avg = mean(total_goals), 
                         med= median(total_goals),
                         StandardDeviation= sd(total_goals),
                         Variance = var(total_goals),
                         RangeOfGoals = diff(range(total_goals)),
                         InterQuartileRange = IQR(total_goals))




#2
boxplot(ByLeagueID$home_team_goal, ByLeagueID$away_team_goal)


#3



#4

ByLeagueID$year <- as.Date(ByLeagueID$date)
ByLeagueID$year <- format(as.Date(ByLeagueID$year, format="%d/%m/%Y"),"%Y")

Top4Leagues <-  subset(ByLeagueID, ByLeagueID$EUidentifier!=0)

Top4Leagues <- Top4Leagues %>%
  select(league_id,date, home_team_goal, away_team_goal, total_goals, EUidentifier,year) %>% group_by(year)

AvgGoalsScoredPY <- Top4Leagues %>% summarise(avg_goals_scored = mean(total_goals))

plot_ly(AvgGoalsScoredPY, labels = ~AvgGoalsScoredPY$year, values = ~AvgGoalsScoredPY$avg_goals_scored,  type = 'pie', hole = 0.6,
        domain = list(x = c(0.5, 1)))

#5

HomeTeamPoss <-MatchTable %>%
  select(league_id,date,home_team_goal,home_team_possession )
  
HomeTeamPossWithoutNa <- subset(HomeTeamPoss, ! is.na(HomeTeamPoss$home_team_possession))
MeanHomeTeamPoss <- mean(HomeTeamPossWithoutNa$home_team_possession)

attach(HomeTeamPossWithoutNa)
par(mfrow=c(2,1))

d <- density(HomeTeamPossWithoutNa$home_team_possession)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue") 

qqnorm(HomeTeamPossWithoutNa$home_team_possession, pch = 1, frame = FALSE)
qqline(HomeTeamPossWithoutNa$home_team_possession, col = "steelblue", lwd = 2)



#6

  very_low_possession <- HomeTeamPossWithoutNa %>% filter(home_team_possession <= 25)  %>% select(very_low_possession = home_team_goal)
  low_possession <- HomeTeamPossWithoutNa %>% filter(25 < home_team_possession & home_team_possession <= 50) %>% select(low_possession = home_team_goal)
  high_possession <- HomeTeamPossWithoutNa %>% filter(50 < home_team_possession & home_team_possession <= 75) %>% select(high_possession = home_team_goal)
  very_high_possession <- HomeTeamPossWithoutNa %>% filter(75 < home_team_possession) %>% select(very_high_possession = home_team_goal)
  c(very_low_possession,low_possession,high_possession,very_high_possession)
  boxplot(c(very_low_possession,low_possession,high_possession,very_high_possession))

  
  
  
  
  
  
  
  #table(MatchTable$league_id)