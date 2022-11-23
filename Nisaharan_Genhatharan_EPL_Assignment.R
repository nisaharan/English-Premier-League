#Nisaharan Genhatharan - Final Assignment (Individual) - English Premier League Soccer Standings

# Data importing
install.packages("tidyverse")

# load required libraries
library(tidyverse)
library(lubridate)


#Function
EPL_Standings <- function(Date,Season){
  
  
  date_input <- mdy(Date) #to set all the values in the Date column to correct Date format
  
  
  ulnk <- paste(str_sub(Season,3,4), str_sub(Season,6,7),'/', sep = "")#to change season format from '2022/23' to '2223/' to incorporate the same in the url to extract the data from website correctly
  #using str_sub to cut the required four digits and pasting them to insert into url so that it can take any season that is mentioned in the function call
  #ulnk
  
  # url() to download a csv file from the internet
  EPL <- read.csv(paste("https://www.football-data.co.uk/mmz4281/", ulnk, "E0.csv", sep = ""))
  EPL_Essential <- select(EPL, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)%>% #select only the columns that are required for the project
    mutate(Date = dmy(Date)) %>%  #extracting year from Date column and add the correct format to another column called "Date_modified" to address 'YYYY' format and 'YY' format
    filter(Date <= date_input) #filter the data so that only the matches played up until the specified data remain
  #calculating all the wins, loses and draws of each team (home and away) which'll make it easy for us caluculate the requested dat in the next steps
  #ifelse(test, yes, no)
  
  EPL_Essential <- mutate(EPL_Essential, HW = ifelse(FTR == 'H',1,0), #Home team - wins
                          AW = ifelse(FTR == 'A',1,0), #Away team - wins
                          HD = ifelse(FTR == 'D',1,0), #Home team - draws
                          AD = ifelse(FTR == 'D',1,0), #Away team - draws
                          HL = ifelse(FTR == 'A',1,0), #Home team - Losses
                          AL = ifelse(FTR == 'H',1,0), #Away team - Losses
                          HP = ifelse(FTR == 'D', 1, ifelse(FTR == 'H', 3, ifelse(FTR == 'A', 0,NA))), #points earned by home team in a game
                          AP = ifelse(FTR == 'D', 1, ifelse(FTR == 'H', 0, ifelse(FTR == 'A', 3,NA)))) #points earned by away team in a game
  
  
  ######To deal with Home and Away team/goal columns - separate Home and Away df's#####
  
  #Home df for calculating data on the Home team
  home_df <- select(EPL_Essential, Date, HomeTeam, FTHG:FTR,HW,HD,HL,HP) #selecting only the required data of Home team from main table
  names(home_df)[names(home_df) == "HomeTeam"] <- "TeamName" #rename "HomeTeam" column to "TeamName"
  
  
  home_df1 <- group_by(home_df, TeamName) %>% #groupby the data by the "TeamName" for further calculation
    summarise(Total_Home_games = n(), #total games played by Home team
              HW = sum(HW), #calculating total wins of home team
              HD = sum(HD), #calculating total draws of home team
              HL = sum(HL), #calculating total losses of home team
              HP = sum(HP), #calculating total points of home team
              goals_by_home = sum(FTHG), #total goals scored by home team
              goals_against_home = sum(FTAG)) #total goals scored by other team against home team
  
  #Away df for calculating data on the Home team
  away_df <- select(EPL_Essential, Date, AwayTeam, FTHG:FTR,AW,AD,AL,AP) #selecting only the required data of Away team from main table
  names(away_df)[names(away_df) == "AwayTeam"] <- "TeamName" #rename "AwayTeam" column to "TeamName"
  
  away_df1 <- group_by(away_df, TeamName) %>% #groupby the data by the "TeamName" for further calculation
    summarise(Total_Away_games = n(), #total games played by Away team
              AW = sum(AW), #calculating total wins of Away team
              AD = sum(AD), #calculating total draws of Away team
              AL = sum(AL), #calculating total losses of Away team
              AP = sum(AP), #calculating total points of Away team
              goals_by_away = sum(FTAG), #total goals scored by away team
              goals_against_away = sum(FTHG)) #total goals scored by other team against home team
  
  #full outer join - merge
  join_df <- merge(home_df1, away_df1, by = "TeamName") #merge both Home and Away dataframes on col = "TeamName"
  
  join_df <- mutate(join_df, wins = HW + AW, #total wins
                    loses = HL + AL, #total losses
                    draws = HD + AD) #total ties
  
  final_df <- join_df %>%
    mutate(Record = paste0(wins,'-',loses,'-',draws), #record as wins-loses-ties (Record) 
           HomeRec = paste0(HW,'-',HL,'-',HD), #Home record of wins-loses-ties
           AwayRec = paste0(AW,'-',AL,'-',AD), #Away record of wins-loses-ties
           MatchesPlayed = Total_Home_games + Total_Away_games, #total matches played as home and away teams
           Points = HP + AP, #total points earned as home and away teams
           PPM = round(Points/MatchesPlayed,5), #points per match
           PtPct = round(Points/(3*MatchesPlayed),5), #point percentage = points / 3 * the number of games played
           GS = goals_by_home + goals_by_away, #total goals scored as home and away teams
           GSM = round(GS/MatchesPlayed,5), #goals scored per match
           GA = goals_against_home + goals_against_away, #total goals allowed as home and away teams
           GAM = round(GA/MatchesPlayed,5)) #goals allowed per match 
  
  
  final_df <- arrange(final_df, desc(PPM), desc(wins), desc(GSM), GAM) #arrange the data in descending format as requested
  final_df <- select(final_df, TeamName, Record:GAM) #finally selecting the desired columns to display the output
  
  
  return(final_df)
  #return(final_df) #return the final_df
  
  }

EPL_Standings("10/18/2022", "2022/23")
EPL_Standings("10/18/2021", "2021/22")
EPL_Standings("10/18/2020", "2020/21")

