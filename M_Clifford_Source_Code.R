#### Enhancing Player Performance Evaluation in The Premier League

#### Load libraries and data ####
library(dplyr)
library(elo)
library(tidyverse)
library(shiny)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

#### ELO Algorithm  definitions####

"""""

We have definitions:

Ai = Player rating for team A
R_Ai = Rating of Player Ai
R'_Ai = Updated Rating of player R_Ai after each game
R_A = Average Rating of team A
R_B = Average of team B
E_Ai = Expected player outcome
M_Ai = Player Minutes in pitch
M_Max = max mins possible for that game
D_Ai = Individuals goal difference during their time spent on the pitch
S_A = Actual Outcome Factor W/D/L = 1/0.5/0
C_Ai  = Player Rating Change

The formulas: 

with, k = 40, w = 1

E_Ai <- 1 / (1 + 10^((R_B - R_Ai)/400))

S_Ai = 1 (D_Ai > 0)
S_Ai = 0.5 (D_Ai = 0)
S_Ai = 0 (D_Ai < 0)

R_A = ((sum(R_Ai) x M_Ai)/sum(M_Ai))

C_Ai = w x (S_Ai - E_Ai) x sqrt(abs(D_Ai), 3)  ,  for when D_Ai doesnt equal 0

C_Ai = w x (S_Ai - E_Ai) x (M_Ai/M_max) , for when D_Ai equals 0

R'_Ai = R_Ai + (K x C_Ai)


"""""



#### Extracting GD, MP function to produce 'player_performance' df ####

calculate_performance <- function(match_data) {
  player_performance <- data.frame() ## Create empty df for results
  
  unique_match_ids <- unique(match_data$matchID) ## Get unique match IDs
  
  for (match in unique_match_ids) {. ## Iterate over each match
    match_subset <- match_data[match_data$matchID == match,] ## Get match data
    
    home_players <- unique(unlist(match_subset[paste0("H_XI.", 1:11)])) ## Get player IDs fot home players
    away_players <- unique(unlist(match_subset[paste0("A_XI.", 1:11)])) ## away players
    
    for (player in home_players) { ## Over players find min and last instance in their match to find mins played
      if(player != 0){
        player_rows <- which(match_subset == player, arr.ind = TRUE)
        player_row_indices <- player_rows[, 1]
        min_time <- min(match_subset[player_row_indices, 'Event.Time'])
        max_time <- max(match_subset[player_row_indices, 'Event.Time'])
        minutes_played <- max_time - min_time
        
        first_instance <- min(player_row_indices)
        last_instance <- max(player_row_indices)
        goal_diff <- (match_subset[last_instance, 'H_goals'] - match_subset[first_instance, 'H_goals']) - ## Calculate GD of that player in mins played
          (match_subset[last_instance, 'A_goals'] - match_subset[first_instance, 'A_goals'])
        
        player_performance <- rbind(player_performance, 
                                    data.frame(matchID = match, playerID = player, GD = goal_diff, Minutes_played = minutes_played)) ## Add to player performance df
      }
    }
    
    for (player in away_players) { ## Now same for away players
      if(player != 0){
        player_rows <- which(match_subset == player, arr.ind = TRUE)
        player_row_indices <- player_rows[, 1]
        min_time <- min(match_subset[player_row_indices, 'Event.Time'])
        max_time <- max(match_subset[player_row_indices, 'Event.Time'])
        minutes_played <- max_time - min_time
        
        first_instance <- min(player_row_indices)
        last_instance <- max(player_row_indices)
        goal_diff <- (match_subset[last_instance, 'A_goals'] - match_subset[first_instance, 'A_goals']) -
          (match_subset[last_instance, 'H_goals'] - match_subset[first_instance, 'H_goals'])
        
        player_performance <- rbind(player_performance, 
                                    data.frame(matchID = match, playerID = player, GD = goal_diff, Minutes_played = minutes_played))
      }
    }
  }
  
  return(player_performance)
}

## This function produces a df based on the format of our matches data for each player in each games minutes played, GD, matchID, playerID


#### Elo function ####

Elo_function <- function(player_performance, player_ratings, matches_XI, w, K) { ## Matches_XI will be specific for each season
  initiall_ratings <- player_ratings  ##Want to use initial ratings for a fair playing field
  
  for (i in 1:nrow(player_performance)) { ## Loop over player performance, extracting matchID and GD and MP
    matchID <- player_performance$matchID[i]
    playerID <- player_performance$playerID[i]
    GD <- player_performance$GD[i]
    Minutes_played <- player_performance$Minutes_played[i]
    
    H_ID <- matches_XI$H_ID[matches_XI$matchID == matchID]  ## Get the home and away team IDs for the given matchID
    A_ID <- matches_XI$A_ID[matches_XI$matchID == matchID]
    
    H_XI_ratings <- player_ratings$Rating[player_ratings$playerID %in% matches_XI$H_XI.1[H_ID] |  ## Get the average ratings for the home and away teams from matches_XI df
                                            player_ratings$playerID %in% matches_XI$H_XI.2[H_ID] |
                                            player_ratings$playerID %in% matches_XI$H_XI.3[H_ID] |
                                            player_ratings$playerID %in% matches_XI$H_XI.4[H_ID] |
                                            player_ratings$playerID %in% matches_XI$H_XI.5[H_ID] |
                                            player_ratings$playerID %in% matches_XI$H_XI.6[H_ID] |
                                            player_ratings$playerID %in% matches_XI$H_XI.7[H_ID] |
                                            player_ratings$playerID %in% matches_XI$H_XI.8[H_ID] |
                                            player_ratings$playerID %in% matches_XI$H_XI.9[H_ID] |
                                            player_ratings$playerID %in% matches_XI$H_XI.10[H_ID] |
                                            player_ratings$playerID %in% matches_XI$H_XI.11[H_ID]]
    
    R_A <- mean(H_XI_ratings)
    
    A_XI_ratings <- player_ratings$Rating[player_ratings$playerID %in% matches_XI$A_XI.1[A_ID] |
                                            player_ratings$playerID %in% matches_XI$A_XI.2[A_ID] |
                                            player_ratings$playerID %in% matches_XI$A_XI.3[A_ID] |
                                            player_ratings$playerID %in% matches_XI$A_XI.4[A_ID] |
                                            player_ratings$playerID %in% matches_XI$A_XI.5[A_ID] |
                                            player_ratings$playerID %in% matches_XI$A_XI.6[A_ID] |
                                            player_ratings$playerID %in% matches_XI$A_XI.7[A_ID] |
                                            player_ratings$playerID %in% matches_XI$A_XI.8[A_ID] |
                                            player_ratings$playerID %in% matches_XI$A_XI.9[A_ID] |
                                            player_ratings$playerID %in% matches_XI$A_XI.10[A_ID] |
                                            player_ratings$playerID %in% matches_XI$A_XI.11[A_ID]]
    
    R_B <- mean(A_XI_ratings)
    
    R_Ai <- player_ratings$Rating[player_ratings$playerID == playerID]   ## Players current rating
    
    M_Max <- matches_XI$M_Max[matches_XI$matchID == matchID] ## Max minutes for the given matchID
    
    E_Ai <- 1 / (1 + 10^((R_B - R_Ai) / 400))  ## Calculate Expected player outcome
    
    S_Ai <- ifelse(GD > 0, 1, ifelse(GD == 0, 0.5, 0)) ## Calculate Actual Outcome Factor
    
    if (GD != 0) {     ## Calculate Player Rating Change
      C_Ai <- w * (S_Ai - E_Ai) * abs(GD)^(1/3)
    } else {
      C_Ai <- w * (S_Ai - E_Ai) * (Minutes_played / M_Max)
    }
    
    R_Ai_updated <- R_Ai + (K * C_Ai) ## Update the players rating 
    
    player_ratings$Rating[player_ratings$playerID == playerID] <- R_Ai_updated ## Update the player_ratings data frame with the players new rating
  }
  
  return(player_ratings)
}

#### NOTE : each season was calculated separately as initially i only had one season 
#### of data to implement an algorithm so i repeated this process and combined my results at the end

#### Important note: I gathered ratings by individual seasons and combined them at the end
#### As I found a method of doing it when I had just one season of data, I combined at the end
#### Season 13/14 ####

## Read season

matches13_14 <- read.csv("matches13_14.csv")
matches13_14

## load full players 
players<- read.csv("full_players.csv")
matches<- matches13_14


## Create df for player ratings
(player_ids <- unique(unlist(matches[, paste0(rep(c("H_XI.", "A_XI."), each = 11), 1:11)])))
## Creates new dataframe ratings
(ratings <- data.frame(Player_ID = player_ids, Rating = rep(4000, length(player_ids))))

## Get playerID and Team of each player
match1_teams <- matches %>%
  select(H_Team, A_Team, starts_with("H_XI"), starts_with("A_XI")) %>%
  pivot_longer(cols = starts_with("H_XI") | starts_with("A_XI"), names_to = "Player_Type", values_to = "Player_ID") %>%
  mutate(Team = ifelse(grepl("H_XI", Player_Type), H_Team, A_Team)) %>%
  select(Player_ID, Team)

## Remove duplicates 
match1_teams <- distinct(match1_teams)

## Join the ratings teams and players to one data frame
ratings <- left_join(ratings, match1_teams, by = "Player_ID")
ratings <- left_join(ratings, players, by = "Player_ID")

## Remove rows with Player_ID == 0
player_ratings <- ratings %>%
  filter(Player_ID != 0)


## Extract MP, GD 

player_performance13_14 <- calculate_performance(matches13_14)
player_performance13_14

## Select needed columns

selected_columns <- c("matchID", "Date", "H_Team", "A_Team", "H_ID", "A_ID", "H_XI.1", "H_XI.2", "H_XI.3", "H_XI.4", "H_XI.5", "H_XI.6", "H_XI.7", "H_XI.8", "H_XI.9", "H_XI.10", "H_XI.11", "A_XI.1", "A_XI.2", "A_XI.3", "A_XI.4", "A_XI.5", "A_XI.6", "A_XI.7", "A_XI.8", "A_XI.9", "A_XI.10", "A_XI.11")

## Create matches df, using each match when event time is 0

Matches_13_14 <- matches13_14[matches$Event.Time == 0, selected_columns] ##Get initial match values

## Create an empty M_Max column in matches_XI
Matches_13_14$M_Max <- NA

## Iterate over each match in matches_XI
for (i in 1:nrow(Matches_13_14)) {
  matchID <- Matches_13_14$matchID[i]
  max_minutes <- max(player_performance13_14$Minutes_played[player_performance13_14$matchID == matchID])## Find the max minutes played in the current match
  Matches_13_14$M_Max[i] <- max_minutes ## Assign the maximum minutes played to the M_Max column in matches_XI
}

colnames(player_ratings)[colnames(player_ratings) == "Player_ID"] <- "playerID"


## Calculate ratings

w <- 1
k <- 40

final_ratings_1314 <- Elo_function(player_performance13_14, player_ratings, Matches_13_14, w, k)
final_ratings_1314

## Remove players with less than 450 minutes

player_minutes <- aggregate(Minutes_played ~ playerID, data = player_performance13_14, FUN = sum)
player_minutes
not_eligable <- subset(player_minutes, Minutes_played < 450)
head(not_eligable)

not_eligable

subset(final_ratings_1314, playerID == 1218)

## See final ratings for season

final_ratings_1314 <- final_ratings_1314[!(final_ratings_1314$playerID %in% not_eligable$playerID), ]

final_ratings_1314

top_ratings1314 <- head(final_ratings_1314[order(final_ratings_1314$Rating, decreasing = TRUE), ], 20)
print(top_ratings1314)

#### Season 14/15 ####

## Read season

matches14_15 <- read.csv("matches14_15.csv")
matches14_15

matches<- matches14_15

## Create df for player ratings
(player_ids <- unique(unlist(matches[, paste0(rep(c("H_XI.", "A_XI."), each = 11), 1:11)])))
## Creates new dataframe ratings
(ratings <- data.frame(Player_ID = player_ids, Rating = rep(4000, length(player_ids))))

## Get playerID and Team of each player
match1_teams <- matches %>%
  select(H_Team, A_Team, starts_with("H_XI"), starts_with("A_XI")) %>%
  pivot_longer(cols = starts_with("H_XI") | starts_with("A_XI"), names_to = "Player_Type", values_to = "Player_ID") %>%
  mutate(Team = ifelse(grepl("H_XI", Player_Type), H_Team, A_Team)) %>%
  select(Player_ID, Team)


## Remove duplicates
match1_teams <- distinct(match1_teams)

## Join the ratings teams and players to one data frame
ratings <- left_join(ratings, match1_teams, by = "Player_ID")
ratings <- left_join(ratings, players, by = "Player_ID")

## Remove rows with Player_ID == 0
player_ratings <- ratings %>%
  filter(Player_ID != 0)

## Extract MP, GD 

player_performance14_15 <- calculate_performance(matches14_15)

## Create matches df, using each match when event time is 0

Matches_14_15 <- matches14_15[matches$Event.Time == 0, selected_columns]

## Create an empty M_Max column in matches_XI
Matches_14_15$M_Max <- NA

## Iterate over each match in matches_XI
for (i in 1:nrow(Matches_14_15)) {
  matchID <- Matches_14_15$matchID[i]
  max_minutes <- max(player_performance14_15$Minutes_played[player_performance14_15$matchID == matchID])   ## Find the max minutes played in the current match
  Matches_14_15$M_Max[i] <- max_minutes ## Assign the maximum minutes played to the M_Max column in matches_XI
}

colnames(player_ratings)[colnames(player_ratings) == "Player_ID"] <- "playerID"


## Calculate ratings

final_ratings_1415 <- Elo_function(player_performance14_15, player_ratings, Matches_14_15, w, k)
final_ratings_1415

## Remove players with less than 450 minutes

player_minutes <- aggregate(Minutes_played ~ playerID, data = player_performance14_15, FUN = sum)
player_minutes
not_eligable <- subset(player_minutes, Minutes_played < 450)
head(not_eligable)

not_eligable

subset(final_ratings_1415, playerID == 1131)

## See final ratings for season

final_ratings_1415 <- final_ratings_1415[!(final_ratings_1415$playerID %in% not_eligable$playerID), ]

final_ratings_1415

top_ratings1415 <- head(final_ratings_1415[order(final_ratings_1415$Rating, decreasing = TRUE), ], 20)
print(top_ratings1415)


#### Season 15/16 ####

## Read season

matches15_16 <- read.csv("matches15_16.csv")
matches15_16

matches<- matches15_16

## Create df for player ratings
(player_ids <- unique(unlist(matches[, paste0(rep(c("H_XI.", "A_XI."), each = 11), 1:11)])))
## creates new dataframe ratings
(ratings <- data.frame(Player_ID = player_ids, Rating = rep(4000, length(player_ids))))

## Get playerID and Team of each player
match1_teams <- matches %>%
  select(H_Team, A_Team, starts_with("H_XI"), starts_with("A_XI")) %>%
  pivot_longer(cols = starts_with("H_XI") | starts_with("A_XI"), names_to = "Player_Type", values_to = "Player_ID") %>%
  mutate(Team = ifelse(grepl("H_XI", Player_Type), H_Team, A_Team)) %>%
  select(Player_ID, Team)


## Remove duplicates 
match1_teams <- distinct(match1_teams)

## Join the ratings teams and players to one data frame
ratings <- left_join(ratings, match1_teams, by = "Player_ID")
ratings <- left_join(ratings, players, by = "Player_ID")

## Remove rows with Player_ID == 0
player_ratings <- ratings %>%
  filter(Player_ID != 0)

## Extract MP, GD 

player_performance15_16 <- calculate_performance(matches15_16)
player_performance15_16

## Create matches df, using each match when event time is 0

Matches_15_16 <- matches15_16[matches$Event.Time == 0, selected_columns]

## Create an empty M_Max column in matches_XI
Matches_15_16$M_Max <- NA

## Iterate over each match in matches_XI
for (i in 1:nrow(Matches_15_16)) {
  matchID <- Matches_15_16$matchID[i]
  max_minutes <- max(player_performance15_16$Minutes_played[player_performance15_16$matchID == matchID]) ## Find the max minutes played in the current match
  Matches_15_16$M_Max[i] <- max_minutes ## Assign the maximum minutes played to the M_Max column in matches_XI
}

colnames(player_ratings)[colnames(player_ratings) == "Player_ID"] <- "playerID"


## Calculate ratings

final_ratings_1516 <- Elo_function(player_performance15_16, player_ratings, Matches_15_16, w, k)
final_ratings_1516

## Remove players with less than 450 minutes

player_minutes <- aggregate(Minutes_played ~ playerID, data = player_performance15_16, FUN = sum)
player_minutes
not_eligable <- subset(player_minutes, Minutes_played < 450)
head(not_eligable)

not_eligable

subset(final_ratings_1516, playerID == 851)

## See final ratings for season

final_ratings_1516 <- final_ratings_1516[!(final_ratings_1516$playerID %in% not_eligable$playerID), ]

final_ratings_1516

top_ratings1516 <- head(final_ratings_1516[order(final_ratings_1516$Rating, decreasing = TRUE), ], 20)
print(top_ratings1516)

#### Season 16/17 ####

## Read season

matches16_17 <- read.csv("matches16_17.csv")
matches16_17

matches<- matches16_17

## Create df for player ratings
(player_ids <- unique(unlist(matches[, paste0(rep(c("H_XI.", "A_XI."), each = 11), 1:11)])))
## Creates new dataframe ratings
(ratings <- data.frame(Player_ID = player_ids, Rating = rep(4000, length(player_ids))))

## Get playerID and Team of each player
match1_teams <- matches %>%
  select(H_Team, A_Team, starts_with("H_XI"), starts_with("A_XI")) %>%
  pivot_longer(cols = starts_with("H_XI") | starts_with("A_XI"), names_to = "Player_Type", values_to = "Player_ID") %>%
  mutate(Team = ifelse(grepl("H_XI", Player_Type), H_Team, A_Team)) %>%
  select(Player_ID, Team)


## Remove duplicates
match1_teams <- distinct(match1_teams)

## Join the ratings teams and players to one data frame
ratings <- left_join(ratings, match1_teams, by = "Player_ID")
ratings <- left_join(ratings, players, by = "Player_ID")


## Remove rows with Player_ID == 0
player_ratings <- ratings %>%
  filter(Player_ID != 0)

## Extract MP, GD 

player_performance16_17 <- calculate_performance(matches16_17)
player_performance16_17

## Create matches df, using each match when event time is 0

Matches_16_17 <- matches16_17[matches$Event.Time == 0, selected_columns]

## Create an empty M_Max column in matches_XI
Matches_16_17$M_Max <- NA

## Iterate over each match in matches_XI
for (i in 1:nrow(Matches_16_17)) {
  matchID <- Matches_16_17$matchID[i]
  max_minutes <- max(player_performance16_17$Minutes_played[player_performance16_17$matchID == matchID])  ## Find the max minutes played in the current match
  Matches_16_17$M_Max[i] <- max_minutes   ## Assign the maximum minutes played to the M_Max column in matches_XI
}

colnames(player_ratings)[colnames(player_ratings) == "Player_ID"] <- "playerID"

## Calculate ratings

final_ratings_1617 <- Elo_function(player_performance16_17, player_ratings, Matches_16_17, w, k)
final_ratings_1617

## Remove players with less than 450 minutes

player_minutes <- aggregate(Minutes_played ~ playerID, data = player_performance16_17, FUN = sum)
player_minutes
not_eligable <- subset(player_minutes, Minutes_played < 450)
head(not_eligable)

not_eligable

subset(final_ratings_1617, playerID == 1404)

## See final ratings for season

final_ratings_1617 <- final_ratings_1617[!(final_ratings_1617$playerID %in% not_eligable$playerID), ]

final_ratings_1617

top_ratings1617 <- head(final_ratings_1617[order(final_ratings_1617$Rating, decreasing = TRUE), ], 20)
print(top_ratings1617)


#### Season 17/18 ####

## Read season

matches17_18 <- read.csv("matches17_18.csv")
matches17_18


matches<- matches17_18

## Create df for player ratings
(player_ids <- unique(unlist(matches[, paste0(rep(c("H_XI.", "A_XI."), each = 11), 1:11)])))
## creates new dataframe ratings
(ratings <- data.frame(Player_ID = player_ids, Rating = rep(4000, length(player_ids))))

## Get playerID and Team of each player
match1_teams <- matches %>%
  select(H_Team, A_Team, starts_with("H_XI"), starts_with("A_XI")) %>%
  pivot_longer(cols = starts_with("H_XI") | starts_with("A_XI"), names_to = "Player_Type", values_to = "Player_ID") %>%
  mutate(Team = ifelse(grepl("H_XI", Player_Type), H_Team, A_Team)) %>%
  select(Player_ID, Team)


## Remove duplicates
match1_teams <- distinct(match1_teams)

## Join the ratings teams and players to one data frame
ratings <- left_join(ratings, match1_teams, by = "Player_ID")
ratings <- left_join(ratings, players, by = "Player_ID")


## Remove rows with Player_ID == 0
player_ratings <- ratings %>%
  filter(Player_ID != 0)

## Extract MP, GD 

player_performance17_18 <- calculate_performance(matches17_18)
player_performance17_18

## Create matches df, using each match when event time is 0

Matches_17_18 <- matches17_18[matches$Event.Time == 0, selected_columns]

## Create an empty M_Max column in matches_XI
Matches_17_18$M_Max <- NA

## Iterate over each match in matches_XI
for (i in 1:nrow(Matches_17_18)) {
  matchID <- Matches_17_18$matchID[i]
  max_minutes <- max(player_performance17_18$Minutes_played[player_performance17_18$matchID == matchID])  ## Find the max minutes played in the current match
  Matches_17_18$M_Max[i] <- max_minutes ## Assign the maximum minutes played to the M_Max column in matches_XI
}

colnames(player_ratings)[colnames(player_ratings) == "Player_ID"] <- "playerID"


## Calculate ratings

final_ratings_1718 <- Elo_function(player_performance17_18, player_ratings, Matches_17_18, w, k)
final_ratings_1718

## Remove players with less than 450 minutes

player_minutes <- aggregate(Minutes_played ~ playerID, data = player_performance17_18, FUN = sum)
player_minutes
not_eligable <- subset(player_minutes, Minutes_played < 450)
head(not_eligable)

not_eligable

subset(final_ratings_1718, playerID == 1634)

## See final ratings for season

final_ratings_1718 <- final_ratings_1718[!(final_ratings_1718$playerID %in% not_eligable$playerID), ]

final_ratings_1718

top_ratings1718 <- head(final_ratings_1718[order(final_ratings_1718$Rating, decreasing = TRUE), ], 25)
print(top_ratings1718)


#### Season 18/19 ####

## Read season

matches18_19 <- read.csv("matches18_19.csv")
matches18_19

matches<- matches18_19

## Create df for player ratings
(player_ids <- unique(unlist(matches[, paste0(rep(c("H_XI.", "A_XI."), each = 11), 1:11)])))
## Creates new dataframe ratings
(ratings <- data.frame(Player_ID = player_ids, Rating = rep(4000, length(player_ids))))

## Get playerID and Team of each player
match1_teams <- matches %>%
  select(H_Team, A_Team, starts_with("H_XI"), starts_with("A_XI")) %>%
  pivot_longer(cols = starts_with("H_XI") | starts_with("A_XI"), names_to = "Player_Type", values_to = "Player_ID") %>%
  mutate(Team = ifelse(grepl("H_XI", Player_Type), H_Team, A_Team)) %>%
  select(Player_ID, Team)


## Remove duplicates
match1_teams <- distinct(match1_teams)

## Join the ratings teams and players to one data frame
ratings <- left_join(ratings, match1_teams, by = "Player_ID")
ratings <- left_join(ratings, players, by = "Player_ID")


## Remove rows with Player_ID == 0
player_ratings <- ratings %>%
  filter(Player_ID != 0)

## Extract MP, GD 

player_performance18_19 <- calculate_performance(matches18_19)
player_performance18_19

## Create matches df, using each match when event time is 0

Matches_18_19 <- matches18_19[matches$Event.Time == 0, selected_columns]

## Create an empty M_Max column in matches_XI
Matches_18_19$M_Max <- NA

## Iterate over each match in matches_XI
for (i in 1:nrow(Matches_18_19)) {
  matchID <- Matches_18_19$matchID[i]
  max_minutes <- max(player_performance18_19$Minutes_played[player_performance18_19$matchID == matchID]) ## Find the max minutes played in the current match
  Matches_18_19$M_Max[i] <- max_minutes ## Assign the maximum minutes played to the M_Max column in matches_XI
}

colnames(player_ratings)[colnames(player_ratings) == "Player_ID"] <- "playerID"


## Calculate ratings

final_ratings_1819 <- Elo_function(player_performance18_19, player_ratings, Matches_18_19, w, k)
final_ratings_1819

## Remove players with less than 450 minutes

player_minutes <- aggregate(Minutes_played ~ playerID, data = player_performance18_19, FUN = sum)
player_minutes
not_eligable <- subset(player_minutes, Minutes_played < 450)
head(not_eligable)

not_eligable

subset(final_ratings_1819, playerID == 1526)

## See final ratings for season

final_ratings_1819 <- final_ratings_1819[!(final_ratings_1819$playerID %in% not_eligable$playerID), ]

final_ratings_1819

top_ratings1819 <- head(final_ratings_1819[order(final_ratings_1819$Rating, decreasing = TRUE), ], 25)
print(top_ratings1819)



#### Season 19/20 ####

## Read season

matches19_20 <- read.csv("matches19_20.csv")
matches19_20

matches<- matches19_20

## Create df for player ratings
(player_ids <- unique(unlist(matches[, paste0(rep(c("H_XI.", "A_XI."), each = 11), 1:11)])))
## Creates new dataframe ratings
(ratings <- data.frame(Player_ID = player_ids, Rating = rep(4000, length(player_ids))))

## Get playerID and Team of each player
match1_teams <- matches %>%
  select(H_Team, A_Team, starts_with("H_XI"), starts_with("A_XI")) %>%
  pivot_longer(cols = starts_with("H_XI") | starts_with("A_XI"), names_to = "Player_Type", values_to = "Player_ID") %>%
  mutate(Team = ifelse(grepl("H_XI", Player_Type), H_Team, A_Team)) %>%
  select(Player_ID, Team)


## Remove duplicates
match1_teams <- distinct(match1_teams)

## Join the ratings teams and players to one data frame
ratings <- left_join(ratings, match1_teams, by = "Player_ID")
ratings <- left_join(ratings, players, by = "Player_ID")


# Remove rows with Player_ID == 0
player_ratings <- ratings %>%
  filter(Player_ID != 0)


## Extract MP, GD 

player_performance19_20 <- calculate_performance(matches19_20)
player_performance19_20

## Create matches df, using each match when event time is 0

Matches_19_20 <- matches19_20[matches$Event.Time == 0, selected_columns]

## Create an empty M_Max column in matches_XI
Matches_19_20$M_Max <- NA

## Iterate over each match in matches_XI
for (i in 1:nrow(Matches_19_20)) {
  matchID <- Matches_19_20$matchID[i]
  max_minutes <- max(player_performance19_20$Minutes_played[player_performance19_20$matchID == matchID])  ## Find the max minutes played in the current match
  Matches_19_20$M_Max[i] <- max_minutes   ## Assign the maximum minutes played to the M_Max column in matches_XI
}

colnames(player_ratings)[colnames(player_ratings) == "Player_ID"] <- "playerID"


## Calculate ratings

final_ratings_1920 <- Elo_function(player_performance19_20, player_ratings, Matches_19_20, w, k)
final_ratings_1920

## Remove players with less than 450 minutes

player_minutes <- aggregate(Minutes_played ~ playerID, data = player_performance19_20, FUN = sum)
player_minutes
not_eligable <- subset(player_minutes, Minutes_played < 450)
head(not_eligable)

not_eligable

subset(final_ratings_1920, playerID == 2216)

## See final ratings for season

final_ratings_1920 <- final_ratings_1920[!(final_ratings_1920$playerID %in% not_eligable$playerID), ]

final_ratings_1920

top_ratings1920 <- head(final_ratings_1920[order(final_ratings_1920$Rating, decreasing = TRUE), ], 25)
print(top_ratings1920)


#### Season 20/21 ####

## Read season

matches20_21 <- read.csv("matches20_21.csv")
matches20_21

matches<- matches20_21

# Create df for player ratings
(player_ids <- unique(unlist(matches[, paste0(rep(c("H_XI.", "A_XI."), each = 11), 1:11)])))

## Creates new dataframe ratings
(ratings <- data.frame(Player_ID = player_ids, Rating = rep(4000, length(player_ids))))

## Get playerID and Team of each player
match1_teams <- matches %>%
  select(H_Team, A_Team, starts_with("H_XI"), starts_with("A_XI")) %>%
  pivot_longer(cols = starts_with("H_XI") | starts_with("A_XI"), names_to = "Player_Type", values_to = "Player_ID") %>%
  mutate(Team = ifelse(grepl("H_XI", Player_Type), H_Team, A_Team)) %>%
  select(Player_ID, Team)

## Remove duplicates
match1_teams <- distinct(match1_teams)

## Join the ratings teams and players to one data frame
ratings <- left_join(ratings, match1_teams, by = "Player_ID")
ratings <- left_join(ratings, players, by = "Player_ID")

## Remove rows with Player_ID == 0
player_ratings <- ratings %>%
  filter(Player_ID != 0)

## Extract MP, GD 

player_performance20_21 <- calculate_performance(matches20_21)
player_performance20_21

## Create matches df, using each match when event time is 0
Matches_20_21 <- matches20_21[matches$Event.Time == 0, selected_columns]

## Create an empty M_Max column in matches_XI
Matches_20_21$M_Max <- NA

## Iterate over each match in matches_XI
for (i in 1:nrow(Matches_20_21)) {
  matchID <- Matches_20_21$matchID[i]
  max_minutes <- max(player_performance20_21$Minutes_played[player_performance20_21$matchID == matchID])  ## Find the max minutes played in the current match
  Matches_20_21$M_Max[i] <- max_minutes   ## Assign the maximum minutes played to the M_Max column in matches_
}

colnames(player_ratings)[colnames(player_ratings) == "Player_ID"] <- "playerID"


## Calculate ratings
final_ratings_2021 <- Elo_function(player_performance20_21, player_ratings, Matches_20_21, w, k)
final_ratings_2021

## Remove players with less than 450 minutes

player_minutes <- aggregate(Minutes_played ~ playerID, data = player_performance20_21, FUN = sum)
player_minutes
not_eligable <- subset(player_minutes, Minutes_played < 450)
head(not_eligable)

not_eligable

subset(final_ratings_2021, playerID == 2216)

## See final ratings for season

final_ratings_2021 <- final_ratings_2021[!(final_ratings_2021$playerID %in% not_eligable$playerID), ]
final_ratings_2021

top_ratings2021 <- head(final_ratings_2021[order(final_ratings_2021$Rating, decreasing = TRUE), ], 25)
print(top_ratings2021)



#### Season 21/22 ####

## Read season

matches21_22 <- read.csv("matches21_22.csv")
matches21_22

matches<- matches21_22

## Create df for player ratings
(player_ids <- unique(unlist(matches[, paste0(rep(c("H_XI.", "A_XI."), each = 11), 1:11)])))
## Creates new dataframe ratings
(ratings <- data.frame(Player_ID = player_ids, Rating = rep(4000, length(player_ids))))

## Get playerID and Team of each player
match1_teams <- matches %>%
  select(H_Team, A_Team, starts_with("H_XI"), starts_with("A_XI")) %>%
  pivot_longer(cols = starts_with("H_XI") | starts_with("A_XI"), names_to = "Player_Type", values_to = "Player_ID") %>%
  mutate(Team = ifelse(grepl("H_XI", Player_Type), H_Team, A_Team)) %>%
  select(Player_ID, Team)

## Remove duplicates
match1_teams <- distinct(match1_teams)

## Join the ratings teams and players to one data frame
ratings <- left_join(ratings, match1_teams, by = "Player_ID")
ratings <- left_join(ratings, players, by = "Player_ID")

## Remove rows with Player_ID == 0
player_ratings <- ratings %>%
  filter(Player_ID != 0)

## Extract MP, GD 

player_performance21_22 <- calculate_performance(matches21_22)
player_performance21_22

## Create matches df, using each match when event time is 0
Matches_21_22 <- matches21_22[matches$Event.Time == 0, selected_columns]

## Create an empty M_Max column in matches_XI
Matches_21_22$M_Max <- NA

## Iterate over each match in matches_XI
for (i in 1:nrow(Matches_21_22)) {
  matchID <- Matches_21_22$matchID[i]
  max_minutes <- max(player_performance21_22$Minutes_played[player_performance21_22$matchID == matchID])  ## Find the max minutes played in the current match
  Matches_21_22$M_Max[i] <- max_minutes   ## Assign the maximum minutes played to the M_Max column in matches_
}

colnames(player_ratings)[colnames(player_ratings) == "Player_ID"] <- "playerID"


## Calculate ratings

final_ratings_2122 <- Elo_function(player_performance21_22, player_ratings, Matches_21_22, w, k)
final_ratings_2122

## Remove players with less than 450 minutes

player_minutes <- aggregate(Minutes_played ~ playerID, data = player_performance21_22, FUN = sum)
player_minutes
not_eligable <- subset(player_minutes, Minutes_played < 450)
head(not_eligable)

not_eligable

subset(final_ratings_2122, playerID == 2537)

## See final ratings for season

final_ratings_2122 <- final_ratings_2122[!(final_ratings_2122$playerID %in% not_eligable$playerID), ]
final_ratings_2122

top_ratings2122 <- head(final_ratings_2122[order(final_ratings_2122$Rating, decreasing = TRUE), ], 25)
print(top_ratings2122)


#### Season 22/23 ####

## Read season

matches22_23 <- read.csv("matches22_23.csv")
matches22_23

matches<- matches22_23

## Create df for player ratings
(player_ids <- unique(unlist(matches[, paste0(rep(c("H_XI.", "A_XI."), each = 11), 1:11)])))
## Creates new dataframe ratings
(ratings <- data.frame(Player_ID = player_ids, Rating = rep(4000, length(player_ids))))

## Get playerID and Team of each player
match1_teams <- matches %>%
  select(H_Team, A_Team, starts_with("H_XI"), starts_with("A_XI")) %>%
  pivot_longer(cols = starts_with("H_XI") | starts_with("A_XI"), names_to = "Player_Type", values_to = "Player_ID") %>%
  mutate(Team = ifelse(grepl("H_XI", Player_Type), H_Team, A_Team)) %>%
  select(Player_ID, Team)

## Remove duplicates
match1_teams <- distinct(match1_teams)

## Join the ratings teams and players to one data frame
ratings <- left_join(ratings, match1_teams, by = "Player_ID")
ratings <- left_join(ratings, players, by = "Player_ID")

## Remove rows with Player_ID == 0
player_ratings <- ratings %>%
  filter(Player_ID != 0)


## Extract MP, GD 

player_performance22_23 <- calculate_performance(matches22_23)
player_performance22_23

## Create matches df, using each match when event time is 0
Matches_22_23 <- matches22_23[matches$Event.Time == 0, selected_columns]

## Create an empty M_Max column in matches_XI
Matches_22_23$M_Max <- NA

## Iterate over each match in matches_XI
for (i in 1:nrow(Matches_22_23)) {
  matchID <- Matches_22_23$matchID[i]
  max_minutes <- max(player_performance22_23$Minutes_played[player_performance22_23$matchID == matchID])   ## Find the max minutes played in the current match
  Matches_22_23$M_Max[i] <- max_minutes   ## Assign the maximum minutes played to the M_Max column in matches_
}

colnames(player_ratings)[colnames(player_ratings) == "Player_ID"] <- "playerID"


## Calculate ratings

final_ratings_2223 <- Elo_function(player_performance22_23, player_ratings, Matches_22_23, w, k)
final_ratings_2223

## Remove players with less than 450 minutes

player_minutes <- aggregate(Minutes_played ~ playerID, data = player_performance22_23, FUN = sum)
player_minutes
not_eligable <- subset(player_minutes, Minutes_played < 450)
head(not_eligable)

not_eligable

subset(final_ratings_2223, playerID == 2537)

## See final ratings for season

final_ratings_2223 <- final_ratings_2223[!(final_ratings_2223$playerID %in% not_eligable$playerID), ]

final_ratings_2223

top_ratings2223 <- head(final_ratings_2223[order(final_ratings_2223$Rating, decreasing = TRUE), ], 25)
print(top_ratings2223)

###### Combine ratings df ####


# Add a "Season" column to each ratings df
final_ratings_1314$Season <- "13/14"
final_ratings_1415$Season <- "14/15"
final_ratings_1516$Season <- "15/16"
final_ratings_1617$Season <- "16/17"
final_ratings_1718$Season <- "17/18"
final_ratings_1819$Season <- "18/19"
final_ratings_1920$Season <- "19/20"
final_ratings_2021$Season <- "20/21"
final_ratings_2122$Season <- "21/22"
final_ratings_2223$Season <- "22/23"

### final_ratings_1415
# put all df into one
# final_ratings <- rbind(final_ratings_1314, final_ratings_1415, final_ratings_1516,
                       # final_ratings_1617, final_ratings_1718, final_ratings_1819,
                         #final_ratings_1920, final_ratings_2021, final_ratings_2122,
                        # final_ratings_2223) 





# write.csv(final_ratings, "final_ratings1.csv", row.names = FALSE)

# final_ratings <- read.csv("/Users/matthewclifford/Documents/final_ratings1.csv")
final_ratings


#### Graphs ####
final_ratings <- read.csv("final_ratings1.csv")
final_ratings

#### Player distribution ####


## Retrieve seasons
seasons <- unique(final_ratings$Season)

## Empty list for histogram plots
season_list <- list()

for (i in 1:length(seasons)) {
  ## Get data for the current season
  season_data <- subset(final_ratings, Season == seasons[i])
  
  ## Plot the histogram for the current season
  p <- ggplot(season_data, aes(x = Rating)) +
    geom_histogram(bins = 30, fill = 'skyblue', colour = 'black') +
    geom_density() +
    labs(title = paste("Season", seasons[i]), y = "Count") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11)
    )
  
  ## Add to list
  season_list[[i]] <- p
}

p <- grid.arrange(grobs = season_list, ncol = 3)

## Top 5% to get table

top_5_percentiles <- final_ratings %>%
  group_by(Season) %>%
  summarise(Rating = quantile(Rating, 0.95))

top_5_percentiles


#### Top 3 players ####

## Pick players
filtered_data <- final_ratings %>%
  filter(Player_Name %in% c('Mohamed Salah', 'Harry Kane', 'Kevin De Bruyne')) %>%
  filter(Season %in% c('17/18', '18/19', '19/20', '20/21', '21/22', '22/23'))

## Choose Seasons to compare
filtered_data$Season <- factor(filtered_data$Season, levels = c('17/18', '18/19', '19/20', '20/21', '21/22', '22/23'))

## Plot
ggplot(filtered_data, aes(x = Season, y = Rating, colour = Player_Name, group = Player_Name)) +
  geom_line(size = 1) +
  geom_point(size = 1.8) +
  scale_y_continuous(limits = c(4000, NA)) +
  scale_colour_manual(values = c('Mohamed Salah' = 'red', 'Harry Kane' = 'black', 'Kevin De Bruyne' = 'lightblue')) +
  labs(x = "Season", y = "Rating", title = "Player Ratings Over Seasons", colour = "Player") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14), 
    legend.key.size = unit(1, "cm"),
    legend.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.text = element_text(size = 14)
  )






#### Traditional top 6 average top 11 players ####

## Choose teams
filtered_teams <- final_ratings %>%
  filter(Team %in% c('Manchester United', 'Manchester City', 'Arsenal', 'Chelsea', 'Liverpool', 'Tottenham Hotspur'))

## Average top 11 players for clubs in each season
avg_top11_rating_per_team_season <- filtered_teams %>%
  group_by(Season, Team) %>%
  top_n(11, Rating) %>%
  summarise(AvgTop11Rating = mean(Rating)) %>%
  ungroup()

## Choose seasons
avg_top11_rating_per_team_season$Season <- factor(avg_top11_rating_per_team_season$Season, levels = c('13/14', '14/15', '15/16', '16/17', '17/18', '18/19', '19/20', '20/21', '21/22', '22/23'))

## Use simple LM to fit a trend line to the seasons
model <- lm(AvgTop11Rating ~ as.numeric(Season), data = avg_top11_rating_per_team_season)
avg_top11_rating_per_team_season$Trend <- predict(model, avg_top11_rating_per_team_season)

ggplot(avg_top11_rating_per_team_season, aes(x = Season, y = AvgTop11Rating, colour = Team, group = Team)) +
  geom_line(size = 1) +
  geom_line(aes(y = Trend), colour = 'green', size = 1, linetype = "dashed") +
  scale_colour_manual(values = c('Arsenal' = 'darkred', 'Manchester United' = 'lightcoral', 'Liverpool' = 'red', 'Manchester City' = 'lightblue', 'Chelsea' = 'darkblue', 'Tottenham Hotspur' = 'black')) +
  labs(x = "Season", y = "Average Rating of Top 11 Players", title = "Average Rating of Top 11 Players by Team for Each Season", colour = "Team") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size =18),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.key.size = unit(1, "cm"),
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.text = element_text(size = 12)
  )


#### Entire League average ratings ####

## Get top 11 players for each team each season
top_11_players <- final_ratings %>%
  arrange(desc(Rating)) %>%
  group_by(Season, Team) %>%
  slice_head(n = 11)

## Get average rating
average_ratings <- top_11_players %>%
  group_by(Season, Team) %>%
  summarise(Rating = mean(Rating, na.rm = TRUE))

## Get overal average rating each season
overall_average_ratings <- average_ratings %>%
  group_by(Season) %>%
  summarise(Rating = mean(Rating, na.rm = TRUE))

overall_average_ratings$Season_num <- as.numeric(factor(overall_average_ratings$Season, levels = unique(overall_average_ratings$Season)))
## Fit LM to get trend
model <- lm(Rating ~ Season_num, data = overall_average_ratings)
overall_average_ratings$Trend <- predict(model, overall_average_ratings)

# Plot graph
ggplot(overall_average_ratings, aes(x = Season, y = Rating)) +
  geom_line(aes(group = 1), size = 1.2, colour = 'darkblue') +
  geom_line(aes(y=Trend, group = 1), colour = "lightgreen", linetype = "dashed", size = 1.3) +
  geom_point(size = 3, colour = 'blue') +
  ggtitle('Average Rating of Top 11 Players for All Teams per Season') +
  xlab('Season') +
  ylab('Average Team Rating') +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size =18),
    axis.text = element_text(size = 12),
  )


#### PFA ratings ####

## PFA player awards
pfa_players_season <- data.frame(
  Player_Name = c('Luis Suárez', 'Eden Hazard', 'Riyad Mahrez', 'N\'Golo Kanté', 'Mohamed Salah', 'Virgil van Dijk', 'Kevin De Bruyne', 'Kevin De Bruyne', 'Mohamed Salah'),
  Season = c('13/14', '14/15', '15/16', '16/17', '17/18', '18/19', '19/20', '20/21', '21/22')
)

## Merge data
pfa_data <- merge(final_ratings, pfa_players_season)
pfa_data

team_colour <- c('Liverpool' = 'red', 'Chelsea' = 'darkblue', 'Leicester City' = 'purple', 'Manchester City' = 'lightblue')


## Plot
ggplot(pfa_data, aes(x = Season, y = Rating, fill = Team)) +
  geom_bar(stat = 'identity', width = 0.5) +
  scale_fill_manual(values = team_colour) +
  geom_text(aes(label = Player_Name), vjust = -0.5, hjust = 0.5, size = 5) +
  labs(x = 'Season', y = 'Rating', title = 'Ratings of PFA Player of the Year for Each Season', fill = 'Team') +
  coord_cartesian(ylim = c(4000, NA)) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size =24),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(face = "bold", size =18),
        axis.title.y = element_text(face = "bold", size =18),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", hjust = 0.5),
        legend.text = element_text(size = 12))


#### Relegated teams ####

## List of relegated teams for each season
relegated_teams <- list(
  "13/14" = c("Cardiff City", "Fulham", "Norwich City"),
  "14/15" = c("Queens Park Rangers", "Hull City", "Burnley"),
  "15/16" = c("Aston Villa", "Newcastle United", "Norwich City"),
  "16/17" = c("Hull City", "Sunderland", "Middlesbrough"),
  "17/18" = c("Swansea City", "Stoke City", "West Bromwich Albion"),
  "18/19" = c("Huddersfield Town", "Fulham", "Cardiff City"),
  "19/20" = c("Bournemouth", "Watford", "Norwich City"),
  "20/21" = c("West Bromwich Albion", "Fulham", "Sheffield United"),
  "21/22" = c("Burnley", "Watford", "Norwich City"),
  "22/23" = c("Southampton", "Leeds United", "Leicester City")
)

## Create empty dataframe
average_ratings <- data.frame()


## For each season and each team
for (season in names(relegated_teams)) {
  for (team in relegated_teams[[season]]) {
    
    ## Get teams current rating
    team_data <- final_ratings[final_ratings$Season == season & final_ratings$Team == team, ]
    ## Get average rating of the top 11 players
    top_11_avg <- mean(head(sort(team_data$Rating, decreasing = TRUE), 11))
    ## Bind average rating to the dataframe
    average_ratings <- rbind(average_ratings, data.frame(Season = season, Team = team, `Average Rating` = top_11_avg))
  }
}

## Calculate average rating of relegated teams for each season
average_ratings_season <- average_ratings %>%
  group_by(Season) %>%
  summarise(AverageRating = mean(Average.Rating))


## Convert Season to factor
average_ratings_season$Season <- factor(average_ratings_season$Season, levels = c('13/14', '14/15', '15/16', '16/17', '17/18', '18/19', '19/20', '20/21', '21/22', '22/23'))

## Fit LM to get trend
model <- lm(AverageRating ~ as.numeric(Season), data = average_ratings_season)
average_ratings_season$Trend <- predict(model, average_ratings_season)


## Plot
ggplot(average_ratings_season, aes(x = Season, group = 1)) +
  geom_line(aes(y = AverageRating), colour = "darkblue", size = 1.2) +
  geom_point(aes(y = AverageRating), colour = "blue", size = 3) +
  geom_line(aes(y = Trend), colour = "lightgreen", linetype = "dashed", size = 1.3) +
  labs(x = "Season", y = "Average Team Rating",
       title = "Average Rating of Relegated Teams Top 11 players Each Season")  +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size =18),
    axis.text = element_text(size = 12)
        )
