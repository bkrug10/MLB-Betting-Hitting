SGPTeam <- NA
library(tidyverse)
library(baseballr)
oldteams <- c("ATL", "LAD", "TEX", "NYM", "SEA", "KC", "SD", "MIL", "CLE", "BAL", "HOU", "TB", "PHI", "SF", "TOR", "COL", "CHC", "STL", "BOS", "OAK", "MIN", "PIT", "ARI", "WSH", "LAA", "DET", "MIA", "NYY", "CIN", "CWS", "AZ")
newteams <- c("Atlanta Braves", "Los Angeles Dodgers", "Texas Rangers", "New York Mets", "Seattle Mariners", "Kansas City Royals", "San Diego Padres", "Milwaukee Brewers", "Cleveland Guardians", "Baltimore Orioles", "Houston Astros", "Tampa Bay Rays", "Philadelphia Phillies", "San Francisco Giants", "Toronto Blue Jays", "Colorado Rockies", "Chicago Cubs", "St. Louis Cardinals", "Boston Red Sox", "Oakland Athletics", "Minnesota Twins", "Pittsburgh Pirates", "Arizona Diamondbacks", "Washington Nationals", "Los Angeles Angels", "Detroit Tigers", "Miami Marlins", "New York Yankees", "Cincinnati Reds", "Chicago White Sox", "Arizona Diamondbacks")

oldnames <- c("Domingo German", "Jose Tena", "Andres Chaparro", "Jose Urena", "Pedro Leon", "Nacho Alvarez", "Aledmys Diaz", "Cesar Salazar", "Pablo Lopez", "Jose Berrios", "Jose Soriano", "Randy Vasquez", "Yariel Rodriguez", "Ranger Suarez", "Edwin Rios", "Albert Suarez", "Roddery Munoz", "Martin Perez", "Cristopher Sanchez", "Ali Sanchez", "Carlos Rodon", "Leo Jimenez", "Luis Ortiz", "Reynaldo Lopez", "Rafael Marchan", "Angel Martinez",  "J.P. Martinez", "Jose Fermin", "Oscar Colas", "Teoscar Hernandez", "Fernando Tatis", "Martin Maldonado", "Rene Pinto", "Nelson Velazquez", "Jose Caballero", "Yoan Moncada", "Jazz Chisholm", "Avisail Garcia", "Luis Robert", "Adolis Garcia", "Javier Baez", "Ramon Laureano", "Elias Diaz", "Eloy Jimenez", "Andres Gimenez", "Jeremy Pena", "Eugenio Suarez", "LaMonte Wade", "Jose Abreu", "Vladimir Guerrero", "Jose Ramirez", "Andy Ibanez", "Harold Ramirez", "Bobby Witt", "Yandy Diaz", "Julio Rodriguez", "Lourdes Gurriel", "Jesus Sanchez", "Ramon Urias", "Gary Sanchez", "Enrique Hernandez", "Michael Taylor", "Michael Harris", "Ronald Acuna", "Luis Urias", "Miguel Sano", "Omar Narvaez", "Victor Scott", "Vidal Brujan", "Christian Vazquez", "Jose Azocar", "Ivan Herrera", "Mauricio Dubon", "Nasim Nunez", "Wenceel Perez", "Mike Siani", "Tomas Nido", "Pedro Pages")
newnames <- c("Domingo Germán", "José Tena", "Andrés Chaparro", "José Ureña", "Pedro León", "Nacho Alvarez Jr.", "Aledmys Díaz", "César Salazar", "Pablo López", "José Berríos", "José Soriano", "Randy Vásquez", "Yariel Rodríguez", "Ranger Suárez", "Edwin Ríos", "Albert Suárez", "Roddery Muñoz", "Martín Pérez", "Cristopher Sánchez", "Ali Sánchez", "Carlos Rodón", "Leo Jiménez", "Luis L. Ortiz", "Reynaldo López", "Rafael Marchán", "Angel Martínez", "J.P. Martínez", "José Fermín", "Oscar Colás", "Teoscar Hernández", "Fernando Tatis Jr.", "Martín Maldonado", "René Pinto", "Nelson Velázquez", "José Caballero", "Yoán Moncada", "Jazz Chisholm Jr.", "Avisaíl García", "Luis Robert Jr.", "Adolis García", "Javier Báez", "Ramón Laureano", "Elias Díaz", "Eloy Jiménez", "Andrés Giménez", "Jeremy Peña", "Eugenio Suárez", "LaMonte Wade Jr.", "José Abreu", "Vladimir Guerrero Jr.", "José Ramírez", "Andy Ibáñez", "Harold Ramírez", "Bobby Witt Jr.", "Yandy Díaz", "Julio Rodríguez", "Lourdes Gurriel Jr.", "Jesús Sánchez", "Ramón Urías", "Gary Sánchez", "Enrique Hernández", "Michael A. Taylor", "Michael Harris II", "Ronald Acuña Jr.", "Luis Urías", "Miguel Sanó", "Omar Narváez", "Victor Scott II", "Vidal Bruján", "Christian Vázquez", "José Azocar", "Iván Herrera", "Mauricio Dubón", "Nasim Nuñez", "Wenceel Pérez", "Michael Siani", "Tomás Nido", "Pedro Pagés")
Games <- mlb_schedule(year(today()))
Todays_Games <- filter(Games, Games$date==today())
if (Todays_Games$series_description[1]=="Regular Season") {
  pbp1 <- read.csv("MLB Sims/Hitting Stats/PBP.csv") 
  pbp2 <- read.csv("MLB Sims/Hitting Stats/savant_data(4).csv")
  pbp2 <- filter(pbp2, events!="") %>% 
    select(-c(bat_speed, swing_length))
  pbp <- rbind(pbp1, pbp2)
  pbp <- unique.array(pbp)
  write_csv(pbp, "MLB Sims/Hitting Stats/PBP.csv")
  pbp$home_team[pbp$home_team %in% oldteams] <- newteams[match(pbp$home_team, oldteams, nomatch = 0)]
  pbp$away_team[pbp$away_team %in% oldteams] <- newteams[match(pbp$away_team, oldteams, nomatch = 0)]
  pbp <- pbp %>%   
    mutate(TB=case_when(events=="single"~1,
                        events=="double"~2,
                        events=="triple"~3,
                        events=="home_run"~4,
                        TRUE~0),
           RBIs=case_when(events != "strikeout" | events != "grounded_into_double_play" | events != "field_error" | events != "sac_fly_double_play" | events != "strikeout_double_play" | events != "double_play" | events != "wild_pitch" | events != "triple_play" | events != "pickoff_error_3b"~post_bat_score - bat_score),
           Runs=post_bat_score - bat_score,
           Hits=case_when(events=="single" | events=="double" | events=="triple" | events=="home_run"~1,
                          TRUE~0),
           HittingTeam=case_when(inning_topbot=="Top"~away_team,
                                 inning_topbot=="Bot"~home_team),
           PitchingTeam=case_when(inning_topbot=="Top"~home_team,
                                  inning_topbot=="Bot"~away_team),
           Month=month(game_date)) %>% 
    arrange(desc(game_date)) 
} else {
  pbp1 <- read.csv("MLB Sims/Hitting Stats/Playoff PBP.csv") 
  pbp2 <- read.csv("MLB Sims/Hitting Stats/savant_data (2).csv")
  pbp2 <- filter(pbp2, events!="") %>% 
    select(-c(bat_speed, swing_length))
  playoff_pbp <- rbind(pbp1, pbp2)
  playoff_pbp <- unique.array(playoff_pbp)
  write_csv(playoff_pbp, "MLB Sims/Hitting Stats/Playoff PBP.csv")
  regular_pbp <- read.csv("MLB Sims/Hitting Stats/PBP.csv") 
  pbp <- rbind(playoff_pbp, regular_pbp)
  pbp$home_team[pbp$home_team %in% oldteams] <- newteams[match(pbp$home_team, oldteams, nomatch = 0)]
  pbp$away_team[pbp$away_team %in% oldteams] <- newteams[match(pbp$away_team, oldteams, nomatch = 0)]
  pbp <- pbp %>%   
    mutate(TB=case_when(events=="single"~1,
                        events=="double"~2,
                        events=="triple"~3,
                        events=="home_run"~4,
                        TRUE~0),
           RBIs=case_when(events != "strikeout" | events != "grounded_into_double_play" | events != "field_error" | events != "sac_fly_double_play" | events != "strikeout_double_play" | events != "double_play" | events != "wild_pitch" | events != "triple_play" | events != "pickoff_error_3b"~post_bat_score - bat_score),
           Runs=post_bat_score - bat_score,
           Hits=case_when(events=="single" | events=="double" | events=="triple" | events=="home_run"~1,
                          TRUE~0),
           HittingTeam=case_when(inning_topbot=="Top"~away_team,
                                 inning_topbot=="Bot"~home_team),
           PitchingTeam=case_when(inning_topbot=="Top"~home_team,
                                  inning_topbot=="Bot"~away_team),
           Month=month(game_date)) %>% 
    arrange(desc(game_date)) 
}
if (nrow(filter(Games, date<today())) > 0) {
  year1 = year(today())
  year2 = year(today())-1
  year3 = year(today())-2
} else {
  year1 = year(today())-1
  year2 = year(today())-2
  year3 = year(today())-3
}
Starters <- data.frame()
for (i in 1:nrow(Todays_Games)) {
  q = mlb_probables(Todays_Games$game_pk[i])
  Starters <- rbind(Starters, q)
}
Batters1 <- fg_batter_leaders(startseason = year1, endseason = year1) %>% 
  select(PlayerName, Bats, playerid, AB, xMLBAMID)
Batters2 <- fg_batter_leaders(startseason = year2, endseason = year2) %>% 
  select(PlayerName, Bats, playerid, AB, xMLBAMID)
Batters3 <- fg_batter_leaders(startseason = year3, endseason = year3) %>% 
  select(PlayerName, Bats, playerid, AB, xMLBAMID)
BattersInfo <- rbind(Batters1, Batters2, Batters3)
BattersInfo <- filter(BattersInfo, AB>=15*3) %>% 
  select(PlayerName, Bats, playerid, xMLBAMID)
BattersInfo <- unique.array(BattersInfo)
Pitcher1 <- fg_pitcher_leaders(startseason = year1, endseason = year1) %>% 
  select(PlayerName, Throws, playerid, IP, xMLBAMID)
Pitcher2 <- fg_pitcher_leaders(startseason = year2, endseason = year2) %>% 
  select(PlayerName, Throws, playerid, IP, xMLBAMID)
Pitcher3 <- fg_pitcher_leaders(startseason = year3, endseason = year3) %>% 
  select(PlayerName, Throws, playerid, IP, xMLBAMID)
PitcherInfo <- rbind(Pitcher1, Pitcher2, Pitcher3)
Pitcher <- filter(PitcherInfo, IP >= 24) %>% 
  select(PlayerName, Throws, playerid, xMLBAMID)
Pitcher <- unique.array(Pitcher)
for (i in 1:nrow(Pitcher)) {
  start <- filter(Starters, id==Pitcher$xMLBAMID[i])
  if (nrow(start)>0) {
    Pitcher$Team[i]=start$team
  } else {
    Pitcher$Team[i]=NA
  }
}
Pitcher <- na.omit(Pitcher)
`NRFI/YRFI Stats` <- Pitcher
Batters <- pbp %>% 
  select(batter, game_date)
stats <- unique(Batters$game_date)
Batters <- filter(Batters, game_date>=stats[7]) %>% 
  select(batter)
Batters <- data.frame(unique(Batters$batter))
colnames(Batters) <- "xMLBAMID"

for (i in 1:nrow(Batters)) {
  stats <- filter(BattersInfo, xMLBAMID==Batters$xMLBAMID[i])
  if (nrow(stats)>0) {
    Batters$playerid[i]=stats$playerid
    Batters$Name[i]=stats$PlayerName
    Batters$Bats[i]=stats$Bats
  } else {
    Batters$playerid[i]=NA
    Batters$Name[i]=NA
    Batters$Bats[i]=NA
  }
  stats <- filter(pbp, batter==Batters$xMLBAMID[i])
  if (nrow(stats)>0) {
    Batters$Team[i]=stats$HittingTeam[1]
  } else {
    Batters$Team[i]=NA
  }
  stats <- filter(Todays_Games, teams_away_team_name==Batters$Team[i] | teams_home_team_name==Batters$Team[i])
  if (nrow(stats)>0) {
    if (stats$teams_away_team_name[1]==Batters$Team[i]) {
      Batters$Opp[i]=stats$teams_home_team_name[1]
      Batters$Full_Opp[i]=stats$teams_home_team_name[1]
      Batters$Home_Away[i]="Away"
    } else {
      Batters$Opp[i]=stats$teams_away_team_name[1]
      Batters$Full_Opp[i]=stats$teams_away_team_name[1]
      Batters$Home_Away[i]="Home"
    }
  } else {
    Batters$Opp[i]=NA
    Batters$Full_Opp[i]=NA
    Batters$Home_Away[i]=NA
  }
}
Batters <- na.omit(Batters)
for (i in 1:nrow(Batters)) {
  starter <- filter(Pitcher, Team==Batters$Full_Opp[i])
  if (nrow(starter)>0) {
    Batters$OppStarter[i]=starter$PlayerName
    Batters$OppStarterID[i]=starter$xMLBAMID
    Batters$OppStarterPlayerID[i]=starter$playerid
    Batters$OppStarterThrows[i]=starter$Throws
  } else {
    Batters$OppStarter[i]=NA
    Batters$OppStarterID[i]=NA
    Batters$OppStarterPlayerID[i]=NA
    Batters$OppStarterThrows[i]=NA
  }
}
Batters <- filter(Batters, !is.na(OppStarter))
for (i in 1:nrow(Batters)) {
  player_games <- filter(pbp, batter==Batters$xMLBAMID[i]) %>% 
    select(game_date, game_pk)
  player_games <- unique.array(player_games)
  if (nrow(player_games)>0) {
    cutoff <- head(player_games, 15)
    Batters$Last15Cutoff[i]=cutoff$game_date[nrow(cutoff)]
    cutoff <- head(player_games, 30)
    Batters$Last30Cutoff[i]=cutoff$game_date[nrow(cutoff)]
    cutoff <- head(player_games, 60)
    Batters$Last60Cutoff[i]=cutoff$game_date[nrow(cutoff)]
    cutoff <- head(player_games, 120)
    Batters$Last120Cutoff[i]=cutoff$game_date[nrow(cutoff)]
  } else {
    Batters$Last15Cutoff[i]=NA
    Batters$Last30Cutoff[i]=NA
    Batters$Last60Cutoff[i]=NA
    Batters$Last120Cutoff[i]=NA
  }
  player_games <- filter(pbp, pitcher==Batters$OppStarterID[i]) %>% 
    select(game_date, game_pk)
  player_games <- unique.array(player_games)
  if (nrow(player_games)>0) {
    cutoff <- head(player_games, 7)
    Batters$OppStarterLast7Cutoff[i]=cutoff$game_date[nrow(cutoff)]
    cutoff <- head(player_games, 15)
    Batters$OppStarterLast15Cutoff[i]=cutoff$game_date[nrow(cutoff)]
    cutoff <- head(player_games, 30)
    Batters$OppStarterLast30Cutoff[i]=cutoff$game_date[nrow(cutoff)]
    cutoff <- head(player_games, 60)
    Batters$OppStarterLast60Cutoff[i]=cutoff$game_date[nrow(cutoff)]
  } else {
    Batters$OppStarterLast7Cutoff[i]=NA
    Batters$OppStarterLast15Cutoff[i]=NA
    Batters$OppStarterLast30Cutoff[i]=NA
    Batters$OppStarterLast60Cutoff[i]=NA
  }
}
Batters <- filter(Batters, !is.na(OppStarterLast60Cutoff)) %>% 
  mutate(Bats=case_when(Bats=="B" & OppStarterThrows=="R"~"L",
                        Bats=="B" & OppStarterThrows=="L"~"R",
                        TRUE~Bats),
         Ballpark=case_when(Home_Away=="Home"~Team,
                            Home_Away=="Away"~Full_Opp))
for (i in 1:nrow(Batters)) {
  pitches <- filter(pbp, pitcher==Batters$OppStarterID[i])
  pitches <- data.frame(unique(pitches$pitch_type))
  for (l in 1:nrow(pitches)) {
    stats <- head(filter(pbp, batter==Batters$xMLBAMID[i] & pitch_type==pitches$unique.pitches.pitch_type.[l]), 120)
    pitches$Last30Hitting[l]=sum(stats$Hits)/nrow(stats)
    pitches$Last30TB[l]=sum(stats$TB)/nrow(stats)
    pitches$Last30RBIs[l]=sum(stats$RBIs)/nrow(stats)
    pitches$Last30HR[l]=sum(stats$events=="home_run")/nrow(stats)
    pitches$Last30SO[l]=sum(stats$events=="strikeout")/nrow(stats)
    Last30AB=nrow(stats)
    last30Date=tail(stats$game_date, 1)
    stats <- head(filter(pbp, batter==Batters$xMLBAMID[i] & pitch_type==pitches$unique.pitches.pitch_type.[l]), 240)
    pitches$Last60Hitting[l]=sum(stats$Hits)/nrow(stats)
    pitches$Last60TB[l]=sum(stats$TB)/nrow(stats)
    pitches$Last60RBIs[l]=sum(stats$RBIs)/nrow(stats)
    pitches$Last60HR[l]=sum(stats$events=="home_run")/nrow(stats)
    pitches$Last60SO[l]=sum(stats$events=="strikeout")/nrow(stats)
    Last60AB=nrow(stats)
    last60Date=tail(stats$game_date, 1)
    if (length(last30Date)>0) {
      stats <- filter(pbp, pitch_type==pitches$unique.pitches.pitch_type.[l] & game_date>=last30Date)
      pitches$Last30Runs[l]=sum(grepl(paste(Batters$Name[i], "scores"), stats$des)==TRUE | grepl(paste(Batters$Name[i], "homers"), stats$des)==TRUE)/Last30AB
    } else {
      pitches$Last30Runs[l]=NA
    }
    if (length(last60Date)>0) {
      stats <- filter(pbp, pitch_type==pitches$unique.pitches.pitch_type.[l] & game_date>=last60Date)
      pitches$Last60Runs[l]=sum(grepl(paste(Batters$Name[i], "scores"), stats$des)==TRUE | grepl(paste(Batters$Name[i], "homers"), stats$des)==TRUE)/Last60AB
    } else {
      pitches$Last60Runs[l]=NA
    }
    stats <- head(filter(pbp, batter==Batters$xMLBAMID[i] & pitch_type==pitches$unique.pitches.pitch_type.[l] & p_throws==Batters$OppStarterThrows[i]),120)
    pitches$`Last30L/RHitting`[l]=sum(stats$Hits)/nrow(stats)
    pitches$`Last30L/RTB`[l]=sum(stats$TB)/nrow(stats)
    pitches$`Last30L/RRBIs`[l]=sum(stats$RBIs)/nrow(stats)
    pitches$`Last30L/RHR`[l]=sum(stats$events=="home_run")/nrow(stats)
    pitches$`Last30L/RSO`[l]=sum(stats$events=="strikeout")/nrow(stats)
    Last30AB=nrow(stats)
    last30Date=tail(stats$game_date, 1)
    stats <- head(filter(pbp, batter==Batters$xMLBAMID[i] & pitch_type==pitches$unique.pitches.pitch_type.[l] & p_throws==Batters$OppStarterThrows[i]), 240)
    pitches$`Last60L/RHitting`[l]=sum(stats$Hits)/nrow(stats)
    pitches$`Last60L/RTB`[l]=sum(stats$TB)/nrow(stats)
    pitches$`Last60L/RRBIs`[l]=sum(stats$RBIs)/nrow(stats)
    pitches$`Last60L/RHR`[l]=sum(stats$events=="home_run")/nrow(stats)
    pitches$`Last60L/RSO`[l]=sum(stats$events=="strikeout")/nrow(stats)
    Last60AB=nrow(stats)
    last60Date=tail(stats$game_date, 1)
    if (length(last30Date)>0) {
      stats <- filter(pbp, pitch_type==pitches$unique.pitches.pitch_type.[l] & game_date>=last30Date)
      pitches$`Last30L/RRuns`[l]=sum(grepl(paste(Batters$Name[i], "scores"), stats$des)==TRUE | grepl(paste(Batters$Name[i], "homers"), stats$des)==TRUE)/Last30AB
    } else {
      pitches$`Last30L/RRuns`[l]=NA
    }
    if (length(last60Date)>0) {
      stats <- filter(pbp, pitch_type==pitches$unique.pitches.pitch_type.[l] & game_date>=last60Date)
      pitches$`Last60L/RRuns`[l]=sum(grepl(paste(Batters$Name[i], "scores"), stats$des)==TRUE | grepl(paste(Batters$Name[i], "homers"), stats$des)==TRUE)/Last60AB
    } else {
      pitches$`Last60L/RRuns`[l]=NA
    }
    stats <- filter(pbp, pitcher==Batters$OppStarterID[i] & game_date>=Batters$OppStarterLast15Cutoff[i])
    Last15AB <- nrow(stats)
    stats <- filter(pbp, pitcher==Batters$OppStarterID[i] & pitch_type==pitches$unique.pitches.pitch_type.[l] & game_date>=Batters$OppStarterLast15Cutoff[i])
    pitches$Last15PitcherHitting[l]=sum(stats$Hits)/nrow(stats)
    pitches$Last15PitcherTB[l]=sum(stats$TB)/nrow(stats)
    pitches$Last15PitcherRBIs[l]=sum(stats$RBIs)/nrow(stats)
    pitches$Last15PitcherHR[l]=sum(stats$events=="home_run")/nrow(stats)
    pitches$Last15PitcherRuns[l]=sum(stats$Runs)/nrow(stats)
    pitches$Last15PitcherSO[l]=sum(stats$events=="strikeout")/nrow(stats)
    pitches$`Last15Pitch%`[l]=nrow(stats)/Last15AB
    stats <- filter(pbp, pitcher==Batters$OppStarterID[i] & game_date>=Batters$OppStarterLast30Cutoff[i])
    Last30AB <- nrow(stats)
    stats <- filter(pbp, pitcher==Batters$OppStarterID[i] & pitch_type==pitches$unique.pitches.pitch_type.[l] & game_date>=Batters$OppStarterLast30Cutoff[i])
    pitches$Last30PitcherHitting[l]=sum(stats$Hits)/nrow(stats)
    pitches$Last30PitcherTB[l]=sum(stats$TB)/nrow(stats)
    pitches$Last30PitcherRBIs[l]=sum(stats$RBIs)/nrow(stats)
    pitches$Last30PitcherHR[l]=sum(stats$events=="home_run")/nrow(stats)
    pitches$Last30PitcherRuns[l]=sum(stats$Runs)/nrow(stats)
    pitches$Last30PitcherSO[l]=sum(stats$events=="strikeout")/nrow(stats)
    pitches$`Last30Pitch%`[l]=nrow(stats)/Last30AB
    stats <- filter(pbp, pitcher==Batters$OppStarterID[i] & game_date>=Batters$OppStarterLast15Cutoff[i] & stand==Batters$Bats[i])
    Last15AB <- nrow(stats)
    stats <- filter(pbp, pitcher==Batters$OppStarterID[i] & pitch_type==pitches$unique.pitches.pitch_type.[l] & game_date>=Batters$OppStarterLast15Cutoff[i] & stand==Batters$Bats[i])
    pitches$`Last15L/RPitcherHitting`[l]=sum(stats$Hits)/nrow(stats)
    pitches$`Last15L/RPitcherTB`[l]=sum(stats$TB)/nrow(stats)
    pitches$`Last15L/RPitcherRBIs`[l]=sum(stats$RBIs)/nrow(stats)
    pitches$`Last15L/RPitcherHR`[l]=sum(stats$events=="home_run")/nrow(stats)
    pitches$`Last15L/RPitcherRuns`[l]=sum(stats$Runs)/nrow(stats)
    pitches$`Last15L/RPitcherSO`[l]=sum(stats$events=="strikeout")/nrow(stats)
    pitches$`Last15L/RPitch%`[l]=nrow(stats)/Last15AB
    stats <- filter(pbp, pitcher==Batters$OppStarterID[i] & game_date>=Batters$OppStarterLast30Cutoff[i] & stand==Batters$Bats[i])
    Last30AB <- nrow(stats)
    stats <- filter(pbp, pitcher==Batters$OppStarterID[i] & pitch_type==pitches$unique.pitches.pitch_type.[l] & game_date>=Batters$OppStarterLast30Cutoff[i] & stand==Batters$Bats[i])
    pitches$`Last30L/RPitcherHitting`[l]=sum(stats$Hits)/nrow(stats)
    pitches$`Last30L/RPitcherTB`[l]=sum(stats$TB)/nrow(stats)
    pitches$`Last30L/RPitcherRBIs`[l]=sum(stats$RBIs)/nrow(stats)
    pitches$`Last30L/RPitcherHR`[l]=sum(stats$events=="home_run")/nrow(stats)
    pitches$`Last30L/RPitcherRuns`[l]=sum(stats$Runs)/nrow(stats)
    pitches$`Last30L/RPitcherSO`[l]=sum(stats$events=="strikeout")/nrow(stats)
    pitches$`Last30L/RPitch%`[l]=nrow(stats)/Last30AB
  }
  pitches <- pitches %>% 
    mutate(Last30Hitting=case_when(is.na(Last30Hitting)~Last15PitcherHitting,
                                   !is.na(Last30Hitting)~Last30Hitting),
           Last60Hitting=case_when(is.na(Last60Hitting)~Last30PitcherHitting,
                                   !is.na(Last60Hitting)~Last60Hitting),
           Last30TB=case_when(is.na(Last30TB)~Last15PitcherTB,
                              !is.na(Last30TB)~Last30TB),
           Last60TB=case_when(is.na(Last60TB)~Last30PitcherTB,
                              !is.na(Last60TB)~Last60TB),
           Last30RBIs=case_when(is.na(Last30RBIs)~Last15PitcherRBIs,
                                !is.na(Last30RBIs)~Last30RBIs),
           Last60RBIs=case_when(is.na(Last60RBIs)~Last30PitcherRBIs,
                                !is.na(Last60RBIs)~Last60RBIs),
           Last30HR=case_when(is.na(Last30HR)~Last15PitcherHR,
                              !is.na(Last30HR)~Last30HR),
           Last60HR=case_when(is.na(Last60HR)~Last30PitcherHR,
                              !is.na(Last60HR)~Last60HR),
           Last30Runs=case_when(is.na(Last30Runs)~Last15PitcherRuns,
                                !is.na(Last30Runs)~Last30Runs),
           Last60Runs=case_when(is.na(Last60Runs)~Last30PitcherRuns,
                                !is.na(Last60Runs)~Last60Runs),
           Last30SO=case_when(is.na(Last30SO)~Last15PitcherSO,
                                !is.na(Last30SO)~Last30SO),
           Last60SO=case_when(is.na(Last60SO)~Last30PitcherSO,
                                !is.na(Last60SO)~Last60SO),
           `Last30L/RHitting`=case_when(is.na(`Last30L/RHitting`)~`Last15L/RPitcherHitting`,
                                        !is.na(`Last30L/RHitting`)~`Last30L/RHitting`),
           `Last60L/RHitting`=case_when(is.na(`Last60L/RHitting`)~`Last30L/RPitcherHitting`,
                                        !is.na(`Last60L/RHitting`)~`Last60L/RHitting`),
           `Last30L/RTB`=case_when(is.na(`Last30L/RTB`)~`Last15L/RPitcherTB`,
                                   !is.na(`Last30L/RTB`)~`Last30L/RTB`),
           `Last60L/RTB`=case_when(is.na(`Last60L/RTB`)~`Last30L/RPitcherTB`,
                                   !is.na(`Last60L/RTB`)~`Last60L/RTB`),
           `Last30L/RRBIs`=case_when(is.na(`Last30L/RRBIs`)~`Last15L/RPitcherRBIs`,
                                     !is.na(`Last30L/RRBIs`)~`Last30L/RRBIs`),
           `Last60L/RRBIs`=case_when(is.na(`Last60L/RRBIs`)~`Last30L/RPitcherRBIs`,
                                     !is.na(`Last60L/RRBIs`)~`Last60L/RRBIs`),
           `Last30L/RHR`=case_when(is.na(`Last30L/RHR`)~`Last15L/RPitcherHR`,
                                   !is.na(`Last30L/RHR`)~`Last30L/RHR`),
           `Last60L/RHR`=case_when(is.na(`Last60L/RHR`)~`Last30L/RPitcherHR`,
                                   !is.na(`Last60L/RHR`)~`Last60L/RHR`),
           `Last30L/RRuns`=case_when(is.na(`Last30L/RRuns`)~`Last15L/RPitcherRuns`,
                                     !is.na(`Last30L/RRuns`)~`Last30L/RRuns`),
           `Last60L/RRuns`=case_when(is.na(`Last60L/RRuns`)~`Last30L/RPitcherRuns`,
                                     !is.na(`Last60L/RRuns`)~`Last60L/RRuns`),
           `Last30L/RSO`=case_when(is.na(`Last30L/RSO`)~`Last15L/RPitcherSO`,
                                     !is.na(`Last30L/RSO`)~`Last30L/RSO`),
           `Last60L/RSO`=case_when(is.na(`Last60L/RSO`)~`Last30L/RPitcherSO`,
                                     !is.na(`Last60L/RSO`)~`Last60L/RSO`),
           `Last15L/RPitcherHitting`=case_when(is.na(`Last15L/RPitcherHitting`)~0,
                                                TRUE~`Last15L/RPitcherHitting`),
           `Last15L/RPitcherTB`=case_when(is.na(`Last15L/RPitcherTB`)~0,
                                                TRUE~`Last15L/RPitcherTB`),
           `Last15L/RPitcherRBIs`=case_when(is.na(`Last15L/RPitcherRBIs`)~0,
                                                TRUE~`Last15L/RPitcherRBIs`),
           `Last15L/RPitcherHR`=case_when(is.na(`Last15L/RPitcherHR`)~0,
                                                TRUE~`Last15L/RPitcherHR`),
           `Last15L/RPitcherRuns`=case_when(is.na(`Last15L/RPitcherRuns`)~0,
                                                TRUE~`Last15L/RPitcherRuns`),
           `Last15L/RPitcherSO`=case_when(is.na(`Last15L/RPitcherSO`)~0,
                                            TRUE~`Last15L/RPitcherSO`),
           `Last30L/RPitcherHitting`=case_when(is.na(`Last30L/RPitcherHitting`)~0,
                                                TRUE~`Last30L/RPitcherHitting`),
           `Last30L/RPitcherTB`=case_when(is.na(`Last30L/RPitcherTB`)~0,
                                           TRUE~`Last30L/RPitcherTB`),
           `Last30L/RPitcherRBIs`=case_when(is.na(`Last30L/RPitcherRBIs`)~0,
                                             TRUE~`Last30L/RPitcherRBIs`),
           `Last30L/RPitcherHR`=case_when(is.na(`Last30L/RPitcherHR`)~0,
                                           TRUE~`Last30L/RPitcherHR`),
           `Last30L/RPitcherRuns`=case_when(is.na(`Last30L/RPitcherRuns`)~0,
                                             TRUE~`Last30L/RPitcherRuns`),
           `Last30L/RPitcherSO`=case_when(is.na(`Last30L/RPitcherSO`)~0,
                                            TRUE~`Last30L/RPitcherSO`),
           `Last15PitcherHitting`=case_when(is.na(`Last15PitcherHitting`)~0,
                                               TRUE~`Last15PitcherHitting`),
           `Last15PitcherTB`=case_when(is.na(`Last15PitcherTB`)~0,
                                          TRUE~`Last15PitcherTB`),
           `Last15PitcherRBIs`=case_when(is.na(`Last15PitcherRBIs`)~0,
                                            TRUE~`Last15PitcherRBIs`),
           `Last15PitcherHR`=case_when(is.na(`Last15PitcherHR`)~0,
                                          TRUE~`Last15PitcherHR`),
           `Last15PitcherRuns`=case_when(is.na(`Last15PitcherRuns`)~0,
                                            TRUE~`Last15PitcherRuns`),
           `Last15PitcherSO`=case_when(is.na(`Last15PitcherSO`)~0,
                                         TRUE~`Last15PitcherSO`),
           `Last30PitcherHitting`=case_when(is.na(`Last30PitcherHitting`)~0,
                                               TRUE~`Last30PitcherHitting`),
           `Last30PitcherTB`=case_when(is.na(`Last30PitcherTB`)~0,
                                          TRUE~`Last30PitcherTB`),
           `Last30PitcherRBIs`=case_when(is.na(`Last30PitcherRBIs`)~0,
                                            TRUE~`Last30PitcherRBIs`),
           `Last30PitcherHR`=case_when(is.na(`Last30PitcherHR`)~0,
                                          TRUE~`Last30PitcherHR`),
           `Last30PitcherRuns`=case_when(is.na(`Last30PitcherRuns`)~0,
                                            TRUE~`Last30PitcherRuns`),
           `Last30PitcherSO`=case_when(is.na(`Last30PitcherSO`)~0,
                                         TRUE~`Last30PitcherSO`))
  hitstuff=0
  TBstuff=0
  HRstuff=0
  RBIstuff=0
  Runstuff=0
  Kstuff=0
  for (q in 1:nrow(pitches)) {
    hitstuff=hitstuff+(((pitches$Last30Hitting[q] + pitches$Last60Hitting[q] + pitches$`Last30L/RHitting`[q] + pitches$`Last60L/RHitting`[q] + pitches$Last15PitcherHitting[q] + pitches$Last30PitcherHitting[q] + pitches$`Last15L/RPitcherHitting`[q] + pitches$`Last30L/RPitcherHitting`[q])/8)*((pitches$`Last15Pitch%`[q] + pitches$`Last30Pitch%`[q] + pitches$`Last15L/RPitch%`[q] + pitches$`Last30L/RPitch%`[q])/4))
    TBstuff=TBstuff+(((pitches$Last30TB[q] + pitches$Last60TB[q] + pitches$`Last30L/RTB`[q] + pitches$`Last60L/RTB`[q] + pitches$Last15PitcherTB[q] + pitches$Last30PitcherTB[q] + pitches$`Last15L/RPitcherTB`[q] + pitches$`Last30L/RPitcherTB`[q])/8)*((pitches$`Last15Pitch%`[q] + pitches$`Last30Pitch%`[q] + pitches$`Last15L/RPitch%`[q] + pitches$`Last30L/RPitch%`[q])/4))
    HRstuff=HRstuff+(((pitches$Last30HR[q] + pitches$Last60HR[q] + pitches$`Last30L/RHR`[q] + pitches$`Last60L/RHR`[q] + pitches$Last15PitcherHR[q] + pitches$Last30PitcherHR[q] + pitches$`Last15L/RPitcherHR`[q] + pitches$`Last30L/RPitcherHR`[q])/8)*((pitches$`Last15Pitch%`[q] + pitches$`Last30Pitch%`[q] + pitches$`Last15L/RPitch%`[q] + pitches$`Last30L/RPitch%`[q])/4))
    RBIstuff=RBIstuff+(((pitches$Last30RBIs[q] + pitches$Last60RBIs[q] + pitches$`Last30L/RRBIs`[q] + pitches$`Last60L/RRBIs`[q] + pitches$Last15PitcherRBIs[q] + pitches$Last30PitcherRBIs[q] + pitches$`Last15L/RPitcherRBIs`[q] + pitches$`Last30L/RPitcherRBIs`[q])/8)*((pitches$`Last15Pitch%`[q] + pitches$`Last30Pitch%`[q] + pitches$`Last15L/RPitch%`[q] + pitches$`Last30L/RPitch%`[q])/4))
    Runstuff=Runstuff+(((pitches$Last30Runs[q] + pitches$Last60Runs[q] + pitches$`Last30L/RRuns`[q] + pitches$`Last60L/RRuns`[q] + pitches$Last15PitcherRuns[q] + pitches$Last30PitcherRuns[q] + pitches$`Last15L/RPitcherRuns`[q] + pitches$`Last30L/RPitcherRuns`[q])/8)*((pitches$`Last15Pitch%`[q] + pitches$`Last30Pitch%`[q] + pitches$`Last15L/RPitch%`[q] + pitches$`Last30L/RPitch%`[q])/4))
    Kstuff=Kstuff+(((pitches$Last30SO[q] + pitches$Last60SO[q] + pitches$`Last30L/RSO`[q] + pitches$`Last60L/RSO`[q] + pitches$Last15PitcherSO[q] + pitches$Last30PitcherSO[q] + pitches$`Last15L/RPitcherSO`[q] + pitches$`Last30L/RPitcherSO`[q])/8)*((pitches$`Last15Pitch%`[q] + pitches$`Last30Pitch%`[q] + pitches$`Last15L/RPitch%`[q] + pitches$`Last30L/RPitch%`[q])/4))
  }
  Batters$HitStuff[i]=hitstuff
  Batters$TBStuff[i]=TBstuff
  Batters$HRStuff[i]=HRstuff
  Batters$RBIStuff[i]=RBIstuff
  Batters$RunStuff[i]=Runstuff
  Batters$KStuff[i]=Kstuff
}
Batters <- filter(Batters, !is.na(HitStuff)) 
for (i in 1:nrow(Batters)) {
  stats <- filter(Batters, RunStuff!=Inf & playerid==Batters$playerid[i])
  stat <- filter(Batters, RunStuff!=Inf)
  Batters$`HitStuff+`[i]=Batters$HitStuff[i]/mean(Batters$HitStuff)
  Batters$`TBStuff+`[i]=Batters$TBStuff[i]/mean(Batters$TBStuff)
  Batters$`HRStuff+`[i]=Batters$HRStuff[i]/mean(Batters$HRStuff)
  Batters$`RBIStuff+`[i]=Batters$RBIStuff[i]/mean(Batters$RBIStuff)
  if (nrow(stats)>0) {
    Batters$`RunStuff+`[i]=stats$RunStuff/mean(stat$RunStuff)
  } else {
    Batters$`RunStuff+`[i]=NA
  }
}
BatterLogs <- data.frame()
for (i in 1:nrow(Batters)) {
  q = fg_batter_game_logs(Batters$playerid[i], year(Sys.Date())) 
  k = fg_batter_game_logs(Batters$playerid[i], year(Sys.Date())-1)
  s = fg_batter_game_logs(Batters$playerid[i], year(Sys.Date())-2)
  f = fg_batter_game_logs(Batters$playerid[i], year(Sys.Date())-3)
  g = fg_batter_game_logs(Batters$playerid[i], year(Sys.Date())-4)
  if (nrow(k)<1 & nrow(q)<1 & nrow(s)<1 & nrow(f)<1 & nrow(g)<1) {
    BatterLogs <- BatterLogs
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)<1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g)
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, f)
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)<1) {
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, s)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)<1 & nrow(g)<1) {
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)<1 & nrow(f)<1 & nrow(g)<1) {
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, k)
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, f)
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, s)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)<1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, k)
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, f, s)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, f, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, f, k)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)<1) {
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, s, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)<1) {
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, s, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)<1 & nrow(g)<1) {
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, q, k)
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, f, s)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, f, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, f, k)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, s, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, s, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, q, k)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, f, s, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, f, s, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, f, q, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)<1) {
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, f, s, q)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, f, s, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, f, s, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, f, q, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, s, q, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, f, s, q, k)
  } else {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI)
    BatterLogs <- rbind(BatterLogs, g, f, s, q, k)
  }
}
BatterLogs <- BatterLogs %>% 
  mutate(TB= `1B` + 2*`2B` + 3*`3B` + 4*`HR`) %>% 
  mutate(Home_Away=ifelse(grepl("@", Opp), "Away", "Home")) %>% 
  mutate(Full_Opp=ifelse(grepl("ATL", Opp), "Atlanta Braves", ifelse(grepl("LAD", Opp), "Los Angeles Dodgers", ifelse(grepl("TEX", Opp), "Texas Rangers", ifelse(grepl("NYM", Opp), "New York Mets", ifelse(grepl("SEA", Opp), "Seattle Mariners", ifelse(grepl("KCR", Opp), "Kansas City Royals", ifelse(grepl("SDP", Opp), "San Diego Padres", ifelse(grepl("MIL", Opp), "Milwaukee Brewers", ifelse(grepl("CLE", Opp), "Cleveland Guardians", ifelse(grepl("BAL", Opp), "Baltimore Orioles", ifelse(grepl("HOU", Opp), "Houston Astros", ifelse(grepl("TBR", Opp), "Tampa Bay Rays", ifelse(grepl("PHI", Opp), "Philadelphia Phillies", ifelse(grepl("SFG", Opp), "San Francisco Giants", ifelse(grepl("TOR", Opp), "Toronto Blue Jays", ifelse(grepl("COL", Opp), "Colorado Rockies", ifelse(grepl("CHC", Opp), "Chicago Cubs", ifelse(grepl("STL", Opp), "St. Louis Cardinals", ifelse(grepl("BOS", Opp), "Boston Red Sox", ifelse(grepl("OAK", Opp), "Oakland Athletics", ifelse(grepl("MIN", Opp), "Minnesota Twins", ifelse(grepl("PIT", Opp), "Pittsburgh Pirates", ifelse(grepl("ARI", Opp), "Arizona Diamondbacks", ifelse(grepl("WSN", Opp), "Washington Nationals", ifelse(grepl("LAA", Opp), "Los Angeles Angels", ifelse(grepl("DET", Opp), "Detroit Tigers", ifelse(grepl("MIA", Opp), "Miami Marlins", ifelse(grepl("NYY", Opp), "New York Yankees", ifelse(grepl("CIN", Opp), "Cincinnati Reds", ifelse(grepl("CHW", Opp), "Chicago White Sox", ""))))))))))))))))))))))))))))))) %>% 
  mutate(Team=ifelse(grepl("ATL", Team), "Atlanta Braves", ifelse(grepl("LAD", Team), "Los Angeles Dodgers", ifelse(grepl("TEX", Team), "Texas Rangers", ifelse(grepl("NYM", Team), "New York Mets", ifelse(grepl("SEA", Team), "Seattle Mariners", ifelse(grepl("KCR", Team), "Kansas City Royals", ifelse(grepl("SDP", Team), "San Diego Padres", ifelse(grepl("MIL", Team), "Milwaukee Brewers", ifelse(grepl("CLE", Team), "Cleveland Guardians", ifelse(grepl("BAL", Team), "Baltimore Orioles", ifelse(grepl("HOU", Team), "Houston Astros", ifelse(grepl("TBR", Team), "Tampa Bay Rays", ifelse(grepl("PHI", Team), "Philadelphia Phillies", ifelse(grepl("SFG", Team), "San Francisco Giants", ifelse(grepl("TOR", Team), "Toronto Blue Jays", ifelse(grepl("COL", Team), "Colorado Rockies", ifelse(grepl("CHC", Team), "Chicago Cubs", ifelse(grepl("STL", Team), "St. Louis Cardinals", ifelse(grepl("BOS", Team), "Boston Red Sox", ifelse(grepl("OAK", Team), "Oakland Athletics", ifelse(grepl("MIN", Team), "Minnesota Twins", ifelse(grepl("PIT", Team), "Pittsburgh Pirates", ifelse(grepl("ARI", Team), "Arizona Diamondbacks", ifelse(grepl("WSN", Team), "Washington Nationals", ifelse(grepl("LAA", Team), "Los Angeles Angels", ifelse(grepl("DET", Team), "Detroit Tigers", ifelse(grepl("MIA", Team), "Miami Marlins", ifelse(grepl("NYY", Team), "New York Yankees", ifelse(grepl("CIN", Team), "Cincinnati Reds", ifelse(grepl("CHW", Team), "Chicago White Sox", ""))))))))))))))))))))))))))))))) %>% 
  mutate(Ballpark=case_when(Home_Away=="Home"~Team,
                            Home_Away=="Away"~Full_Opp),
         Month=month(Date),
         DayOfWeek=as.POSIXlt(Date)$wday,
         Game_Type="R")
if (Todays_Games$series_description[1]=="Regular Season") {
  playoff_BatterLog <- data.frame()
} else {
  playoff_BatterLog <- playoff_pbp %>% 
    select(batter, game_date)
  playoff_BatterLog <- unique.array(playoff_BatterLog)
  colnames(playoff_BatterLog) <- c("batter", "Date")
  for (i in 1:nrow(playoff_BatterLog)) {
    stats <- filter(BattersInfo, xMLBAMID==playoff_BatterLog$batter[i])
    if (nrow(stats)>0) {
      playoff_BatterLog$PlayerName[i]=stats$PlayerName
      playoff_BatterLog$playerid[i]=stats$playerid
    } else {
      playoff_BatterLog$PlayerName[i]=NA
      playoff_BatterLog$playerid[i]=NA
    }
    stats <- filter(pbp, batter==playoff_BatterLog$batter[i] & game_date==playoff_BatterLog$Date[i])
    playoff_BatterLog$Team[i]=stats$HittingTeam[1]
    playoff_BatterLog$Opp[i]=stats$PitchingTeam[1]
    playoff_BatterLog$Full_Opp[i]=stats$PitchingTeam[1]
    playoff_BatterLog$PA[i]=nrow(stats)
    playoff_BatterLog$H[i]=sum(stats$Hits)
    playoff_BatterLog$`1B`[i]=sum(stats$events=="single")
    playoff_BatterLog$`2B`[i]=sum(stats$events=="double")
    playoff_BatterLog$`3B`[i]=sum(stats$events=="triple")
    playoff_BatterLog$HR[i]=sum(stats$events=="home_run")
    playoff_BatterLog$`TB`[i]=sum(stats$TB)
    playoff_BatterLog$RBI[i]=sum(stats$RBIs)
    if (stats$inning_topbot[1]=="Top") {
      playoff_BatterLog$Home_Away[i]="Away"
      playoff_BatterLog$Ballpark[i]=stats$PitchingTeam[1]
    } else {
      playoff_BatterLog$Home_Away[i]="Home"
      playoff_BatterLog$Ballpark[i]=stats$HittingTeam[1]
    }
    stats <- filter(pbp, game_date==playoff_BatterLog$Date[i])
    playoff_BatterLog$R[i]=sum(grepl(paste(playoff_BatterLog$PlayerName[i], "scores"), stats$des)==TRUE | grepl(paste(playoff_BatterLog$PlayerName[i], "homers"), stats$des)==TRUE)
    playoff_BatterLog$Month[i]=month(playoff_BatterLog$Date[i])
    playoff_BatterLog$DayOfWeek[i]=as.POSIXlt(playoff_BatterLog$Date)$wday
    playoff_BatterLog$Game_Type[i]="P"
  }
  playoff_BatterLog <- playoff_BatterLog %>% 
    select(PlayerName, playerid, Date, Team, Opp, PA, H, `1B`, `2B`, `3B`, HR, R, RBI, TB, Home_Away, Full_Opp, Ballpark, Month, DayOfWeek, Game_Type)
}
BatterLogs <- rbind(BatterLogs, playoff_BatterLog) %>% 
  arrange(desc(Date))

PitcherLogs <- data.frame()
for (i in 1:nrow(Pitcher)) {
  q = fg_pitcher_game_logs(Pitcher$playerid[i], year(Sys.Date())) 
  k = fg_pitcher_game_logs(Pitcher$playerid[i], year(Sys.Date())-1)
  s = fg_pitcher_game_logs(Pitcher$playerid[i], year(Sys.Date())-2)
  f = fg_pitcher_game_logs(Pitcher$playerid[i], year(Sys.Date())-3)
  g = fg_pitcher_game_logs(Pitcher$playerid[i], year(Sys.Date())-4)
  if (nrow(k)<1 & nrow(q)<1 & nrow(s)<1 & nrow(f)<1 & nrow(g)<1) {
    PitcherLogs <- PitcherLogs
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)<1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g)
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, f)
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)<1) {
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, s)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)<1 & nrow(g)<1) {
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)<1 & nrow(f)<1 & nrow(g)<1) {
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, k)
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, f)
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, s)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)<1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, k)
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, f, s)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, f, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, f, k)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)<1) {
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, s, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)<1) {
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, s, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)<1 & nrow(g)<1) {
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, q, k)
  } else if (nrow(k)<1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, f, s)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, f, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, f, k)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, s, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, s, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, q, k)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, f, s, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, f, s, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, f, q, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)<1) {
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, f, s, q)
  } else if (nrow(k)<1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, f, s, q)
  } else if (nrow(k)>=1 & nrow(q)<1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, f, s, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)<1 & nrow(f)>=1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, f, q, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)<1 & nrow(g)>=1) {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, s, q, k)
  } else if (nrow(k)>=1 & nrow(q)>=1 & nrow(s)>=1 & nrow(f)>=1 & nrow(g)<1) {
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, f, s, q, k)
  } else {
    g = g %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    f = f %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    s = s %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    q = q %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    k = k %>% 
      select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO)
    PitcherLogs <- rbind(PitcherLogs, g, f, s, q, k)
  }
}
NRFILogs <- PitcherLogs %>% 
  select(-SO) %>% 
  mutate(Full_Opp=ifelse(grepl("ATL", Opp), "Atlanta Braves", ifelse(grepl("LAD", Opp), "Los Angeles Dodgers", ifelse(grepl("TEX", Opp), "Texas Rangers", ifelse(grepl("NYM", Opp), "New York Mets", ifelse(grepl("SEA", Opp), "Seattle Mariners", ifelse(grepl("KCR", Opp), "Kansas City Royals", ifelse(grepl("SDP", Opp), "San Diego Padres", ifelse(grepl("MIL", Opp), "Milwaukee Brewers", ifelse(grepl("CLE", Opp), "Cleveland Guardians", ifelse(grepl("BAL", Opp), "Baltimore Orioles", ifelse(grepl("HOU", Opp), "Houston Astros", ifelse(grepl("TBR", Opp), "Tampa Bay Rays", ifelse(grepl("PHI", Opp), "Philadelphia Phillies", ifelse(grepl("SFG", Opp), "San Francisco Giants", ifelse(grepl("TOR", Opp), "Toronto Blue Jays", ifelse(grepl("COL", Opp), "Colorado Rockies", ifelse(grepl("CHC", Opp), "Chicago Cubs", ifelse(grepl("STL", Opp), "St. Louis Cardinals", ifelse(grepl("BOS", Opp), "Boston Red Sox", ifelse(grepl("OAK", Opp), "Oakland Athletics", ifelse(grepl("MIN", Opp), "Minnesota Twins", ifelse(grepl("PIT", Opp), "Pittsburgh Pirates", ifelse(grepl("ARI", Opp), "Arizona Diamondbacks", ifelse(grepl("WSN", Opp), "Washington Nationals", ifelse(grepl("LAA", Opp), "Los Angeles Angels", ifelse(grepl("DET", Opp), "Detroit Tigers", ifelse(grepl("MIA", Opp), "Miami Marlins", ifelse(grepl("NYY", Opp), "New York Yankees", ifelse(grepl("CIN", Opp), "Cincinnati Reds", ifelse(grepl("CHW", Opp), "Chicago White Sox", ""))))))))))))))))))))))))))))))) %>% 
  mutate(Team=ifelse(grepl("ATL", Team), "Atlanta Braves", ifelse(grepl("LAD", Team), "Los Angeles Dodgers", ifelse(grepl("TEX", Team), "Texas Rangers", ifelse(grepl("NYM", Team), "New York Mets", ifelse(grepl("SEA", Team), "Seattle Mariners", ifelse(grepl("KCR", Team), "Kansas City Royals", ifelse(grepl("SDP", Team), "San Diego Padres", ifelse(grepl("MIL", Team), "Milwaukee Brewers", ifelse(grepl("CLE", Team), "Cleveland Guardians", ifelse(grepl("BAL", Team), "Baltimore Orioles", ifelse(grepl("HOU", Team), "Houston Astros", ifelse(grepl("TBR", Team), "Tampa Bay Rays", ifelse(grepl("PHI", Team), "Philadelphia Phillies", ifelse(grepl("SFG", Team), "San Francisco Giants", ifelse(grepl("TOR", Team), "Toronto Blue Jays", ifelse(grepl("COL", Team), "Colorado Rockies", ifelse(grepl("CHC", Team), "Chicago Cubs", ifelse(grepl("STL", Team), "St. Louis Cardinals", ifelse(grepl("BOS", Team), "Boston Red Sox", ifelse(grepl("OAK", Team), "Oakland Athletics", ifelse(grepl("MIN", Team), "Minnesota Twins", ifelse(grepl("PIT", Team), "Pittsburgh Pirates", ifelse(grepl("ARI", Team), "Arizona Diamondbacks", ifelse(grepl("WSN", Team), "Washington Nationals", ifelse(grepl("LAA", Team), "Los Angeles Angels", ifelse(grepl("DET", Team), "Detroit Tigers", ifelse(grepl("MIA", Team), "Miami Marlins", ifelse(grepl("NYY", Team), "New York Yankees", ifelse(grepl("CIN", Team), "Cincinnati Reds", ifelse(grepl("CHW", Team), "Chicago White Sox", ""))))))))))))))))))))))))))))))) %>% 
  mutate(Ballpark=case_when(HomeAway=="H"~Team,
                            HomeAway=="A"~Full_Opp),
         Month=month(Date),
         DayOfWeek=as.POSIXlt(Date)$wday) %>%
  arrange(desc(Date))
PitcherLogs <- PitcherLogs %>% 
  mutate(Full_Opp=ifelse(grepl("ATL", Opp), "Atlanta Braves", ifelse(grepl("LAD", Opp), "Los Angeles Dodgers", ifelse(grepl("TEX", Opp), "Texas Rangers", ifelse(grepl("NYM", Opp), "New York Mets", ifelse(grepl("SEA", Opp), "Seattle Mariners", ifelse(grepl("KCR", Opp), "Kansas City Royals", ifelse(grepl("SDP", Opp), "San Diego Padres", ifelse(grepl("MIL", Opp), "Milwaukee Brewers", ifelse(grepl("CLE", Opp), "Cleveland Guardians", ifelse(grepl("BAL", Opp), "Baltimore Orioles", ifelse(grepl("HOU", Opp), "Houston Astros", ifelse(grepl("TBR", Opp), "Tampa Bay Rays", ifelse(grepl("PHI", Opp), "Philadelphia Phillies", ifelse(grepl("SFG", Opp), "San Francisco Giants", ifelse(grepl("TOR", Opp), "Toronto Blue Jays", ifelse(grepl("COL", Opp), "Colorado Rockies", ifelse(grepl("CHC", Opp), "Chicago Cubs", ifelse(grepl("STL", Opp), "St. Louis Cardinals", ifelse(grepl("BOS", Opp), "Boston Red Sox", ifelse(grepl("OAK", Opp), "Oakland Athletics", ifelse(grepl("MIN", Opp), "Minnesota Twins", ifelse(grepl("PIT", Opp), "Pittsburgh Pirates", ifelse(grepl("ARI", Opp), "Arizona Diamondbacks", ifelse(grepl("WSN", Opp), "Washington Nationals", ifelse(grepl("LAA", Opp), "Los Angeles Angels", ifelse(grepl("DET", Opp), "Detroit Tigers", ifelse(grepl("MIA", Opp), "Miami Marlins", ifelse(grepl("NYY", Opp), "New York Yankees", ifelse(grepl("CIN", Opp), "Cincinnati Reds", ifelse(grepl("CHW", Opp), "Chicago White Sox", ""))))))))))))))))))))))))))))))) %>% 
  mutate(Team=ifelse(grepl("ATL", Team), "Atlanta Braves", ifelse(grepl("LAD", Team), "Los Angeles Dodgers", ifelse(grepl("TEX", Team), "Texas Rangers", ifelse(grepl("NYM", Team), "New York Mets", ifelse(grepl("SEA", Team), "Seattle Mariners", ifelse(grepl("KCR", Team), "Kansas City Royals", ifelse(grepl("SDP", Team), "San Diego Padres", ifelse(grepl("MIL", Team), "Milwaukee Brewers", ifelse(grepl("CLE", Team), "Cleveland Guardians", ifelse(grepl("BAL", Team), "Baltimore Orioles", ifelse(grepl("HOU", Team), "Houston Astros", ifelse(grepl("TBR", Team), "Tampa Bay Rays", ifelse(grepl("PHI", Team), "Philadelphia Phillies", ifelse(grepl("SFG", Team), "San Francisco Giants", ifelse(grepl("TOR", Team), "Toronto Blue Jays", ifelse(grepl("COL", Team), "Colorado Rockies", ifelse(grepl("CHC", Team), "Chicago Cubs", ifelse(grepl("STL", Team), "St. Louis Cardinals", ifelse(grepl("BOS", Team), "Boston Red Sox", ifelse(grepl("OAK", Team), "Oakland Athletics", ifelse(grepl("MIN", Team), "Minnesota Twins", ifelse(grepl("PIT", Team), "Pittsburgh Pirates", ifelse(grepl("ARI", Team), "Arizona Diamondbacks", ifelse(grepl("WSN", Team), "Washington Nationals", ifelse(grepl("LAA", Team), "Los Angeles Angels", ifelse(grepl("DET", Team), "Detroit Tigers", ifelse(grepl("MIA", Team), "Miami Marlins", ifelse(grepl("NYY", Team), "New York Yankees", ifelse(grepl("CIN", Team), "Cincinnati Reds", ifelse(grepl("CHW", Team), "Chicago White Sox", ""))))))))))))))))))))))))))))))) %>% 
  mutate(Ballpark=case_when(HomeAway=="H"~Team,
                            HomeAway=="A"~Full_Opp),
         Month=month(Date),
         DayOfWeek=as.POSIXlt(Date)$wday,
         Game_Type="R")

for (i in 1:nrow(PitcherLogs)) {
  pitcher_id <- filter(Pitcher, playerid==PitcherLogs$playerid[i])
  PitcherLogs$MLB_ID[i]=pitcher_id$xMLBAMID
  game_pbp <- filter(pbp, game_date==PitcherLogs$Date[i] & pitcher==PitcherLogs$MLB_ID[i])
  if (nrow(game_pbp)<1) {
    PitcherLogs$TB[i]=NA
    PitcherLogs$Hits[i]=NA
    PitcherLogs$HR[i]=NA
    PitcherLogs$TBF[i]=NA
    PitcherLogs$RBI[i]=NA
    PitcherLogs$Run[i]=NA
  } else {
    PitcherLogs$TB[i]=sum(game_pbp$TB)
    PitcherLogs$Hits[i]=sum(game_pbp$Hits)
    PitcherLogs$HR[i]=sum(game_pbp$events=="home_run")
    PitcherLogs$TBF[i]=nrow(game_pbp)
    PitcherLogs$RBI[i]=sum(game_pbp$RBIs)
    PitcherLogs$Run[i]=sum(game_pbp$Runs)
  }
}
PitcherLogs <- na.omit(PitcherLogs)
if (Todays_Games$series_description[1]=="Regular Season") {
  playoff_PitcherLog <- data.frame()
} else {
  playoff_PitcherLog <- playoff_pbp %>% 
    select(pitcher, game_date)
  playoff_PitcherLog <- unique.array(playoff_PitcherLog)
  colnames(playoff_PitcherLog) <- c("MLB_ID", "Date")
  for (i in 1:nrow(playoff_PitcherLog)) {
    stats <- filter(PitcherInfo, xMLBAMID==playoff_PitcherLog$MLB_ID[i])
    playoff_PitcherLog$PlayerName[i]=stats$PlayerName[1]
    playoff_PitcherLog$playerid[i]=stats$playerid[1]
    stats <- filter(pbp, pitcher==playoff_PitcherLog$MLB_ID[i] & game_date==playoff_PitcherLog$Date[i])
    playoff_PitcherLog$Team[i]=stats$PitchingTeam[1]
    playoff_PitcherLog$Opp[i]=stats$HittingTeam[1]
    playoff_PitcherLog$Full_Opp[i]=stats$HittingTeam[1]
    if (stats$inning_topbot[1]=="Top") {
      playoff_PitcherLog$HomeAway[i]="H"
      playoff_PitcherLog$Ballpark[i]=stats$PitchingTeam[1]
    } else {
      playoff_PitcherLog$HomeAway[i]="A"
      playoff_PitcherLog$Ballpark[i]=stats$HittingTeam
    }
    playoff_PitcherLog$TB[i]=sum(stats$TB)
    playoff_PitcherLog$Hits[i]=sum(stats$Hits)
    playoff_PitcherLog$HR[i]=sum(stats$events=="home_run")
    playoff_PitcherLog$TBF[i]=nrow(stats)
    playoff_PitcherLog$RBI[i]=sum(stats$RBIs)
    playoff_PitcherLog$Run[i]=sum(stats$Runs)
    playoff_PitcherLog$SO[i]=sum(stats$events=="strikeout")
    playoff_PitcherLog$Month[i]=month(playoff_PitcherLog$Date[i])
    playoff_PitcherLog$DayOfWeek[i]=as.POSIXlt(playoff_PitcherLog$Date[i])$wday
    playoff_PitcherLog$Game_Type[i]="P"
  }
  playoff_PitcherLog <- playoff_PitcherLog %>% 
    select(PlayerName, playerid, Date, Team, Opp, HomeAway, SO, Full_Opp, Ballpark, Month, DayOfWeek, Game_Type, MLB_ID, TB, Hits, HR, TBF, RBI, Run)
}
PitcherLogs <- rbind(PitcherLogs, playoff_PitcherLog) %>% 
  arrange(desc(Date))

for (i in 1:nrow(Batters)) {
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i]),10)
  Batters$`Last 10 ABs`[i]=sum(stats$PA)/nrow(stats)
  Batters$`Last 10 Hitting%`[i]=sum(stats$H)/sum(stats$PA)
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Home_Away==Batters$Home_Away[i]),10)
  Batters$`Last 10 Home/Away Hitting%`[i]=sum(stats$H)/sum(stats$PA)
  stats <- filter(BatterLogs, playerid==Batters$playerid[i] & Full_Opp==Batters$Full_Opp[i])
  if (nrow(stats)>2) {
    Batters$`vOpp Hitting%`[i]=sum(stats$H)/sum(stats$PA)
  } else {
    Batters$`vOpp Hitting%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Ballpark==Batters$Ballpark[i]), 30)
  if (nrow(stats)>2) {
    Batters$`Ballpark Hitting%`[i]=sum(stats$H)/sum(stats$PA)
  } else {
    Batters$`Ballpark Hitting%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Month==month(today())), 60)
  if (nrow(stats)>2) {
    Batters$`Month Hitting%`[i]=sum(stats$H)/sum(stats$PA)
  } else {
    Batters$`Month Hitting%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & DayOfWeek==as.POSIXlt(today())$wday),60)
  if (nrow(stats)>2) {
    Batters$`Day of Week Hitting%`[i]=sum(stats$H)/sum(stats$PA)
  } else {
    Batters$`Day of Week Hitting%`[i]=NA
  }
  stats <- filter(pbp, batter==Batters$xMLBAMID[i] & pitcher==Batters$OppStarterID[i])
  vStartPitcher=nrow(stats)
  if (nrow(stats)>0) {
    Batters$`vStarting Pitcher Hitting%`[i]=sum(stats$Hits)/nrow(stats)
  } else {
    Batters$`vStarting Pitcher Hitting%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Game_Type=="P"), 15)
  if (nrow(stats)>2) {
    Batters$`Playoff Hitting%`[i]=sum(stats$H)/sum(stats$PA)
  } else {
    Batters$`Playoff Hitting%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i]),10)
  Batters$`Last 10 TB%`[i]=sum(stats$TB)/sum(stats$PA)
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Home_Away==Batters$Home_Away[i]),10)
  Batters$`Last 10 Home/Away TB%`[i]=sum(stats$TB)/sum(stats$PA)
  stats <- filter(BatterLogs, playerid==Batters$playerid[i] & Full_Opp==Batters$Full_Opp[i])
  if (nrow(stats)>2) {
    Batters$`vOpp TB%`[i]=sum(stats$TB)/sum(stats$PA)
  } else {
    Batters$`vOpp TB%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Ballpark==Batters$Ballpark[i]),30)
  if (nrow(stats)>2) {
    Batters$`Ballpark TB%`[i]=sum(stats$TB)/sum(stats$PA)
  } else {
    Batters$`Ballpark TB%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Month==month(today())),60)
  if (nrow(stats)>2) {
    Batters$`Month TB%`[i]=sum(stats$TB)/sum(stats$PA)
  } else {
    Batters$`Month TB%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & DayOfWeek==as.POSIXlt(today())$wday),60)
  if (nrow(stats)>2) {
    Batters$`Day of Week TB%`[i]=sum(stats$TB)/sum(stats$PA)
  } else {
    Batters$`Day of Week TB%`[i]=NA
  }
  stats <- filter(pbp, batter==Batters$xMLBAMID[i] & pitcher==Batters$OppStarterID[i])
  Batters$TBProp[i]=paste(Batters$TBProp[i], "Total Bases")
  if (nrow(stats)>0) {
    Batters$`vStarting Pitcher TB%`[i]=sum(stats$TB)/nrow(stats)
  } else {
    Batters$`vStarting Pitcher TB%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Game_Type=="P"), 15)
  if (nrow(stats)>2) {
    Batters$`Playoff TB%`[i]=sum(stats$TB)/sum(stats$PA)
  } else {
    Batters$`Playoff TB%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i]),10)
  Batters$`Last 10 RBIs%`[i]=sum(stats$RBI)/sum(stats$PA)
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Home_Away==Batters$Home_Away[i]),10)
  Batters$`Last 10 Home/Away RBIs%`[i]=sum(stats$RBI)/sum(stats$PA)
  stats <- filter(BatterLogs, playerid==Batters$playerid[i] & Full_Opp==Batters$Full_Opp[i])
  if (nrow(stats)>2) {
    Batters$`vOpp RBIs%`[i]=sum(stats$RBI)/sum(stats$PA)
  } else {
    Batters$`vOpp RBIs%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Ballpark==Batters$Ballpark[i]),30)
  if (nrow(stats)>2) {
    Batters$`Ballpark RBIs%`[i]=sum(stats$RBI)/sum(stats$PA)
  } else {
    Batters$`Ballpark RBIs%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Month==month(today())),60)
  if (nrow(stats)>2) {
    Batters$`Month RBIs%`[i]=sum(stats$RBI)/sum(stats$PA)
  } else {
    Batters$`Month RBIs%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & DayOfWeek==as.POSIXlt(today())$wday),60)
  if (nrow(stats)>2) {
    Batters$`Day of Week RBIs%`[i]=sum(stats$RBI)/sum(stats$PA)
  } else {
    Batters$`Day of Week RBIs%`[i]=NA
  }
  stats <- filter(pbp, batter==Batters$xMLBAMID[i] & pitcher==Batters$OppStarterID[i])
  Batters$RBIProp[i]=paste(Batters$RBIProp[i], "+ RBIs")
  if (nrow(stats)>0) {
    Batters$`vStarting Pitcher RBIs%`[i]=sum(stats$RBIs)/nrow(stats)
  } else {
    Batters$`vStarting Pitcher RBIs%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Game_Type=="P"), 15)
  if (nrow(stats)>2) {
    Batters$`Playoff RBIs%`[i]=sum(stats$RBIs)/sum(stats$PA)
  } else {
    Batters$`Playoff RBIs%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i]),10)
  Batters$`Last 10 HR%`[i]=sum(stats$HR)/sum(stats$PA)
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Home_Away==Batters$Home_Away[i]),10)
  Batters$`Last 10 Home/Away HR%`[i]=sum(stats$HR)/sum(stats$PA)
  stats <- filter(BatterLogs, playerid==Batters$playerid[i] & Full_Opp==Batters$Full_Opp[i])
  if (nrow(stats)>2) {
    Batters$`vOpp HR%`[i]=sum(stats$HR)/sum(stats$PA)
  } else {
    Batters$`vOpp HR%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Ballpark==Batters$Ballpark[i]),30)
  if (nrow(stats)>2) {
    Batters$`Ballpark HR%`[i]=sum(stats$HR)/sum(stats$PA)
  } else {
    Batters$`Ballpark HR%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Month==month(today())),60)
  if (nrow(stats)>2) {
    Batters$`Month HR%`[i]=sum(stats$HR)/sum(stats$PA)
  } else {
    Batters$`Month HR%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & DayOfWeek==as.POSIXlt(today())$wday),60)
  if (nrow(stats)>2) {
    Batters$`Day of Week HR%`[i]=sum(stats$HR)/sum(stats$PA)
  } else {
    Batters$`Day of Week HR%`[i]=NA
  }
  stats <- filter(pbp, batter==Batters$xMLBAMID[i] & pitcher==Batters$OppStarterID[i])
  Batters$HRProp[i]=paste(Batters$HRProp[i], "+ HRs")
  if (nrow(stats)>0) {
    Batters$`vStarting Pitcher HR%`[i]=sum(stats$events=="home_run")/nrow(stats)
  } else {
    Batters$`vStarting Pitcher HR%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Game_Type=="P"), 15)
  if (nrow(stats)>2) {
    Batters$`Playoff HR%`[i]=sum(stats$HR)/sum(stats$PA)
  } else {
    Batters$`Playoff HR%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i]),10)
  Batters$`Last 10 Runs%`[i]=sum(stats$R)/sum(stats$PA)
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Home_Away==Batters$Home_Away[i]),10)
  Batters$`Last 10 Home/Away Runs%`[i]=sum(stats$R)/sum(stats$PA)
  stats <- filter(BatterLogs, playerid==Batters$playerid[i] & Full_Opp==Batters$Full_Opp[i])
  if (nrow(stats)>2) {
    Batters$`vOpp Runs%`[i]=sum(stats$R)/sum(stats$PA)
  } else {
    Batters$`vOpp Runs%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Ballpark==Batters$Ballpark[i]), 30)
  if (nrow(stats)>2) {
    Batters$`Ballpark Runs%`[i]=sum(stats$R)/sum(stats$PA)
  } else {
    Batters$`Ballpark Runs%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Month==month(today())), 60)
  if (nrow(stats)>2) {
    Batters$`Month Runs%`[i]=sum(stats$R)/sum(stats$PA)
  } else {
    Batters$`Month Runs%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & DayOfWeek==as.POSIXlt(today())$wday), 60)
  if (nrow(stats)>2) {
    Batters$`Day of Week Runs%`[i]=sum(stats$R)/sum(stats$PA)
  } else {
    Batters$`Day of Week Runs%`[i]=NA
  }
  stats <- filter(pbp, pitcher==Batters$OppStarterID[i])
  if (vStartPitcher>0) {
    Batters$`vStarting Pitcher Runs%`[i]=sum(grepl(paste(Batters$Name[i], "scores"), stats$des)==TRUE | grepl(paste(Batters$Name[i], "homers"), stats$des)==TRUE)/vStartPitcher
  } else {
    Batters$`vStarting Pitcher Runs%`[i]=NA
  }
  stats <- head(filter(BatterLogs, playerid==Batters$playerid[i] & Game_Type=="P"), 15)
  if (nrow(stats)>2) {
    Batters$`Playoff Runs%`[i]=sum(stats$R)/sum(stats$PA)
  } else {
    Batters$`Playoff Runs%`[i]=NA
  }
  stats <- head(filter(PitcherLogs, playerid==Batters$OppStarterPlayerID[i]),10)
  Batters$`OppStarterHitting`[i]=sum(stats$Hits)/sum(stats$TBF)
  Batters$`OppStarterTB`[i]=sum(stats$TB)/sum(stats$TBF)
  Batters$`OppStarterHR`[i]=sum(stats$HR)/sum(stats$TBF)
  Batters$`OppStarterRBIs`[i]=sum(stats$RBI)/sum(stats$TBF)
  Batters$`OppStarterRuns`[i]=sum(stats$Run)/sum(stats$TBF)
  if (Batters$Home_Away[i]=="Home") {
    stats <- head(filter(PitcherLogs, playerid==Batters$OppStarterPlayerID[i] & HomeAway=="A"),10)
  } else {
    stats <- head(filter(PitcherLogs, playerid==Batters$OppStarterPlayerID[i] & HomeAway=="H"),10)
  }
  Batters$`OppStarterHome/AwayHitting`[i]=sum(stats$Hits)/sum(stats$TBF)
  Batters$`OppStarterHome/AwayTB`[i]=sum(stats$TB)/sum(stats$TBF)
  Batters$`OppStarterHome/AwayHR`[i]=sum(stats$HR)/sum(stats$TBF)
  Batters$`OppStarterHome/AwayRBIs`[i]=sum(stats$RBI)/sum(stats$TBF)
  Batters$`OppStarterHome/AwayRuns`[i]=sum(stats$Run)/sum(stats$TBF)
  stats <- filter(PitcherLogs, playerid==Batters$OppStarterPlayerID[i] & Full_Opp==Batters$Team[i])
  if (nrow(stats)>0) {
    Batters$`OppStarterOppHitting`[i]=sum(stats$Hits)/sum(stats$TBF)
    Batters$`OppStarterOppTB`[i]=sum(stats$TB)/sum(stats$TBF)
    Batters$`OppStarterOppHR`[i]=sum(stats$HR)/sum(stats$TBF)
    Batters$`OppStarterOppRBIs`[i]=sum(stats$RBI)/sum(stats$TBF)
    Batters$`OppStarterOppRuns`[i]=sum(stats$Run)/sum(stats$TBF)
  } else {
    Batters$`OppStarterOppHitting`[i]=NA
    Batters$`OppStarterOppTB`[i]=NA
    Batters$`OppStarterOppHR`[i]=NA
    Batters$`OppStarterOppRBIs`[i]=NA
    Batters$`OppStarterOppRuns`[i]=NA
  }
  stats <- filter(PitcherLogs, playerid==Batters$OppStarterPlayerID[i] & Ballpark==Batters$Ballpark[i])
  if (nrow(stats)>0) {
    Batters$`OppStarterBallparkHitting`[i]=sum(stats$Hits)/sum(stats$TBF)
    Batters$`OppStarterBallparkTB`[i]=sum(stats$TB)/sum(stats$TBF)
    Batters$`OppStarterBallparkHR`[i]=sum(stats$HR)/sum(stats$TBF)
    Batters$`OppStarterBallparkRBIs`[i]=sum(stats$RBI)/sum(stats$TBF)
    Batters$`OppStarterBallparkRuns`[i]=sum(stats$Run)/sum(stats$TBF)
  } else {
    Batters$`OppStarterBallparkHitting`[i]=NA
    Batters$`OppStarterBallparkTB`[i]=NA
    Batters$`OppStarterBallparkHR`[i]=NA
    Batters$`OppStarterBallparkRBIs`[i]=NA
    Batters$`OppStarterBallparkRuns`[i]=NA
  }
  stats <- filter(PitcherLogs, playerid==Batters$OppStarterPlayerID[i] & Month==month(today()))
  if (nrow(stats)>0) {
    Batters$`OppStarterMonthHitting`[i]=sum(stats$Hits)/sum(stats$TBF)
    Batters$`OppStarterMonthTB`[i]=sum(stats$TB)/sum(stats$TBF)
    Batters$`OppStarterMonthHR`[i]=sum(stats$HR)/sum(stats$TBF)
    Batters$`OppStarterMonthRBIs`[i]=sum(stats$RBI)/sum(stats$TBF)
    Batters$`OppStarterMonthRuns`[i]=sum(stats$Run)/sum(stats$TBF)
  } else {
    Batters$`OppStarterMonthHitting`[i]=NA
    Batters$`OppStarterMonthTB`[i]=NA
    Batters$`OppStarterMonthHR`[i]=NA
    Batters$`OppStarterMonthRBIs`[i]=NA
    Batters$`OppStarterMonthRuns`[i]=NA
  }
  stats <- filter(PitcherLogs, playerid==Batters$OppStarterPlayerID[i] & DayOfWeek==as.POSIXlt(today())$wday)
  if (nrow(stats)>0) {
    Batters$`OppStarterDayofWeekHitting`[i]=sum(stats$Hits)/sum(stats$TBF)
    Batters$`OppStarterDayofWeekTB`[i]=sum(stats$TB)/sum(stats$TBF)
    Batters$`OppStarterDayofWeekHR`[i]=sum(stats$HR)/sum(stats$TBF)
    Batters$`OppStarterDayofWeekRBIs`[i]=sum(stats$RBI)/sum(stats$TBF)
    Batters$`OppStarterDayofWeekRuns`[i]=sum(stats$Run)/sum(stats$TBF)
  } else {
    Batters$`OppStarterDayofWeekHitting`[i]=NA
    Batters$`OppStarterDayofWeekTB`[i]=NA
    Batters$`OppStarterDayofWeekHR`[i]=NA
    Batters$`OppStarterDayofWeekRBIs`[i]=NA
    Batters$`OppStarterDayofWeekRuns`[i]=NA
  }
  stats <- filter(PitcherLogs, playerid==Batters$OppStarterPlayerID[i] & Game_Type=="P")
  if (nrow(stats)>0) {
    Batters$`OppStarterPlayoffHitting`[i]=sum(stats$Hits)/sum(stats$TBF)
    Batters$`OppStarterPlayoffTB`[i]=sum(stats$TB)/sum(stats$TBF)
    Batters$`OppStarterPlayoffHR`[i]=sum(stats$HR)/sum(stats$TBF)
    Batters$`OppStarterPlayoffRBIs`[i]=sum(stats$RBI)/sum(stats$TBF)
    Batters$`OppStarterPlayoffRuns`[i]=sum(stats$Run)/sum(stats$TBF)
  } else {
    Batters$`OppStarterPlayoffHitting`[i]=NA
    Batters$`OppStarterPlayoffTB`[i]=NA
    Batters$`OppStarterPlayoffHR`[i]=NA
    Batters$`OppStarterPlayoffRBIs`[i]=NA
    Batters$`OppStarterPlayoffRuns`[i]=NA
  }
}
for (i in 1:nrow(Batters)) {
  stats <- Batters %>% 
    select(HitStuff, `Last 10 Hitting%`, `Last 10 Home/Away Hitting%`, `vOpp Hitting%`, `Ballpark Hitting%`, `Month Hitting%`, `Day of Week Hitting%`, `vStarting Pitcher Hitting%`, `Playoff Hitting%`, `OppStarterHome/AwayHitting`, OppStarterBallparkHitting, OppStarterOppHitting, OppStarterMonthHitting, OppStarterDayofWeekHitting, OppStarterPlayoffHitting, OppStarterHitting)
  Batters$Hits[i]=sum(stats[i, 1:16], na.rm = TRUE)/(sum(!is.na(stats[i, 1:16]))) * Batters$`Last 10 ABs`[i]
  stats <- Batters %>% 
    select(TBStuff, `Last 10 TB%`, `Last 10 Home/Away TB%`, `vOpp TB%`, `Ballpark TB%`, `Month TB%`, `Day of Week TB%`, `vStarting Pitcher TB%`, `Playoff TB%`, `OppStarterHome/AwayTB`, OppStarterBallparkTB, OppStarterOppTB, OppStarterMonthTB, OppStarterDayofWeekTB, OppStarterPlayoffTB, OppStarterTB)
  Batters$TotalBases[i]=sum(stats[i, 1:16], na.rm = TRUE)/(sum(!is.na(stats[i, 1:16]))) * Batters$`Last 10 ABs`[i]
  stats <- Batters %>% 
    select(RBIStuff, `Last 10 RBIs%`, `Last 10 Home/Away RBIs%`, `vOpp RBIs%`, `Ballpark RBIs%`, `Month RBIs%`, `Day of Week RBIs%`, `vStarting Pitcher RBIs%`, `Playoff RBIs%`, `OppStarterHome/AwayRBIs`, OppStarterBallparkRBIs, OppStarterOppRBIs, OppStarterMonthRBIs, OppStarterDayofWeekRBIs, OppStarterPlayoffRBIs, OppStarterRBIs)
  Batters$RBIs[i]=sum(stats[i, 1:16], na.rm = TRUE)/(sum(!is.na(stats[i, 1:16]))) * Batters$`Last 10 ABs`[i]
  stats <- Batters %>% 
    select(HRStuff, `Last 10 HR%`, `Last 10 Home/Away HR%`, `vOpp HR%`, `Ballpark HR%`, `Month HR%`, `Day of Week HR%`, `vStarting Pitcher HR%`, `Playoff HR%`, `OppStarterHome/AwayHR`, OppStarterBallparkHR, OppStarterOppHR, OppStarterMonthHR, OppStarterDayofWeekHR, OppStarterPlayoffHR, OppStarterHR)
  Batters$HR[i]=sum(stats[i, 1:16], na.rm = TRUE)/(sum(!is.na(stats[i, 1:16]))) * Batters$`Last 10 ABs`[i]
  stats <- Batters %>% 
    select(RunStuff, `Last 10 Runs%`, `Last 10 Home/Away Runs%`, `vOpp Runs%`, `Ballpark Runs%`, `Month Runs%`, `Day of Week Runs%`, `vStarting Pitcher Runs%`, `Playoff Runs%`, `OppStarterHome/AwayRuns`, OppStarterBallparkRuns, OppStarterOppRuns, OppStarterMonthRuns, OppStarterDayofWeekRuns, OppStarterPlayoffRuns, OppStarterRuns)
  Batters$Runs[i]=sum(stats[i, 1:16], na.rm = TRUE)/(sum(!is.na(stats[i, 1:16]))) * Batters$`Last 10 ABs`[i]
}
Bets <- Batters %>% 
  mutate(`Hits + Runs + RBIs`=Hits + Runs + RBIs) %>% 
  select(Name, Team, Opp, OppStarter, Hits, TotalBases, RBIs, HR, Runs, `Hits + Runs + RBIs`)
View(Bets)
if (is.na(SGPTeam)) {
  SGP <- NA
} else {
  sgp_opp <- filter(Todays_Games, teams_home_team_name==SGPTeam | teams_away_team_name==SGPTeam)
  if (sgp_opp$teams_home_team_name[1]==SGPTeam) {
    sgp_opp=sgp_opp$teams_away_team_name[1]
  } else {
    sgp_opp=sgp_opp$teams_home_team_name[1]
  }
  SGP <- filter(Bets, Team==SGPTeam | Team==sgp_opp)
  SGP_TB <- SGP %>% 
    select(Name, Team, Opp, OppStarter, TotalBases) %>% 
    arrange(-TotalBases) %>% 
    mutate(Prop="2+ Total Bases") %>% 
    select(-TotalBases)
  SGP_TB <- head(SGP_TB, 1)
  SGP_Hits <- filter(SGP, Name!=SGP_TB$Name[1] | Name!=SGP_TB$Name[2]) %>% 
    select(Name, Team, Opp, OppStarter, Hits) %>% 
    arrange(-Hits) %>% 
    mutate(Prop="1+ Hits") %>% 
    select(-Hits)
  SGP_Hits <- head(SGP_Hits, 2)
  SGP_RBI <- SGP %>% 
    select(Name, Team, Opp, OppStarter, RBIs) %>% 
    arrange(-RBIs) %>% 
    mutate(Prop="1+ RBIs") %>% 
    select(-RBIs)
  SGP_RBI <- head(SGP_RBI, 1)
  SGP_Run <- SGP %>% 
    select(Name, Team, Opp, OppStarter, Runs) %>% 
    arrange(-Runs) %>% 
    mutate(Prop="1+ Runs") %>% 
    select(-Runs)
  SGP_Run <- head(SGP_Run, 1)
  SGP <- rbind(SGP_TB, SGP_Hits, SGP_RBI, SGP_Run)
  view(SGP)
}
if (weekdays(today())=="Tuesday") {
  DingerTuesday <- Batters %>% 
    select(Name, Team, Full_Opp, HR)
  for (i in 1:nrow(DingerTuesday)) {
    stats <- filter(DingerTuesday, Full_Opp==DingerTuesday$Team[i] | Team==DingerTuesday$Team[i])
    DingerTuesday$DingerTuesdayHRRate[i]=.7*mean(stats$HR) + .3*DingerTuesday$HR[i]
  }
  DingerTuesday <- DingerTuesday %>% 
    select(-HR) %>% 
    arrange(-DingerTuesdayHRRate)
  view(DingerTuesday)
} else {
  DingerTuesday <- NA
}
StrikeOutProps <- read_csv("MLB Sims/Hitting Stats/untitled folder/mlb-player-props-strikeouts.csv", skip=1)
StrikeOutProps$Player[StrikeOutProps$Player %in% oldnames] <- newnames[match(StrikeOutProps$Player, oldnames, nomatch = 0)]
for (i in 1:nrow(Pitcher)) {
  stats <- filter(StrikeOutProps, Player==Pitcher$PlayerName[i])
  if (nrow(stats)>0) {
    Pitcher$Opponent[i]=stats$Opp
    Pitcher$StrikeoutProp[i]=stats$Strikeouts...7
  } else {
    Pitcher$Opponent[i]=NA
    Pitcher$StrikeoutProp[i]=NA
  }
}
Pitcher <- filter(Pitcher, !is.na(StrikeoutProp)) %>% 
  mutate(Home_Away=ifelse(grepl("@", Opponent), "A", "H")) %>% 
  mutate(Full_Opponent=ifelse(grepl("ATL", Opponent), "Atlanta Braves", ifelse(grepl("LAD", Opponent), "Los Angeles Dodgers", ifelse(grepl("TEX", Opponent), "Texas Rangers", ifelse(grepl("NYM", Opponent), "New York Mets", ifelse(grepl("SEA", Opponent), "Seattle Mariners", ifelse(grepl("KC", Opponent), "Kansas City Royals", ifelse(grepl("SD", Opponent), "San Diego Padres", ifelse(grepl("MIL", Opponent), "Milwaukee Brewers", ifelse(grepl("CLE", Opponent), "Cleveland Guardians", ifelse(grepl("BAL", Opponent), "Baltimore Orioles", ifelse(grepl("HOU", Opponent), "Houston Astros", ifelse(grepl("TB", Opponent), "Tampa Bay Rays", ifelse(grepl("PHI", Opponent), "Philadelphia Phillies", ifelse(grepl("SF", Opponent), "San Francisco Giants", ifelse(grepl("TOR", Opponent), "Toronto Blue Jays", ifelse(grepl("COL", Opponent), "Colorado Rockies", ifelse(grepl("CHC", Opponent), "Chicago Cubs", ifelse(grepl("STL", Opponent), "St. Louis Cardinals", ifelse(grepl("BOS", Opponent), "Boston Red Sox", ifelse(grepl("OAK", Opponent), "Oakland Athletics", ifelse(grepl("MIN", Opponent), "Minnesota Twins", ifelse(grepl("PIT", Opponent), "Pittsburgh Pirates", ifelse(grepl("ARI", Opponent), "Arizona Diamondbacks", ifelse(grepl("WSH", Opponent), "Washington Nationals", ifelse(grepl("LAA", Opponent), "Los Angeles Angels", ifelse(grepl("DET", Opponent), "Detroit Tigers", ifelse(grepl("MIA", Opponent), "Miami Marlins", ifelse(grepl("NYY", Opponent), "New York Yankees", ifelse(grepl("CIN", Opponent), "Cincinnati Reds", ifelse(grepl("CWS", Opponent), "Chicago White Sox", ""))))))))))))))))))))))))))))))) %>% 
  mutate(Ballpark=case_when(Home_Away=="H"~Team,
                            Home_Away=="A"~Full_Opponent))
for (i in 1:nrow(Pitcher)) {
  stats <- head(filter(PitcherLogs, playerid==Pitcher$playerid[i]),7)
  Pitcher$Last7Cutoff[i]=tail(stats$Date,1)
  stats <- head(filter(PitcherLogs, playerid==Pitcher$playerid[i]),15)
  Pitcher$Last15Cutoff[i]=tail(stats$Date,1)
  stats <- head(filter(PitcherLogs, playerid==Pitcher$playerid[i]),30)
  Pitcher$Last30Cutoff[i]=tail(stats$Date,1)
  stats <- head(filter(PitcherLogs, playerid==Pitcher$playerid[i]),60)
  Pitcher$Last60Cutoff[i]=tail(stats$Date,1)
  stats <- filter(Batters, OppStarterID==Pitcher$xMLBAMID[i])
  Pitcher$KStuff[i]=mean(stats$KStuff)
}
New_Pitcher <- filter(Pitcher, !is.na(KStuff)) 
New_Pitcher <- New_Pitcher %>% 
  mutate(`KStuff+`=KStuff/mean(New_Pitcher$KStuff))
for (i in 1:nrow(New_Pitcher)) {
  if (New_Pitcher$`KStuff+`[i]>1) {
    stats <- head(filter(PitcherLogs, playerid==New_Pitcher$playerid[i]),10)
    New_Pitcher$`Last 10 K's`[i]=sum(stats$SO>New_Pitcher$StrikeoutProp[i])/nrow(stats)
    stats <- head(filter(PitcherLogs, playerid==New_Pitcher$playerid[i] & HomeAway==New_Pitcher$Home_Away[i]),10)
    New_Pitcher$`Last 10 Home/Away K's`[i]=sum(stats$SO>New_Pitcher$StrikeoutProp[i])/nrow(stats)
    stats <- filter(PitcherLogs, playerid==New_Pitcher$playerid[i] & Full_Opp==New_Pitcher$Full_Opponent[i])
    New_Pitcher$`v Opp K's`[i]=sum(stats$SO>New_Pitcher$StrikeoutProp[i])/nrow(stats)
    stats <- head(filter(PitcherLogs, playerid==New_Pitcher$playerid[i] & Month==month(today())), 10)
    New_Pitcher$`Month K's`[i]=sum(stats$SO>New_Pitcher$StrikeoutProp[i])/nrow(stats)
    stats <- head(filter(PitcherLogs, playerid==New_Pitcher$playerid[i] & DayOfWeek==as.POSIXlt(today())$wday), 10)
    New_Pitcher$`Day of Week K's`[i]=sum(stats$SO>New_Pitcher$StrikeoutProp[i])/nrow(stats)
    New_Pitcher$`KStuff+`[i]=New_Pitcher$`KStuff+`[i]
    New_Pitcher$StrikeoutProp[i]=paste("Over", New_Pitcher$StrikeoutProp[i])
  } else {
    stats <- head(filter(PitcherLogs, playerid==New_Pitcher$playerid[i]),10)
    New_Pitcher$`Last 10 K's`[i]=sum(stats$SO<New_Pitcher$StrikeoutProp[i])/nrow(stats)
    stats <- head(filter(PitcherLogs, playerid==New_Pitcher$playerid[i] & HomeAway==New_Pitcher$Home_Away[i]),10)
    New_Pitcher$`Last 10 Home/Away K's`[i]=sum(stats$SO<New_Pitcher$StrikeoutProp[i])/nrow(stats)
    stats <- filter(PitcherLogs, playerid==New_Pitcher$playerid[i] & Full_Opp==New_Pitcher$Full_Opponent[i])
    New_Pitcher$`v Opp K's`[i]=sum(stats$SO<New_Pitcher$StrikeoutProp[i])/nrow(stats)
    stats <- head(filter(PitcherLogs, playerid==New_Pitcher$playerid[i] & Month==month(today())), 10)
    New_Pitcher$`Month K's`[i]=sum(stats$SO<New_Pitcher$StrikeoutProp[i])/nrow(stats)
    stats <- head(filter(PitcherLogs, playerid==New_Pitcher$playerid[i] & DayOfWeek==as.POSIXlt(today())$wday), 10)
    New_Pitcher$`Day of Week K's`[i]=sum(stats$SO<New_Pitcher$StrikeoutProp[i])/nrow(stats)
    New_Pitcher$`KStuff+`[i]=2-New_Pitcher$`KStuff+`[i]
    New_Pitcher$StrikeoutProp[i]=paste("Under", New_Pitcher$StrikeoutProp[i])
  }
}
StrikeoutBets <- New_Pitcher %>% 
  mutate(KRate=case_when(is.na(`v Opp K's`) & is.na(`Month K's`) & is.na(`Day of Week K's`)~(.3*`Last 10 K's` + .7*`Last 10 Home/Away K's`)*`KStuff+`,
                         !is.na(`v Opp K's`) & is.na(`Month K's`) & is.na(`Day of Week K's`)~(.167*`Last 10 K's` + .333*`Last 10 Home/Away K's` + .5*`v Opp K's`)*`KStuff+`,
                         is.na(`v Opp K's`) & !is.na(`Month K's`) & is.na(`Day of Week K's`)~(.167*`Last 10 K's` + .333*`Last 10 Home/Away K's` + .5*`Month K's`)*`KStuff+`,
                         is.na(`v Opp K's`) & is.na(`Month K's`) & !is.na(`Day of Week K's`)~(.167*`Last 10 K's` + .333*`Last 10 Home/Away K's` + .5*`Day of Week K's`)*`KStuff+`,
                         !is.na(`v Opp K's`) & is.na(`Month K's`) & !is.na(`Day of Week K's`)~(.1*`Last 10 K's` + .2*`Last 10 Home/Away K's` + .4*`v Opp K's` + .3*`Day of Week K's`)*`KStuff+`,
                         is.na(`v Opp K's`) & !is.na(`Month K's`) & !is.na(`Day of Week K's`)~(.1*`Last 10 K's` + .2*`Last 10 Home/Away K's` + .3*`Month K's` + .4*`Day of Week K's`)*`KStuff+`,
                         !is.na(`v Opp K's`) & !is.na(`Month K's`) & is.na(`Day of Week K's`)~(.1*`Last 10 K's` + .2*`Last 10 Home/Away K's` + .3*`Month K's` + .4*`v Opp K's`)*`KStuff+`,
                         TRUE~(.1*`Last 10 K's` + .15*`Last 10 Home/Away K's` + .3*`v Opp K's` + .2*`Month K's` + .25*`Day of Week K's`)*`KStuff+`)) %>% 
  select(PlayerName, Team, Opponent, StrikeoutProp, KRate) %>% 
  arrange(-KRate)
view(StrikeoutBets)
Team_Totals <- data.frame(unique(Bets$Team))
colnames(Team_Totals) <- "Team"
for (i in 1:nrow(Team_Totals)) {
  stats <- filter(Bets, Team==Team_Totals$Team[i])
  Team_Totals$ScoringNumber[i]=.6*mean(stats$TotalBases) + .4*mean(stats$Runs)
}
Team_Totals <- Team_Totals %>% 
  arrange(-ScoringNumber)
View(Team_Totals)