### LOAD IN TIDYVERSE, GLMNET, MATRIX, AND NECESSARY FILES ###

library(tidyverse)
library(glmnet)

### DON'T WORRY ABOUT THIS - IT JUST CHANGES THE NUMBER OF DIGITS DISPLAYED AND MAKES THINGS NEATER ###

options(scipen=999, digits = 2)

### THE FIRST ONE IS THE BIG CSV, THE SECOND IS THE SMALL CSV. SMALL CSV IS AVAILABLE THROUGH GITHUB; LARGE CSV MUST BE ACQUIRED THROUGH GOOGLE DOCS ###

### LARGE CSV LINK: https://drive.google.com/file/d/1dGRizxZRhm92COlmEaJbvwFKqJbbwDqn/view?usp=sharing ###

#all1819 <- read.csv("All_Events_1819_With_EV_PP_xG.csv", fileEncoding = "UTF-8-BOM") ###

all1819 <- read.csv("Small_Events_1819_With_EV_xG.csv", fileEncoding = "UTF-8-BOM")

### ADD IN YOUR EVENTS ###

fenwick <- c("SHOT", "MISS", "GOAL")
corsi <- c("SHOT", "MISS", "BLOCK", "GOAL")
even_strength <- c("3v3", "4v4", "5v5")
power_play <- c("5v4", "4v5", "5v3", "3v5", "4v3", "3v4")
home_power_play <- c("5v4", "5v3", "4v3")
away_power_play <- c("4v5", "3v5", "3v4")
empty_net <- c("5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3")
meaningful_events <- c("FAC", "GOAL", "BLOCK", "SHOT", "MISS", "HIT", "TAKE", "GIVE", "CHANGE")
stoppages <- c("PGSTR", "PGEND", "ANTHEM", "PSTR", "FAC", "GOAL", "STOP", "PENL", "PEND", "CHL", "GEND", "GOFF", "EISTR", "EIEND", "EGT", "EGPID")

### CREATE BACKUP IF ERRORS OCCUR - CAN RETURN TO ORIGINAL WITHOUT LOADING ANOTHER CSV ###

backup <- all1819

### IF YOU WANT TO MAKE THINGS SMALLER, YOU CAN FILTER FROM YOUR BACKUP TO ONLY TAKE A SMALL SAMPLE ###

### I COMMENTED IT OUT, BUT IF YOU REMOVE THOSE COMMENTS YOU WILL FILTER ONLY THE FIRST 100 GAMES ###

#all1819 <- backup %>%
  #filter(game_id<2018020101)

### ADD BACK TO BACKS ###

teams <- all1819 %>%
  group_by(game_date) %>%
  summarize(Yesterday_Home_Team = unique(home_team), Yesterday_Away_Team = unique(away_team))

teams <- teams %>%
  group_by(game_date) %>%
  summarise_all(funs(paste(na.omit(.), collapse = " - ")))

teams$game_date <- as.Date(teams$game_date) + 1
all1819$game_date <- as.Date(all1819$game_date)

all1819 <- left_join(all1819, teams)

all1819$Home_Home_Yesterday <- mapply(grepl, pattern=all1819$home_team, x=all1819$Yesterday_Home_Team)
all1819$Home_Away_Yesterday <- mapply(grepl, pattern=all1819$home_team, x=all1819$Yesterday_Away_Team)
all1819$Away_Home_Yesterday <- mapply(grepl, pattern=all1819$away_team, x=all1819$Yesterday_Home_Team)
all1819$Away_Away_Yesterday <- mapply(grepl, pattern=all1819$away_team, x=all1819$Yesterday_Away_Team)

all1819$Home_BTB <- ifelse((all1819$Home_Home_Yesterday==1 | all1819$Home_Away_Yesterday==1), 1, 0)
all1819$Away_BTB <- ifelse((all1819$Away_Home_Yesterday==1 | all1819$Away_Away_Yesterday==1), 1, 0)

### GET RID OF EVENTS WE DO NOT CARE ABOUT ###

all1819 <- all1819 %>%
  filter(game_period < 5)

### CONVERT NAs TO "MISSING PLAYER" ###

all1819$home_on_1 <- ifelse(is.na(all1819$home_on_1), "MISSING_PLAYER", all1819$home_on_1)
all1819$home_on_2 <- ifelse(is.na(all1819$home_on_2), "MISSING_PLAYER", all1819$home_on_2)
all1819$home_on_3 <- ifelse(is.na(all1819$home_on_3), "MISSING_PLAYER", all1819$home_on_3)
all1819$home_on_4 <- ifelse(is.na(all1819$home_on_4), "MISSING_PLAYER", all1819$home_on_4)
all1819$home_on_5 <- ifelse(is.na(all1819$home_on_5), "MISSING_PLAYER", all1819$home_on_5)
all1819$home_on_6 <- ifelse(is.na(all1819$home_on_6), "MISSING_PLAYER", all1819$home_on_6)
all1819$away_on_1 <- ifelse(is.na(all1819$away_on_1), "MISSING_PLAYER", all1819$away_on_1)
all1819$away_on_2 <- ifelse(is.na(all1819$away_on_2), "MISSING_PLAYER", all1819$away_on_2)
all1819$away_on_3 <- ifelse(is.na(all1819$away_on_3), "MISSING_PLAYER", all1819$away_on_3)
all1819$away_on_4 <- ifelse(is.na(all1819$away_on_4), "MISSING_PLAYER", all1819$away_on_4)
all1819$away_on_5 <- ifelse(is.na(all1819$away_on_5), "MISSING_PLAYER", all1819$away_on_5)
all1819$away_on_6 <- ifelse(is.na(all1819$away_on_6), "MISSING_PLAYER", all1819$away_on_6)

### ADD SHIFT CHANGE INDEX ###

all1819$shift_change <- ifelse(lag(all1819$home_on_1)==all1819$home_on_1 & lag(all1819$home_on_2)==all1819$home_on_2 & lag(all1819$home_on_3)==all1819$home_on_3 &
                                 lag(all1819$home_on_4)==all1819$home_on_4 & lag(all1819$home_on_5)==all1819$home_on_5 & lag(all1819$home_on_6)==all1819$home_on_6 &
                                 lag(all1819$away_on_1)==all1819$away_on_1 & lag(all1819$away_on_2)==all1819$away_on_2 & lag(all1819$away_on_3)==all1819$away_on_3 &
                                 lag(all1819$away_on_4)==all1819$away_on_4 & lag(all1819$away_on_5)==all1819$away_on_5 & lag(all1819$away_on_6)==all1819$away_on_6 &
                                 lag(all1819$game_score_state)==all1819$game_score_state & lag(all1819$game_period)==all1819$game_period &  
                                 lag(all1819$home_goalie)==all1819$home_goalie & lag(all1819$away_goalie)==all1819$away_goalie & lag(all1819$game_id)==all1819$game_id, 0, 1)

all1819$shift_change <- ifelse(is.na(all1819$shift_change), 0, all1819$shift_change)

all1819$shift_change_index <- cumsum(all1819$shift_change)

### ADD PP EXPIRY STATUS ###

all1819$home_pp_expiry <- ifelse(all1819$game_strength_state %in% even_strength & lag(all1819$game_strength_state) %in% home_power_play &
                                   all1819$shift_change==1, 1, 0)

all1819$home_pp_expiry <- ifelse((lag(all1819$event_type) %in% stoppages & all1819$game_seconds==lag(all1819$game_seconds)) | 
                                   (lag(lag(all1819$event_type)) %in% stoppages & lag(lag(all1819$game_seconds))==all1819$game_seconds) | 
                                   (lag(lag(lag(all1819$event_type))) %in% stoppages & lag(lag(lag(all1819$game_seconds)))==all1819$game_seconds),
                                 0, all1819$home_pp_expiry)

all1819$home_pp_expiry[is.na(all1819$home_pp_expiry)] <- 0

all1819$away_pp_expiry <- ifelse(all1819$game_strength_state %in% even_strength & lag(all1819$game_strength_state) %in% away_power_play &
                                   all1819$shift_change==1, 1, 0)

all1819$away_pp_expiry <- ifelse((lag(all1819$event_type) %in% stoppages & all1819$game_seconds==lag(all1819$game_seconds)) | 
                                   (lag(lag(all1819$event_type)) %in% stoppages & lag(lag(all1819$game_seconds))==all1819$game_seconds) | 
                                   (lag(lag(lag(all1819$event_type))) %in% stoppages & lag(lag(lag(all1819$game_seconds)))==all1819$game_seconds),
                                 0, all1819$away_pp_expiry)

all1819$away_pp_expiry[is.na(all1819$away_pp_expiry)] <- 0

### ADD IMPORTANT CONTEXT TO PBP DATA ###

all1819$event_zone <- ifelse(all1819$event_zone=="Def" & all1819$event_type=="BLOCK", "Off", all1819$event_zone)
all1819$home_zone <- ifelse(all1819$event_team==all1819$home_team, all1819$event_zone, NA)
all1819$home_zone <- ifelse(all1819$event_team==all1819$away_team & all1819$event_zone=="Off", "Def", all1819$home_zone)
all1819$home_zone <- ifelse(all1819$event_team==all1819$away_team & all1819$event_zone=="Def", "Off", all1819$home_zone)
all1819$home_zone <- ifelse(all1819$event_zone=="Neu", "Neu", all1819$home_zone)

all1819$home_ozs <- ifelse((all1819$event_type=="FAC" & all1819$home_zone=="Off") & (
  (all1819$shift_change==1) | 
    (all1819$game_seconds==lag(all1819$game_seconds) & lag(all1819$shift_change==1)) |
    (all1819$game_seconds==lag(lag(all1819$game_seconds)) & lag(lag((all1819$shift_change)==1))) |
    (all1819$game_seconds==lag(lag(lag(all1819$game_seconds))) & lag(lag(lag(all1819$shift_change)))==1) |
    (all1819$game_seconds==lag(lag(lag(lag(all1819$game_seconds)))) & lag(lag(lag(lag(all1819$shift_change))))==1)
), 1, 0)

all1819$away_ozs <- ifelse((all1819$event_type=="FAC" & all1819$home_zone=="Def") & (
  (all1819$shift_change==1) | 
    (all1819$game_seconds==lag(all1819$game_seconds) & lag(all1819$shift_change==1)) |
    (all1819$game_seconds==lag(lag(all1819$game_seconds)) & lag(lag((all1819$shift_change)==1))) |
    (all1819$game_seconds==lag(lag(lag(all1819$game_seconds))) & lag(lag(lag(all1819$shift_change)))==1) |
    (all1819$game_seconds==lag(lag(lag(lag(all1819$game_seconds)))) & lag(lag(lag(lag(all1819$shift_change))))==1)
), 1, 0)

all1819$nzs <- ifelse((all1819$event_type=="FAC" & all1819$home_zone=="Neu") & (
  (all1819$shift_change==1) | 
    (all1819$game_seconds==lag(all1819$game_seconds) & lag(all1819$shift_change==1)) |
    (all1819$game_seconds==lag(lag(all1819$game_seconds)) & lag(lag((all1819$shift_change)==1))) |
    (all1819$game_seconds==lag(lag(lag(all1819$game_seconds))) & lag(lag(lag(all1819$shift_change)))==1) |
    (all1819$game_seconds==lag(lag(lag(lag(all1819$game_seconds)))) & lag(lag(lag(lag(all1819$shift_change))))==1)
), 1, 0)


all1819$home_xGF <- ifelse(all1819$event_team==all1819$home_team, all1819$xG, 0)
all1819$away_xGF <- ifelse(all1819$event_team!=all1819$home_team, all1819$xG, 0)
all1819$home_ozs[is.na(all1819$home_ozs)] <- 0
all1819$away_ozs[is.na(all1819$away_ozs)] <- 0
all1819$tied <- ifelse(all1819$home_score==all1819$away_score, 1, 0)
all1819$home_lead_1 <- ifelse(all1819$home_score-all1819$away_score==1, 1, 0)
all1819$home_lead_2 <- ifelse(all1819$home_score-all1819$away_score==2, 1, 0)
all1819$home_lead_3 <- ifelse(all1819$home_score-all1819$away_score>=3, 1, 0)
all1819$away_lead_1 <- ifelse(all1819$home_score-all1819$away_score==(-1), 1, 0)
all1819$away_lead_2 <- ifelse(all1819$home_score-all1819$away_score==(-2), 1, 0)
all1819$away_lead_3 <- ifelse(all1819$home_score-all1819$away_score<=(-3), 1, 0)
all1819$Five <- ifelse(all1819$game_strength_state=="5v5", 1, 0)
all1819$Four <- ifelse(all1819$game_strength_state=="4v4", 1, 0)
all1819$Three <- ifelse(all1819$game_strength_state=="3v3", 1, 0)
all1819$home_xGF[is.na(all1819$home_xGF)] <- 0
all1819$away_xGF[is.na(all1819$away_xGF)] <- 0
all1819$period_1 <- ifelse(all1819$game_period=="1", 1, 0)
all1819$period_2 <- ifelse(all1819$game_period=="2", 1, 0)
all1819$period_3 <- ifelse(all1819$game_period=="3", 1, 0)
all1819$Home_BTB[is.na(all1819$Home_BTB)] <- 0
all1819$Away_BTB[is.na(all1819$Away_BTB)] <- 0

all1819ev <- all1819 %>%
  filter(game_strength_state %in% even_strength)

### GROUP SHIFTS TO PREPARE FOR RAPM ###

shifts_grouped <- all1819ev %>%
  group_by(game_id, shift_change_index, game_period, game_score_state, home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6, 
           away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, home_goalie, away_goalie) %>%
  summarise(shift_length = sum(event_length), homexGF = sum(home_xGF), awayxGF = sum(away_xGF), 
            Home_OZS = max(home_ozs), Away_OZS = max(away_ozs), NZS = max(nzs), Home_Up_1 = max(home_lead_1), Home_Up_2 = max(home_lead_2), Home_Up_3 = max(home_lead_3),
            Away_Up_1 = max(away_lead_1), Away_Up_2 = max(away_lead_2), Away_Up_3 = max(away_lead_3), Tied = max(tied), 
            State_5v5 = max(Five), State_4v4 = max(Four), State_3v3 = max(Three), Period_1 = max(period_1), Period_2 = max(period_2), Period_3 = max(period_3),
            Home_BTB = max(Home_BTB), Away_BTB = max(Away_BTB), Home_PPx = max(home_pp_expiry), Away_PPx = max(away_pp_expiry)) %>%
  filter(shift_length > 0)

### BUILD SHIFTS FROM THE PERSPECTIVE OF OFFENSE AS HOME ###

home_offense_all <- shifts_grouped %>%
  rename(offense_1 = home_on_1, offense_2 = home_on_2, offense_3 = home_on_3, offense_4 = home_on_4, offense_5 = home_on_5, offense_6 = home_on_6,
         defense_1 = away_on_1, defense_2 = away_on_2, defense_3 = away_on_3, defense_4 = away_on_4, defense_5 = away_on_5, defense_6 = away_on_6,
         Off_Zonestart = Home_OZS, Def_Zonestart = Away_OZS, Neu_Zonestart = NZS, Up_1 = Home_Up_1, Up_2 = Home_Up_2, Up_3 = Home_Up_3,
         Down_1 = Away_Up_1, Down_2 = Away_Up_2, Down_3 = Away_Up_3, xGF = homexGF, BTB = Home_BTB, Opp_BTB = Away_BTB, PPx = Home_PPx, PKx = Away_PPx) %>%
  select(game_id, shift_change_index, game_period, game_score_state,
         offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, 
         defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, 
         xGF, shift_length, shift_change_index, Tied, State_5v5, State_4v4, State_3v3, 
         Up_1, Up_2, Up_3, Down_1, Down_2, Down_3, Off_Zonestart, Def_Zonestart, Neu_Zonestart,
         offense_goalie = home_goalie, defense_goalie = away_goalie, Period_1, Period_2, Period_3, BTB, Opp_BTB, PPx, PKx) %>%
  mutate(xGF_60 = xGF*3600/shift_length, is_home = 1)

### BUILD SHIFTS FROM THE PERSPECTIVE OF OFFENSE AS AWAY ###

away_offense_all <- shifts_grouped %>%
  rename(offense_1 = away_on_1, offense_2 = away_on_2, offense_3 = away_on_3, offense_4 = away_on_4, offense_5 = away_on_5, offense_6 = away_on_6,
         defense_1 = home_on_1, defense_2 = home_on_2, defense_3 = home_on_3, defense_4 = home_on_4, defense_5 = home_on_5, defense_6 = home_on_6,
         Off_Zonestart = Away_OZS, Def_Zonestart = Home_OZS, Neu_Zonestart = NZS, Up_1 = Away_Up_1, Up_2 = Away_Up_2, Up_3 = Away_Up_3,
         Down_1 = Home_Up_1, Down_2 = Home_Up_2, Down_3 = Home_Up_3, xGF = awayxGF, BTB = Away_BTB, Opp_BTB = Home_BTB, PPx = Away_PPx, PKx = Home_PPx) %>%
  select(game_id, shift_change_index, game_period, game_score_state,
         offense_1, offense_2, offense_3, offense_4, offense_5, offense_6, 
         defense_1, defense_2, defense_3, defense_4, defense_5, defense_6, 
         xGF, shift_length, shift_change_index, Tied, State_5v5, State_4v4, State_3v3, 
         Up_1, Up_2, Up_3, Down_1, Down_2, Down_3, Off_Zonestart, Def_Zonestart, Neu_Zonestart,
         offense_goalie = away_goalie, defense_goalie = home_goalie, Period_1, Period_2, Period_3, BTB, Opp_BTB, PPx, PKx) %>%
  mutate(xGF_60 = xGF*3600/shift_length, is_home = 0)

### MAKE NOTE OF GOALIES USING FUNNY NAME - THEY WILL BE REMOVED LATER ###

home_offense_all$offense_1 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_1, "GOALIE.GUY", home_offense_all$offense_1)
home_offense_all$offense_2 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_2, "GOALIE.GUY", home_offense_all$offense_2)
home_offense_all$offense_3 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_3, "GOALIE.GUY", home_offense_all$offense_3)
home_offense_all$offense_4 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_4, "GOALIE.GUY", home_offense_all$offense_4)
home_offense_all$offense_5 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_5, "GOALIE.GUY", home_offense_all$offense_5)
home_offense_all$offense_6 <- ifelse(home_offense_all$offense_goalie==home_offense_all$offense_6, "GOALIE.GUY", home_offense_all$offense_6)

home_offense_all$defense_1 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_1, "GOALIE.GUY", home_offense_all$defense_1)
home_offense_all$defense_2 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_2, "GOALIE.GUY", home_offense_all$defense_2)
home_offense_all$defense_3 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_3, "GOALIE.GUY", home_offense_all$defense_3)
home_offense_all$defense_4 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_4, "GOALIE.GUY", home_offense_all$defense_4)
home_offense_all$defense_5 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_5, "GOALIE.GUY", home_offense_all$defense_5)
home_offense_all$defense_6 <- ifelse(home_offense_all$defense_goalie==home_offense_all$defense_6, "GOALIE.GUY", home_offense_all$defense_6)

away_offense_all$offense_1 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_1, "GOALIE.GUY", away_offense_all$offense_1)
away_offense_all$offense_2 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_2, "GOALIE.GUY", away_offense_all$offense_2)
away_offense_all$offense_3 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_3, "GOALIE.GUY", away_offense_all$offense_3)
away_offense_all$offense_4 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_4, "GOALIE.GUY", away_offense_all$offense_4)
away_offense_all$offense_5 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_5, "GOALIE.GUY", away_offense_all$offense_5)
away_offense_all$offense_6 <- ifelse(away_offense_all$offense_goalie==away_offense_all$offense_6, "GOALIE.GUY", away_offense_all$offense_6)

away_offense_all$defense_1 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_1, "GOALIE.GUY", away_offense_all$defense_1)
away_offense_all$defense_2 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_2, "GOALIE.GUY", away_offense_all$defense_2)
away_offense_all$defense_3 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_3, "GOALIE.GUY", away_offense_all$defense_3)
away_offense_all$defense_4 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_4, "GOALIE.GUY", away_offense_all$defense_4)
away_offense_all$defense_5 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_5, "GOALIE.GUY", away_offense_all$defense_5)
away_offense_all$defense_6 <- ifelse(away_offense_all$defense_goalie==away_offense_all$defense_6, "GOALIE.GUY", away_offense_all$defense_6)

combined_shifts <- full_join(home_offense_all, away_offense_all)

### REMOVE DUMMY VARIABLES THAT WE WILL NOT BE USING ###

subsetted_shifts = subset(combined_shifts, select = -c(game_id:game_period, game_score_state, xGF, Tied, State_5v5, Period_1:Period_3, offense_goalie, defense_goalie))

### CONVERT INTO DUMMY VARIABLES ###

combined_shifts_dummies <- fastDummies::dummy_cols(subsetted_shifts)

memory.limit(50000000)

combined_shifts_dummies = subset(combined_shifts_dummies, select = -c(offense_1:defense_6))


### CLEAN NAMES - THIS WAY WE CAN REDUCE TO EACH INDIVIDUAL SKATER ###

colnames(combined_shifts_dummies) = gsub("offense_1", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_2", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_3", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_4", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_5", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("offense_6", "offense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_1", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_2", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_3", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_4", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_5", "defense_", colnames(combined_shifts_dummies))
colnames(combined_shifts_dummies) = gsub("defense_6", "defense_", colnames(combined_shifts_dummies))

### REMOVE DUPLICATE SKATERS ###

combined_shifts_dummies <- as.data.frame(lapply(split.default(combined_shifts_dummies, names(combined_shifts_dummies)), function(x) Reduce(`+`, x)))

### REMOVE GOALIES ###

combined_shifts_dummies <- combined_shifts_dummies %>% select(-contains("Goalie"))
combined_shifts_dummies <- combined_shifts_dummies %>% select(-contains("Missing"))

### CREATE VECTORS TO PREPARE FOR RAPM CALCULATION ###

xGF60 <- as.numeric(c(combined_shifts_dummies$xGF_60))
shift_length <- as.numeric(c(combined_shifts_dummies$shift_length))
subsetted_dummies = subset(combined_shifts_dummies, select = -c(shift_length, xGF_60))
RAPM_xGF <- as.matrix(subsetted_dummies)
RAPM_xGF[!is.finite(RAPM_xGF)] <- 0

### CONVERT MATRIX TO SPARSE - MAKES CROSS VALIDATION TEN TIMES FASTER!!! ###

Sparse_RAPM_xGF <- Matrix(RAPM_xGF, sparse = TRUE)

Cross_Validated_Results <- cv.glmnet(x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)

### INSERT LAMBDA OBTAINED FROM CROSS VALIDATION INTO FULL RUN ###

Run_RAPM <- glmnet(x=Sparse_RAPM_xGF, y=xGF60, weights=shift_length, lambda = Cross_Validated_Results[["lambda.min"]], alpha=0, nfolds=10, standardize=FALSE, parallel=TRUE)

### OBTAIN COEFFICIENTS FROM THE REGRESSION ###

RAPM_coefficients <- as.data.frame(as.matrix(coef(Run_RAPM)))

### CLEAN DATA AND MAKE IT PRESENTABLE ###

Binded_Coefficients <- cbind(rownames(RAPM_coefficients), RAPM_coefficients) %>%
  rename(Player = `rownames(RAPM_coefficients)`, xGF_60 = s0)

write_excel_csv(Binded_Coefficients, "Vanilla_EV_RAPM_1819_Coefficients.csv")

offense_RAPM <- Binded_Coefficients %>%
  filter(grepl("offense", Binded_Coefficients$Player))

offense_RAPM$Player = str_replace_all(offense_RAPM$Player, "offense__", "")

defense_RAPM <- Binded_Coefficients %>%
  filter(grepl("defense", Binded_Coefficients$Player)) %>%
  rename(xGA_60 = xGF_60)

defense_RAPM$Player = str_replace_all(defense_RAPM$Player, "defense__", "")

joined_RAPM <- inner_join(offense_RAPM, defense_RAPM, by="Player")

joined_RAPM$xGPM_60 <- joined_RAPM$xGF_60 - joined_RAPM$xGA_60

joined_RAPM <- joined_RAPM %>%
  arrange(desc(xGPM_60))

### VIEW THE FINISHED PRODUCT! :) ###

View(joined_RAPM)

sd(joined_RAPM$xGPM_60)

write_excel_csv(joined_RAPM, "Cleaned_EV_Vanilla_RAPM_1819.csv")
