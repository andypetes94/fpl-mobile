#!/usr/bin/env Rscript

#---------------------------------------------
# FPL Data Downloader Script
# Command-line friendly with output path
#---------------------------------------------

suppressMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(progress)
  library(optparse)
  library(zoo)
})

#-------------------------------
# Command-line options
#-------------------------------
option_list <- list(
  make_option(c("-o", "--output"), type = "character", default = "./25_26",
              help = "Output directory for CSV files [default %default]", metavar = "path")
)

opt <- parse_args(OptionParser(option_list = option_list))
out_dir <- opt$output

# Create output folder if it doesn't exist
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

#-------------------------------
# 1️⃣ Download General Info
#-------------------------------
cat("Downloading General Info...\n")
FPL_General <- GET('https://fantasy.premierleague.com/api/bootstrap-static/')
JSON_Output <- fromJSON(content(FPL_General, as="text", encoding = "UTF-8"))

Teams <- JSON_Output$teams %>%
  select(id, name, short_name, strength, strength_overall_home, strength_overall_away, strength_attack_home, strength_attack_away, strength_defence_home, strength_defence_away) %>%
  rename(team_name = name,
         short_team_name = short_name,
         code = id)

General_Info <- JSON_Output$elements %>%
  select(id, first_name, second_name, web_name, team, element_type, photo) %>%
  left_join(Teams, by = c('team' = 'code'))

write.csv(General_Info, file.path(out_dir, "General_Info.csv"), row.names = FALSE)
cat("General Info saved.\n\n")

#-------------------------------
# 2️⃣ Download Player History
#-------------------------------
cat("Downloading Player History...\n")
All_Players <- list()
pb <- progress_bar$new(
  format = "  Downloading [:bar] :percent ETA: :eta",
  total = nrow(General_Info), clear = FALSE, width = 60
)

for (id_value in General_Info$id) {
  FPL_Player <- GET(paste0('https://fantasy.premierleague.com/api/element-summary/', id_value, '/'))
  JSON_Output <- fromJSON(content(FPL_Player, as = "text", encoding = "UTF-8"))
  
  History <- JSON_Output$history %>%
    mutate(id = id_value)
  
  temp_output <- History %>%
    left_join(General_Info, by = 'id')
  
  All_Players <- bind_rows(All_Players, temp_output)
  pb$tick()
}

test <- All_Players %>%
  group_by(round, short_team_name) %>%
  summarise(total_xg_conceded = max(expected_goals_conceded), .groups = "drop")


All_Players <- All_Players %>%
  left_join(test, by = c('round', 'short_team_name'))


Opp_Team <- Teams$code

for (var in colnames(Teams)){
  
  nam <- paste0("opp_", var)
  
  #column <- sbdata_style[var] * sbdata_style$elo_adjust
  column <- Teams[var]
  
  colnames(column) <- c(nam)
  
  Opp_Team <- cbind(Opp_Team,column)
  
}

Opp_Team <- Opp_Team %>%
  select(-Opp_Team)

All_Players_Final <- All_Players %>%
  left_join(Opp_Team, by = c('opponent_team' = 'opp_code')) %>%
  mutate(team_strength = case_when(was_home == "TRUE" ~ strength_overall_home, 
                                   T ~ strength_overall_away),
         opp_team_strength = case_when(was_home == "FALSE" ~ opp_strength_overall_home, 
                                       T ~ opp_strength_overall_away),
         team_strength_att = case_when(was_home == "TRUE" ~ strength_attack_home, 
                                       T ~ strength_attack_away),
         opp_team_strength_att = case_when(was_home == "FALSE" ~ opp_strength_attack_home, 
                                           T ~ opp_strength_attack_away),
         team_strength_def = case_when(was_home == "TRUE" ~ strength_defence_home, 
                                       T ~ strength_defence_away),
         opp_team_strength_def = case_when(was_home == "FALSE" ~ opp_strength_defence_home, 
                                           T ~ opp_strength_defence_away)) %>%
  select(-strength_overall_home, -strength_overall_away, -strength_attack_home, -strength_attack_away, -strength_defence_home, -strength_defence_away) %>%
  select(-opp_strength_overall_home, -opp_strength_overall_away, -opp_strength_attack_home, -opp_strength_attack_away, -opp_strength_defence_home, -opp_strength_defence_away) %>%
  mutate(Position = case_when(element_type == 1 ~ "GK",
                              element_type == 2 ~ "DEF",
                              element_type == 3 ~ "MID",
                              element_type == 4 ~ "ST")) %>%
  mutate(Goals = case_when(was_home == "TRUE" ~ team_h_score,
                           T ~ team_a_score)) %>%
  mutate(Opp_Goals = case_when(was_home == "FALSE" ~ team_h_score,
                               T ~ team_a_score)) %>%
  mutate(name_club = paste0(web_name," (", short_team_name, ")")) %>%
  mutate(full_name = paste0(first_name, " ", second_name)) %>%
  select(-team_h_score, -team_a_score,-element,-fixture,-opponent_team,-element_type,-team, -team_name) %>%
  mutate(influence = as.numeric(influence),
         creativity = as.numeric(creativity),
         threat = as.numeric(threat),
         ict_index = as.numeric(ict_index),
         starts = as.numeric(starts),
         expected_goals = as.numeric(expected_goals),
         expected_assists = as.numeric(expected_assists),
         expected_goal_involvements = as.numeric(expected_goal_involvements),
         expected_goals_conceded = as.numeric(expected_goals_conceded),
         web_name = as.factor(web_name),
         #id = as.factor(id),
         short_team_name = as.factor(short_team_name),
         opp_short_team_name = as.factor(opp_short_team_name),
         Position = as.factor(Position)) %>%
  group_by(id) %>%
  mutate(cs_mean = lag(rollapplyr(clean_sheets, 5, mean, partial = TRUE)),
         #recent_starts = lag(rollapplyr(starts, 5, mean, partial = TRUE)),
         recent_starts = lag(rollapplyr(minutes, 5, mean, partial = TRUE)),
         points_mean = lag(rollapplyr(total_points, 5, mean, partial = TRUE)),
         bps_mean = lag(rollapplyr(bps, 5, mean, partial = TRUE)),
         xg_mean = lag(rollapplyr(expected_goals, 5, mean, partial = TRUE)),
         xa_mean = lag(rollapplyr(expected_assists, 5, mean, partial = TRUE)),
         xgi_mean = lag(rollapplyr(expected_goal_involvements, 5, mean, partial = TRUE)),
         xg_against = lag(rollapplyr(expected_goals_conceded, 5, mean, partial = TRUE)),
         goals_mean = lag(rollapplyr(Goals, 5, mean, partial = TRUE)),
         oppgoals_mean = lag(rollapplyr(Opp_Goals, 5, mean, partial = TRUE)),
         threat_mean = lag(rollapplyr(threat, 5, mean, partial = TRUE)),
         creativity_mean = lag(rollapplyr(creativity, 5, mean, partial = TRUE)),
         influence_mean = lag(rollapplyr(influence, 5, mean, partial = TRUE)),
         ict_mean = lag(rollapplyr(ict_index, 5, mean, partial = TRUE))) %>%
  ungroup() %>%
  group_by(short_team_name, round) %>%
  mutate(team_xg = sum(expected_goals)) %>%
  ungroup() %>%
  mutate(team_xg_mean = lag(rollapplyr(team_xg, 5, mean, partial = TRUE)))


write.csv(All_Players_Final, file.path(out_dir, "Players_History.csv"), row.names = FALSE)
cat("Players History saved.\n\n")

#-------------------------------
# 3️⃣ Players_Gameweek
#-------------------------------
cat("Generating Players_Gameweek...\n")
Players_Gameweek <- All_Players_Final %>%
  group_by(name_club, round, .drop = FALSE) %>%
  summarise(total_points = sum(total_points),
            opp_team = paste(opp_short_team_name, collapse=","),
            id = unique(id),
            games = n(),
            .groups = "drop") %>%
  as.data.frame() %>%
  tidyr::complete(name_club, round = All_Players_Final$round) %>%
  fill(id, .direction = "updown") %>%
  mutate(GW_Type = case_when(games == 2 ~ "DGW",
                             games == 1 ~ "SGW",
                             is.na(games) ~ "BGW")) 

write.csv(Players_Gameweek, file.path(out_dir, "Players_Gameweek.csv"), row.names = FALSE)
cat("Players_Gameweek saved.\n\n")

#-------------------------------
# 4️⃣ Fixtures
#-------------------------------
cat("Downloading Fixtures...\n")
Fixtures <- list()
pb <- progress_bar$new(
  format = "  Downloading [:bar] :percent ETA: :eta",
  total = nrow(General_Info), clear = FALSE, width = 60
)

for (id_value in General_Info$id) {
  FPL_Player <- GET(paste0('https://fantasy.premierleague.com/api/element-summary/', id_value, '/'))
  JSON_Output <- fromJSON(content(FPL_Player, as = "text", encoding = "UTF-8"))
  
  History <- JSON_Output$fixtures %>%
    mutate(id = id_value)
  
  temp_output <- History %>%
    left_join(General_Info, by = "id")
  
  Fixtures <- bind_rows(Fixtures, temp_output)
  pb$tick()
}

Fixtures2 <- Fixtures %>%
  mutate(team_id = case_when(is_home == "TRUE" ~ team_h,
                             T ~ team_a),
         opp_team_id = case_when(is_home == "FALSE" ~ team_h,
                                 T ~ team_a)) %>%
  #select(id, first_name, second_name, web_name, team, element_type, photo) %>%
  left_join(Opp_Team, by = c('opp_team_id' = 'opp_code')) %>%
  mutate(id = as.factor(id)) %>%
  rename(was_home = is_home) %>%
  mutate(team_strength = case_when(was_home == "TRUE" ~ strength_overall_home, 
                                   T ~ strength_overall_away),
         home_team = case_when(was_home == "TRUE" ~ team_name, 
                               T ~ opp_team_name),
         away_team = case_when(was_home == "FALSE" ~ team_name, 
                               T ~ opp_team_name),
         opp_team_strength = case_when(was_home == "FALSE" ~ opp_strength_overall_home, 
                                       T ~ opp_strength_overall_away),
         team_strength_att = case_when(was_home == "TRUE" ~ strength_attack_home, 
                                       T ~ strength_attack_away),
         opp_team_strength_att = case_when(was_home == "FALSE" ~ opp_strength_attack_home, 
                                           T ~ opp_strength_attack_away),
         team_strength_def = case_when(was_home == "TRUE" ~ strength_defence_home, 
                                       T ~ strength_defence_away),
         opp_team_strength_def = case_when(was_home == "FALSE" ~ opp_strength_defence_home, 
                                           T ~ opp_strength_defence_away)) %>%
  select(-strength_overall_home, -strength_overall_away, -strength_attack_home, -strength_attack_away, -strength_defence_home, -strength_defence_away) %>%
  select(-opp_strength_overall_home, -opp_strength_overall_away, -opp_strength_attack_home, -opp_strength_attack_away, -opp_strength_defence_home, -opp_strength_defence_away) %>%
  separate(event_name, c("string", "round")) %>%
  select(-string)

write.csv(Fixtures2, file.path(out_dir, "Fixtures.csv"), row.names = FALSE)
cat("Fixtures saved.\n\n")
cat("All files successfully downloaded to:", normalizePath(out_dir), "\n")