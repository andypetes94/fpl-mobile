
g_percentiles <- function(data, positions, gameweeks_1, gameweeks_2, mins_1, v) {     
  
  data %>%
    # Filter Positions
    filter(Position %in% positions) %>%
    # Filter Gameweeks
    filter(round >= gameweeks_1 & round <= gameweeks_2) %>%
    separate(photo, c("photo_id","string")) %>%
    mutate(pimage = paste0("<img src='","https://resources.premierleague.com/premierleague25/photos/players/110x140/",photo_id,".png'"," height='60'>")) %>%
    group_by(name_club,short_team_name,pimage,Position) %>%
    mutate(game = n()) %>%
    nest() %>%
    mutate(games = map(.x = data, ~ first(.x$game)),
           #Value = map(.x = data, ~ last(.x$value)),
           Value = map(.x = data, ~ last(.x$value)),
           starts = map(.x = data, ~ sum(.x$starts)),
           mins = map(.x = data, ~ sum(.x$minutes)),
           t_points = map(.x = data, ~ sum(.x$total_points)),
           xgc = map(.x = data, ~ sum(.x$expected_goals_conceded)),
           csheets = map(.x = data, ~ sum(.x$clean_sheets)),
           s = map(.x = data, ~ sum(.x$saves)),
           g = map(.x = data, ~ sum(.x$goals_scored)),
           a = map(.x = data, ~ sum(.x$assists)),
           xg = map(.x = data, ~ sum(.x$expected_goals)),
           xa = map(.x = data, ~ sum(.x$expected_assists)),
           xgi = map(.x = data, ~ sum(.x$expected_goal_involvements)),
           b = map(.x = data, ~ sum(.x$bps))) %>%
    select(-data) %>%
    mutate(Value = as.numeric(Value) / 10) %>%
    # Filter Value
    filter(Value <= v) %>%
    filter(mins >= mins_1) %>%
    unnest(cols = c(games, Value, starts, mins,t_points, xgc, s, g, a, xg, xa, xgi, b, csheets)) %>%
    as.data.frame() %>%
    mutate(ga = g + a) %>%
    # Per 90
    mutate(`xG p90` = (xg / mins) *90,
           `xG Conceded p90` = (xgc / mins) *90,
           `C Sheets p90` = (csheets / mins) *90,
           `Saves p90` = (s / mins) *90,
           `xA p90` = (xa / mins) *90,
           `xGI p90` = (xgi / mins) *90,
           `BPS p90` = (b / mins) *90,
           `Goals p90` = (g / mins) *90,
           `Assists p90` = (a / mins) *90,
           `Points p90` = (t_points / mins) *90,
           `G+A p90` = (ga / mins) *90) %>%
    {if(nrow(.) <= 100) 
      mutate(., xGoals = (ntile(`xG p90`,100) / n())*100,
             xAssists = (ntile(`xA p90`,100) / n())*100,
             Saves = (ntile(`Saves p90`,100) / n())*100,
             `xG Conceded` = 100 - ((ntile(`xG Conceded p90`,100) / n())*100),
             `C Sheets` = (ntile(`C Sheets p90`,100) / n())*100,
             `xG+xA` = (ntile(`xGI p90`,100) / n())*100,
             Bonus = (ntile(`BPS p90`,100) / n())*100,
             Goals = (ntile(`Goals p90`,100) / n())*100,
             Assists = (ntile(`Assists p90`,100) / n())*100,
             `Total Points` = (ntile(`Points p90`,100) / n())*100,
             `G+A` = (ntile(`G+A p90`,100) / n())*100)
      else if (nrow(.) > 100) 
        mutate(., xGoals = ntile(`xG p90`,100),
               xAssists = ntile(`xA p90`,100),
               Saves = ntile(`Saves p90`,100),
               `xG Conceded` = 100 - (ntile(`xG Conceded p90`,100)),
               `C Sheets` = ntile(`C Sheets p90`,100),
               `xG+xA` = ntile(`xGI p90`,100),
               Bonus = ntile(`BPS p90`,100),
               Goals = ntile(`Goals p90`,100),
               Assists = ntile(`Assists p90`,100),
               `Total Points` = ntile(`Points p90`,100),
               `G+A` = ntile(`G+A p90`,100))} %>%
    mutate(Goals = case_when(g == 0 ~ 0,
                             T ~ as.numeric(Goals)),
           `Overall Points` = t_points,
           `xG Conceded` = case_when(xgc == 0 ~ 0,
                                     T ~ as.numeric(`xG Conceded`)),
           `C Sheets` = case_when(csheets == 0 ~ 0,
                                  T ~ as.numeric(`C Sheets`)),
           Assists = case_when(a == 0 ~ 0,
                               T ~ as.numeric(Assists)),
           `G+A` = case_when(ga == 0 ~ 0,
                             T ~ as.numeric(`G+A`)))
  
  
}


comparison_parse <- function(data) {  
  
  if (unique(data$Position) == "GK"){
  
  metrics = c("Bonus","Total Points","xG Conceded","C Sheets","Saves")
  
} else if (unique(data$Position) == "DEF"){
  
  metrics = c("xGoals","xAssists","Bonus","Total Points","xG+xA","G+A","xG Conceded","C Sheets")
  
} else if (unique(data$Position) %in% c("MID")){
  
  metrics = c("xGoals","xAssists","Bonus","Total Points","xG+xA","Goals","G+A","xG Conceded","C Sheets")
  
}
  else if (unique(data$Position) %in% c("ST")){
  
  metrics = c("xGoals","xAssists","Bonus","Total Points","xG+xA","Goals","G+A")
  
}

# Filter Player Name
Comparison <- data %>%
  #filter(name_club %in% input$player_names) %>%
  pivot_longer(., cols = metrics, names_to = "Names", values_to = "Percentiles") %>%
  #pivot_longer(., cols = c("xG Conceded", "C Sheets", "Saves", "Bonus", "Total Points"), names_to = "Names", values_to = "Percentiles") %>%
  # c("xGoals","Goals","Bonus","Total Points","xG+xA","G+A","xG Conceded","C Sheets","Saves")
  mutate(Info = case_when(Names %in% c("Saves") ~ "Saves",
                          Names %in% c("xGoals","Goals") ~ "Goals",
                          Names %in% c("xG Conceded", "C Sheets") ~ "Defence",
                          Names %in% c("xG+xA","G+A","xAssists") ~ "Contributions",
                          Names %in% c("Bonus","Total Points") ~ "Points")) %>%
  mutate(ninety = round(mins / 90, digits = 1)) %>%
  mutate(Details = paste0(name_club, "\nFixtures: ",games," | Starts: ", starts, "\nMins: ", mins, " | 90s: ",ninety)) %>%
  mutate(v = paste0("£",Value," M")) %>%
  #mutate(final_image = paste0("<img src='./Logo4.png' width='30'> ", name_club, " - 23/24 Summary<br> <span style = 'font-size:8pt;'>Total Points: ", t_points ," | Latest Value: ", v, " | Total Mins: ", mins, " | Games Started: ", starts)) %>%
  mutate(final_image = paste0(pimage," ", name_club, " <img src='./Logo4.png' width='36'><br> <span style = 'font-size:12pt;'>Total Points: ", t_points ," | Recent Value: ", v, " | Total Mins: ", mins, " | Games Started: ", starts)) %>%
  mutate(stats = case_when(Names == "xGoals" ~ paste0("xG (", round(`xG p90`, digits = 2), ")"),
                           Names == "xAssists" ~ paste0("xA (", round(`xA p90`, digits = 2), ")"),
                           Names == "xG Conceded" ~ paste0("Team xGA (", round(`xG Conceded p90`, digits = 2), ")"),
                           Names == "xG+xA" ~ paste0("xG+xA (", round(`xGI p90`, digits = 2), ")"),
                           Names == "G+A" ~ paste0("G+A (", round(`G+A p90`, digits = 2), ")"),
                           Names == "Bonus" ~ paste0("BPS (", round(`BPS p90`, digits = 2), ")"),
                           Names == "Assists" ~ paste0("Assists (", round(`Assists p90`, digits = 2), ")"),
                           Names == "Goals" ~ paste0("Goals (", round(`Goals p90`, digits = 2), ")"),
                           Names == "Saves" ~ paste0("Saves (", round(`Saves p90`, digits = 2), ")"),
                           Names == "C Sheets" ~ paste0("C Sheets (", round(`C Sheets p90`, digits = 2), ")"),
                           Names == "Total Points" ~ paste0("Points (", round(`Points p90`, digits = 2), ")"),
  ))

}


comparison_bar <- function(data) {  
  
  ggplot(data, aes(x = Percentiles, y = Names, color = Percentiles)) +
    geom_segment(aes(x=-30,xend = 5, y = Names,yend= Names), size = 4.5, color = "white", show.legend = F) +
    geom_segment(aes(x=1,xend = 99, y = Names,yend= Names), size = 4.5, color = "gray80", show.legend = F) +

    geom_point(aes(x = 98.8, y = Names), color = "gray80", size = 3.75) +
    geom_point(aes(x = 1.2, y = Names), color = "gray80", size = 3.75) +
    geom_segment(aes(x=Percentiles,xend = 50, y = Names,yend= Names), size = 4.5, show.legend = F) +
    geom_vline(xintercept = 50, linetype = "solid", color = "white", size = 0.75) +
    geom_point(size = 11, show.legend = F, alpha = 0.2) +
    geom_point(size = 7, pch = 21, stroke = 2, fill = "white", show.legend = F) +
    geom_text(aes(label = paste0(round(Percentiles, digits = 0))), face = "bold", size = 3, color = "black", family = "Lato") +
    #geom_shadowtext(aes(x = -3.8, y = Names, label = stats), size = 2.5, color = "gray20", family = "Lato", hjust = 1, bg.r = 0.03, bg.colour = NA, show.legend = F) +
    geom_shadowtext(aes(x = -3.8, y = Names, label = stats), size = 3.5, color = "gray20", family = "Lato", hjust = 1, bg.r = 0.03, bg.colour = NA, show.legend = F) +
    scale_color_gradient2(low = '#DB444B', mid = 'yellow', high = '#379A8B',midpoint = 50) +
    scale_x_continuous(breaks = c(0,20,40,60,80,100), expand = c(0, 0), limits = c(-30,105)) + 
    coord_cartesian(clip = "off") +
    facet_nested(Info~.,scales = "free") +
    labs(title = data$final_image,
         caption = " Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='10'>") +
    #Adjust Theme to suit this plot
    theme(#panel.grid.major.x = element_line(colour = "white", size = 0.5, linetype = "dashed"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "top",
      plot.caption = element_markdown(size = 6, color = "gray10", family = "Lato", hjust = 1, margin=margin(0,0,0,0)),
      plot.margin=unit(c(1,0,0,0), "mm"),
      plot.title = element_markdown(color = "gray10", size = 14, family = "Lato", hjust = 0, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray30", family = "Lato", hjust = 0),
      legend.text = element_text(color = "gray20", size = 14, family = "Lato", hjust = 0),
      #axis.text.y = element_text(color ="gray25", size = 10, family = "Lato", vjust = 0.5, hjust = 1),
      axis.text.y = element_blank(),
      axis.text.x = element_text(color ="gray25", size = 10, family = "Lato", vjust = 0.5, hjust = 0.5),
      panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
      plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
      legend.title = element_text(color ="black", size = 12, family = "Lato"),
      legend.key = element_blank(),
      axis.title.x = element_text(color = "gray20", size = 11, family = "Lato", hjust = 0.5669291),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_markdown(color = "black", size = 10, family = "Lato"),
      strip.text.y = element_text(color = "black", size = 8, family = "Lato"),
      legend.background = element_blank())
  
}

team_parse <- function(data, start_gameweek, end_gameweek, team_minutes, team_name) {
  
  fixtures_df <- data %>%
    mutate(opposition = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (H)"),
                                  was_home == 'FALSE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (A)"))) %>%
    mutate(team = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', short_team_name, " (A)"),
                            was_home == 'FALSE' ~ paste0('GW', round, ' - ', short_team_name, " (H)"))) %>%
    group_by(short_team_name, round, opposition, team) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    group_by(short_team_name, round) %>%
    filter(count >= 11) %>%
    ungroup() %>%
    unite("merged",opposition:team, remove = F)
  
  
  Minutes_Data <- data %>%
    mutate(opposition = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (H)"),
                                  was_home == 'FALSE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (A)"))) %>%
    mutate(team = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', short_team_name, " (A)"),
                            was_home == 'FALSE' ~ paste0('GW', round, ' - ', short_team_name, " (H)"))) %>%
    unite("merged",opposition:team, remove = F) %>%
    filter(round >= start_gameweek & round <= end_gameweek) %>%
    filter(opposition %in% fixtures_df$opposition) %>%
    filter(merged %in% fixtures_df$merged) %>%
    group_by(short_team_name, opp_short_team_name, name_club, merged,kickoff_time) %>%
    reframe(minutes = minutes,
            Game = case_when(was_home == "TRUE" ~ "H",
                             T ~ "A"),
            opposition = unique(opposition),
            team = unique(team),
            xgi = expected_goal_involvements, 
            #all_xgi = sum(expected_goal_involvements),
            name = web_name,
            points = total_points,
            Position = Position,
            opp_team = opp_team_name,
            #points = total_points,
            #all_points = sum(total_points),
            #all_minutes = sum(minutes),
            #opp_name = paste0(opp_short_team_name, " (",Game,") - GW ", round),
            opp_name = paste0(opp_short_team_name, "\n(",Game,")\n", round),
            Gameweek = round) %>%
    unite("merged2",team:opposition, remove = F) %>%
    arrange(desc(merged)) %>%
    ungroup() %>%
    group_by(name_club) %>%
    mutate(all_minutes = sum(minutes),
           all_points = sum(points),
           all_xgi = sum(xgi)) %>%
    ungroup() %>%
    #filter(all_minutes >= 300) %>%
    filter(all_minutes >= team_minutes) %>%
    #filter(Gameweek >= 18 & Gameweek <= 23) %>%
    #filter(all_minutes >= 600) %>%
    mutate(Minutes_Type = case_when(minutes == 90 ~ "90 Minutes",
                                    minutes == 0 ~ "Zero Minutes",
                                    minutes < 60 ~ "Under 60 Minutes",
                                    T ~ "60 Minutes & Over")) %>%
    mutate(Points_Type = case_when(points >= 0 & points <=3 ~ "Blank",
                                   points >=  10 ~ "Double Digit", 
                                   points < 0 ~ "Minus",
                                   T ~ "Return")) %>%
    mutate(xgi_Type = case_when(xgi >= 0.05 & xgi <= 0.3 ~ "Not Involved",
                                xgi <  0.05 ~ "Anonymous",
                                xgi > 0.3 & xgi <= 0.5 ~ "Involved",
                                T ~ "Very Involved")) %>%
    #filter(short_team_name == input$team_team) %>%
    filter(short_team_name == team_name) %>%
    filter(minutes > 0) %>%
    arrange(Gameweek) %>%
    mutate(opp_crest = case_when(opp_team == "Arsenal" ~ "arsenal",
                                 opp_team == "Aston Villa" ~ "aston villa",
                                 opp_team == "Bournemouth"~ "bournemouth",
                                 opp_team == "Brentford" ~ "brentford",
                                 opp_team == "Brighton" ~ "brighton",
                                 opp_team == "Burnley"  ~ "burnley",
                                 opp_team == "Chelsea"  ~ "chelsea",
                                 opp_team == "Crystal Palace" ~ "crystal palace",
                                 opp_team == "Everton" ~ "everton",
                                 opp_team == "Fulham" ~ "fulham",
                                 opp_team == "Liverpool" ~ "liverpool",
                                 opp_team == "Luton" ~ "luton town",
                                 opp_team == "Man City" ~ "man city",
                                 opp_team == "Man Utd" ~ "man utd",
                                 opp_team == "Newcastle" ~ "newcastle utd",
                                 opp_team == "Nott'm Forest" ~ "forest",
                                 opp_team == "Sheffield Utd"  ~ "sheffield utd",
                                 opp_team == "Spurs" ~ "tottenham",
                                 opp_team == "West Ham" ~ "west ham",
                                 opp_team == "Wolves" ~ "wolves" )) %>%
    ungroup() %>%
    separate_wider_delim(kickoff_time, "T", names = c("Date","Time")) %>%
    mutate(Date = ymd(Date))
  
  
}

team_parse_pos <- function(data, start_gameweek, end_gameweek, team_minutes, position_name, player_number) {
  
  fixtures_df <- data %>%
    mutate(opposition = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (H)"),
                                  was_home == 'FALSE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (A)"))) %>%
    mutate(team = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', short_team_name, " (A)"),
                            was_home == 'FALSE' ~ paste0('GW', round, ' - ', short_team_name, " (H)"))) %>%
    group_by(short_team_name, round, opposition, team) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    group_by(short_team_name, round) %>%
    filter(count >= 11) %>%
    ungroup() %>%
    unite("merged",opposition:team, remove = F)
  
  
  Minutes_Data <- data %>%
    mutate(opposition = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (H)"),
                                  was_home == 'FALSE' ~ paste0('GW', round, ' - ', opp_short_team_name, " (A)"))) %>%
    mutate(team = case_when(was_home == 'TRUE' ~ paste0('GW', round, ' - ', short_team_name, " (A)"),
                            was_home == 'FALSE' ~ paste0('GW', round, ' - ', short_team_name, " (H)"))) %>%
    unite("merged",opposition:team, remove = F) %>%
    filter(round >= start_gameweek & round <= end_gameweek) %>%
    filter(opposition %in% fixtures_df$opposition) %>%
    filter(merged %in% fixtures_df$merged) %>%
    group_by(short_team_name, opp_short_team_name, name_club, merged,round, kickoff_time) %>%
    reframe(minutes = minutes,
            Game = case_when(was_home == "TRUE" ~ "H",
                             T ~ "A"),
            opposition = unique(opposition),
            team = unique(team),
            xgi = expected_goal_involvements, 
            xg = expected_goals,
            xa = expected_assists,
            #all_xgi = sum(expected_goal_involvements),
            name = web_name,
            points = total_points,
            Position = Position,
            opp_team = opp_team_name,
            #points = total_points,
            #all_points = sum(total_points),
            #all_minutes = sum(minutes),
            #opp_name = paste0(opp_short_team_name, " (",Game,") - GW ", round),
            opp_name = paste0(opp_short_team_name, "\n(",Game,")\n", round),
            Gameweek = round) %>%
    unite("merged2",team:opposition, remove = F) %>%
    arrange(desc(merged)) %>%
    ungroup() %>%
    group_by(name_club) %>%
    mutate(all_minutes = sum(minutes),
           all_points = sum(points),
           all_xg = sum(xg),
           all_xa = sum(xa),
           all_xgi = sum(xgi)) %>%
    ungroup() %>%
    #filter(all_minutes >= 300) %>%
    filter(all_minutes >= team_minutes) %>%
    #filter(Gameweek >= 18 & Gameweek <= 23) %>%
    #filter(all_minutes >= 600) %>%
    mutate(Minutes_Type = case_when(minutes == 90 ~ "90 Minutes",
                                    minutes == 0 ~ "Zero Minutes",
                                    minutes < 60 ~ "Under 60 Minutes",
                                    T ~ "60 Minutes & Over")) %>%
    mutate(Points_Type = case_when(points >= 0 & points <=3 ~ "Blank",
                                   points >=  10 ~ "Double Digit", 
                                   points < 0 ~ "Minus",
                                   T ~ "Return")) %>%
    mutate(xgi_Type = case_when(xgi >= 0.05 & xgi <= 0.3 ~ "Not Involved",
                                xgi <  0.05 ~ "Anonymous",
                                xgi > 0.3 & xgi <= 0.5 ~ "Involved",
                                T ~ "Very Involved")) %>%
    mutate(xg_Type = case_when(xg >= 0.05 & xg <= 0.3 ~ "Not Involved",
                                xg <  0.05 ~ "Anonymous",
                                xg > 0.3 & xg <= 0.5 ~ "Involved",
                                T ~ "Very Involved")) %>%
    mutate(xa_Type = case_when(xa >= 0.05 & xa <= 0.1 ~ "Not Involved",
                                xa <  0.05 ~ "Anonymous",
                                xa > 0.1 & xa <= 0.5 ~ "Involved",
                                T ~ "Very Involved")) %>%
    #filter(short_team_name == input$team_team) %>%
    filter(Position == position_name) %>%
    filter(minutes > 0) %>%
    arrange(Gameweek) %>%
    mutate(opp_crest = case_when(opp_team == "Arsenal" ~ "arsenal",
                                 opp_team == "Aston Villa" ~ "aston villa",
                                 opp_team == "Bournemouth"~ "bournemouth",
                                 opp_team == "Brentford" ~ "brentford",
                                 opp_team == "Brighton" ~ "brighton",
                                 opp_team == "Burnley"  ~ "burnley",
                                 opp_team == "Chelsea"  ~ "chelsea",
                                 opp_team == "Crystal Palace" ~ "crystal palace",
                                 opp_team == "Everton" ~ "everton",
                                 opp_team == "Fulham" ~ "fulham",
                                 opp_team == "Liverpool" ~ "liverpool",
                                 opp_team == "Luton" ~ "luton town",
                                 opp_team == "Man City" ~ "man city",
                                 opp_team == "Man Utd" ~ "man utd",
                                 opp_team == "Newcastle" ~ "newcastle utd",
                                 opp_team == "Nott'm Forest" ~ "forest",
                                 opp_team == "Sheffield Utd"  ~ "sheffield utd",
                                 opp_team == "Spurs" ~ "tottenham",
                                 opp_team == "West Ham" ~ "west ham",
                                 opp_team == "Wolves" ~ "wolves" )) %>%
    ungroup() %>%
    separate_wider_delim(kickoff_time, "T", names = c("Date","Time")) %>%
    mutate(Date = ymd(Date)) #%>%
    #head(player_number)
  
  
}

 team_gw_plot <- function(input_metrics, data, team_name, team_metrics) {
   
   if (input_metrics == "Minutes") {
     
     y_var <- "all_minutes"
     text_var <- "minutes"
     col_var <- "Minutes_Type"
     color_palette <- scale_fill_manual(breaks = c("Under 60 Minutes","60 Minutes & Over","90 Minutes"),
                                        #values = c('#76D7C4','#76BAD7','#9376D7',"#5E4FA2","#F2055C")) +
                                        values = c('#DB444B','#3EBCD2','#006BA2')) 
     #values = c('#DB444B','#006BA2','#379A8B')) 
     scale1 <- scale_y_reordered(labels = setNames(paste0(data$name, " (", data$all_minutes," Mins)"),  paste0(data$Position,"___",data$name)),
                                 position = "right")
     
     text_labels <- geom_text(aes(label = get(text_var)), color = "#F3E9E2", family = "Lato", size = 2.5, fontface = "bold", show.legend = F)
   }
   
   if (input_metrics == "Points") {
     
     y_var <- "all_points"
     text_var <- "points"
     col_var <- "Points_Type"
     color_palette <- scale_fill_manual(breaks = c("Minus","Blank","Return","Double Digit"),
                                        values = c('#DB444B','#3EBCD2','#006BA2','#379A8B'))
     #values = c('#DB444B','#EBB434','#3EBCD2','#006BA2'))
     #c('#DB444B','#9A607F','#3EBCD2','#006BA2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C','#758D99')
     #values = c("#F2300F","#E79805","#66C2A5", "#0B775E"))
     scale1 <- scale_y_reordered(labels = setNames(paste0(data$name, " (", data$all_points," Pts)"),  paste0(data$Position,"___",data$name)),
                                 position = "right")
     
     text_labels <- geom_text(aes(label = get(text_var)), color = "#F3E9E2", family = "Lato", size = 2.5, fontface = "bold", show.legend = F)
   }
   
   if (input_metrics == "XGI") {
     
     y_var <- "all_xgi"
     text_var <- "xgi"
     col_var <- "xgi_Type"
     color_palette <- scale_fill_manual(breaks = c("Anonymous","Not Involved","Involved","Very Involved"),
                                        values = c('#DB444B','#3EBCD2','#006BA2','#379A8B'))
     #values = c('#DB444B','#3EBCD2','#006BA2','#379A8B'))
     #values = c("#F2300F","#E79805","#66C2A5", "#0B775E"))
     scale1 <- scale_y_reordered(labels = setNames(paste0(data$name, " (", data$all_xgi," xGI)"),  paste0(data$Position,"___",data$name)),
                                 position = "right")
     text_labels <- geom_text(aes(label = get(text_var)), color = "#F3E9E2", family = "Lato", size = 1.7, fontface = "bold", show.legend = F)
   }
   
   p <- ggplot(data, aes(x = reorder(opp_name, Date), y = reorder_within(Position, get(y_var), name), fill = get(col_var), colour = get(col_var))) +
     geom_point(shape = 22, color = "#F3E9E2", size = 6) +
     text_labels +
     facet_grid(Position ~., scales = "free", space = "free", switch = "y") +
     color_palette +
     scale1 +
     scale_x_discrete(name = NULL, labels = setNames(as.character(data$opp_name), data$opp_team)) +
     xlab('FPL Gameweeks') +
     ylab('Player Names') +
     labs(title = paste0("<img src='./Logo4.png' width='30'> ", team_name, " | Player ", input_metrics, " Matrix"),
          #subtitle = "Cumulative Points Totals For the Highest Scoring FPL Players",
          #caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
          size = 'Number Of Starts',
          fill = 'Appearance Type') +
     #guides(fill = guide_legend(override.aes = list(size = 10))) +
     #Adjust Theme to suit this plot
     theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
       panel.grid.major.x = element_blank(),
       #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
       panel.grid.major.y = element_line(color = "gray", linewidth = 0.5),
       panel.grid.minor.x = element_blank(),
       panel.grid.minor.y = element_blank(),
       panel.border = element_blank(),
       panel.spacing = unit(0.5, "lines"),
       #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
       legend.position = "top",
       panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
       plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
       plot.title = element_markdown(color = "gray10", size = 14, family = "Lato", hjust = 0, face = "bold"),
       plot.subtitle = element_text(size = 12, color = "gray30", family = "Lato", hjust = 0),
       plot.caption = element_markdown(color = "black", size = 6, family = "Lato"),
       legend.text = element_text(color = "black", size = 8, family = "Lato", hjust = 0),
       legend.title = element_blank(),
       #legend.key = element_blank(),
       #legend.key.height= unit(0.1, 'cm'),
       #legend.key.width= unit(0.1, 'cm'),
       legend.key.size = unit(0.1, 'cm'),
       legend.margin=margin(0,0,0,0),
       legend.box.margin=margin(0,0,0,0),
       axis.text.x = element_text(color ="black", size = 6, family = "Lato"),
       #axis.text.x = element_markdown(color ="black", size = 10, family = "Lato"),
       axis.text.y = element_text(color ="black", size = 6, family = "Lato", hjust = 0, face = 'bold'),
       #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
       axis.title.x = element_text(color = "black", size = 12,  family = "Lato", face = 'bold'),
       axis.title.y = element_blank(),
       axis.ticks.x = element_blank(),
       axis.ticks.y = element_blank(),
       axis.line.x = element_blank(),
       axis.line.y = element_line(color = "gray30", size = 0.7),
       strip.background = element_blank(),
       strip.text.x = element_markdown(color = "gray20", size = 6, family = "Lato", face = "bold"),
       strip.text.y.left = element_markdown(color = "gray20", size = 8, family = "Lato", angle = 90, face = "bold"),
       legend.background = element_blank())   
   
   p
   
 }
 
 
player_xgi_plot <- function(input_metrics, data, position_name, player_number) {
   
  if (input_metrics == "all_xgi") {
    
    y_var <- "all_xgi"
    text_var <- "xgi"
    col_var <- "xgi_Type"
    color_palette <- scale_fill_manual(breaks = c("Anonymous","Not Involved","Involved","Very Involved"),
                                       values = c('#DB444B','#3EBCD2','#006BA2','#379A8B'))
    scale1 <- scale_y_reordered(labels = setNames(paste0(data$name, " (", data$all_xgi," xGI)"),  paste0(data$Position,"___",data$name)),
                                position = "right")
    text_labels <- geom_text(aes(label = get(text_var)), color = "#F3E9E2", family = "Lato", size = 1.7, fontface = "bold", show.legend = F)
    
  } else if (input_metrics == "all_xg") {
    
    y_var <- "all_xg"
    text_var <- "xg"
    col_var <- "xg_Type"
    color_palette <- scale_fill_manual(breaks = c("Anonymous","Not Involved","Involved","Very Involved"),
                                       values = c('#DB444B','#3EBCD2','#006BA2','#379A8B'))
    scale1 <- scale_y_reordered(labels = setNames(paste0(data$name, " (", data$all_xg," xG)"),  paste0(data$Position,"___",data$name)),
                                position = "right")
    text_labels <- geom_text(aes(label = get(text_var)), color = "#F3E9E2", family = "Lato", size = 1.7, fontface = "bold", show.legend = F)
    
  } else if (input_metrics == "all_xa") {
    
    y_var <- "all_xa"
    text_var <- "xa"
    col_var <- "xa_Type"
    color_palette <- scale_fill_manual(breaks = c("Anonymous","Not Involved","Involved","Very Involved"),
                                       values = c('#DB444B','#3EBCD2','#006BA2','#379A8B'))
    scale1 <- scale_y_reordered(labels = setNames(paste0(data$name, " (", data$all_xa," xA)"),  paste0(data$Position,"___",data$name)),
                                position = "right")
    text_labels <- geom_text(aes(label = get(text_var)), color = "#F3E9E2", family = "Lato", size = 1.7, fontface = "bold", show.legend = F)
    
  }
  
  
  
  p <- ggplot(data, aes(x = round, y = reorder_within(Position, get(y_var), name), fill = get(col_var), colour = get(col_var))) +
    geom_point(shape = 22, color = "#F3E9E2", size = 6) +
    text_labels +
    #facet_grid(Position ~., scales = "free", space = "free", switch = "y") +
    color_palette +
    scale1 +
    scale_x_continuous(breaks = seq(1,38)) +
    xlab('FPL Gameweeks') +
    ylab('Player Names') +
    labs(title = paste0("<img src='./Logo4.png' width='30'> Top ", player_number, " ", position_name, " | ", text_var, " Matrix"),
         #subtitle = "Cumulative Points Totals For the Highest Scoring FPL Players",
         #caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
         size = 'Number Of Starts',
         fill = 'Appearance Type') +
    #guides(fill = guide_legend(override.aes = list(size = 10))) +
    #Adjust Theme to suit this plot
    theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
      panel.grid.major.x = element_blank(),
      #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
      panel.grid.major.y = element_line(color = "gray", linewidth = 0.5),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
      legend.position = "top",
      panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
      plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
      plot.title = element_markdown(color = "gray10", size = 14, family = "Lato", hjust = 0, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray30", family = "Lato", hjust = 0),
      plot.caption = element_markdown(color = "black", size = 6, family = "Lato"),
      legend.text = element_text(color = "black", size = 8, family = "Lato", hjust = 0),
      legend.title = element_blank(),
      #legend.key = element_blank(),
      #legend.key.height= unit(0.1, 'cm'),
      #legend.key.width= unit(0.1, 'cm'),
      legend.key.size = unit(0.1, 'cm'),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(0,0,0,0),
      axis.text.x = element_text(color ="black", size = 6, family = "Lato"),
      #axis.text.x = element_markdown(color ="black", size = 10, family = "Lato"),
      axis.text.y = element_text(color ="black", size = 6, family = "Lato", hjust = 0, face = 'bold'),
      #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
      axis.title.x = element_text(color = "black", size = 12,  family = "Lato", face = 'bold'),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_line(color = "gray30", size = 0.7),
      strip.background = element_blank(),
      strip.text.x = element_markdown(color = "gray20", size = 6, family = "Lato", face = "bold"),
      strip.text.y.left = element_markdown(color = "gray20", size = 8, family = "Lato", angle = 90, face = "bold"),
      legend.background = element_blank())   
  
   
   
 }
 
# Add this helper function at the top of your script
check_image_url <- function(url) {
  tryCatch({
    response <- httr::HEAD(url, timeout = 3)  # 3 second timeout
    httr::status_code(response) == 200
  }, error = function(e) {
    FALSE
  })
}

player_points_parse <- function(data, input_position, players_2, players_1) {
   
   total_df <- data %>%
     filter(Position == input_position) %>%
     mutate(appearance = case_when(minutes >= 60 ~ 2,
                                   minutes == 0 ~ 0,
                                   T ~ 1)) %>%
     mutate(c_sheet = case_when(clean_sheets == 1 & appearance == 2 ~ 1,
                                T ~ 0)) %>%
     separate_wider_delim(photo, ".", names = c("image_id", "image_type")) %>%
     #mutate(player_images = paste0("<img src='", 'https://resources.premierleague.com/premierleague25/photos/players/110x140/',image_id,'.png', "' width='17' /><br>")) %>%
     mutate(player_images = {
       img_url <- paste0('https://resources.premierleague.com/premierleague25/photos/players/110x140/', image_id, '.png')
       ifelse(
         !is.na(image_id) & image_id != "" & check_image_url(img_url),
         paste0("<img src='", img_url, "' width='17' /><br>"),
         paste0(web_name, "<br>")  # Fallback to player name only
       )
     }) %>%
     group_by(web_name) %>%
     {if(input_position == "GK")
       reframe(., points = sum(total_points),
               position = unique(Position),
               bonus = sum(bonus),
               goals_conceded = sum(goals_conceded),
               clean_sheets = sum(c_sheet),
               saves = sum(saves),
               save_points = floor(sum(saves)/3),
               cs_points = sum(c_sheet)*4,
               bookings = sum(yellow_cards)*-1,
               red_cards = sum(red_cards)*-3,
               appearance = sum(appearance),
               goals = sum(goals_scored)*7,
               assists = sum(assists)*3,
               photo = unique(player_images),
               matches = n())
       else if(input_position == "DEF") 
         reframe(., points = sum(total_points),
                 position = unique(Position),
                 bonus = sum(bonus),
                 goals_conceded = sum(goals_conceded),
                 clean_sheets = sum(c_sheet),
                 cs_points = sum(c_sheet)*4,
                 bookings = sum(yellow_cards)*-1,
                 red_cards = sum(red_cards)*-3,
                 appearance = sum(appearance),
                 goals = sum(goals_scored)*6,
                 assists = sum(assists)*3,
                 photo = unique(player_images),
                 matches = n())
       else if(input_position == "MID") 
         reframe(., points = sum(total_points),
                 position = unique(Position),
                 bonus = sum(bonus),
                 goals_conceded = sum(goals_conceded),
                 clean_sheets = sum(c_sheet),
                 cs_points = sum(c_sheet)*1,
                 bookings = sum(yellow_cards)*-1,
                 red_cards = sum(red_cards)*-3,
                 appearance = sum(appearance),
                 goals = sum(goals_scored)*5,
                 assists = sum(assists)*3,
                 photo = unique(player_images),
                 matches = n())
       else if(input_position == "ST") 
         reframe(., points = sum(total_points),
                 position = unique(Position),
                 bonus = sum(bonus),
                 goals_conceded = sum(goals_conceded),
                 clean_sheets = sum(c_sheet),
                 cs_points = sum(c_sheet)*0,
                 bookings = sum(yellow_cards)*-1,
                 red_cards = sum(red_cards)*-3,
                 appearance = sum(appearance),
                 goals = sum(goals_scored)*4,
                 assists = sum(assists)*3,
                 photo = unique(player_images),
                 matches = n()) } %>%
     arrange(desc(points)) %>%
     head(players_2) %>%
     tail(players_2 - players_1) %>%
     {if(input_position == "GK") mutate(., Other = points - (save_points + assists + cs_points + bonus)) 
       else if(input_position == "DEF") mutate(., Other = points - (goals + assists + cs_points + bonus))
       else if(input_position == "MID") mutate(., Other = points - (goals + assists + bonus))
       else if(input_position == "ST") mutate(., Other = points - (goals + assists + bonus))} %>%
     {if(input_position == "GK") pivot_longer(., cols = c("save_points","assists","cs_points","bonus","Other"))
       else if(input_position == "DEF") pivot_longer(., cols = c("goals","assists","cs_points","bonus","Other"))
       else if(input_position == "MID") pivot_longer(., cols = c("goals","assists","bonus","Other"))
       else if(input_position == "ST") pivot_longer(., cols = c("goals","assists","bonus","Other"))}
   
   
}


player_points_plot1 <- function(input_position, data, axis_length) {
  
  if(input_position == "GK") {
    data$name <- factor(data$name, levels = rev(c("save_points","assists","cs_points","bonus","Other")))
    color_scale <- scale_fill_manual(limits = c("save_points","assists","cs_points","bonus","Other"),
                                     labels = c("Saves","Assists","Clean Sheets","Bonus","Other"),
                                     values = c('#DB444B','#006BA2','#3EBCD2','#379A8B','lightgray')) 
  } else if(input_position == "DEF") {
    data$name <- factor(data$name, levels = rev(c("goals","assists","cs_points","bonus","Other")))
    color_scale <- scale_fill_manual(limits = c("goals","assists","cs_points","bonus","Other"),
                                     labels = c("Goals","Assists","Clean Sheets","Bonus","Other"),
                                     values = c('#DB444B','#006BA2','#3EBCD2','#379A8B','lightgray')) 
  } else if(input_position == "MID") {
    data$name <- factor(data$name, levels = rev(c("goals","assists","bonus","Other")))
    color_scale <- scale_fill_manual(limits = c("goals","assists","bonus","Other"),
                                     labels = c("Goals","Assists","Bonus","Other"),
                                     values = c('#DB444B','#006BA2','#3EBCD2','lightgray')) 
  } else if(input_position == "ST") {
    data$name <- factor(data$name, levels = rev(c("goals","assists","bonus","Other")))
    color_scale <- scale_fill_manual(limits = c("goals","assists","bonus","Other"),
                                     labels = c("Goals","Assists","Bonus","Other"),
                                     values = c('#DB444B','#006BA2','#3EBCD2','lightgray')) 
  }
  
  p <- ggplot(data = data, aes(x = value, y = reorder(photo, points), fill = name)) +
    geom_bar(stat = "identity",  width = 0.8) +
    geom_text(aes(x = points, y = reorder(photo, points), label = paste0(web_name, " - ", points)), family = "Lato", nudge_x = -0.5, hjust = 1, size = 2.5) +
    geom_shadowtext(data = data %>% filter(value > 0) %>% filter(name != "Other"), aes(label = value), size = 2.5, position = position_stack(vjust = 0.5), face = "bold", family = "Lato", bg.colour = "black", color ="white") +
    scale_x_continuous(breaks = seq(0,1000,10),
                       limits = c(0,axis_length),
                       expand = c(0,0)) +
    color_scale +
    labs(title = paste0("<img src='./Logo4.png' width='30'> ", head(data$web_name, n = 1) ," is FPL's Highest Scoring ", unique(data$position)),
         subtitle = paste0("Points Scored by ", unique(data$position)," (24/25 Season)"),
         caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +
    #Adjust Theme to suit this plot
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(0.5, "lines"),
          legend.position = "top",
          panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.title = element_markdown(color = "gray10", size = 14, family = "Lato", hjust = 0, face = "bold"),
          plot.subtitle = element_text(size = 12, color = "gray30", family = "Lato", hjust = 0),
          plot.caption = element_markdown(color = "black", size = 6, family = "Lato"),
          legend.text = element_text(color = "black", size = 6, family = "Lato", hjust = 0),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 6, family = "Lato"),
          axis.text.y = element_markdown(),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_line(color = "gray30", size = 0.5),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_blank(),
          #axis.line.x = element_line(color = "gray30"),
          axis.line.y = element_line(color = "gray30"),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  
  p
  
  
  
}


top_performers_parse <- function(data, input_position, input_metrics) {
  
  Players_Filtered <- data %>%
    filter(Position == input_position) %>%
    mutate(opposition = case_when(was_home == 'TRUE' ~ paste0(opp_short_team_name, " (H)"),
                                  was_home == 'FALSE' ~ paste0(opp_short_team_name, " (A)"))) %>%
    {tryCatch({
      separate_wider_delim(., photo, ".", names = c("image_id", "image_type"), too_few = "align_start", too_many = "drop")
    }, error = function(e) {
      cat("Error in separate_wider_delim():", e$message, "\n")
      mutate(., 
             image_id = str_extract(photo, "^[^.]*"),
             image_type = str_extract(photo, "[^.]*$"))
    })} %>%
    # Improved image handling - don't check URLs, just create them
    mutate(player_images = case_when(
      !is.na(image_id) & image_id != "" ~ paste0('https://resources.premierleague.com/premierleague25/photos/players/110x140/', image_id, '.png'),
      TRUE ~ paste0("https://via.placeholder.com/110x140/cccccc/000000?text=", URLencode(web_name))
    )) %>%
    # Rest of your existing processing...
    group_by(image_id) %>%
    arrange(round) %>%
    mutate(latest_value = last(value),
           points = cumsum(get(input_metrics))) %>%
    fill(latest_value, .direction = 'up') %>%
    ungroup() %>%
    mutate(actual_value = paste0("£",as.numeric(latest_value) / 10)) %>%
    mutate(name_value = paste0(web_name, "\n(£",as.numeric(latest_value) / 10,")")) %>%
    arrange(desc(points)) %>%
    mutate(highlight_color = case_when(
      short_team_name == "ARS" ~ "#EF0107",
      short_team_name == "AVL" ~ "#670E36",
      short_team_name == "BOU" ~ "#B50E12",
      short_team_name == "BRE" ~ "#E30613",
      short_team_name == "BHA" ~ "#0057B8",
      short_team_name == "CHE" ~ "#034694",
      short_team_name == "CRY" ~ "#1B458F",
      short_team_name == "EVE" ~ "#003399",
      short_team_name == "FUL" ~ "#000000",
      short_team_name == "IPS" ~ "#3A64A3",
      short_team_name == "LEI" ~ "#003090",
      short_team_name == "LIV" ~ "#C8102E",
      short_team_name == "MCI" ~ "#6CABDD",
      short_team_name == "MUN" ~ "#DA291C",
      short_team_name == "NEW" ~ "#241F20",
      short_team_name == "NFO" ~ "#DD0000",
      short_team_name == "SOU" ~ "#D71920",
      short_team_name == "TOT" ~ "#132257",
      short_team_name == "WHU" ~ "#7A263A",
      short_team_name == "WOL" ~ "#FDB913",
      TRUE ~ "#000000"
    ))
  
  return(Players_Filtered)
}


top_performers_plot <- function(data, input_position, input_metrics) {
  
  p <- ggplot(data, aes(x = round, y = points, color = web_name, fill = web_name)) +
    geom_line(size = 0.75, show.legend = F) +
    #stat_summary(fun.data = "mean_cl_boot", geom = "smooth", se = TRUE) +
    gghighlight(use_direct_label = F) +
    geom_shadowtext(data = data %>% filter(round == max(data$round)), aes(label = points), nudge_x = 0.5, size = 2.5, hjust = 0, bg.r = 0.05, bg.color = "#F3E9E2", show.legend = F) +
    geom_vline(xintercept = max(data$round), color = 'black') +
    geom_point(shape = 21, color = '#F3E9E2', size = 1, stroke = 0.5, show.legend = F) +
    # Saka Injury
    #geom_point(data = data %>% filter(web_name == 'Saka') %>% filter(round == '8'), shape = 21, color = '#EF0107', fill = '#F3E9E2', size = 2, stroke = 1, show.legend = F) +
    #geom_shadowtext(data = data %>% filter(web_name == 'Saka'), aes(x = 9, y = 23), size = 2.8, label = 'Bukayo Saka missed\nGW8 Through Injury', bg.r = 0.05, bg.color = "#F3E9E2", show.legend = F) +
    #geom_curve(data = data %>% filter(web_name == 'Saka'), aes(y = 32, yend = 49, x = 9, xend = 8.2), arrow = arrow(length = unit(0.05, "npc")), curvature = 0.2, show.legend = F) +
    facet_wrap(~labels, scales = 'free_x', ncol = 3, strip.position = "left") +
    scale_x_continuous(breaks = seq(1,max(data$round) + 1,2),
                       limits = c(0,max(data$round) + 2)) +
    scale_fill_manual(breaks = data$web_name,
                      values = data$highlight_color) +
    scale_color_manual(breaks = data$web_name,
                       values = data$highlight_color) +
    xlab('FPL Gameweeks') +
    ylab('Cumulative FPL Points Totals') +
    labs(title = paste0("<img src='./Logo4.png' width='30'> Top ", input_metrics, " For ",  input_position),
         subtitle = paste0("Cumulative ", input_metrics, " For Top 9 Players (", input_position, ")"),
         caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
         size = 'Number Of Starts',
         fill = 'Fixture Difficulty Rating (FDR)') +
    #Adjust Theme to suit this plot
    theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
      panel.grid.major.x = element_blank(),
      #panel.grid.major.y = element_line(color = "white", linewidth = 1),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
      legend.position = "bottom",
      panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
      plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
      plot.title = element_markdown(color = "gray10", size = 14, family = "Lato", hjust = 0, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray30", family = "Lato", hjust = 0),
      plot.caption = element_markdown(color = "black", size = 6, family = "Lato"),
      legend.text = element_text(color = "black", size = 6, family = "Lato", hjust = 0),
      legend.title = element_text(color = "gray20", size = 10,  family = "Lato", face = 'bold'),
      legend.key = element_blank(),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(0,0,0,0),
      axis.text.x = element_text(color ="black", size = 6, family = "Lato"),
      axis.text.y = element_blank(),
      #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
      axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
      axis.title.y = element_blank(),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.ticks.x = element_line(color = "gray30", size = 1),
      axis.ticks.length.y = unit(0.3, "cm"),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(color = "gray30"),
      axis.line.y = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
      strip.text.y.left = element_markdown(color = "gray20", size = 8, family = "Lato", angle = 360, vjust = 0),
      legend.background = element_blank())
  
  p
  
}



captaincy_parse_mobile <- function(team_id, Players_History, Gameweeks) {
  
  Team_Selection <- c()
  Subs <- c()
  
  for (GW in seq(1,Gameweeks)) {
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id, '/event/',GW,'/picks/'))
    
    jsonRespText <- content(FPL_Team, as="text") 
    JSON_Output <- fromJSON(jsonRespText)
    
    Team_1 <- JSON_Output$picks %>%
      mutate(round = GW)
    
    Team_Selection <- bind_rows(Team_Selection, Team_1)
    
    Sub_1 <- JSON_Output$automatic_subs 
    
    Subs <- bind_rows(Subs, Sub_1)
  }
  
  SubsIn <- Subs %>%
    mutate(sub_in = "Yes") %>%
    select(element_in,event,sub_in)
  
  SubsOut <- Subs %>%
    mutate(sub_out = "Yes") %>%
    select(element_out,event,sub_out)
  
  Team_Selection_Final <- Team_Selection %>%
    left_join(SubsIn, by = c('element' = 'element_in', 'round' = 'event')) %>%
    left_join(SubsOut, by = c('element' = 'element_out', 'round' = 'event')) %>%
    left_join(Players_History %>% select(id,name_club) %>% unique(), by = c('element' = 'id')) %>%
    left_join(Players_History %>% select(id,round,total_points,strength,opp_strength,short_team_name,opp_short_team_name,photo), by = c('element' = 'id', 'round' = 'round')) %>%
    group_by(round) %>%
    mutate(captain_subbed = case_when(is_captain == T & sub_out == "Yes" ~ "Yes")) %>%
    fill(captain_subbed, .direction = "updown") %>%
    mutate(vice_subbed = case_when(is_vice_captain == T & sub_out == "Yes" ~ "Yes")) %>%
    fill(vice_subbed, .direction = "updown") %>%
    mutate(both_captains_subbed = case_when(captain_subbed == "Yes" & vice_subbed == "Yes" ~ "Yes")) %>%
    mutate(captain_after_sub = case_when(is_captain == T & is.na(captain_subbed) ~ "Yes",
                                         is_vice_captain == T & captain_subbed == "Yes" & is.na(vice_subbed) ~ "Yes",
                                         both_captains_subbed == "Yes" ~ "No",
                                         T ~ "No")) %>%
    ungroup() %>%
    group_by(round, name_club) %>%
    summarise(games = n(),
              sub_in = paste(opp_short_team_name, collapse=","),
              oppsition = paste(opp_short_team_name, collapse=","),
              opp_strength = paste(opp_strength, collapse=","),
              total_points = sum(total_points),
              played_captain = unique(captain_after_sub),
              photo = unique(photo),
              captain = unique(is_captain),
              vice = unique(is_vice_captain)) %>%
    mutate(captaincy = case_when(total_points <= 3 & played_captain == "Yes" ~ "Fail",
                                 total_points >= 4 & total_points <= 9 & played_captain == "Yes" ~ "Success",
                                 total_points >= 10 & played_captain == "Yes" ~ "Double Digits")) %>%
    mutate(DGW = case_when(games == 2 ~ "Yes",
                           T ~ "No")) %>%
    mutate(SGW = case_when(games == 1 & is.na(total_points) == F ~ "Yes",
                           T ~ "No")) %>%
    mutate(BGW = case_when(is.na(total_points) ~ "Yes",
                           T ~ "No")) %>%
    ungroup() %>%
    as.data.frame() %>%
    mutate(label1 = paste0(name_club,"\nVs: ",oppsition),
           label2 = paste0("GW", round, "<br />(",oppsition,")")) %>%
    # Fix: Use tryCatch for the separate function
    {tryCatch({
      separate(., photo, c("photo_id","string"), sep = "\\.", remove = FALSE, convert = TRUE)
    }, error = function(e) {
      cat("Error in separate():", e$message, "\n")
      # If separate fails, add empty columns and continue
      mutate(., 
             photo_id = str_extract(photo, "^[^.]*"),
             string = str_extract(photo, "[^.]*$"))
    })} %>%
    mutate(player_images = paste0("https://resources.premierleague.com/premierleague25/photos/players/110x140/",photo_id,".png"))
  
  return(Team_Selection_Final)
}

captaincy_down_plot <- function(data, gw, round1, Team_Name) {
  
  p <- ggplot(data %>% filter(played_captain == "Yes") %>% mutate(line_color = lead(total_points)), aes(x = reorder(img_labels, round), y = total_points)) +
    #geom_vline(data = small_df2, aes(xintercept = round), color = "lightgray", size = 9) +
    geom_bar(aes(fill = as.factor(captaincy)), stat = "identity", color = NA, width = 0.8) +
    #geom_segment(aes(x = round, xend = round, y = 0, yend = total_points, color = as.factor(captaincy)), size = 5) +
    #geom_point(aes(color = as.factor(captaincy), fill = as.factor(captaincy)), shape = 21, stroke = 1.5, size = 6, color = "#F3E9E2") +
    geom_text(aes(label = total_points, color = as.factor(captaincy)), size = 5, family = "Lato", fontface = "bold", nudge_y = 1, show.legend = F) +
    #geom_shadowtext(aes(label = name_info), family = "Lato", size = 2.2, color = "black", bg.colour = "#F3E9E2", bg.r = 0.02, nudge_y = 1.3, lineheight = .9) +
    scale_color_manual(breaks = c("Double Digits","Success","Fail"),
                       values = c("#0B775E",'#66C2A5','#F2055C')) +
    #values = c('#006BA2','#DB444B','#379A8B')) +
    scale_fill_manual(breaks = c("Double Digits","Success","Fail"),
                      values = c("#0B775E",'#66C2A5','#F2055C')) +
    #values = c('#006BA2','#DB444B','#379A8B')) + 
    coord_cartesian(clip = "off") +
    #scale_x_discrete(name = NULL, labels = setNames(as.character(Team_Selection_Final$label1),round)) +
    scale_x_discrete(name = NULL, labels = setNames(as.character(gw),as.character(round1))) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
      title =  paste0("<img src='./Logo4.png' width='30'>", " Captaincy Success - ", Team_Name ),
      subtitle = "Points for Captained Players",
      caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
      fill = "Captaincy Success",
      color = "Captaincy Success") +
    xlab("Gameweek") +
    ylab("Captaincy Points") +
    #Adjust Theme to suit this plot
    theme(panel.grid.major.x = element_blank(),
          #panel.grid.major.x = element_blank(),
          #panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(0.5, "lines"),
          #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
          legend.position = "top",
          panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.title = element_markdown(color = "black", size = 30, family = "Lato", face = "bold"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
          plot.subtitle = element_markdown(size = 22, color = "gray30", family = "Lato"),
          legend.text = element_text(color = "gray20", size = 16, family = "Lato", hjust = 0),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          #axis.text.x = element_markdown(color ="black", size = 6, family = "Lato"),
          axis.text.x = element_markdown(color ="black", size = 6, family = "Lato", face = "bold"),
          #axis.text.y = element_text(color ="black", size = 12, family = "Lato", vjust = -0.3, hjust = 1),
          axis.text.y = element_blank(),
          axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
          axis.title.y = element_blank(),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_blank(),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line(color = "gray20", linewidth = 0.75),
          axis.line.y = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  
  p
  
  
}

captaincy_plot <- function(data, gw, round1, Team_Name) {
  
  p <- ggplot(data %>% filter(played_captain == "Yes") %>% mutate(line_color = lead(total_points)), aes(x = reorder(img_labels, round), y = total_points)) +
    #geom_vline(data = small_df2, aes(xintercept = round), color = "lightgray", size = 9) +
    geom_bar(aes(fill = as.factor(captaincy)), stat = "identity", color = NA, width = 0.6) +
    #geom_segment(aes(x = round, xend = round, y = 0, yend = total_points, color = as.factor(captaincy)), size = 5) +
    #geom_point(aes(color = as.factor(captaincy), fill = as.factor(captaincy)), shape = 21, stroke = 1.5, size = 6, color = "#F3E9E2") +
    geom_text(aes(label = total_points, color = as.factor(captaincy)), size = 3, family = "Lato", fontface = "bold", nudge_y = 0.75, show.legend = F) +
    #geom_shadowtext(aes(label = name_info), family = "Lato", size = 2.2, color = "black", bg.colour = "#F3E9E2", bg.r = 0.02, nudge_y = 1.3, lineheight = .9) +
    scale_color_manual(breaks = c("Double Digits","Success","Fail"),
                       values = c("#0B775E",'#66C2A5','#F2055C')) +
    #values = c('#006BA2','#DB444B','#379A8B')) +
    scale_fill_manual(breaks = c("Double Digits","Success","Fail"),
                      values = c("#0B775E",'#66C2A5','#F2055C')) +
    #values = c('#006BA2','#DB444B','#379A8B')) + 
    coord_cartesian(clip = "off") +
    #scale_x_discrete(name = NULL, labels = setNames(as.character(Team_Selection_Final$label1),round)) +
    scale_x_discrete(name = NULL, labels = setNames(as.character(gw),as.character(round1))) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
      title =  paste0("<img src='./Logo4.png' width='30'>", " Captaincy Success - ", Team_Name ),
      subtitle = "Points for Captained Players",
      caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
      fill = "Captaincy Success",
      color = "Captaincy Success") +
    xlab("Gameweek") +
    ylab("Captaincy Points") +
    #Adjust Theme to suit this plot
    theme(panel.grid.major.x = element_blank(),
          #panel.grid.major.x = element_blank(),
          #panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(0.5, "lines"),
          #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
          legend.position = "top",
          panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.title = element_markdown(color = "black", size = 14, family = "Lato", face = "bold"),
          plot.caption = element_markdown(color = "black", size = 6, family = "Lato"),
          plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Lato", hjust = 0),
          legend.text = element_text(color = "gray20", size = 8, family = "Lato", hjust = 0),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          #axis.text.x = element_markdown(color ="black", size = 6, family = "Lato"),
          axis.text.x = element_markdown(color ="black", size = 3, family = "Lato", face = "bold"),
          #axis.text.y = element_text(color ="black", size = 12, family = "Lato", vjust = -0.3, hjust = 1),
          axis.text.y = element_blank(),
          axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
          axis.title.y = element_blank(),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_blank(),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line(color = "gray20", linewidth = 0.75),
          axis.line.y = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 8, family = "Lato"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  
  p
  
  
}


team_picks_parse <- function(team_id, Gameweeks) {
  
  req(team_id)
  
  Team_Summary <- c()
  
  team <- team_id
  
  for (GW in seq(1,Gameweeks)) {
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team, '/event/',GW,'/picks/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    
    JSON_Output <- fromJSON(jsonRespText)
    Entry_History <- JSON_Output$entry_history %>%
      unlist() %>%
      t() %>%
      as.data.frame()
    
    
    Team_Summary <- bind_rows(Team_Summary, Entry_History)
    
  }
  
  Team_Summary
  
}

rank_down_plot <- function(data, small_df2, Names1, Breaks1, Team_Name) {
  
  p <- ggplot(data, aes(x = event, y = overall_rank)) +
    #geom_vline(data = small_df2, aes(xintercept = event), color = "lightgray", size = 5) +
    geom_rect(data = small_df2, aes(xmin = event - 0.4, xmax = event + 0.4, ymin = 0, ymax = Inf), fill = "lightgray", color = NA, show.legend = F) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray", alpha = 0.6) +
    geom_borderline(color = "gray60", size = 2, bordercolour = "#F3E9E2", borderwidth = 0.5, show.legend = F) +
    geom_point(aes(fill = movement), shape = 21, stroke = 1, size = 7, color = "#F3E9E2") +
    geom_text(aes(label = total_short), size = 1.8, fontface = "bold", family = "Roboto", color = "white") +
    coord_cartesian(clip = "off") +
    scale_x_continuous(name = NULL, labels = setNames(Names1,Breaks1), breaks = Breaks1) +
    geom_hline(yintercept = 0) +
    scale_y_log10(labels = function(x) paste0(x / 1e6, "M")) +
    scale_color_manual(breaks = c("Red Arrow","No Arrow","Green Arrow"),
                       values = c('#F2055C','gray60','#66C2A5')) +
    scale_fill_manual(breaks = c("Red Arrow","No Arrow","Green Arrow"),
                      values = c('#F2055C','gray60','#66C2A5')) +
    labs(title =  paste0("<img src='./Logo4.png' width='30'>"," Gameweek Ranks - ", Team_Name),
         caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
         subtitle = paste0("Overall Rank by Gameweek (Log Scale)"),
         color = "Team Name",
         fill = "Arrow Type") +
    xlab("Gameweek") +
    ylab("Overall Rank (Log Scale)") +
    #Adjust Theme to suit this plot
    theme(panel.grid.major.x = element_blank(),
          #panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.5),
          #panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(0.5, "lines"),
          #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
          legend.position = "top",
          panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.title = element_markdown(color = "black", size = 30, family = "Lato", face = "bold"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
          plot.subtitle = element_markdown(size = 22, color = "gray30", family = "Lato"),
          legend.text = element_text(color = "gray20", size = 16, family = "Lato", hjust = 0),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 12, family = "Lato"),
          axis.text.y = element_text(color ="black", size = 12, family = "Lato", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
          axis.title.y = element_blank(),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_blank(),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  p
  
  
}


rank_plot <- function(data, small_df2, Names1, Breaks1, Team_Name) {
  
  p <- ggplot(data, aes(x = event, y = overall_rank)) +
    #geom_vline(data = small_df2, aes(xintercept = event), color = "lightgray", size = 5) +
    geom_rect(data = small_df2, aes(xmin = event - 0.4, xmax = event + 0.4, ymin = 0, ymax = Inf), fill = "lightgray", color = NA, show.legend = F) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray", alpha = 0.6) +
    geom_borderline(color = "gray60", size = 1.5, bordercolour = "#F3E9E2", borderwidth = 0.3, show.legend = F) +
    geom_point(aes(fill = movement), shape = 21, stroke = 0.75, size = 4, color = "#F3E9E2") +
    geom_text(aes(label = total_short), size = 1.2, fontface = "bold", family = "Roboto", color = "white") +
    coord_cartesian(clip = "off") +
    scale_x_continuous(name = NULL, labels = setNames(Names1,Breaks1), breaks = Breaks1) +
    geom_hline(yintercept = 0) +
    scale_y_log10(labels = function(x) paste0(x / 1e6, "M")) +
    scale_color_manual(breaks = c("Red Arrow","No Arrow","Green Arrow"),
                       values = c('#F2055C','gray60','#66C2A5')) +
    scale_fill_manual(breaks = c("Red Arrow","No Arrow","Green Arrow"),
                      values = c('#F2055C','gray60','#66C2A5')) +
    labs(title =  paste0("<img src='./Logo4.png' width='30'>"," Gameweek Ranks - ", Team_Name),
         caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
         subtitle = paste0("Overall Rank by Gameweek (Log Scale)"),
         color = "Team Name",
         fill = "Arrow Type") +
    xlab("Gameweek") +
    ylab("Overall Rank (Log Scale)") +
    #Adjust Theme to suit this plot
    theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
      panel.grid.major.x = element_blank(),
      #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
      panel.grid.major.y = element_line(color = "gray", linewidth = 0.5),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
      legend.position = "top",
      panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
      plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
      plot.title = element_markdown(color = "black", size = 14, family = "Lato", face = "bold"),
      plot.caption = element_markdown(color = "black", size = 6, family = "Lato"),
      plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Lato", hjust = 0),
      legend.text = element_text(color = "black", size = 8, family = "Lato", hjust = 0),
      legend.title = element_blank(),
      legend.key.size = unit(0.1, 'cm'),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(0,0,0,0),
      axis.text.x = element_text(color ="black", size = 7, family = "Lato"),
      #axis.text.x = element_markdown(color ="black", size = 10, family = "Lato"),
      axis.text.y = element_text(color ="black", size = 8, family = "Lato", vjust = -0.3, hjust = 1),
      #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
      axis.title.x = element_text(color = "black", size = 12,  family = "Lato", face = 'bold'),
      axis.title.y = element_blank(),
      axis.ticks.length.x = unit(0.1, "cm"),
      axis.ticks.x = element_blank(),
      axis.ticks.length.y = unit(0.1, "cm"),
      axis.ticks.y = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_markdown(color = "gray20", size = 6, family = "Lato", face = 'bold'),
      #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Lato", face = 'bold', angle = 360, vjust = 0),
      legend.background = element_blank())  
  
  p
  
  
}


transfer_down_plot <- function(data, small_df2, Names1, Breaks1, Team_Name) {
  
  p <- ggplot(data, aes(x = Gameweek, y = `Points Difference After Hits`, color = as.factor(movement), fill = as.factor(movement))) +
    #geom_vline(data = small_df2, aes(xintercept = event), color = "lightgray", size = 5) +
    geom_rect(data = small_df2, aes(xmin = event - 0.4, xmax = event + 0.4, ymin = -Inf, ymax = Inf), fill = "lightgray", color = NA, show.legend = F) +
    geom_bar(stat = "identity", color = NA, width = 0.8) +
    #geom_segment(aes(x = Gameweek, xend = Gameweek, y = 0, yend = `Points Difference After Hits`), size = 3, show.legend = F) +
    geom_hline(yintercept = 0, color = "gray30", size = 0.75) +
    #geom_point(shape = 21, stroke = 1.25, size = 6, color = "#F3E9E2") +
    geom_text(data = data %>% filter(`Points Difference After Hits` >= 0), aes(label = round(`Points Difference After Hits`, digits = 0)), size = 5, fontface = "bold", family = "Lato", nudge_y = 1, show.legend = F) +
    geom_text(data = data %>% filter(`Points Difference After Hits` < 0), aes(label = round(`Points Difference After Hits`, digits = 0)), size = 5, fontface = "bold", family = "Lato", nudge_y = -1, show.legend = F) +    
    scale_color_manual(limits = c("Good","Average","Poor"),
                       #values = c('#66C2A5','#FFBF00','#F2055C'),
                       values = c('#379A8B','#006BA2','#DB444B')) +
    scale_fill_manual(limits = c("Good","Average","Poor"),
                      #values = c('#66C2A5','#FFBF00','#F2055C'),
                      values = c('#379A8B','#006BA2','#DB444B')) +
    coord_cartesian(clip = "off") +
    #scale_x_continuous(breaks = seq(1,38,1)) +
    scale_x_continuous(name = NULL, labels = setNames(Names1,Breaks1), breaks = Breaks1) +
    #scale_y_reverse(limits = c(105,0)) +
    #ylim(0,105) +
    #scale_x_discrete() +
    labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
      title =  paste0("<img src='./Logo4.png' width='30'>", " Transfers - ", Team_Name),
      subtitle = "Points Difference After Hits",
      caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
      fill = "Points Difference After Hits",
      color = "Points Difference After Hits") +
    xlab("Gameweek") +
    ylab("Points Difference After Hits") +
    #Adjust Theme to suit this plot
    theme(panel.grid.major.x = element_blank(),
          #panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.5),
          #panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(0.5, "lines"),
          #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
          legend.position = "top",
          panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.title = element_markdown(color = "black", size = 30, family = "Lato", face = "bold"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Lato"),
          plot.subtitle = element_markdown(size = 22, color = "gray30", family = "Lato"),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(color = "gray20", size = 16, family = "Lato", hjust = 0),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 12, family = "Lato"),
          axis.text.y = element_text(color ="black", size = 12, family = "Lato", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
          axis.title.y = element_blank(),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_blank(),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  p
  
  
}


transfer_plot <- function(data, small_df2, Names1, Breaks1, Team_Name) {
  
  p <- ggplot(data, aes(x = Gameweek, y = `Points Difference After Hits`, color = as.factor(movement), fill = as.factor(movement))) +
    #geom_vline(data = small_df2, aes(xintercept = Gameweek), color = "lightgray", size = 5) +
    geom_rect(data = small_df2, aes(xmin = Gameweek - 0.4, xmax = Gameweek + 0.4, ymin = -Inf, ymax = Inf), fill = "lightgray", color = NA, show.legend = F) +
    geom_bar(stat = "identity", color = NA, width = 0.6) +
    #geom_segment(aes(x = Gameweek, xend = Gameweek, y = 0, yend = `Points Difference After Hits`), size = 3, show.legend = F) +
    geom_hline(yintercept = 0, color = "gray30", size = 0.75) +
    #geom_point(shape = 21, stroke = 1.25, size = 6, color = "#F3E9E2") +
    geom_text(data = data %>% filter(`Points Difference After Hits` >= 0), aes(label = round(`Points Difference After Hits`, digits = 0)), size = 3, fontface = "bold", family = "Lato", nudge_y = 0.7, show.legend = F) +
    geom_text(data = data %>% filter(`Points Difference After Hits` < 0), aes(label = round(`Points Difference After Hits`, digits = 0)), size = 3, fontface = "bold", family = "Lato", nudge_y = -0.7, show.legend = F) +
    scale_color_manual(limits = c("Good","Average","Poor"),
                       #values = c('#66C2A5','#FFBF00','#F2055C'),
                       values = c('#379A8B','#006BA2','#DB444B')) +
    scale_fill_manual(limits = c("Good","Average","Poor"),
                      #values = c('#66C2A5','#FFBF00','#F2055C'),
                      values = c('#379A8B','#006BA2','#DB444B')) +
    coord_cartesian(clip = "off") +
    #scale_x_continuous(breaks = seq(1,38,1)) +
    scale_x_continuous(name = NULL, labels = setNames(Names1,Breaks1), breaks = Breaks1) +
    #scale_y_reverse(limits = c(105,0)) +
    #ylim(0,105) +
    #scale_x_discrete() +
    labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
      title =  paste0("<img src='./Logo4.png' width='30'>", " Transfers - ", Team_Name),
      subtitle = "Points Difference After Hits",
      caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
      fill = "Points Difference After Hits",
      color = "Points Difference After Hits") +
    xlab("Gameweek") +
    ylab("Points Difference After Hits") +
    #Adjust Theme to suit this plot
    theme(panel.grid.major.x = element_blank(),
          #panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "lightgray", linewidth = 0.5),
          #panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(0.5, "lines"),
          #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
          legend.position = "top",
          panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
          plot.title = element_markdown(color = "gray10", size = 14, family = "Lato", hjust = 0, face = "bold"),
          plot.subtitle = element_text(size = 12, color = "gray30", family = "Lato", hjust = 0),
          plot.caption = element_markdown(color = "black", size = 6, family = "Lato"),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(color = "black", size = 8, family = "Lato", hjust = 0),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 6, family = "Lato"),
          axis.text.y = element_text(color ="black", size = 8, family = "Lato", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Lato", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Lato"),
          axis.title.y = element_blank(),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_blank(),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Lato"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Lato", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  p
  
  
}
