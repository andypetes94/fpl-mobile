generate_percentiles <- function(data, positions, teams, gameweeks_1, gameweeks_2, starts_1, calculations, players, v) {     
  
  data %>%
  # Filter Positions
  filter(Position %in% positions) %>%
  # Filter Gameweeks
  filter(round >= gameweeks_1 & round <= gameweeks_2) %>%
  separate(photo, c("photo_id","string")) %>%
  mutate(pimage = paste0("<img src='","https://resources.premierleague.com/premierleague/photos/players/110x140/p",photo_id,".png'"," height='60'>")) %>%
  group_by(name_club,short_team_name,pimage) %>%
  mutate(game = n()) %>%
  nest() %>%
  mutate(games = map(.x = data, ~ first(.x$game)),
         #Value = map(.x = data, ~ last(.x$value)),
         Value = map(.x = data, ~ first(.x$value)),
         starts = map(.x = data, ~ sum(.x$starts)),
         mins = map(.x = data, ~ sum(.x$minutes)),
         t_points = map(.x = data, ~ sum(.x$total_points)),
         cs = map(.x = data, ~ sum(.x$total_xg_conceded)),
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
  # Filter Mins >= 600
  #filter(mins >= 600) %>%
  unnest(cols = c(games, Value, starts, mins,t_points, cs, s, g, a, xg, xa, xgi, b)) %>%
  as.data.frame() %>%
  mutate(ga = g + a) %>%
  # Per Game
  mutate(t_points_pg = round(t_points / games, digits = 2),
         cs_pg = round(cs / games, digits = 2),
         s_pg = round(s / games, digits = 2),
         g_pg = round(g / games, digits = 2),
         a_pg = round(a / games, digits = 2),
         xg_pg = round(xg / games, digits = 2),
         xa_pg = round(xa / games, digits = 2),
         xgi_pg = round(xgi /games, digits = 2),
         ga_pg = round(ga /games, digits = 2),
         b_pg = round(b / games, digits = 2)) %>%
  # Per 90
  mutate(xg_90 = (xg / mins) *90,
         cs_90 = (cs / mins) *90,
         s_90 = (s / mins) *90,
         xa_90 = (xa / mins) *90,
         xgi_90 = (xgi / mins) *90,
         b_90 = (b / mins) *90,
         g_90 = (g / mins) *90,
         a_90 = (a / mins) *90,
         t_points_90 = (t_points / mins) *90,
         ga_90 = (ga / mins) *90) %>%
  # filter by starts
    filter(starts >= starts_1) %>%
    {if(calculations == "Total" && nrow(.) <= 100) 
      mutate(., xGoals = (ntile(xg,100) / n())*100,
             xAssists = (ntile(xa,100) / n())*100,
             Saves = (ntile(s,100) / n())*100,
             `Clean Sheets` = 100 - ((ntile(cs,100) / n())*100),
             `xG+xA` = (ntile(xgi,100) / n())*100,
             Bonus = (ntile(b,100) / n())*100,
             Goals = (ntile(g,100) / n())*100,
             Assists = (ntile(a,100) / n())*100,
             `Total Points` = (ntile(t_points,100) / n())*100,
             `G+A` = (ntile(ga,100) / n())*100)
      else if (calculations == "Total" && nrow(.) > 100) 
        mutate(., xGoals = ntile(xg,100),
               xAssists = ntile(xa,100),
               Saves = ntile(s,100),
               `Clean Sheets` = 100 - (ntile(cs,100)),
               `xG+xA` = ntile(xgi,100),
               Bonus = ntile(b,100),
               Goals = ntile(g,100),
               Assists = ntile(a,100),
               `Total Points` = ntile(t_points,100),
               `G+A` = ntile(ga,100))
      else if(calculations == "Per Game" && nrow(.) <= 100) 
        mutate(., xGoals = (ntile(xg_pg,100) / n())*100,
               xAssists = (ntile(xa_pg,100) / n())*100,
               Saves = (ntile(s_pg,100) / n())*100,
               `Clean Sheets` = 100 - ((ntile(cs_pg,100) / n())*100),
               `xG+xA` = (ntile(xgi_pg,100) / n())*100,
               Bonus = (ntile(b_pg,100) / n())*100,
               Goals = (ntile(g_pg,100) / n())*100,
               Assists = (ntile(a_pg,100) / n())*100,
               `Total Points` = (ntile(t_points_pg,100) / n())*100,
               cs = round(cs_pg, digits = 2),
               g = round(g_pg, digits = 2),
               a = round(a_pg, digits = 2),
               xg = round(xg_pg, digits = 2),
               xa = round(xa_pg, digits = 2),
               xgi = round(xgi_pg, digits = 2),
               b = round(b_pg, digits = 2),
               t_points = round(t_points_pg, digits = 2),
               ga = round(ga_pg, digits = 2),
               `G+A` = (ntile(ga_pg,100) / n())*100)
      else if (calculations == "Per Game" && nrow(.) > 100) 
        mutate(., xGoals = ntile(xg_pg,100),
               xAssists = ntile(xa_pg,100),
               Saves = ntile(s_pg,100),
               `Clean Sheets` = 100 - (ntile(cs_pg,100)),
               `xG+xA` = ntile(xgi_pg,100),
               Bonus = ntile(b_pg,100),
               Goals = ntile(g_pg,100),
               Assists = ntile(a_pg,100),
               `Total Points` = ntile(t_points_pg,100),
               cs = round(cs_pg, digits = 2),
               g = round(g_pg, digits = 2),
               a = round(a_pg, digits = 2),
               xg = round(xg_pg, digits = 2),
               xa = round(xa_pg, digits = 2),
               xgi = round(xgi_pg, digits = 2),
               b = round(b_pg, digits = 2),
               t_points = round(t_points_pg, digits = 2),
               ga = round(ga_pg, digits = 2),
               `G+A` = ntile(ga_pg,100))
      else if(calculations == "Per 90" && nrow(.) <= 100) 
        mutate(., xGoals = (ntile(xg_90,100) / n())*100,
               xAssists = (ntile(xa_90,100) / n())*100,
               Saves = (ntile(s_90,100) / n())*100,
               `Clean Sheets` = 100 - ((ntile(cs_90,100) / n())*100),
               `xG+xA` = (ntile(xgi_90,100) / n())*100,
               Bonus = (ntile(b_90,100) / n())*100,
               Goals = (ntile(g_90,100) / n())*100,
               Assists = (ntile(a_90,100) / n())*100,
               `Total Points` = (ntile(t_points_90,100) / n())*100,
               cs = round(cs_90, digits = 2),
               #g = round(g_90, digits = 2),
               #a = round(a_90, digits = 2),
               #xg = round(xg_90, digits = 2),
               #xa = round(xa_90, digits = 2),
               #xgi = round(xgi_90, digits = 2),
               #b = round(b_90, digits = 2),
               #t_points = round(t_points_90, digits = 2),
               #t_points = t_points,
               ga = round(ga_90, digits = 2),
               `G+A` = (ntile(ga_90,100) / n())*100)
      else if (calculations == "Per 90" && nrow(.) > 100) 
        mutate(., xGoals = ntile(xg_90,100),
               xAssists = ntile(xa_90,100),
               Saves = ntile(s_90,100),
               `Clean Sheets` = 100 - (ntile(cs_90,100)),
               `xG+xA` = ntile(xgi_90,100),
               Bonus = ntile(b_90,100),
               Goals = ntile(g_90,100),
               Assists = ntile(a_90,100),
               `Total Points` = ntile(t_points_90,100),
               #cs = round(cs_90, digits = 2),
               #g = round(g_90, digits = 2),
               #a = round(a_90, digits = 2),
               #xg = round(xg_90, digits = 2),
               #xa = round(xa_90, digits = 2),
               #xgi = round(xgi_90, digits = 2),
               #b = round(b_90, digits = 2),
               #t_points = round(t_points_90, digits = 2),
               #t_points = t_points,
               ga = round(ga_90, digits = 2),
               `G+A` = ntile(ga_90,100))} %>%
  mutate(Goals = case_when(g == 0 ~ 0,
                           T ~ as.numeric(Goals)),
         `Clean Sheets` = case_when(cs == 0 ~ 0,
                             T ~ as.numeric(`Clean Sheets`)),
         Assists = case_when(a == 0 ~ 0,
                             T ~ as.numeric(Assists)),
         `G+A` = case_when(ga == 0 ~ 0,
                           T ~ as.numeric(`G+A`))) %>%
    rename(`xG Conceded` = `Clean Sheets`) %>%
  # Filter Team name
  filter(short_team_name %in% teams) %>%
  # Filter Player Name
  filter(name_club %in% players) %>%
  {if (positions %in% c("MID","ST"))
  pivot_longer(., cols = c("xGoals","Goals","xAssists","Assists","xG+xA","G+A", "Bonus", "Total Points"), names_to = "Names", values_to = "Percentiles") %>%
  mutate(Info = case_when(Names %in% c("xGoals","Goals") ~ "Goals",
                          Names %in% c("xAssists","Assists") ~ "Assists",
                          Names %in% c("xG+xA","G+A") ~ "Productivity",
                          Names %in% c("Bonus","Total Points") ~ "Points")) %>%
  mutate(ninety = round(mins / 90, digits = 1)) %>%
  mutate(Details = paste0(name_club, "\nFixtures: ",games," | Starts: ", starts, "\nMins: ", mins, " | 90s: ",ninety))

    else if (positions %in% c("DEF"))
      pivot_longer(., cols = c("xGoals","xAssists","xG Conceded","xG+xA","G+A", "Bonus", "Total Points"), names_to = "Names", values_to = "Percentiles") %>%
      mutate(Info = case_when(Names %in% c("xGoals","xAssists") ~ "Expected",
                              Names %in% c("xG Conceded") ~ "Defensive",
                              Names %in% c("xG+xA","G+A") ~ "Productivity",
                              Names %in% c("Bonus","Total Points") ~ "Points")) %>%
      mutate(ninety = round(mins / 90, digits = 1)) %>%
      mutate(Details = paste0(name_club, "\nFixtures: ",games," | Starts: ", starts, "\nMins: ", mins, " | 90s: ",ninety))
    else if (positions %in% c("GK"))
      pivot_longer(., cols = c("xG Conceded", "Saves", "Bonus", "Total Points"), names_to = "Names", values_to = "Percentiles") %>%
      mutate(Info = case_when(Names %in% c("Saves") ~ "Saves",
                              Names %in% c("xG Conceded") ~ "Productivity",
                              #Names %in% c("xG+xA","G+A") ~ "Contributions",
                              Names %in% c("Bonus","Total Points") ~ "Points")) %>%
      mutate(ninety = round(mins / 90, digits = 1)) %>%
      mutate(Details = paste0(name_club, "\nFixtures: ",games," | Starts: ", starts, "\nMins: ", mins, " | 90s: ",ninety))
      
  }
  
  
  }


g_percentiles <- function(data, positions, gameweeks_1, gameweeks_2, mins_1, v) {     
  
  data %>%
    # Filter Positions
    filter(Position %in% positions) %>%
    # Filter Gameweeks
    filter(round >= gameweeks_1 & round <= gameweeks_2) %>%
    separate(photo, c("photo_id","string")) %>%
    mutate(pimage = paste0("<img src='","https://resources.premierleague.com/premierleague/photos/players/110x140/p",photo_id,".png'"," height='60'>")) %>%
    group_by(name_club,short_team_name,pimage,Position) %>%
    mutate(game = n()) %>%
    nest() %>%
    mutate(games = map(.x = data, ~ first(.x$game)),
           #Value = map(.x = data, ~ last(.x$value)),
           Value = map(.x = data, ~ first(.x$value)),
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
    # Filter Mins >= 600
    filter(mins >= mins_1) %>%
    unnest(cols = c(games, Value, starts, mins,t_points, xgc, s, g, a, xg, xa, xgi, b, csheets)) %>%
    as.data.frame() %>%
    mutate(ga = g + a) %>%
    # Per Game
    #mutate(t_points_pg = round(t_points / games, digits = 2),
    #       cs_pg = round(cs / games, digits = 2),
    #       s_pg = round(s / games, digits = 2),
    #       g_pg = round(g / games, digits = 2),
    #       a_pg = round(a / games, digits = 2),
    #       xg_pg = round(xg / games, digits = 2),
    #       xa_pg = round(xa / games, digits = 2),
    #       xgi_pg = round(xgi /games, digits = 2),
    #       ga_pg = round(ga /games, digits = 2),
    #       csheets_pg = round(csheets /games, digits = 2),
    #       b_pg = round(b / games, digits = 2)) %>%
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
    # filter by starts
    #filter(starts >= starts_1) %>%
    #{if(calculations == "Total" && nrow(.) <= 100) 
    #  mutate(., xGoals = (ntile(xg,100) / n())*100,
    #         xAssists = (ntile(xa,100) / n())*100,
    #         Saves = (ntile(s,100) / n())*100,
    #         `Clean Sheets` = 100 - ((ntile(cs,100) / n())*100),
    #         `xG+xA` = (ntile(xgi,100) / n())*100,
    #         Bonus = (ntile(b,100) / n())*100,
    #         Goals = (ntile(g,100) / n())*100,
    #         Assists = (ntile(a,100) / n())*100,
    #         `Total Points` = (ntile(t_points,100) / n())*100,
    #         `G+A` = (ntile(ga,100) / n())*100)
    #  else if (calculations == "Total" && nrow(.) > 100) 
    #    mutate(., xGoals = ntile(xg,100),
    #           xAssists = ntile(xa,100),
    #           Saves = ntile(s,100),
    #           `Clean Sheets` = 100 - (ntile(cs,100)),
    #           `xG+xA` = ntile(xgi,100),
    #           Bonus = ntile(b,100),
    #           Goals = ntile(g,100),
    #           Assists = ntile(a,100),
    #           `Total Points` = ntile(t_points,100),
    #           `G+A` = ntile(ga,100))
    #  else if(calculations == "Per Game" && nrow(.) <= 100) 
    #    mutate(., xGoals = (ntile(xg_pg,100) / n())*100,
    #           xAssists = (ntile(xa_pg,100) / n())*100,
    #           Saves = (ntile(s_pg,100) / n())*100,
    #           `Clean Sheets` = 100 - ((ntile(cs_pg,100) / n())*100),
    #           `xG+xA` = (ntile(xgi_pg,100) / n())*100,
    #           Bonus = (ntile(b_pg,100) / n())*100,
      #         Goals = (ntile(g_pg,100) / n())*100,
      #         Assists = (ntile(a_pg,100) / n())*100,
      #         `Total Points` = (ntile(t_points_pg,100) / n())*100,
      #         cs = round(cs_pg, digits = 2),
      #         g = round(g_pg, digits = 2),
      #         a = round(a_pg, digits = 2),
      #         xg = round(xg_pg, digits = 2),
      #         xa = round(xa_pg, digits = 2),
      #         xgi = round(xgi_pg, digits = 2),
      #         b = round(b_pg, digits = 2),
      #         t_points = round(t_points_pg, digits = 2),
      #         ga = round(ga_pg, digits = 2),
      #         `G+A` = (ntile(ga_pg,100) / n())*100)
      #else if (calculations == "Per Game" && nrow(.) > 100) 
      #  mutate(., xGoals = ntile(xg_pg,100),
      #         xAssists = ntile(xa_pg,100),
      #         Saves = ntile(s_pg,100),
      #         `Clean Sheets` = 100 - (ntile(cs_pg,100)),
      #         `xG+xA` = ntile(xgi_pg,100),
      #         Bonus = ntile(b_pg,100),
      #         Goals = ntile(g_pg,100),
      #         Assists = ntile(a_pg,100),
      #         `Total Points` = ntile(t_points_pg,100),
      #         cs = round(cs_pg, digits = 2),
      #         g = round(g_pg, digits = 2),
      #         a = round(a_pg, digits = 2),
      #         xg = round(xg_pg, digits = 2),
      #         xa = round(xa_pg, digits = 2),
      #         xgi = round(xgi_pg, digits = 2),
      #         b = round(b_pg, digits = 2),
      #         t_points = round(t_points_pg, digits = 2),
      #         ga = round(ga_pg, digits = 2),
      #         `G+A` = ntile(ga_pg,100))
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

comparison_plot <- function(data, gameweeks_1, gameweeks_2, starts_1, calculations) { 

  p <- ggplot(data, aes(x = Percentiles, y = Names, color = Percentiles)) +
    geom_vline(xintercept = 50, linetype = "dashed", color = "black", size = 1) +
    geom_segment(aes(x=Percentiles,xend = 50, y = Names,yend= Names), size = 6, show.legend = F) +
    geom_point(size = 6, pch = 21, stroke = 3, fill = "white", show.legend = F) +
    geom_text(aes(label = paste0(round(Percentiles, digits = 0))), face = "bold", size = 3, color = "black", family = "Roboto") +
    scale_color_gradient2(low = "red", mid = "yellow", high = "green",midpoint = 50) +
    scale_x_continuous(breaks = c(0,20,40,60,80,100), limits = c(-10,110)) +
    facet_nested(Info~Details,scales = "free") +
    #facet_wrap(~name_club,scales = "free", nrow = 1) +
    labs(title = paste0("FPL Player Comparison: GW ", gameweeks_1, " - ",  gameweeks_2, " (",calculations,")"),
         subtitle = paste0("Sample includes players with at least: ", starts_1, " Starts")) +
    #Adjust Theme to suit this plot
    theme(panel.grid.major.x = element_line(colour = "white", size = 0.5, linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "top",
          plot.background = element_rect(color = "white", fill = "white"),
          plot.title = element_text(color = "black", size = 16, face = "bold", family = "Roboto", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5, color = "black", family = "Roboto"),
          panel.background = element_rect(color = "#F5F5DC", fill = "#F5F5DC"),
          legend.text = element_text(color ="black", size = 10, family = "Roboto"),
          legend.title = element_text(color ="black", size = 12, face = "bold", family = "Roboto"),
          legend.key = element_blank(),
          axis.text.x = element_text(color ="black", size = 8, family = "Roboto"),
          axis.text.y = element_text(color ="black", face = "bold", size = 8, family = "Roboto"),
          axis.title.x = element_text(color ="black", size = 10, face = "bold",  family = "Roboto"),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "red"),
          axis.ticks.y = element_line(colour="red", size=0.5),
          axis.line.x = element_line(colour="black"),
          axis.line.y = element_line(colour="black"),
          strip.background = element_blank(),
          strip.text.x = element_text(color = "black", face = "bold", size = 10, family = "Roboto"),
          strip.text.y = element_text(color = "black", face = "bold", size = 9, family = "Roboto"),
          legend.background = element_blank())
  
  
 
  
   
}


player_points_plot1 <- function(data, starts_1, gameweeks_1, gameweeks_2, player_metric) {

  p <- ggplot(data, aes(x = round, y = get(player_metric), fill = name_club, color = name_club)) +
    geom_borderline(size = 2, bordercolour = "#F3E9E2", borderwidth = 0.5, show.legend = F) +
    geom_point(shape = 21, stroke = 1.5, size = 7, color = "#F3E9E2") +
    geom_text(aes(label = get(player_metric)), size = 2, family = "Ubuntu", color = "white") +
    coord_cartesian(clip = "off") +
    scale_x_continuous(breaks = seq(0,38)) +
    scale_color_manual(values = c('#DB444B','#006BA2','#3EBCD2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C')) +
    scale_fill_manual(values = c('#DB444B','#006BA2','#3EBCD2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C')) +
    #scale_color_manual(values = c("#66C2A5","#3288BD","#5E4FA2","#D12E7C","#F47D20")) +
    #scale_fill_manual(values = c("#66C2A5","#3288BD","#5E4FA2","#D12E7C","#F47D20")) +
    labs(title =  paste0("<img src='./Logo4.png' width='30'> ", player_metric, " Comparison - GW", gameweeks_1, " - GW", gameweeks_2),
      #labs(title =  paste0("<img src='./Logo4.png' width='30'>", " Current Rank:"),
      caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
      #subtitle = paste0("Sample includes players with at least: ", starts_1, " Starts"),
      color = "Team Name",
      fill = "Player") +
    xlab("Gameweek") +
    #ylab("Overall Rank (Log Scale)") +
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
          plot.title = element_markdown(color = "black", size = 24, family = "Ubuntu"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Ubuntu"),
          plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Ubuntu", hjust = 0.19),
          legend.text = element_text(color = "gray20", size = 12, family = "Ubuntu", hjust = 0),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 10, family = "Ubuntu"),
          axis.text.y = element_text(color ="black", size = 10, family = "Ubuntu", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Ubuntu", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.title.y = element_blank(),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_line(color = "gray30", size = 1),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_line(color = "gray30", size = 1),
          axis.line.x = element_line(color = "gray30"),
          axis.line.y = element_line(color = "gray30"),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Ubuntu"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Ubuntu", angle = 360, vjust = 0),
          legend.background = element_blank())
  
   

}
  
create_table1 <- function(data, positions, gameweeks_1, gameweeks_2, starts_1, calculations, v) {

  
  data %>%
  # Get Link to Photos
  separate_wider_delim(photo, ".", names = c("image_id", "image_type")) %>%
  mutate(player_images = paste0('https://resources.premierleague.com/premierleague/photos/players/110x140/p',image_id,'.png')) %>%
  # Filter Positions
  filter(Position %in% positions) %>%
  # Filter Gameweeks
  filter(round >= gameweeks_1 & round <= gameweeks_2) %>%
  group_by(name_club, short_team_name) %>%
  mutate(game = n()) %>%
    nest() %>%
    mutate(games = map(.x = data, ~ first(.x$game)),
           Value = map(.x = data, ~ last(.x$value)),
           starts = map(.x = data, ~ sum(.x$starts)),
           mins = map(.x = data, ~ sum(.x$minutes)),
           t_points = map(.x = data, ~ sum(.x$total_points)),
           cs = map(.x = data, ~ sum(as.numeric(.x$total_xg_conceded))),
           s = map(.x = data, ~ sum(.x$saves)),
           g = map(.x = data, ~ sum(.x$goals_scored)),
           a = map(.x = data, ~ sum(.x$assists)),
           xg = map(.x = data, ~ sum(.x$expected_goals)),
           xa = map(.x = data, ~ sum(.x$expected_assists)),
           image = map(.x = data, ~ unique(.x$player_images)),
           xgi = map(.x = data, ~ sum(.x$expected_goal_involvements)),
           b = map(.x = data, ~ sum(.x$bps))) %>%
    select(-data) %>%
    mutate(Value = as.numeric(Value) / 10) %>%
    # Filter Value
    filter(Value <= v) %>%
    unnest(cols = c(games, Value, starts, mins,t_points, cs, s, g, a, xg, xa, xgi, b, image)) %>%
    as.data.frame() %>%
    mutate(ga = g + a) %>%
    # Per Game
    mutate(t_points_pg = round(t_points / games, digits = 2),
           cs_pg = round(cs / games, digits = 2),
           s_pg = round(s / games, digits = 2),
           g_pg = round(g / games, digits = 2),
           a_pg = round(a / games, digits = 2),
           xg_pg = round(xg / games, digits = 2),
           xa_pg = round(xa / games, digits = 2),
           xgi_pg = round(xgi /games, digits = 2),
           ga_pg = round(ga /games, digits = 2),
           b_pg = round(b / games, digits = 2)) %>%
    # Per 90
    mutate(xg_90 = (xg / mins) *90,
           cs_90 = (cs / mins) *90,
           s_90 = (s / mins) *90,
           xa_90 = (xa / mins) *90,
           xgi_90 = (xgi / mins) *90,
           b_90 = (b / mins) *90,
           g_90 = (g / mins) *90,
           a_90 = (a / mins) *90,
           t_points_90 = (t_points / mins) *90,
           ga_90 = (ga / mins) *90) %>%
    # filter by starts
    filter(starts >= starts_1) %>%
    {if(calculations == "Total" && nrow(.) <= 100) 
      mutate(., xGoals = (ntile(xg,100) / n())*100,
             xAssists = (ntile(xa,100) / n())*100,
             Saves = (ntile(s,100) / n())*100,
             `Clean Sheets` = 100 - ((ntile(cs,100) / n())*100),
             `xG+xA` = (ntile(xgi,100) / n())*100,
             Bonus = (ntile(b,100) / n())*100,
             Goals = (ntile(g,100) / n())*100,
             Assists = (ntile(a,100) / n())*100,
             `Total Points` = (ntile(t_points,100) / n())*100,
             cs = round(cs, digits = 2),
             g = round(g, digits = 2),
             a = round(a, digits = 2),
             xg = round(xg, digits = 2),
             xa = round(xa, digits = 2),
             xgi = round(xgi, digits = 2),
             b = round(b, digits = 2),
             t_points = round(t_points, digits = 2),
             ga = round(ga, digits = 2),
             `G+A` = (ntile(ga,100) / n())*100)
      else if (calculations == "Total" && nrow(.) > 100) 
        mutate(., xGoals = ntile(xg,100),
               xAssists = ntile(xa,100),
               Saves = ntile(s,100),
               `Clean Sheets` = 100 - (ntile(cs,100)),
               `xG+xA` = ntile(xgi,100),
               Bonus = ntile(b,100),
               Goals = ntile(g,100),
               Assists = ntile(a,100),
               `Total Points` = ntile(t_points,100),
               cs = round(cs, digits = 2),
               g = round(g, digits = 2),
               a = round(a, digits = 2),
               xg = round(xg, digits = 2),
               xa = round(xa, digits = 2),
               xgi = round(xgi, digits = 2),
               b = round(b, digits = 2),
               t_points = round(t_points, digits = 2),
               ga = round(ga, digits = 2),
               `G+A` = ntile(ga,100))
      else if(calculations == "Per Game" && nrow(.) <= 100) 
        mutate(., xGoals = (ntile(xg_pg,100) / n())*100,
               xAssists = (ntile(xa_pg,100) / n())*100,
               Saves = (ntile(s_pg,100) / n())*100,
               `Clean Sheets` = 100 - ((ntile(cs_pg,100) / n())*100),
               `xG+xA` = (ntile(xgi_pg,100) / n())*100,
               Bonus = (ntile(b_pg,100) / n())*100,
               Goals = (ntile(g_pg,100) / n())*100,
               Assists = (ntile(a_pg,100) / n())*100,
               `Total Points` = (ntile(t_points_pg,100) / n())*100,
               cs = round(cs_pg, digits = 2),
               g = round(g_pg, digits = 2),
               a = round(a_pg, digits = 2),
               xg = round(xg_pg, digits = 2),
               xa = round(xa_pg, digits = 2),
               xgi = round(xgi_pg, digits = 2),
               b = round(b_pg, digits = 2),
               t_points = round(t_points_pg, digits = 2),
               ga = round(ga_pg, digits = 2),
               `G+A` = (ntile(ga_pg,100) / n())*100)
      else if (calculations == "Per Game" && nrow(.) > 100) 
        mutate(., xGoals = ntile(xg_pg,100),
               xAssists = ntile(xa_pg,100),
               Saves = ntile(s_pg,100),
               `Clean Sheets` = 100 - (ntile(cs_pg,100)),
               `xG+xA` = ntile(xgi_pg,100),
               Bonus = ntile(b_pg,100),
               Goals = ntile(g_pg,100),
               Assists = ntile(a_pg,100),
               `Total Points` = ntile(t_points_pg,100),
               cs = round(cs_pg, digits = 2),
               g = round(g_pg, digits = 2),
               a = round(a_pg, digits = 2),
               xg = round(xg_pg, digits = 2),
               xa = round(xa_pg, digits = 2),
               xgi = round(xgi_pg, digits = 2),
               b = round(b_pg, digits = 2),
               t_points = round(t_points_pg, digits = 2),
               ga = round(ga_pg, digits = 2),
               `G+A` = ntile(ga_pg,100))
      else if(calculations == "Per 90" && nrow(.) <= 100) 
        mutate(., xGoals = (ntile(xg_90,100) / n())*100,
               xAssists = (ntile(xa_90,100) / n())*100,
               Saves = (ntile(s_90,100) / n())*100,
               `Clean Sheets` = 100 - ((ntile(cs_90,100) / n())*100),
               `xG+xA` = (ntile(xgi_90,100) / n())*100,
               Bonus = (ntile(b_90,100) / n())*100,
               Goals = (ntile(g_90,100) / n())*100,
               Assists = (ntile(a_90,100) / n())*100,
               `Total Points` = (ntile(t_points_90,100) / n())*100,
               cs = round(cs_90, digits = 2),
               g = round(g_90, digits = 2),
               a = round(a_90, digits = 2),
               xg = round(xg_90, digits = 2),
               xa = round(xa_90, digits = 2),
               xgi = round(xgi_90, digits = 2),
               b = round(b_90, digits = 2),
               t_points = round(t_points_90, digits = 2),
               ga = round(ga_90, digits = 2),
               `G+A` = (ntile(ga_90,100) / n())*100)
      else if (calculations == "Per 90" && nrow(.) > 100) 
        mutate(., xGoals = ntile(xg_90,100),
               xAssists = ntile(xa_90,100),
               Saves = ntile(s_90,100),
               `Clean Sheets` = 100 - (ntile(cs_90,100)),
               `xG+xA` = ntile(xgi_90,100),
               Bonus = ntile(b_90,100),
               Goals = ntile(g_90,100),
               Assists = ntile(a_90,100),
               `Total Points` = ntile(t_points_90,100),
               cs = round(cs_90, digits = 2),
               g = round(g_90, digits = 2),
               a = round(a_90, digits = 2),
               xg = round(xg_90, digits = 2),
               xa = round(xa_90, digits = 2),
               xgi = round(xgi_90, digits = 2),
               b = round(b_90, digits = 2),
               t_points = round(t_points_90, digits = 2),
               ga = round(ga_90, digits = 2),
               `G+A` = ntile(ga_90,100))} %>%
    mutate(Goals = case_when(g == 0 ~ 0,
                             T ~ as.numeric(Goals)),
           `Clean Sheets` = case_when(cs == 0 ~ 0,
                                      T ~ as.numeric(`Clean Sheets`)),
           Assists = case_when(a == 0 ~ 0,
                               T ~ as.numeric(Assists)),
           `G+A` = case_when(ga == 0 ~ 0,
                             T ~ as.numeric(`G+A`))) %>%
  select(name_club,short_team_name,Value,Goals,g,xGoals,xg,Assists,a,xAssists,xa,`xG+xA`,xgi,`G+A`,ga,mins,b,cs,`Clean Sheets`,`Total Points`, t_points, image) %>%
  rename(`Goals Percentile` = Goals,
         Goals = g,
         `xG Conceded` = cs,
         `xG Conceded Percentile` = `Clean Sheets`,
         Team = short_team_name,
         `xG+xA Value` = xgi,
         `Assists Percentile` = Assists,
         Assists = a,
         `xG Percentile` = xGoals,
         `xA Percentile` = xAssists,
         `xG Value` = xg,
         `xA Value` = xa,
         `xG+xA Percentile` = `xG+xA`,
         `G+A Percentile` = `G+A`,
         `G+A` = ga,
         Player = name_club,
         `Total Points Percenitle` = `Total Points`,
         `Total Points` = t_points,
         `Mins Played` = mins,
         `Bonus Points` = b) %>%
  # This simply rounds a lot of the variables    
  mutate(`Goals Percentile` = round(`Goals Percentile`, digits = 0),
         `xG Conceded Percentile` = round(`xG Conceded Percentile`, digits = 0),
         `Total Points Percenitle` = round(`Total Points Percenitle`, digits = 0),
         `Assists Percentile` = round(`Assists Percentile`, digits = 0),
         `xG Percentile` = round(`xG Percentile`, digits = 0),
         `xA Percentile` = round(`xA Percentile`, digits = 0),
         `xG+xA Percentile` = round(`xG+xA Percentile`, digits = 0),
         `G+A Percentile` = round(`G+A Percentile`, digits = 0))
  
}


rank_calculations <- function(teamid, Gameweeks) {
  
  Team_Summary <- c()
  
  team <- teamid
  
  for (GW in seq(1,Gameweeks)) {
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team, '/event/',GW,'/picks/'))
    
    #FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/99950/event/2/picks/'))
    
    
    http_type(FPL_Team)
    http_error(FPL_Team)
    
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



rank_percentile_plot <- function(data, small_df2, Names1, Breaks1) {
  
  p <- ggplot(data, aes(x = event, y = rank_percentile, color = as.factor(movement), fill = as.factor(movement))) +
    geom_vline(data = small_df2, aes(xintercept = event), color = "lightgray", size = 9) +
    geom_segment(aes(x = event, xend = event, y = 50, yend = rank_percentile, color = as.factor(movement)), size = 3, show.legend = F) +
    geom_hline(yintercept = 50, color = "gray60", size = 1.25) +
    geom_point(shape = 21, stroke = 1.5, size = 6, color = "#F3E9E2") +
    geom_text(aes(label = round(rank_percentile, digits = 0)), size = 2.2, fontface = "bold", family = "Roboto", color = "white") +
    scale_color_manual(limits = c("Elite","Good","Average","Below Average"),
                       #values = c('#0B775E','#66C2A5','#FFBF00','#F2055C'),
                       values = c('#379A8B','#3EBCD2','#006BA2','#DB444B')) +
    scale_fill_manual(limits = c("Elite","Good","Average","Below Average"),
                      #values = c('#0B775E','#66C2A5','#FFBF00','#F2055C'),
                      values = c('#379A8B','#3EBCD2','#006BA2','#DB444B')) +
    #c('#DB444B','#006BA2','#3EBCD2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C')
    coord_cartesian(clip = "off") +
    #scale_x_continuous(breaks = seq(1,38,1)) +
    scale_x_continuous(name = NULL, labels = setNames(Names1,Breaks1), breaks = Breaks1) +
    scale_y_reverse(limits = c(105,0)) +
    labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
      title =  paste0("<img src='./Logo4.png' width='30'>", " Percentile Rank By Gameweek"),
      caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
      fill = "Percentile Rank",
      color = "Percentile Rank") +
    xlab("Gameweek") +
    ylab("Percentile Rank") +
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
          plot.title = element_markdown(color = "black", size = 24, family = "Ubuntu"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Ubuntu"),
          plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Ubuntu", hjust = 0.19),
          legend.text = element_text(color = "gray20", size = 12, family = "Ubuntu", hjust = 0),
          legend.title = element_text(color = "gray10", size = 12,  family = "Ubuntu"),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 10, family = "Ubuntu"),
          axis.text.y = element_text(color ="black", size = 10, family = "Ubuntu", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Ubuntu", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.title.y = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_line(color = "gray30", size = 1),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_line(color = "gray30", size = 1),
          axis.line.x = element_line(color = "gray30"),
          axis.line.y = element_line(color = "gray30"),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Ubuntu"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Ubuntu", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  
  
  p
  
}

rank_plot <- function(data, small_df2, Names1, Breaks1) {
  
  p <- ggplot(data, aes(x = event, y = overall_rank)) +
    geom_vline(data = small_df2, aes(xintercept = event), color = "lightgray", size = 9) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray", alpha = 0.6) +
    geom_borderline(color = "gray60", size = 2, bordercolour = "#F3E9E2", borderwidth = 0.5, show.legend = F) +
    geom_point(aes(fill = movement), shape = 21, stroke = 1, size = 7, color = "#F3E9E2") +
    geom_text(aes(label = total_short), size = 1.8, fontface = "bold", family = "Roboto", color = "white") +
    coord_cartesian(clip = "off") +
    scale_x_continuous(name = NULL, labels = setNames(Names1,Breaks1), breaks = Breaks1) +
    #scale_x_continuous(breaks = seq(0,38)) +
    scale_y_log10() +
    scale_color_manual(breaks = c("Red Arrow","No Arrow","Green Arrow"),
                       values = c('#F2055C','gray60','#66C2A5')) +
    scale_fill_manual(breaks = c("Red Arrow","No Arrow","Green Arrow"),
                      values = c('#F2055C','gray60','#66C2A5')) +
    labs(title =  paste0("<img src='./Logo4.png' width='30'>"," Gameweek Ranks (Current Rank: ", data %>% pull(overall_rank) %>% tail(1),")"),
    #labs(title =  paste0("<img src='./Logo4.png' width='30'>", " Current Rank:"),
         caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
         #subtitle = paste0("Sample includes players with at least: ", starts_1, " Starts"),
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
          plot.title = element_markdown(color = "black", size = 24, family = "Ubuntu"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Ubuntu"),
          plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Ubuntu", hjust = 0.19),
          legend.text = element_text(color = "gray20", size = 12, family = "Ubuntu", hjust = 0),
          legend.title = element_text(color = "gray10", size = 12,  family = "Ubuntu"),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 10, family = "Ubuntu"),
          axis.text.y = element_text(color ="black", size = 10, family = "Ubuntu", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Ubuntu", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.title.y = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_line(color = "gray30", size = 1),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_line(color = "gray30", size = 1),
          axis.line.x = element_line(color = "gray30"),
          axis.line.y = element_line(color = "gray30"),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Ubuntu"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Ubuntu", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  p
  
  
}

bench_points <- function(data, small_df2, Names1, Breaks1) {
  
  p <- ggplot(data, aes(x = event, y = points_on_bench, color = as.factor(movement), fill = as.factor(movement))) +
    geom_vline(data = small_df2, aes(xintercept = event), color = "lightgray", size = 9) +
    geom_segment(aes(x = event, xend = event, y = 0, yend = points_on_bench, color = as.factor(movement)), size = 3, show.legend = F) +
    #scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 6) +
    #new_scale_color() +
    #geom_text_repel(aes(label = oppsition), size = 2, fontface = "bold", family = "Roboto", color = "black", nudge_y = -2) +
    geom_point(shape = 21, stroke = 1.5, size = 6, color = "#F3E9E2") +
    #geom_text_repel(aes(label = label1), size = 2, fontface = "bold", family = "Roboto", color = "black",  box.padding = 0.5) +
    geom_text(aes(label = points_on_bench), size = 2.2, fontface = "bold", family = "Roboto", color = "white") +
    scale_color_manual(limits = c("Stacked","Good","Blank","Minus"),
                       values = c('#379A8B','#3EBCD2','#006BA2','#DB444B')) +
    scale_fill_manual(breaks = c("Stacked","Good","Blank","Minus"),
                      values = c('#379A8B','#3EBCD2','#006BA2','#DB444B')) +
    coord_cartesian(clip = "off") +
    #scale_x_continuous(breaks = seq(0,38)) +
    scale_x_continuous(name = NULL, labels = setNames(Names1,Breaks1), breaks = Breaks1) +
    labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
      title =  paste0("<img src='./Logo4.png' width='30'>", " Points Left On The Bench"),
      caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
      fill = "Points On Bench",
      color = "Points On Bench") +
    xlab("Gameweek") +
    ylab("Bench Points") +
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
          plot.title = element_markdown(color = "black", size = 24, family = "Ubuntu"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Ubuntu"),
          plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Ubuntu", hjust = 0.19),
          legend.text = element_text(color = "gray20", size = 12, family = "Ubuntu", hjust = 0),
          legend.title = element_text(color = "gray10", size = 12,  family = "Ubuntu"),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 10, family = "Ubuntu"),
          axis.text.y = element_text(color ="black", size = 10, family = "Ubuntu", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Ubuntu", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.title.y = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_line(color = "gray30", size = 1),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_line(color = "gray30", size = 1),
          axis.line.x = element_line(color = "gray30"),
          axis.line.y = element_line(color = "gray30"),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Ubuntu"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Ubuntu", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  
  
  p
  
  
}

captaincy_points <- function(data,round1) {
#captaincy_points <- function(data) {
  
  p <- ggplot(data %>% filter(played_captain == "Yes") %>% mutate(line_color = lead(total_points)), aes(x = reorder(label1, round), y = total_points)) +
    geom_segment(aes(x = round, xend = round, y = 0, yend = total_points, color = as.factor(captaincy)), size = 3, show.legend = F) +
    geom_point(aes(color = as.factor(captaincy), fill = as.factor(captaincy)), shape = 21, stroke = 1.5, size = 6, color = "#F3E9E2") +
    geom_text(aes(label = total_points), size = 2.2, fontface = "bold", family = "Roboto", color = "white") +
    scale_color_manual(breaks = c("Success","Fail", "Double Digits"),
                       #values = c('#66C2A5','#F2055C',"#0B775E"),
                       values = c('#006BA2','#DB444B','#379A8B')) +
    scale_fill_manual(breaks = c("Success","Fail", "Double Digits"),
                      #values = c('#66C2A5','#F2055C',"#0B775E"),
                      values = c('#006BA2','#DB444B','#379A8B')) + 
    coord_cartesian(clip = "off") +
    scale_x_discrete(name = NULL, labels = setNames(as.character(labels),round1)) +
    labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
      title =  paste0("<img src='./Logo4.png' width='30'>", " Captaincy Success"),
      caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
      fill = "Captaincy Success",
      color = "Captaincy Success") +
    xlab("Gameweek") +
    ylab("Captaincy Points") +
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
          plot.title = element_markdown(color = "black", size = 24, family = "Ubuntu"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Ubuntu"),
          plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Ubuntu", hjust = 0.19),
          legend.text = element_text(color = "gray20", size = 12, family = "Ubuntu", hjust = 0),
          legend.title = element_text(color = "gray10", size = 12,  family = "Ubuntu"),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_markdown(color ="black", size = 6, family = "Ubuntu"),
          axis.text.y = element_text(color ="black", size = 10, family = "Ubuntu", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Ubuntu", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.title.y = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_line(color = "gray30", size = 1),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_line(color = "gray30", size = 1),
          axis.line.x = element_line(color = "gray30"),
          axis.line.y = element_line(color = "gray30"),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Ubuntu"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Ubuntu", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  
  p
  
  
}


captaincy_points_text <- function(data, small_df2, round1, gw) {
  #captaincy_points <- function(data) {
  
  p <- ggplot(data %>% filter(played_captain == "Yes") %>% mutate(line_color = lead(total_points)), aes(x = reorder(GW_Chip, round), y = total_points)) +
    geom_vline(data = small_df2, aes(xintercept = round), color = "lightgray", size = 9) +
    geom_segment(aes(x = round, xend = round, y = 0, yend = total_points, color = as.factor(captaincy)), size = 3, show.legend = F) +
    geom_point(aes(color = as.factor(captaincy), fill = as.factor(captaincy)), shape = 21, stroke = 1.5, size = 6, color = "#F3E9E2") +
    geom_text(aes(label = total_points), size = 2.2, family = "Ubuntu", color = "white") +
    geom_shadowtext(aes(label = name_info), family = "Ubuntu", size = 2.2, color = "black", bg.colour = "#F3E9E2", bg.r = 0.02, nudge_y = 1.3, lineheight = .9) +
    scale_color_manual(breaks = c("Success","Fail", "Double Digits"),
                       #values = c('#66C2A5','#F2055C',"#0B775E"),
                       values = c('#006BA2','#DB444B','#379A8B')) +
    scale_fill_manual(breaks = c("Success","Fail", "Double Digits"),
                      #values = c('#66C2A5','#F2055C',"#0B775E"),
                      values = c('#006BA2','#DB444B','#379A8B')) + 
    coord_cartesian(clip = "off") +
    #scale_x_discrete(name = NULL, labels = setNames(as.character(Team_Selection_Final$label1),round)) +
    scale_x_discrete(name = NULL, labels = setNames(as.character(gw),as.character(round1))) +
    #scale_x_discrete(name = NULL, labels = setNames(as.character(labels),round1)) +
    labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
      title =  paste0("<img src='./Logo4.png' width='30'>", " Captaincy Success"),
      caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
      fill = "Captaincy Success",
      color = "Captaincy Success") +
    xlab("Gameweek") +
    ylab("Captaincy Points") +
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
          plot.title = element_markdown(color = "black", size = 24, family = "Ubuntu"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Ubuntu"),
          plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Ubuntu", hjust = 0.19),
          legend.text = element_text(color = "gray20", size = 12, family = "Ubuntu", hjust = 0),
          legend.title = element_text(color = "gray10", size = 12,  family = "Ubuntu"),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          #axis.text.x = element_markdown(color ="black", size = 6, family = "Ubuntu"),
          axis.text.x = element_text(color ="black", size = 10, family = "Ubuntu"),
          axis.text.y = element_text(color ="black", size = 10, family = "Ubuntu", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Ubuntu", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.title.y = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_line(color = "gray30", size = 1),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_line(color = "gray30", size = 1),
          axis.line.x = element_line(color = "gray30"),
          axis.line.y = element_line(color = "gray30"),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Ubuntu"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Ubuntu", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  
  p
  
  
}

league_parse <- function(team_id2, input_rival, Gameweeks) { 
  
  
  team_id <- as.character(team_id2)
  
  Team_Summary <- c()
  
  FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id,'/'))
  
  jsonRespText<-content(FPL_Team, as="text") 
  JSON_Output <- fromJSON(jsonRespText)
  team <- JSON_Output$name %>% data.frame() %>% pull(.)
  
  for (GW in seq(1,Gameweeks)) {
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id, '/event/',GW,'/picks/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    
    JSON_Output <- fromJSON(jsonRespText)
    Entry_History <- JSON_Output$entry_history %>%
      unlist() %>%
      t() %>%
      as.data.frame() %>%
      mutate(teamid = team_id)
    
    Team_Summary <- bind_rows(Team_Summary, Entry_History) %>%
      mutate(entry_name = team)
    
  }
  
  Team_Summary2 <- c()
  
  new_id <- as.character(input_rival)
  
  FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', new_id,'/'))
  
  jsonRespText<-content(FPL_Team, as="text") 
  JSON_Output <- fromJSON(jsonRespText)
  opp_team <- JSON_Output$name %>% data.frame() %>% pull(.)
  
  for (GW in seq(1,Gameweeks)) {
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', new_id , '/event/',GW,'/picks/'))
    
    jsonRespText<-content(FPL_Team, as="text") 
    
    JSON_Output <- fromJSON(jsonRespText)
    Entry_History <- JSON_Output$entry_history %>%
      unlist() %>%
      t() %>%
      as.data.frame() %>%
      mutate(teamid = new_id)
    
    Team_Summary2 <- bind_rows(Team_Summary2, Entry_History) %>%
      mutate(entry_name = opp_team)
    
  }
  
  Combined_Team <- bind_rows(Team_Summary, Team_Summary2)
  
}

captaincy_parse <- function(team_id, Players_History, Gameweeks) {
  
  Team_Selection <- c()
  
  Subs <- c()
  
  
  for (GW in seq(1,Gameweeks)) {
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id, '/event/',GW,'/picks/'))
    
    
    jsonRespText<-content(FPL_Team, as="text") 
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
    separate_wider_delim(photo, ".", names = c("image_id", "image_type")) %>%
    mutate(player_images = paste0('https://resources.premierleague.com/premierleague/photos/players/110x140/p',image_id,'.png'))
  
}


league_gwscore <- function(data) { 
  
  p <- ggplot(data, aes(x = event, y = points, color = as.factor(entry_name), fill = as.factor(entry_name))) +
    geom_borderline(size = 2, bordercolour = "#F3E9E2", borderwidth = 0.5, show.legend = F) +
    geom_point(aes(color = as.factor(entry_name)), shape = 21, stroke = 0.75, size = 5, color = "#F3E9E2") +
    geom_text(aes(label = points), size = 2, fontface = "bold", family = "Roboto", color = "white") +
    #scale_color_brewer(palette = "Dark2") +
    #scale_color_manual(values = c("#66C2A5","#3288BD","#5E4FA2")) +
    #scale_fill_manual(values = c("#66C2A5","#3288BD","#5E4FA2")) +
    scale_color_manual(values = c('#006BA2','#3EBCD2')) +
    scale_fill_manual(values = c('#006BA2','#3EBCD2')) +
    #c('#DB444B','#006BA2','#3EBCD2','#379A8B','#EBB434','#B4BA39','#9A607F','#D1B07C','#758D99')
    coord_cartesian(clip = "off") +
    scale_x_continuous(breaks = seq(0,38)) +
    #scale_y_continuous(limits = c(0,max(Combine_Team$points))) +
    labs(title =  paste0("<img src='./Logo4.png' width='30'>", " Manager Points per Gameweek "),
         caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
         color = "Team Name",
         fill = "Team Name") +
    xlab("Gameweek") +
    ylab("Total Points") +
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
          plot.title = element_markdown(color = "black", size = 24, family = "Ubuntu"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Ubuntu"),
          plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Ubuntu", hjust = 0.19),
          legend.text = element_text(color = "gray20", size = 12, family = "Ubuntu", hjust = 0),
          legend.title = element_text(color = "gray10", size = 12,  family = "Ubuntu"),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 10, family = "Ubuntu"),
          axis.text.y = element_text(color ="black", size = 10, family = "Ubuntu", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Ubuntu", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.title.y = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_line(color = "gray30", size = 1),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_line(color = "gray30", size = 1),
          axis.line.x = element_line(color = "gray30"),
          axis.line.y = element_line(color = "gray30"),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Ubuntu"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Ubuntu", angle = 360, vjust = 0),
          legend.background = element_blank())
  
}

league_gwrank <- function(data) { 
  
  p <- ggplot(data, aes(x = event, y = overall_rank, color = as.factor(entry_name), fill = as.factor(entry_name))) +
    geom_borderline(size = 2, bordercolour = "#F3E9E2", borderwidth = 0.5, show.legend = F) +
    geom_point(aes(color = as.factor(entry_name)), shape = 21, stroke = 1, size = 7, color = "#F3E9E2") +
    geom_text(aes(label = total_short), size = 1.8, fontface = "bold", family = "Roboto", color = "white") +
    coord_cartesian(clip = "off") +
    scale_x_continuous(breaks = seq(0,38)) +
    scale_y_log10() +
    #scale_color_manual(values = c("#66C2A5","#3288BD","#5E4FA2")) +
    #scale_fill_manual(values = c("#66C2A5","#3288BD","#5E4FA2")) +
    scale_color_manual(values = c('#006BA2','#3EBCD2')) +
    scale_fill_manual(values = c('#006BA2','#3EBCD2')) +
    labs(title =  paste0("<img src='./Logo4.png' width='30'>", " Manager Overall Rank"),
         caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
         #subtitle = paste0("Sample includes players with at least: ", starts_1, " Starts"),
         color = "Team Name",
         fill = "Team Name") +
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
          plot.title = element_markdown(color = "black", size = 24, family = "Ubuntu"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Ubuntu"),
          plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Ubuntu", hjust = 0.19),
          legend.text = element_text(color = "gray20", size = 12, family = "Ubuntu", hjust = 0),
          legend.title = element_text(color = "gray10", size = 12,  family = "Ubuntu"),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 10, family = "Ubuntu"),
          axis.text.y = element_text(color ="black", size = 10, family = "Ubuntu", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Ubuntu", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.title.y = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_line(color = "gray30", size = 1),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_line(color = "gray30", size = 1),
          axis.line.x = element_line(color = "gray30"),
          axis.line.y = element_line(color = "gray30"),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Ubuntu"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Ubuntu", angle = 360, vjust = 0),
          legend.background = element_blank())
  

  
  
}

league_parse2 <- function(team_id2)  {
  
  req(team_id2)
  
  t_id <- team_id2


FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', t_id,'/'))

jsonRespText<-content(FPL_Team, as="text") 
JSON_Output <- fromJSON(jsonRespText)

Leagues <- JSON_Output$leagues$classic %>%
  filter(league_type == 'x')

}

progress_tracker <- function() {
  # Create 0-row data frame which will be used to store data
  dat <- data.frame(x = numeric(0), y = numeric(0))
  
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Making plot", value = 0)
  
  # Number of times we'll go through the loop
  n <- 10
  
  for (i in 1:n) {
    # Each time through the loop, add another row of data. This is
    # a stand-in for a long-running computation.
    dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
    
    # Increment the progress bar, and update the detail text.
    progress$inc(1/n, detail = paste("Doing part", i))
    
    # Pause for 0.1 seconds to simulate a long computation.
    Sys.sleep(0.1)
  }
  
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

table2_parse <- function(Players_Gameweek, team_id) {


FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id, '/transfers/'))

jsonRespText<-content(FPL_Team, as="text") 
JSON_Output <- fromJSON(jsonRespText)

Transfers <- JSON_Output

FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id, '/history/'))

jsonRespText<-content(FPL_Team, as="text") 
JSON_Output <- fromJSON(jsonRespText)

Manager_History <- JSON_Output$current

Chips <- JSON_Output$chips %>%
  select(-time)

Transfers2 <- Transfers %>%
  left_join(Players_Gameweek %>% select(id,name_club, total_points, round, GW_Type), by = c('element_in' = 'id', 'event' = 'round')) %>%
  rename(transfer_in = name_club,
         In_GW_Type = GW_Type,
         in_points = total_points) %>%
  left_join(Players_Gameweek %>% select(id,name_club, total_points, round, GW_Type), by = c('element_out' = 'id', 'event' = 'round')) %>%
  rename(transfer_out = name_club,
         Out_GW_Type = GW_Type,
         out_points = total_points) %>%
  mutate(in_points = replace_na(in_points, 0),
         out_points = replace_na(out_points, 0)) %>%
  mutate(points_difference = in_points - out_points) %>%
  left_join(Manager_History, by = c('event')) %>%
  left_join(Chips, by = c('event')) %>%
  group_by(event) %>%
  summarise(transfers = n(),
            #manager = unique(entry),
            transfers_in = paste(transfer_in, collapse=","),
            transfers_out = paste(transfer_out, collapse=","),
            GW_in = paste(In_GW_Type, collapse=","),
            GW_out = paste(Out_GW_Type, collapse=","),
            hits_taken = as.numeric(unique(event_transfers_cost)),
            `Points In` = sum(in_points),
            `Points Out` = sum(out_points),
            chip = unique(name)) %>%
  #mutate(chip = case_when(event == "17" ~ "wc wildcard",
  #                        T ~ chip)) %>%
  mutate(`Points Difference` = `Points In` - `Points Out`) %>%
  mutate(`Points Difference After Hits` = `Points Difference` - hits_taken) %>%
  ungroup() %>%
  rename(Gameweek = event)

}

transfer_plot1 <- function(transfer_df, small_df2, Names1, Breaks1, tester_df) {
  
  
  ggplot(transfer_df, aes(x = Gameweek, y = value, color = as.factor(Points), fill = as.factor(Points))) +
    geom_vline(data = small_df2, aes(xintercept = Gameweek), color = "lightgray", size = 9) +
    geom_borderline(data = tester_df, aes(Group = as.factor(Points)), size = 2, bordercolour = "#F3E9E2", borderwidth = 0.5, show.legend = F) +
    geom_point(shape = 21, stroke = 1, size = 7, color = "#F3E9E2") +
    geom_text(aes(label = value), size = 1.8, fontface = "bold", family = "Roboto", color = "white") +
    #coord_cartesian(clip = "off") +
    #scale_x_continuous(breaks = seq(0,38)) +
    scale_x_continuous(name = NULL, labels = setNames(Names1,Breaks1), breaks = Breaks1) +
    scale_color_manual(values = c('#006BA2','#3EBCD2')) +
    scale_fill_manual(values = c('#006BA2','#3EBCD2')) +
    coord_cartesian(clip = "off") +
    #scale_x_continuous(breaks = seq(0,38)) +
    labs(#title =  paste0("Captaincy Success (ID: ", id, ")"),
      title =  paste0("<img src='./Logo4.png' width='30'>", " Transfer Points By Gameweek"),
      caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
      fill = "Total Points",
      color = "Total Points") +
    xlab("Gameweek") +
    ylab("Total Points") +
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
          plot.title = element_markdown(color = "black", size = 24, family = "Ubuntu"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Ubuntu"),
          plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Ubuntu", hjust = 0.19),
          legend.text = element_text(color = "gray20", size = 12, family = "Ubuntu", hjust = 0),
          legend.title = element_text(color = "gray10", size = 12,  family = "Ubuntu"),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 10, family = "Ubuntu"),
          axis.text.y = element_text(color ="black", size = 10, family = "Ubuntu", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Ubuntu", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.title.y = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_line(color = "gray30", size = 1),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_line(color = "gray30", size = 1),
          axis.line.x = element_line(color = "gray30"),
          axis.line.y = element_line(color = "gray30"),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Ubuntu"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Ubuntu", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  
}

transfer_plot2 <- function(transfer_df, small_df2, id, Names1, Breaks1) {
  
  ggplot(transfer_df, aes(x = Gameweek, y = `Points Difference After Hits`, color = as.factor(movement), fill = as.factor(movement))) +
    geom_vline(data = small_df2, aes(xintercept = Gameweek), color = "lightgray", size = 9) +
    geom_segment(aes(x = Gameweek, xend = Gameweek, y = 0, yend = `Points Difference After Hits`), size = 3, show.legend = F) +
    geom_hline(yintercept = 0, color = "gray60", size = 1.25) +
    geom_point(shape = 21, stroke = 1.25, size = 6, color = "#F3E9E2") +
    geom_text(aes(label = round(`Points Difference After Hits`, digits = 0)), size = 2.2, fontface = "bold", family = "Roboto", color = "white") +
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
      title =  paste0("<img src='./Logo4.png' width='30'>", " Points Difference After Hits"),
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
          plot.title = element_markdown(color = "black", size = 24, family = "Ubuntu"),
          plot.caption = element_markdown(color = "black", size = 10, family = "Ubuntu"),
          plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Ubuntu", hjust = 0.19),
          legend.text = element_text(color = "gray20", size = 12, family = "Ubuntu", hjust = 0),
          legend.title = element_text(color = "gray10", size = 12,  family = "Ubuntu"),
          legend.key = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,0,0,0),
          axis.text.x = element_text(color ="black", size = 10, family = "Ubuntu"),
          axis.text.y = element_text(color ="black", size = 10, family = "Ubuntu", vjust = -0.3, hjust = 1),
          #axis.text.y = element_markdown(size = 6, color = 'black', family = "Ubuntu", hjust = 1),
          axis.title.x = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.title.y = element_text(color = "black", size = 12,  family = "Ubuntu"),
          axis.ticks.length.x = unit(0.1, "cm"),
          axis.ticks.x = element_line(color = "gray30", size = 1),
          axis.ticks.length.y = unit(0.1, "cm"),
          axis.ticks.y = element_line(color = "gray30", size = 1),
          axis.line.x = element_line(color = "gray30"),
          axis.line.y = element_line(color = "gray30"),
          strip.background = element_blank(),
          strip.text.x = element_markdown(color = "gray20", size = 12, family = "Ubuntu"),
          strip.text.y.left = element_markdown(color = "gray20", size = 10, family = "Ubuntu", angle = 360, vjust = 0),
          legend.background = element_blank())
  
  
}

import_general <- function(General_Info) {
  
FPL_General <- GET('https://fantasy.premierleague.com/api/bootstrap-static/')

http_type(FPL_General)
http_error(FPL_General)

jsonRespText<-content(FPL_General, as="text") 

JSON_Output <- fromJSON(jsonRespText)

current_players <- JSON_Output$total_players

Teams <- JSON_Output$teams %>%
  select(id, name, short_name, strength, strength_overall_home, strength_overall_away, strength_attack_home, strength_attack_away, strength_defence_home, strength_defence_away) %>%
  rename(team_name = name,
         short_team_name = short_name,
         code = id)

General_Info <- JSON_Output$elements %>%
  select(id, first_name, second_name, web_name, team, element_type, photo) %>%
  left_join(Teams, by = c('team' = 'code'))

}


bar_plot_player <- function(test_df, metric1, top, player_metric, positions, GW1, GW2) {
  
  ggplot(test_df, aes(x = get(metric1), y = reorder(Player_Space, get(metric1)), color = Team)) +
  #geom_bar(stat = "identity", color = "white", show.legend = F) +
  geom_segment(aes(x = 0, xend = get(metric1), y = reorder(Player_Space, get(metric1)), yend = reorder(Player_Space, get(metric1))), size = 5, show.legend = F) +
  geom_point(aes(fill = Team), shape = 21, size = 9, stroke = 1.5, color = "#F3E9E2", show.legend = F) +
  geom_text(aes(label = get(metric1)), size = 3, color = "white", family = "Ubuntu") +
  geom_vline(xintercept = 0, color = "gray30") +
  scale_color_manual(breaks = test_df$Team,
                     values = test_df$team_colour) +
  scale_fill_manual(breaks = test_df$Team,
                    values = test_df$team_colour) +

  #scale_y_discrete(name = NULL, labels = setNames(test_df$labels, test_df$Player)) +
  
  labs(title = paste0("<img src='./Logo4.png' width='30'> Top ", top ," ", player_metric ," For ", positions," From GW", GW1, " To GW", GW2),
       #subtitle = "Cumulative Points Totals For the Highest Scoring FPL Players",
       caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>") +

  theme(#panel.grid.major.x = element_line(color = "gray", linewidth = 1, linetype = 'dotted'),
    panel.grid.major.x = element_line(color = "gray", linewidth = 0.5),
    #panel.grid.major.y = element_line(color = "#D8D9DA", linewidth = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    #panel.border = element_rect(colour = 'gray30', fill = NA, size = 2),
    legend.position = "top",
    panel.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
    plot.background = element_rect(color = "#F3E9E2", fill = "#F3E9E2"),
    plot.title = element_markdown(color = "black", size = 24, family = "Ubuntu"),
    plot.caption = element_markdown(color = "black", size = 8, family = "Ubuntu"),
    plot.subtitle = element_markdown(size = 12, color = "gray30", family = "Ubuntu", hjust = 0),
    legend.text = element_text(color = "gray30", size = 12, family = "Ubuntu", hjust = 0),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(0,0,0,0),
    #axis.text.x = element_text(color ="black", size = 10, family = "Ubuntu", angle = 90),
    axis.text.x = element_text(color ="black", size = 10, family = "Ubuntu"),
    axis.text.y = element_text(color ="black", size = 10, family = "Ubuntu", hjust = 1),
    #axis.text.y = element_markdown(size = 6, color = 'black', family = "Ubuntu", hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(color = "gray30"),
    axis.ticks.y = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.line.x = element_line(color = "gray30"),
    axis.line.y = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_markdown(color = "gray20", size = 8, family = "Ubuntu"),
    #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Ubuntu", , angle = 360, vjust = 0),
    legend.background = element_blank()) #+
    #enter_appear()
    #transition_reveal(get(metric1)) +
    #view_follow(fixed_y = T)
  
}

team_player_viz <- function(Minutes_Data, team_matrix, stat_matrix, y_var, text_var, col_var, color_palette, scale1) {
  
  ggplot(Minutes_Data, aes(x = reorder(labels, Gameweek), y = reorder_within(Position, get(y_var), name), fill = get(col_var), colour = get(col_var))) +
    geom_point(shape = 22, color = "#F3E9E2", size = 9) +
    geom_text(aes(label = get(text_var)), color = "#F3E9E2", fontface = "bold", family = "Ubuntu", size = 3, show.legend = F) +
    facet_grid(Position ~., scales = "free", space = "free", switch = "y") +
    color_palette +
    scale1 +
    scale_x_discrete(name = NULL, labels = setNames(as.character(labels), Minutes_Data$opp_team)) +
    xlab('FPL Gameweeks') +
    ylab('Player Names') +
    labs(title = paste0("<img src='./Logo4.png' width='30'> ", team_matrix, " | Player ", stat_matrix, " Matrix"),
         #subtitle = "Cumulative Points Totals For the Highest Scoring FPL Players",
         caption = "Data Taken From FPL Website | Follow @Data_Vizard_<img src='./Logo4.png' width='13'>",
         size = 'Number Of Starts',
         fill = 'Appearance Type') +
    guides(fill = guide_legend(override.aes = list(size = 10))) +
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
      plot.title = element_markdown(color = "black", size = 26, family = "Ubuntu"),
      plot.caption = element_markdown(color = "black", size = 12, family = "Ubuntu"),
      plot.subtitle = element_markdown(size = 16, color = "gray30", family = "Ubuntu", hjust = 0),
      legend.text = element_text(color = "gray30", size = 12, family = "Ubuntu", hjust = 0),
      legend.title = element_text(color = "gray20", size = 14,  family = "Ubuntu"),
      legend.key = element_blank(),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(0,0,0,0),
      #axis.text.x = element_text(color ="black", size = 10, family = "Ubuntu", angle = 90),
      axis.text.x = element_markdown(color ="black", size = 10, family = "Ubuntu"),
      axis.text.y = element_text(color ="gray20", size = 10, family = "Ubuntu", hjust = 0),
      #axis.text.y = element_markdown(size = 6, color = 'black', family = "Ubuntu", hjust = 1),
      axis.title.x = element_text(color = "black", size = 12,  family = "Ubuntu"),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_line(color = "gray30", size = 0.7),
      strip.background = element_blank(),
      strip.text.x = element_markdown(color = "gray20", size = 14, family = "Ubuntu"),
      #strip.text.y.left = element_markdown(color = "gray20", size = 12, family = "Ubuntu", , angle = 360, vjust = 0),
      legend.background = element_blank())
  
  
}



league_bump <- function(selectize_vector, league_gweeks1, league_gweeks2, bump_breaks) {
    
Team_Summary <- c()

#for (ID in test_vector) {
for (ID in selectize_vector) {
  print(ID)
  print(class(ID))
  FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', ID, '/'))
  
  jsonRespText<-content(FPL_Team, as="text") 
  JSON_Output <- fromJSON(jsonRespText)
  team <- JSON_Output$name
  
  # This shows your gameweek history
  for (GW in seq(from = league_gweeks1, to = league_gweeks2, by = bump_breaks)) {
    
    #FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', team_id, '/event/',GW,'/picks/'))
    
    FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', ID, '/event/',GW,'/picks/'))
    
    
    jsonRespText<-content(FPL_Team, as="text") 
    
    JSON_Output <- fromJSON(jsonRespText)
    Entry_History <- JSON_Output$entry_history %>%
      unlist() %>%
      t() %>%
      as.data.frame() %>%
      mutate(#player_name = player_name,
        entry_name = team)
    
    Team_Summary <- bind_rows(Team_Summary, Entry_History)
    
    
  }
  
}

Team_Summary

}



make_xg_table <- function(data,gweeks1,gweeks2) { 
  
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
    filter(red_cards == 0) %>%
    filter(minutes == 90) %>%
    filter(merged %in% fixtures_df$merged) %>%
    group_by(short_team_name, opp_short_team_name, round, merged) %>%
    reframe(xg_conceded = max(expected_goals_conceded),
            opposition = unique(opposition),
            team = unique(team),
            opp_strength = unique(opp_strength),
            goals_conceded = max(goals_conceded)) %>% 
    unite("merged2",team:opposition, remove = F) %>%
    arrange(desc(merged))
  
  
  tmp <- Minutes_Data %>% select(-short_team_name, -opp_short_team_name, -team, -merged2) %>% rename(opp_team = opposition, mystery = merged, xg = xg_conceded, strength = opp_strength, goals = goals_conceded) #%>%
  #arrange(desc(mystery))
  
  #test <- bind_cols(Minutes_Data,tmp)
  
  final_df <- left_join(Minutes_Data,tmp, by = c('merged2' = 'mystery', 'round')) %>%
    filter(team != opposition) %>%
    select(-merged,-merged2) %>%
    filter(round >= gweeks1 & round <= gweeks2) %>%
    arrange(round, short_team_name) %>%
    group_by(short_team_name) %>%
    summarise(`XG For` = round(mean(xg), digits = 2),
              Goals = round(mean(goals), digits = 2),
              `Goals Minus XG` = round(`XG For` - Goals, digits = 2),
              `Total Goals` = sum(goals),
              `XG Against` = round(mean(xg_conceded), digits = 2),
              `Goals Against` = round(mean(goals_conceded), digits = 2),
              `XG Ag. Minus Goals Ag.` = round(`Goals Against` - `XG Against`, digits = 2),
              `Total Goals Against` = sum(goals_conceded)
    ) %>%
    rename(Team = short_team_name) %>%
    mutate(`GW From` = gweeks1,
           `GW To` = gweeks2) %>% 
    relocate(`GW To`, .after = Team) %>%
    relocate(`GW From`, .after = Team)
  
  
}