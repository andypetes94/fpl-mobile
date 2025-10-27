#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(encoding = "UTF-8")
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

library(shiny)
library(shinyMobile)
library(bslib)
source('./Mobile_Functions.R')
library(dplyr)
library(tidyverse)
library(extrafont)
library(showtext)
library(ggtext)
library(ggborderline)
#library(gganimate)
library(ggtext)
library(shadowtext)
library(ggh4x)
library(stringr)
library(gghighlight)
library(DT)
library(htmltools)
library(tidytext)
library(jsonlite)
library(httr)

#Players_History <- read.csv('./Players_History.csv')
#Players_History <- read.csv('./24_25/Players_History.csv', encoding = "UTF-8")
Players_History <- read.csv('./25_26/Players_History.csv', encoding = "UTF-8")

app_opts <-   list(
  theme = "ios",
  #color = "deeppurple"
  dark = "auto",
  skeletonsOnLoad = FALSE,
  preloader = FALSE,
  filled = FALSE,
  color = "#007aff",
  touch = list(
    touchClicksDistanceThreshold = 5,
    tapHold = TRUE,
    tapHoldDelay = 750,
    tapHoldPreventClicks = TRUE,
    iosTouchRipple = FALSE,
    mdTouchRipple = TRUE
  ),
  iosTranslucentBars = FALSE,
  navbar = list(
    iosCenterTitle = TRUE,
    hideOnPageScroll = TRUE
  ),
  toolbar = list(
    hideOnPageScroll = FALSE
  ),
  pullToRefresh = FALSE
)

shinyApp(
  ui = f7Page(
    options = app_opts,
    title = "FPL Data Vizard",
    f7TabLayout(
      #bootstrapLib(bs_theme()),
      #theme = bs_theme(bootswatch = "darkly"),
      panels = tagList(
        f7Panel(
          HTML('<center><img src="Logo4.png" width="100" height="auto"</center>'),
          HTML('<h1>FPL Data Vizard</h1>'),
          f7Block("Analytics at your Fingertips!"),
          id = "mypanel1",
          side = "left",
          effect = "push",
          title = "Menu",
          #f7Block("A panel with push effect"),
          f7PanelMenu(
            id = "menu",
            f7PanelItem(
              tabName = "Tab1",
              title = "Home",
              icon = f7Icon("house"),
              active = TRUE
            ),
            f7PanelItem(
              tabName = "Tab2",
              title = "Player Analysis",
              icon = f7Icon("person")
            ),
            f7PanelItem(
              tabName = "Tab3",
              title = "Team",
              icon = f7Icon("sportscourt_fill")
            ),
            f7PanelItem(
              tabName = "Tab4",
              title = "Total Metrics",
              icon = f7Icon("graph_square_fill")
            ),
            f7PanelItem(
              tabName = "Tab5",
              title = "FPL Team",
              icon = f7Icon("person_3_fill")
            )
          )
        ),
        

      ),
      navbar = f7Navbar(
        title = "FPL Data Vizard",
        hairline = TRUE,
        leftPanel = TRUE,
        rightPanel = F
      ),
      f7Tabs(
        id = "tabs",
        animated = TRUE,
        #swipeable = TRUE,
        f7Tab(
          HTML('<center><img src="Logo4.png" alt="Description" style="padding-top: 20px; padding-bottom: 0px; display: block; border: none;" width="150" </center>'),
          h1("FPL Data Vizard"),
          p("Follow me on X (@data_vizard_) For Even More FPL Tools!"),
          hr(),
          h1("Soar to the Top of Your FPL Mini-Leagues!"),
          p("Use Data Visualisation and Analytical Tools to Generate Valuable Insights!"),
          title = "Home",
          tabName = "Tab1",
          icon = f7Icon("house"),
          active = TRUE,
          f7Card(title = "Select Your Tool of Analysis!",
                 outline = T,
                 width = 2,
                 color = "Red",
                 buttonColor = "Green",
                 f7Button(inputId = "go_tab2", label = "Go to Player Analysis!", rounded = T, icon = f7Icon("person")),
                 br(),
                 f7Button(inputId = "go_tab3", label = "Go to Team Analysis!", rounded = T,  color = "lightblue", icon = f7Icon("sportscourt_fill")),
                 br(),
                 f7Button(inputId = "go_tab4", label = "Go to Total Metrics Analysis!", rounded = T,  color = "teal", icon = f7Icon("graph_square_fill")),
                 br(),
                 f7Button(inputId = "go_tab5", label = "Go to Team Analysis!", rounded = T,  color = "red", icon = f7Icon("person_3_fill"))
                 
                 ),
        ),
        f7Tab(
          title = "Player Analysis",
          tabName = "Tab2",
          icon = f7Icon("person"),
          HTML('<center><img src="./Player_Analysis.png" alt="Description" style="padding-top: 20px; padding-bottom: 0px; display: block; border: none;" width="400" </center>'),
          f7Card(title = "Player Analysis Tool (25/26 Season)",
                 f7Select(inputId = "player_position", label = "Select Position:", choices = c("GK","DEF","MID","ST"), selected = "MID"),
                 f7Slider(inputId = "player_gameweeks", label = "Select Gameweeks:", min = 1, max = 38, value = c(1,38), scaleSteps = 1, scale = TRUE, labels = tagList(f7Icon("calendar_badge_minus"),f7Icon("calendar_badge_plus"))),
                 f7Slider(inputId = "player_value", label = "Select Max Value (£) For Sample:", min = 1, max = 16, value = 16, step = 0.5, scaleSteps = 15, scale = TRUE, color = "lightblue", labels = tagList(f7Icon("circle"),f7Icon("money_pound_circle"))),
                 f7Slider(inputId = "player_mins", label = "Select Minimum Minutes Threshold:", min = 0, max = 1000, value = 300, step = 50, scaleSteps = 5, scale = TRUE, color = "teal", labels = tagList(f7Icon("clock"),f7Icon("clock_fill"))),
                 br(),
                 selectInput(inputId = "player_names", label = "Select Player:", choices = c("Raya (ARS)"), selected = c("Raya (ARS)")),
                 br(),
                 br(),
                 plotOutput("player_comparison_plot"),
                 f7DownloadButton(outputId = "down_player_comparison", label = "Download this plot!")),
          f7Card(f7SmartSelect(inputId = "table_variables", label = "Select Table Metrics", choices = sort(c("xG p90","xG Conceded p90","C Sheets p90","Saves p90","xA p90","xGI p90","BPS p90","Goals p90","Assists p90","Points p90","G+A p90","xG Percentile","xA Percentile","Saves Percentile","xGA Percentile","CS Percentile","xGI Percentile","BPS Percentile","Goals Percentile","Assists Percentile","Points","G+A Percentile")), multiple = T, selected = c("xG p90","xA p90","xGI p90","BPS p90","Points")),
          tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color: #ffffff;
                    }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #ffffff;
                     }
                    
                    table th {
    background:#212529;
                    }
table tr:nth-child(odd) td{
           background:#414141;
}
table tr:nth-child(even) td{
            background:#000000;
}

div.dataTables_scrollHead span {color: black;}

                   "
                                 
                                 
                 ))
          ),
          dataTableOutput("data1"),
        ),
        f7Tab(
          title = "Team",
          tabName = "Tab3",
          icon = f7Icon("sportscourt_fill"),
          HTML('<center><img src="./Team_Analysis.png" alt="Description" style="padding-top: 20px; padding-bottom: 0px; display: block; border: none;" width="400" </center>'),
          f7Card(
            title = "Team Analysis",
            f7Select(inputId = "team_team", label = "Select Team:", choices = sort(unique(Players_History$short_team_name)), selected = sort(unique(Players_History$short_team_name))[1]),
            #f7Select(inputId = "player_calculation", label = "Select Calculation Type:", choices = c("Per Game","Per 90","Total")),
            f7Slider(inputId = "team_gameweeks", label = "Select Gameweeks:", min = 1, max = 38, value = c(0,38), scaleSteps = 1, scale = TRUE, labels = tagList(f7Icon("calendar_badge_minus"),f7Icon("calendar_badge_plus"))),
            f7Slider(inputId = "team_mins", label = "Select Minimum Minutes Threshold:", min = 0, max = 1000, value = 300, step = 50, scaleSteps = 5, scale = TRUE, color = "teal", labels = tagList(f7Icon("clock"),f7Icon("clock_fill"))),
            br(),
            selectInput(inputId = "team_metrics", label = "Select Metric:", choices = c("Minutes", "Points", "XGI"), selected = c("Minutes")),
            plotOutput("team_comparison_plot"),
            f7DownloadButton(outputId = "down_team_comparison", label = "Download this plot!"),
            br(),
            # selectInput(inputId = "team_positions", label = "Select Position:", choices = c("GK","DEF","MID","ST"), selected = c("ST")),
            # f7Slider(inputId = "players_slider", label = "Select No. of Players:", min = 0, max = 40, value = 10, step = 1, scaleSteps = 8, scale = TRUE, color = "lightblue", labels = tagList(f7Icon("circle"),f7Icon("money_pound_circle"))),
            # br(),
            # selectInput(inputId = "all_metrics", label = "Select Metric:", choices = c("all_xgi","all_xg","all_xa"), selected = c("all_xgi")),
            # plotOutput("player_output_plot"),
            # f7DownloadButton(outputId = "down_player_output_plot", label = "Download this plot!"),
            
          )
        ),
        f7Tab(
          title = "Total Metrics",
          tabName = "Tab4",
          icon = f7Icon("graph_square_fill"),
          HTML('<center><img src="./Total_Metrics.png" alt="Description" style="padding-top: 20px; padding-bottom: 0px; display: block; border: none;" width="400" </center>'),
          f7Card(
            title = "Total Metrics",
            f7Select(inputId = "points_position", label = "Select Position:", choices = c("GK","DEF","MID","ST"), selected = c("Minutes")),            #f7Select(inputId = "player_calculation", label = "Select Calculation Type:", choices = c("Per Game","Per 90","Total")),
            f7Slider(inputId = "axis_length", label = "Select X-Axis Length:", min = 0, max = 300, value = 80, scaleSteps = 1, scale = TRUE, labels = tagList(f7Icon("calendar_badge_minus"),f7Icon("calendar_badge_plus"))),
            f7Slider(inputId = "player_number", label = "Select Number of Players:", min = 0, max = 50, value = c(0,10), step = 1, scaleSteps = 5, scale = TRUE, color = "teal", labels = tagList(f7Icon("clock"),f7Icon("clock_fill"))),
            br(),
            plotOutput("player_points_plot"),
            f7DownloadButton(outputId = "down_player_points", label = "Download this plot!"),
            br(),
            selectInput(inputId = "metric_names", label = "Select Metric:", choices = c("total_points","goals_scored","assists","yellow_cards","red_cards","saves","bonus","bps","influence","creativity","threat","ict_index","expected_goals","expected_assists","expected_goal_involvements","expected_goals_conceded","value","transfers_in","transfers_out"), selected = c("total_points")),
            br(),
            plotOutput("player_top_performers"),
            f7DownloadButton(outputId = "down_player_top", label = "Download this plot!")
            
          )
        ),
        f7Tab(
          title = "FPL Team",
          tabName = "Tab5",
          icon = f7Icon("sportscourt_fill"),
          HTML('<center><img src="./Team_Analysis.png" alt="Description" style="padding-top: 20px; padding-bottom: 0px; display: block; border: none;" width="400" </center>'),
          f7Card(
            title = "Enter your FPL ID",
            f7TextArea(inputId = "FPL_ID", value = '75141', label = "Enter ID"),
            textOutput("team_text"),
            textOutput("player_text"),
            textOutput("country_text"),
            #f7Select(inputId = "team_team", label = "Select Team:", choices = sort(unique(Players_History$short_team_name)), selected = sort(unique(Players_History$short_team_name))[1]),
            #f7Select(inputId = "player_calculation", label = "Select Calculation Type:", choices = c("Per Game","Per 90","Total")),
            #f7Slider(inputId = "team_gameweeks", label = "Select Gameweeks:", min = 1, max = 38, value = c(1,8), scaleSteps = 1, scale = TRUE, labels = tagList(f7Icon("calendar_badge_minus"),f7Icon("calendar_badge_plus"))),
            #f7Slider(inputId = "team_mins", label = "Select Minimum Minutes Threshold:", min = 0, max = 1000, value = 0, step = 50, scaleSteps = 5, scale = TRUE, color = "teal", labels = tagList(f7Icon("clock"),f7Icon("clock_fill"))),
            #br(),
            #selectInput(inputId = "team_metrics", label = "Select Metric:", choices = c("Minutes", "Points", "XGI"), selected = c("Minutes")),
            plotOutput("captaincy_plot"),
            f7DownloadButton(outputId = "down_cap_plot", label = "Download this plot!"),
            br(),
            plotOutput("rank_plot"),
            f7DownloadButton(outputId = "down_rank_plot", label = "Download this plot!"),
            br(),
            plotOutput("transfer_plot"),
            f7DownloadButton(outputId = "down_transfer_plot", label = "Download this plot!")
            
          )
        )
      )
    )
  ),
  
  
  server = function(input, output, session) {
    
    filters <- reactive({
      list(input$player_position, input$player_gameweeks[1], input$player_gameweeks[2], input$player_mins, input$player_value)
    })
    
    
    
    # Extract Players in that position
    observeEvent(filters(),{
      
      player_options <- g_percentiles(data = Players_History, positions = input$player_position, gameweeks_1 = input$player_gameweeks[1], gameweeks_2 = input$player_gameweeks[2], mins_1 = input$player_mins, v =  input$player_value) %>% #g_percentiles(Players_History, positions = position, gameweeks_1 = gw_start, gameweeks_2 = gw_end, mins_1 = mins, v =  value, calculations = "Per 90") %>%
        pull(name_club) 
        #print(name_club)
      
      #updateF7Select(session, inputId="player_names", selected = player_options)
      updateSelectInput(session, inputId="player_names", 
                        choices = sort(player_options), 
                        selected = sort(player_options[1]))
      
    })
    
    New_Teamname <- reactive({
      
      
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$FPL_ID,'/'))
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      JSON_Output$name %>% data.frame() %>% pull(.)
    })
    
    output$team_text <- renderText(paste0("Team Name: ",{New_Teamname()}))
    
    New_Player <- reactive({
      
      
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$FPL_ID,'/'))
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      paste0(JSON_Output$player_first_name, " ", JSON_Output$player_last_name) %>% data.frame() %>% pull(.)
    })
    
    output$player_text <- renderText(paste0("Player Name: ",{New_Player()}))
    
    New_Country <- reactive({
      
      
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$FPL_ID,'/'))
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      JSON_Output$player_region_name %>% data.frame() %>% pull(.)
    })
    
    output$country_text <- renderText(paste0("Region Name: ",{New_Country()}))
    
    output$player_comparison_plot <- renderPlot({
      
      
      test1 <- g_percentiles(data = Players_History, positions = input$player_position, gameweeks_1 = input$player_gameweeks[1], gameweeks_2 = input$player_gameweeks[2], mins_1 = input$player_mins, v =  input$player_value) %>%
        filter(name_club %in% input$player_names)
      
     Comparison <- comparison_parse(data = test1)
      
      p <- comparison_bar(data = Comparison)
      
      p
      
      
    })
    
  
    output$down_player_comparison <- downloadHandler(
        filename = function(){paste("FPL.Player_Stats.", input$player_names,".png",sep='')},
        
        content = function(file){
      
      test1 <- g_percentiles(data = Players_History, positions = input$player_position, gameweeks_1 = input$player_gameweeks[1], gameweeks_2 = input$player_gameweeks[2], mins_1 = input$player_mins, v =  input$player_value) %>%
        filter(name_club %in% input$player_names)
      
      Comparison <- comparison_parse(data = test1)
      
      p <- comparison_bar(data = Comparison)
      
      ggsave(file, plot = p, dpi = 600)
      
      
    })
    
    
    
    observeEvent(input$go_tab2, {
      updateF7Tabs(id = "tabs", 
                   selected = "Tab2",
                   session = session)
    })
    
    observeEvent(input$go_tab3, {
      updateF7Tabs(id = "tabs", 
                   selected = "Tab3",
                   session = session)
    })
    
    observeEvent(input$go_tab4, {
      updateF7Tabs(id = "tabs", 
                   selected = "Tab4",
                   session = session)
    })
    
    observeEvent(input$go_tab5, {
      updateF7Tabs(id = "tabs", 
                   selected = "Tab5",
                   session = session)
    })
    
    # update tabs depending on side panel
    observeEvent(input$menu, {
      updateF7Tabs(id = "tabs",
                   selected = input$menu,
                   session = session)
    })
    
    #output$data <- renderTable({
    #  mtcars[, c("mpg", input$variable), drop = FALSE]
    #}, rownames = TRUE)
    
    
    table_data <- reactive({
  
      vector1 <- c("Player","short_team_name","Position","games","Value","starts","mins")
      metric_vector <- input$table_variables
      
      metrics <- c(vector1, metric_vector)
      
      df <- g_percentiles(data = Players_History, positions = input$player_position, gameweeks_1 = input$player_gameweeks[1], gameweeks_2 = input$player_gameweeks[2], mins_1 = input$player_mins, v =  input$player_value) %>%
        #select(-pimage,-xg,-t_points,-xgc,-csheets,-s,-g,-a,-xa,-xgi,-b,-ga) %>%
        rename(`xG Percentile` = xGoals,
               `xA Percentile` = `xAssists`,
               `Saves Percentile` = Saves,
               `xGA Percentile` = `xG Conceded`,
               `CS Percentile` = `C Sheets`,
               `xGI Percentile` = `xG+xA`,
               `BPS Percentile` = Bonus,
               `Goals Percentile` = Goals,
               `Assists Percentile` = Assists,
               Points = `Overall Points`,   
               `G+A Percentile` = `G+A`,
               Player = name_club) %>% relocate(Player) %>% mutate(across(where(is.numeric), \(x) round(x, digits = 2))) %>%
        select(all_of(metrics))
  
      
      return(df)
      
    })
    
    
    # datatable
    output$data1 <- DT::renderDataTable( table_data(), 
        fill = F, rownames = F, extensions = "FixedColumns", 
      options = list(
        scrollX = T,
        paging = TRUE, 
        searching = TRUE, 
        info = FALSE,
        sort = TRUE,
        fixedColumns = T,
        autoWidth = TRUE))
    
    
    output$team_comparison_plot <- renderPlot({
      
      Minutes_Data <- team_parse(data = Players_History,
                                   start_gameweek = input$team_gameweeks[1], 
                                   end_gameweek = input$team_gameweeks[2], 
                                   team_minutes = input$team_mins,
                                   team_name = input$team_team)
      
      Minutes_Data$Minutes_Type <- ordered(Minutes_Data$Minutes_Type, levels = c("90 Minutes","60 Minutes & Over","Under 60 Minutes"))
      Minutes_Data$Points_Type <- ordered(Minutes_Data$Points_Type, levels = c("Minus","Blank","Return","Double Digit"))
      Minutes_Data$Position <- ordered(Minutes_Data$Position, levels = c("GK","DEF","MID","ST"))
      
      
      
      p <- team_gw_plot(input_metrics = input$team_metrics, 
                        data = Minutes_Data,
                        team_name = input$team_team)
      
      p
        
      
      
    })
    

    
    output$down_team_comparison<- downloadHandler(
      filename = function(){paste("FPL.Team_Stats.", input$team_team,".png",sep='')},
      
      content = function(file){
      
      Minutes_Data <- team_parse(data = Players_History,
                                 start_gameweek = input$team_gameweeks[1], 
                                 end_gameweek = input$team_gameweeks[2], 
                                 team_minutes = input$team_mins,
                                 team_name = input$team_team)
      
      Minutes_Data$Minutes_Type <- ordered(Minutes_Data$Minutes_Type, levels = c("90 Minutes","60 Minutes & Over","Under 60 Minutes"))
      Minutes_Data$Points_Type <- ordered(Minutes_Data$Points_Type, levels = c("Minus","Blank","Return","Double Digit"))
      Minutes_Data$Position <- ordered(Minutes_Data$Position, levels = c("GK","DEF","MID","ST"))
      
      
      
      p <- team_gw_plot(input_metrics = input$team_metrics, 
                        data = Minutes_Data,
                        team_name = input$team_team)
      
      ggsave(file, plot = p, dpi = 600)
      
      
    })
    
    output$player_output_plot <- renderPlot({
      
      Minutes_Data <- team_parse_pos(data = Players_History,
                                     start_gameweek = input$team_gameweeks[1], 
                                     end_gameweek = input$team_gameweeks[2], 
                                     team_minutes = input$team_mins,
                                     position_name = input$team_positions)
      
      subset_df <- Minutes_Data %>%
        select(name_club, input$all_metrics) %>%
        unique() %>%
        as.data.frame() %>%
        arrange(desc(get(input$all_metrics))) %>%
        head(input$players_slider) %>%
        pull(name_club)
      
      Minutes_Data <- Minutes_Data %>%
        filter(name_club %in% subset_df)
      
      Minutes_Data$Minutes_Type <- ordered(Minutes_Data$Minutes_Type, levels = c("90 Minutes","60 Minutes & Over","Under 60 Minutes"))
      Minutes_Data$Points_Type <- ordered(Minutes_Data$Points_Type, levels = c("Minus","Blank","Return","Double Digit"))
      Minutes_Data$Position <- ordered(Minutes_Data$Position, levels = c("GK","DEF","MID","ST"))
      
      p <- player_xgi_plot(input_metrics = input$all_metrics,
                           data = Minutes_Data,
                           position_name = input$team_positions,
                           player_number = input$players_slider)
      
      p
      
    
      
    })
    
    output$down_player_output_plot <- downloadHandler(
      filename = function(){paste("FPL.Player.", input$all_metrics, ".", input$team_positions ,".png",sep='')},
      
      content = function(file){
        
        Minutes_Data <- team_parse_pos(data = Players_History,
                                       start_gameweek = input$team_gameweeks[1], 
                                       end_gameweek = input$team_gameweeks[2], 
                                       team_minutes = input$team_mins,
                                       position_name = input$team_positions) %>%
          arrange(desc(input$all_metrics))
        
        subset_df <- Minutes_Data %>%
          select(name_club, input$all_metrics) %>%
          unique() %>%
          head(input$players_slider) %>%
          pull(name_club)
        
        Minutes_Data <- Minutes_Data %>%
          filter(name_club %in% subset_df)
        
        Minutes_Data$Minutes_Type <- ordered(Minutes_Data$Minutes_Type, levels = c("90 Minutes","60 Minutes & Over","Under 60 Minutes"))
        Minutes_Data$Points_Type <- ordered(Minutes_Data$Points_Type, levels = c("Minus","Blank","Return","Double Digit"))
        Minutes_Data$Position <- ordered(Minutes_Data$Position, levels = c("GK","DEF","MID","ST"))
        
        p <- player_xgi_plot(input_metrics = input$all_metrics,
                             data = Minutes_Data,
                             position_name = input$team_positions,
                             player_number = input$players_slider)
        
        p
        
        ggsave(file, plot = p, dpi = 600)
        
        
      })
    
    
    output$player_points_plot <- renderPlot({
    
      total_df <- player_points_parse(data = Players_History, 
                          input_position = input$points_position, 
                          players_2 = input$player_number[2], 
                          players_1 = input$player_number[1])
      
      
      p <- player_points_plot1(input_position = input$points_position,
                               data = total_df,
                               axis_length = input$axis_length)
      
      p
      
      
    })
    
      
    output$down_player_points <- downloadHandler(
        filename = function(){paste("FPL.Player.Total.", input$points_position,".png",sep='')},
        
        content = function(file){
      
      total_df <- player_points_parse(data = Players_History, 
                                      input_position = input$points_position, 
                                      players_2 = input$player_number[2], 
                                      players_1 = input$player_number[1])
      
      
      p <- player_points_plot1(input_position = input$points_position,
                               data = total_df,
                               axis_length = input$axis_length)
      
      ggsave(file, plot = p, dpi = 600)
      
      
    })
    
    
    output$player_top_performers <- renderPlot({
      
      tryCatch({
        Players_Filtered <- top_performers_parse(data = Players_History,
                                                 input_position = input$points_position,
                                                 input_metrics = input$metric_names)
        
        # Validate that we have a data frame
        if(!is.data.frame(Players_Filtered)) {
          stop("top_performers_parse did not return a data frame")
        }
        
        if(nrow(Players_Filtered) == 0) {
          stop("No data returned from top_performers_parse")
        }
        
        best_performers <- unique(Players_Filtered$web_name) %>% head(9)
        
        Players_Filtered <- Players_Filtered %>%
          filter(web_name %in% best_performers)
        
        labels <- c()
        
        for (i in 1:length(Players_Filtered$player_images)){
          
          img.name <- Players_Filtered$name_value[i]
          value.name <- Players_Filtered$actual_value[i]
          
          labels <- c(labels, paste0("<img src='", Players_Filtered$player_images[i],  "' width='20' /><br>", value.name))
          
        }
        
        # Use bind_cols more safely
        if(length(labels) == nrow(Players_Filtered)) {
          Players_Filtered2 <- bind_cols(Players_Filtered, labels = labels)
        } else {
          # Fallback if lengths don't match
          Players_Filtered2 <- Players_Filtered %>%
            mutate(labels = paste0("<img src='", player_images,  "' width='20' /><br>", actual_value))
        }
        
        Players_Filtered2$web_name <- factor(Players_Filtered2$web_name, levels = best_performers)
        Players_Filtered2$labels <- factor(Players_Filtered2$labels, levels = unique(Players_Filtered2$labels))
        
        p <- top_performers_plot(data = Players_Filtered2,
                                 input_position = input$points_position,
                                 input_metrics = input$metric_names) 
        
        p
        
      }, error = function(e) {
        cat("Error in player_top_performers:", e$message, "\n")
        # Return an empty plot with error message
        ggplot() + 
          ggtitle(paste("Error loading top performers:", e$message)) +
          theme_minimal()
      })
    })
    
    output$down_player_top <- downloadHandler(
      filename = function(){paste("FPL.Player.Top.", input$metric_names,".png",sep='')},
      
      content = function(file){
      
      Players_Filtered <- top_performers_parse(data = Players_History,
                                               input_position = input$points_position,
                                               input_metrics = input$metric_names)
      
      best_performers <- unique(Players_Filtered$web_name) %>% head(9)
      
      Players_Filtered <- Players_Filtered %>%
        filter(web_name %in% best_performers)
      
      labels <- c()
      
      for (i in 1:length(Players_Filtered$player_images)){
        
        img.name <- Players_Filtered$name_value[i]
        value.name <- Players_Filtered$actual_value[i]
        
        labels <- c(labels, paste0("<img src='", Players_Filtered$player_images[i],  "' width='20' /><br>", value.name))
        
      }
      
      Players_Filtered2 <- bind_cols(Players_Filtered, labels)
      
      # Rename Last Column
      names(Players_Filtered2)[length(names(Players_Filtered2))]<-"labels" 
      
      Players_Filtered2$web_name <- factor(Players_Filtered2$web_name, levels = best_performers)
      
      Players_Filtered2$labels <- factor(Players_Filtered2$labels, levels = labels %>% unique())
      
      
      p <- top_performers_plot(data = Players_Filtered2,
                               input_position = input$points_position,
                               input_metrics = input$metric_names) 
      
      ggsave(file, plot = p, dpi = 600)
      
    })
    
    
    output$captaincy_plot <- renderPlot({
      
      tryCatch({
        FPL_ID <- input$FPL_ID
        
        FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', FPL_ID, '/history/'))
        
        jsonRespText <- content(FPL_Team, as="text") 
        JSON_Output <- fromJSON(jsonRespText)
        
        # Fix: Check if chips exists and handle empty/NULL case
        if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
          Chips <- JSON_Output$chips %>%
            select(-time)
        } else {
          # Create empty data frame with expected structure if no chips
          Chips <- data.frame(
            event = integer(0),
            name = character(0),
            stringsAsFactors = FALSE
          )
        }
        
        FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', FPL_ID,'/'))
        
        jsonRespText <- content(FPL_Team, as="text") 
        JSON_Output <- fromJSON(jsonRespText)
        Team_Name <- JSON_Output$name %>% data.frame() %>% pull(.)
        Manager <- paste0(JSON_Output$player_first_name, " ", JSON_Output$player_last_name) %>% data.frame() %>% pull(.)
        
        # Get the data from captaincy_parse_mobile
        captaincy_data <- captaincy_parse_mobile(FPL_ID, Players_History, max(Players_History$round))
        
        # Validate that we have a data frame
        if(!is.data.frame(captaincy_data)) {
          stop("captaincy_parse_mobile did not return a data frame")
        }
        
        Team_Selection_Final <- captaincy_data %>%
          left_join(Chips, by = c('round' = 'event')) %>%
          rename(chip = name) %>%
          mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                        chip == 'freehit' ~ 'FH',
                                        chip == 'bboost' ~ 'BB',
                                        chip == '3xc' ~ 'TC',
                                        chip == 'manager' ~ 'AM',
                                        T ~'')) %>%
          mutate(GW_Chip = paste(round,"\n",chip_short)) %>%
          mutate(GW_Chip = str_replace(GW_Chip, " ","")) %>%
          # Fix: Use tryCatch for the separate function here too
          {tryCatch({
            separate(., name_club, sep = " " ,c("name","club"), remove = FALSE, convert = TRUE)
          }, error = function(e) {
            cat("Error in separate() for name_club:", e$message, "\n")
            # If separate fails, create name and club columns manually
            mutate(., 
                   name = word(name_club, 1),
                   club = word(name_club, -1))
          })} %>%
          mutate(name_info = paste0(name, "\n(",oppsition,")")) %>%
          mutate(img_labels = paste0("<img src='", player_images,  "' width='11' /><br>", label2)) %>%
          mutate(img_labels = str_replace_all(img_labels, ",", "<br>"))
        
        small_df2 <- Team_Selection_Final %>%
          group_by(round) %>%
          summarise(chip = unique(chip)) %>%
          ungroup() %>%
          filter(!is.na(chip))
        
        round1 <- Team_Selection_Final$round
        gw <- Team_Selection_Final$GW_Chip
        
        data <- Team_Selection_Final
        
        p <- captaincy_plot(data = data,
                            gw = gw,
                            round1 = round1,
                            Team_Name = Team_Name)
        
        p
        
      }, error = function(e) {
        cat("Error in captaincy_plot:", e$message, "\n")
        # Return an empty plot with error message
        ggplot() + 
          ggtitle(paste("Error loading captaincy data:", e$message)) +
          theme_minimal()
      })
    })
    
    
    output$down_cap_plot <- downloadHandler(
      filename = function(){paste("FPL.Captaincy",  New_Teamname(),".png",sep='')},
      
      content = function(file){
        
        FPL_ID <- input$FPL_ID
        
        FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', FPL_ID, '/history/'))
        
        
        jsonRespText<-content(FPL_Team, as="text") 
        JSON_Output <- fromJSON(jsonRespText)
        
        # Fix: Check if chips exists and handle empty/NULL case
        if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
          Chips <- JSON_Output$chips %>%
            select(-time)
        } else {
          # Create empty data frame with expected structure if no chips
          Chips <- data.frame(
            event = integer(0),
            name = character(0),
            stringsAsFactors = FALSE
          )
        }
        
        
        FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', FPL_ID,'/'))
        
        jsonRespText<-content(FPL_Team, as="text") 
        JSON_Output <- fromJSON(jsonRespText)
        Team_Name <- JSON_Output$name %>% data.frame() %>% pull(.)
        Manager <- paste0(JSON_Output$player_first_name, " ", JSON_Output$player_last_name) %>% data.frame() %>% pull(.)
        
        
        Team_Selection_Final <- captaincy_parse_mobile(FPL_ID, Players_History, max(Players_History$round)) %>%
          left_join(Chips, by = c('round' = 'event')) %>%
          rename(chip = name) %>%
          mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                        chip == 'freehit' ~ 'FH',
                                        chip == 'bboost' ~ 'BB',
                                        chip == '3xc' ~ 'TC',
                                        chip == 'manager' ~ 'AM',
                                        T ~'')) %>%
          mutate(GW_Chip = paste(round,"\n",chip_short)) %>%
          mutate(GW_Chip = str_replace(GW_Chip, " ","")) %>%
          separate(name_club, sep = " " ,c("name","club")) %>%
          mutate(name_info = paste0(name, "\n(",oppsition,")")) %>%
          mutate(img_labels = paste0("<img src='", player_images,  "' width='20' /><br>", label2)) %>%
          mutate(img_labels = str_replace_all(img_labels, ",", "<br>"))
        
        small_df2 <- Team_Selection_Final %>%
          group_by(round) %>%
          summarise(chip = unique(chip)) %>%
          ungroup() %>%
          filter(!is.na(chip))
        
        
        round1 <- Team_Selection_Final$round
        gw <- Team_Selection_Final$GW_Chip
        
        data <- Team_Selection_Final
        
        p <- captaincy_down_plot(data = data,
                            gw = gw,
                            round1 = round1,
                            Team_Name = Team_Name)
        
        ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
        
      })
    
    
    
    output$rank_plot <- renderPlot({
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$FPL_ID, '/history/'))
      
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      
      # Fix: Check if chips exists and handle empty/NULL case
      if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
        Chips <- JSON_Output$chips %>%
          select(-time)
      } else {
        # Create empty data frame with expected structure if no chips
        Chips <- data.frame(
          event = integer(0),
          name = character(0),
          stringsAsFactors = FALSE
        )
      }
      
      temp_df <- team_picks_parse(input$FPL_ID, max(Players_History$round)) %>%
        left_join(Chips, by = c('event')) %>%
        rename(chip = name) %>%
        mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                      chip == 'freehit' ~ 'FH',
                                      chip == 'bboost' ~ 'BB',
                                      chip == '3xc' ~ 'TC',
                                      chip == 'manager' ~ 'AM',
                                      T ~'')) %>%
        mutate(GW_Chip = paste(event,"\n",chip_short)) %>%
        mutate(GW_Chip = str_replace(GW_Chip, " ",""))
      
      
      Team_Summary <- temp_df %>%
        #mutate(line_color = lead(overall_rank)) %>%
        mutate(line_color = lag(overall_rank)) %>%
        mutate(total_short = case_when(overall_rank >= 1000000 ~ paste0(round(overall_rank / 1000000, digits = 1), "M"),
                                       overall_rank < 1000 ~ as.character(overall_rank),
                                       T ~ paste0(round(overall_rank / 1000, digits = 0),"K"))) %>%
        mutate(movement = case_when(#line_color > overall_rank ~ "Red Arrow",
          #line_color < overall_rank ~ "Green Arrow",
          overall_rank > line_color ~ "Red Arrow",
          overall_rank < line_color~ "Green Arrow",
          T ~ "No Arrow"))
      
      small_df2 <- Team_Summary %>%
        filter(!is.na(chip))
      
      
      Names1 <- Team_Summary$GW_Chip
      Breaks1 <- Team_Summary$event
      
      p <- rank_plot(Team_Summary, small_df2, Names1, Breaks1, 
                     Team_Name = New_Teamname())
      
      
      p
      
      
    })
    
    
    
    output$down_rank_plot <- downloadHandler(
      filename = function(){paste("FPL.Rank",  New_Teamname(),".png",sep='')},
      
      content = function(file){
    
        FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$FPL_ID, '/history/'))
        
        
        jsonRespText<-content(FPL_Team, as="text") 
        JSON_Output <- fromJSON(jsonRespText)
        
        # Fix: Check if chips exists and handle empty/NULL case
        if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
          Chips <- JSON_Output$chips %>%
            select(-time)
        } else {
          # Create empty data frame with expected structure if no chips
          Chips <- data.frame(
            event = integer(0),
            name = character(0),
            stringsAsFactors = FALSE
          )
        }
        
        temp_df <- team_picks_parse(input$FPL_ID, max(Players_History$round)) %>%
          left_join(Chips, by = c('event')) %>%
          rename(chip = name) %>%
          mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                        chip == 'freehit' ~ 'FH',
                                        chip == 'bboost' ~ 'BB',
                                        chip == '3xc' ~ 'TC',
                                        chip == 'manager' ~ 'AM',
                                        T ~'')) %>%
          mutate(GW_Chip = paste(event,"\n",chip_short)) %>%
          mutate(GW_Chip = str_replace(GW_Chip, " ",""))
        
        
        Team_Summary <- temp_df %>%
          #mutate(line_color = lead(overall_rank)) %>%
          mutate(line_color = lag(overall_rank)) %>%
          mutate(total_short = case_when(overall_rank >= 1000000 ~ paste0(round(overall_rank / 1000000, digits = 1), "M"),
                                         overall_rank < 1000 ~ as.character(overall_rank),
                                         T ~ paste0(round(overall_rank / 1000, digits = 0),"K"))) %>%
          mutate(movement = case_when(#line_color > overall_rank ~ "Red Arrow",
            #line_color < overall_rank ~ "Green Arrow",
            overall_rank > line_color ~ "Red Arrow",
            overall_rank < line_color~ "Green Arrow",
            T ~ "No Arrow"))
        
        small_df2 <- Team_Summary %>%
          filter(!is.na(chip))
        
        
        Names1 <- Team_Summary$GW_Chip
        Breaks1 <- Team_Summary$event
        
        p <- rank_down_plot(Team_Summary, small_df2, Names1, Breaks1, 
                       Team_Name = New_Teamname())

        ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
    
      })
    
    output$transfer_plot <- renderPlot({
      
      transfer_input <- "Yes"
      
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$FPL_ID, '/history/'))
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      
      # Fix: Check if chips exists and handle empty/NULL case
      if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
        Chips <- JSON_Output$chips %>%
          select(-time)
      } else {
        # Create empty data frame with expected structure if no chips
        Chips <- data.frame(
          event = integer(0),
          name = character(0),
          stringsAsFactors = FALSE
        )
      }
      
      
      tmp_df2 <- table2_parse(Players_Gameweek,input$FPL_ID)
      
      tmp_vec <- seq(1, max(Players_Gameweek$round),1)
      
      tmp_df <- data.frame(Gameweek = setdiff(tmp_vec, tmp_df2$Gameweek), 
                           chip = NA) %>%
        left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name'))
      
      transfers <- bind_rows(tmp_df2, tmp_df)
      
      calculation1 <- transfer_input
      
      
      
      transfer_df <- transfers %>%
        #{if (input$transfer_input == "No") table2_parse(Players_Gameweek,input$team_id3) %>%
        #filter(., is.na(chip))
        #else table2_parse(Players_Gameweek,input$team_id3)} %>%
        {if(calculation1 == "No") filter(., !chip %in% c('wildcard','freehit')) else mutate(., chip = chip)} %>%
        mutate(movement = case_when(`Points Difference After Hits` <= -1  ~ "Poor",
                                    `Points Difference After Hits` >= 5 ~ "Good",
                                    T ~ "Average")) %>%
        left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name')) %>%
        mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                      chip == 'freehit' ~ 'FH',
                                      chip == 'bboost' ~ 'BB',
                                      chip == '3xc' ~ 'TC',
                                      chip == 'manager' ~ 'AM',
                                      T ~'')) %>%
        mutate(GW_Chip = paste(Gameweek,"\n",chip_short)) %>%
        mutate(GW_Chip = str_replace(GW_Chip, " ",""))
      
      
      small_df2 <- transfer_df %>%
        filter(!is.na(chip))
      
      
      FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$FPL_ID,'/'))
      
      jsonRespText<-content(FPL_Team, as="text") 
      JSON_Output <- fromJSON(jsonRespText)
      Team_Name <- JSON_Output$name %>% data.frame() %>% pull(.)
      Manager <- paste0(JSON_Output$player_first_name, " ", JSON_Output$player_last_name) %>% data.frame() %>% pull(.)
      
      Names1 <- transfer_df$GW_Chip
      Breaks1 <- transfer_df$Gameweek
      
      p <- transfer_plot(transfer_df, small_df2, Names1, Breaks1, 
                     Team_Name = New_Teamname())
      
      
      p
      
      
    })
    
    
    output$down_transfer_plot <- downloadHandler(
      filename = function(){paste("FPL.Transfers",  New_Teamname(),".png",sep='')},
      
      content = function(file){
        
        transfer_input <- "Yes"
        
        
        FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$FPL_ID, '/history/'))
        
        jsonRespText<-content(FPL_Team, as="text") 
        JSON_Output <- fromJSON(jsonRespText)
        
        # Fix: Check if chips exists and handle empty/NULL case
        if(length(JSON_Output$chips) > 0 && is.data.frame(JSON_Output$chips)) {
          Chips <- JSON_Output$chips %>%
            select(-time)
        } else {
          # Create empty data frame with expected structure if no chips
          Chips <- data.frame(
            event = integer(0),
            name = character(0),
            stringsAsFactors = FALSE
          )
        }
        
        
        tmp_df2 <- table2_parse(Players_Gameweek,input$FPL_ID)
        
        tmp_vec <- seq(1, max(Players_Gameweek$round),1)
        
        tmp_df <- data.frame(Gameweek = setdiff(tmp_vec, tmp_df2$Gameweek), 
                             chip = NA) %>%
          left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name'))
        
        transfers <- bind_rows(tmp_df2, tmp_df)
        
        calculation1 <- transfer_input
        
        
        
        transfer_df <- transfers %>%
          #{if (input$transfer_input == "No") table2_parse(Players_Gameweek,input$team_id3) %>%
          #filter(., is.na(chip))
          #else table2_parse(Players_Gameweek,input$team_id3)} %>%
          {if(calculation1 == "No") filter(., !chip %in% c('wildcard','freehit')) else mutate(., chip = chip)} %>%
          mutate(movement = case_when(`Points Difference After Hits` <= -1  ~ "Poor",
                                      `Points Difference After Hits` >= 5 ~ "Good",
                                      T ~ "Average")) %>%
          left_join(Chips, by = c('Gameweek' = 'event', 'chip' = 'name')) %>%
          mutate(chip_short = case_when(chip == 'wildcard' ~ 'WC',
                                        chip == 'freehit' ~ 'FH',
                                        chip == 'bboost' ~ 'BB',
                                        chip == '3xc' ~ 'TC',
                                        chip == 'manager' ~ 'AM',
                                        T ~'')) %>%
          mutate(GW_Chip = paste(Gameweek,"\n",chip_short)) %>%
          mutate(GW_Chip = str_replace(GW_Chip, " ",""))
        
        
        small_df2 <- transfer_df %>%
          filter(!is.na(chip))
        
        
        FPL_Team <- GET(paste0('https://fantasy.premierleague.com/api/entry/', input$FPL_ID,'/'))
        
        jsonRespText<-content(FPL_Team, as="text") 
        JSON_Output <- fromJSON(jsonRespText)
        Team_Name <- JSON_Output$name %>% data.frame() %>% pull(.)
        Manager <- paste0(JSON_Output$player_first_name, " ", JSON_Output$player_last_name) %>% data.frame() %>% pull(.)
        
        Names1 <- transfer_df$GW_Chip
        Breaks1 <- transfer_df$Gameweek
        
        p <- transfer_down_plot(transfer_df, small_df2, Names1, Breaks1, 
                           Team_Name = New_Teamname()) 
        
        
        
      ggsave(file, plot = p, dpi = 600, height = 6, width = 10)
        
      })
    
    
    
  }
  
  
  
  
  
)