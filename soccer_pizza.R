library("tidyverse")
library("dplyr")
library("tidyr")
library("ggplot2")
library("rvest")
library("janitor")
library("xml2")
library("worldfootballR")
library("ggtext")
library("glue")
library("eeptools")
library("stringr")
library("stringi")

####

# Center-forward / Striker Template
#attacking <- [c(10, 47, 50, 18, 103, 115)]
#possession <- [c(33, 107, 113, 114, 119, 124, 259)]
#defending <- [c(89, 95, 135)]

# Attacking Midfielder / Winger Template
#attacking <- [c(10, 47, 49, 50, 103, 115)]
#possession <- [c(33, 48, 107, 113, 114, 119, 259)]
#defending <- [c(89, 95, 135)]

# Central/Defensive Midfielder Template
#attacking <- [c(10, 47, 49, 237)]
#possession <- [c(242, 247, 187, 170, 168, 177, 183)]
#defending <- [c(83, 89, 127, 132, 135)]

# Fullback & wingback template
#attacking <- [c(10, 47, 50, 51)]
#possession <- [c(242, 247, 254, 252, 168, 394)]
#defending <- [c(218, 224, 267, 229, 231, 270)]

# Centerback Template
#attacking <- [c(10, 47)]
#possession <- [c(242, 247, 248, 187, 170, 168, 177, 179)]
#defending <- [c(218, 224, 227, 229, 231, 270)]

# Goalkeeper Template


## Pizza plot 

# Text wrap function
text_wrap <- function(x) {
  wrapped_text <- stri_wrap(x, width = 10, whitespace_only = TRUE, simplify = FALSE)
  final_text <- vapply(wrapped_text, stri_c, collapse = "\n", character(1))
  
  return(final_text) 
}

plot_pizza <- function(data, type = "", Versus, 
                       color_possession = "#5DADE2", color_attack = "#48C9B0", 
                       color_defense = "#EC7063", 
                       player_1, player_2, 
                       color_compare = "#fec44f", 
                       season = "Last 365 Days Men's Big 5 Leagues, UCL, UEL", 
                       season_player_1 = "Last 365 Days Men's Big 5 Leagues, UCL, UEL", 
                       season_player_2 = "Last 365 Days Men's Big 5 Leagues, UCL, UEL", 
                       theme = "") {
  
  if (theme == "dark" || theme == "") {
    fill_b <- "#0d1117"
    color_b <- "#0d1117"
    colorText <- "white"
    gridline <- "565656"
    colorLine <- "white"
  } else if (theme == "black") {
    fill_b <- "black"
    color_b <- "black"
    colorText <- "white"
    gridline <- "565656"
    colorLine <- "white"
  } else if (theme == "white") {
    fill_b <- "white"
    color_b <- "white"
    colorText <- "black"
    gridline <- "565656"
    colorLine <- "black"
  }
  
  if (type == "single" || type == "") { ## SINGLE PLOT ----
    
    data <- data %>%
      filter(scouting_period == season)
    
    # Attacking Midfielder / Wingers
    if (Versus == "Att Mid / Wingers") {
      
      data_selected <- data %>% 
        mutate(stat = case_when(
          Statistic == "npxG: Non-Penalty xG" |
            Statistic == "xA: Expected Assists" |
            Statistic == "Passes into Penalty Area" |
            Statistic == "Crosses into Penalty Area" |
            Statistic == "Touches (Att Pen)" |
            Statistic == "Carries into Penalty Area" ~ "Attacking",
            Statistic == "Pass Completion %" |
            Statistic == "Key Passes" |
            Statistic == "Successful Take-On %" |
            Statistic == "Progressive Carries" |
            Statistic == "Carries into Final Third" |
            Statistic == "Progressive Passes Rec" |
            Statistic == "Fouls Drawn" ~ "Possession",
            Statistic == "% of Dribblers Tackled" |
            Statistic == "Tkl+Int" |
            Statistic == "% of Aerials Won" ~ "Defending",
          TRUE ~ NA_character_
        )) %>%
        drop_na(stat) %>%
        distinct(Statistic, .keep_all = TRUE)
      
    }
    # Striker/Center-forward template
    else if (Versus == "Forwards") {
      
      data_selected <- data %>% 
        mutate(stat = case_when(
          Statistic == "npxG: Non-Penalty xG" |
            Statistic == "xA: Expected Assists" |
            Statistic == "npxG + xAG" |
            Statistic == "Passes into Penalty Area" |
            Statistic == "Shots on Target" |
            Statistic == "Touches (Att Pen)" |
            Statistic == "Carries into Penalty Area" ~ "Attacking",
            Statistic == "Pass Completion %" |
            Statistic == "Successful Take-On %" |
            Statistic == "Progressive Carries" |
            Statistic == "Carries into Final Third" |
            Statistic == "Progressive Passes Rec" |
            Statistic == "Fouls Drawn" ~ "Possession",
            Statistic == "% of Dribblers Tackled" |
            Statistic == "Tkl+Int" |
            Statistic == "% of Aerials Won" ~ "Defending",
          TRUE ~ NA_character_
        )) %>%
        drop_na(stat) %>%
        distinct(Statistic, .keep_all = TRUE)
      
    }
    # Central/Defensive Midfielders Template
    else if (Versus == "Midfielders") {
      data_selected <- data %>% 
        mutate(stat = case_when(
          Statistic == "npxG: Non-Penalty xG" |
            Statistic == "xA: Expected Assists" |
            Statistic == "Passes into Penalty Area" ~ "Attacking",
            Statistic == "Pass Completion %" |
            Statistic == "Pass Completion % (Long)" |
            Statistic == "Progressive Passes" |
            Statistic == "Progressive Passing Distance" |
            Statistic == "Progressive Passes Rec" |
            Statistic == "Successful Take-On %" |
            Statistic == "Progressive Carries" |
            Statistic == "Progressive Carrying Distance" |
            Statistic == "Switches" ~ "Possession",
            Statistic == "Tackles" |
            Statistic == "% of Dribblers Tackled" |
            Statistic == "Interceptions" |
            Statistic == "Ball Recoveries" ~ "Defending",
          TRUE ~ NA_character_
        )) %>%
        drop_na(stat) %>%
        distinct(Statistic, .keep_all = TRUE)
      
    }
    # Fullback / Wingback template
    else if (Versus == "Fullbacks") {
      data_selected <- data %>% 
        mutate(stat = case_when(
          Statistic == "npxG: Non-Penalty xG" |
            Statistic == "xA: Expected Assists" |
            Statistic == "Passes into Penalty Area" |
            Statistic == "Crosses into Penalty Area" ~ "Attacking",
            Statistic == "Pass Completion %" |
            Statistic == "Progressive Passes Rec" |
            Statistic == "Successful Take-On %" |
            Statistic == "Dispossessed" |
            Statistic == "Progressive Carrying Distance" |
            Statistic == "Fouls Drawn" ~ "Possession",
            Statistic == "Tackles" |
            Statistic == "% of Dribblers Tackled" |
            Statistic == "Interceptions" |
            Statistic == "Clearances" |
            Statistic == "Ball Recoveries" |
            Statistic == "% of Aerials Won" ~ "Defending",
          TRUE ~ NA_character_
        )) %>%
        drop_na(stat) %>%
        distinct(Statistic, .keep_all = TRUE)
      
    }
    # Centerback template
    else if (Versus == "Center Backs") {
      data_selected <- data %>% 
        mutate(stat = case_when(
          Statistic == "npxG: Non-Penalty xG" |
            Statistic == "xA: Expected Assists" ~ "Attacking",
            Statistic == "Pass Completion %" |
            Statistic == "Pass Completion % (Long)" |
            Statistic == "Progressive Passes" |
            Statistic == "Progressive Passing Distance" |
            Statistic == "Progressive Carries" |
            Statistic == "Progressive Carrying Distance" |
            Statistic == "Switches" ~ "Possession",
            Statistic == "Tackles" |
            Statistic == "% of Dribblers Tackled" |
            Statistic == "Interceptions" |
            Statistic == "Clearances" |
            Statistic == "Ball Recoveries"|
            Statistic == "% of Aerials Won" |
            Statistic == "Fouls Committed" ~ "Defending",
          TRUE ~ NA_character_
        )) %>%
        drop_na(stat) %>%
        distinct(Statistic, .keep_all = TRUE)
      
    }
    # Goalkeeper template
    else if (Versus == "Goalkeepers") {
      data_selected <- data %>% 
        mutate(stat = case_when(
          Statistic == "Goals Against" |
            Statistic == "PSxG/SoT" | 
            Statistic == "Save Percentage" | 
            Statistic == "PSxG-GA" |
            Statistic == "Crosses Stopped %" |
            Statistic == "Corner Kick Goals Against" |
            Statistic == "Avg. Distance of Def. Actions" |  
            Statistic == "Def. Actions Outside Pen. Area" ~ "Defending",
            Statistic == "Touches" |
            Statistic == "Passes Attempted (GK)" |
            Statistic == "Throws Attempted" |
            Statistic == "Average Pass Length" |
            Statistic == "Passes Attempted (Launched)" |
            Statistic == "Pass Completion Percentage (Launched)" |
            Statistic == "Launch% (Goal Kicks)" | 
            Statistic == "Avg. Length of Goal Kicks" ~ "Possession",
          TRUE ~ NA_character_
        )) %>%
        drop_na(stat) %>%
        distinct(Statistic, .keep_all = TRUE)
      
    }
    
    player_name <- unique(data$Player)
    title <- paste(player_name,"|",single_player_tm$current_club,"|","Age:",single_player_age,"|",single_player_value)
    # min <- unique(data$BasedOnMinutes)
    sub <- unique(data$Versus)
    sub1 <- unique(data$scouting_period)
    subtitle <- paste("Compared to", sub, "|", sub1, "|", "Stats per 90 minutes")
    caption <- "Data from Stats Perform by Opta via FBref.\nPlot code credits to ggshakeR R package and @RobinWilhelmus."
    
    x <- c(data_selected$Statistic, data_selected$stat)
    
    data_selected <- data_selected %>%
      # arrange(desc(stat), desc(Percentile)) %>%
      arrange(desc(stat)) %>%
      mutate(Statistic = factor(Statistic, levels = Statistic))
    
    ggplot(data_selected, aes(Statistic, Percentile)) +
      geom_bar(aes(y = 100, fill = stat), stat = "identity", width = 1, color = fill_b,
               alpha = 0.1, show.legend = FALSE) +
      geom_bar(stat = "identity", width = 1, aes(fill = stat), color = fill_b, alpha = 1) +
      coord_polar(clip = "off") +
      geom_hline(yintercept = 25, color = colorLine, linetype = "dashed", alpha = 0.8) +
      geom_hline(yintercept = 50, color = colorLine, linetype = "dashed", alpha = 0.8) +
      geom_hline(yintercept = 75, color = colorLine, linetype = "dashed", alpha = 0.8) +
      scale_fill_manual(values = c("Possession" = color_possession,
                                   "Attacking" = color_attack,
                                   "Defending" = color_defense)) +
      geom_label(aes(y = 90, label = Per90, fill = stat), size = 3, color = fill_b, show.legend = FALSE) +
      scale_y_continuous(limits = c(-20, 100)) +
      labs(fill = "",
           caption = caption,
           title = title,
           subtitle = str_wrap(subtitle)) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = fill_b, color = color_b),
            panel.background = element_rect(fill = fill_b, color = color_b),
            legend.position = "bottom",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 8, color = colorText),
            text = element_text(color = colorText, size = 10),
            plot.title = element_text(hjust = 0.5, size = 10, color = colorText, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 10, color = colorText),
            plot.caption = element_text(hjust = 0.5, size = 8, color = colorText),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_x_discrete(labels = function(x) text_wrap(x = x))
    
  } else if (type == "comparison") { ## COMPARISON PLOT ----
    
    data_comp$Player <- stri_trans_general(str = data$Player, id = "Latin-ASCII")
    
    # Attacking Midfielder / Wingers
    if (Versus == "Att Mid / Wingers") {
      
      data_comp <- data_comp %>% 
        mutate(stat = case_when(
          Statistic == "npxG: Non-Penalty xG" |
            Statistic == "xA: Expected Assists" |
            Statistic == "Passes into Penalty Area" |
            Statistic == "Crosses into Penalty Area" |
            Statistic == "Touches (Att Pen)" |
            Statistic == "Carries into Penalty Area" ~ "Attacking",
            Statistic == "Pass Completion %" |
            Statistic == "Key Passes" |
            Statistic == "Successful Take-On %" |
            Statistic == "Progressive Carries" |
            Statistic == "Progressive Passes Rec" |
            Statistic == "Fouls Drawn" ~ "Possession",
            Statistic == "% of Dribblers Tackled" |
            Statistic == "Tkl+Int" |
            Statistic == "% of Aerials Won" ~ "Defending",
          TRUE ~ NA_character_
        )) %>%
        drop_na(stat)
      
      data1 <- data_comp %>%
        filter(Player == player_1) %>%
        filter(scouting_period == season_player_1) %>%
        distinct(Statistic, .keep_all = TRUE)
      data2 <- data_comp %>%
        filter(Player == player_2) %>%
        filter(scouting_period == season_player_2) %>%
        distinct(Statistic, .keep_all = TRUE)
      
    }
    # Striker/Center-forward template
    else if (Versus == "Forwards") {
      
      data_comp <- data_comp %>% 
        mutate(stat = case_when(
          Statistic == "npxG: Non-Penalty xG" |
            Statistic == "xA: Expected Assists" |
            Statistic == "Passes into Penalty Area" |
            Statistic == "Shots on Target" |
            Statistic == "Touches (Att Pen)" |
            Statistic == "Carries into Penalty Area" ~ "Attacking",
            Statistic == "Pass Completion %" |
            Statistic == "Successful Take-On %" |
            Statistic == "Progressive Carries" |
            Statistic == "Carries into Final Third" |
            Statistic == "Progressive Passes Rec" |
            Statistic == "Fouls Drawn" ~ "Possession",
            Statistic == "% of Dribblers Tackled" |
            Statistic == "Tkl+Int" |
            Statistic == "% of Aerials Won" ~ "Defending",
          TRUE ~ NA_character_
        )) %>%
        drop_na(stat)
      
      data1 <- data_comp %>%
        filter(Player == player_1) %>%
        filter(scouting_period == season_player_1) %>%
        distinct(Statistic, .keep_all = TRUE)
      data2 <- data_comp %>%
        filter(Player == player_2) %>%
        filter(scouting_period == season_player_2) %>%
        distinct(Statistic, .keep_all = TRUE)
      
    }
    # Central/Defensive Midfielders Template
    else if (Versus == "Midfielders") {
      data_comp <- data_comp %>% 
        mutate(stat = case_when(
          Statistic == "npxG: Non-Penalty xG" |
            Statistic == "xA: Expected Assists" ~ "Attacking",
            Statistic == "Pass Completion %" |
            Statistic == "Pass Completion % (Long)" |
            Statistic == "Progressive Passes" |
            Statistic == "Progressive Passing Distance" |
            Statistic == "Progressive Passes Rec" |
            Statistic == "Successful Take-On %" |
            Statistic == "Progressive Carries" |
            Statistic == "Progressive Carrying Distance" |
            Statistic == "Switches" ~ "Possession",
            Statistic == "Tackles" |
            Statistic == "% of Dribblers Tackled" |
            Statistic == "Interceptions" |
            Statistic == "Ball Recoveries" ~ "Defending",
          TRUE ~ NA_character_
        )) %>%
        drop_na(stat)
      
      data1 <- data_comp %>%
        filter(Player == player_1) %>%
        filter(scouting_period == season_player_1) %>%
        distinct(Statistic, .keep_all = TRUE)
      data2 <- data_comp %>%
        filter(Player == player_2) %>%
        filter(scouting_period == season_player_2) %>%
        distinct(Statistic, .keep_all = TRUE)
      
    }
    # Fullback / Wingback template
    else if (Versus == "Fullbacks") {
      data_comp <- data_comp %>% 
        mutate(stat = case_when(
          Statistic == "npxG: Non-Penalty xG" |
            Statistic == "xA: Expected Assists" |
            Statistic == "Passes into Penalty Area" |
            Statistic == "Crosses into Penalty Area" ~ "Attacking",
          Statistic == "Pass Completion %" |
            Statistic == "Progressive Passes Rec" |
            Statistic == "Successful Take-On %" |
            Statistic == "Progressive Carrying Distance" |
            Statistic == "Fouls Drawn" ~ "Possession",
          Statistic == "Tackles" |
            Statistic == "% of Dribblers Tackled" |
            Statistic == "Interceptions" |
          Statistic == "Clearances" |
          Statistic == "Ball Recoveries" |
          Statistic == "% of Aerials Won" ~ "Defending",
          TRUE ~ NA_character_
        )) %>%
        drop_na(stat)
      
      data1 <- data_comp %>%
        filter(Player == player_1) %>%
        filter(scouting_period == season_player_1) %>%
        distinct(Statistic, .keep_all = TRUE)
      data2 <- data_comp %>%
        filter(Player == player_2) %>%
        filter(scouting_period == season_player_2) %>%
        distinct(Statistic, .keep_all = TRUE)
      
    }
    # Centerback template
    else if (Versus == "Center Backs") {
      data_comp <- data_comp %>% 
        mutate(stat = case_when(
          Statistic == "npxG: Non-Penalty xG" |
            Statistic == "xA: Expected Assists" ~ "Attacking",
          Statistic == "Pass Completion %" |
            Statistic == "Pass Completion % (Long)" |
            Statistic == "Progressive Passes" |
          Statistic == "Progressive Passing Distance" |
            Statistic == "Progressive Carries" |
            Statistic == "Progressive Carrying Distance" |
            Statistic == "Switches" ~ "Possession",
          Statistic == "Tackles" |
            Statistic == "% of Dribblers Tackled" |
            Statistic == "Interceptions" |
            Statistic == "Clearances" |
            Statistic == "Ball Recoveries"|
            Statistic == "% of Aerials Won" | 
          Statistic == "Fouls Committed" ~ "Defending",
          TRUE ~ NA_character_
        )) %>%
        drop_na(stat)
      
      data1 <- data_comp %>%
        filter(Player == player_1) %>%
        filter(scouting_period == season_player_1) %>%
        distinct(Statistic, .keep_all = TRUE)
      data2 <- data_comp %>%
        filter(Player == player_2) %>%
        filter(scouting_period == season_player_2) %>%
        distinct(Statistic, .keep_all = TRUE)
      
    }
    # Goalkeeper template
    else if (Versus == "Goalkeepers") {
      data_comp <- data_comp %>% 
        mutate(stat = case_when(
          Statistic == "Goals Against" |
            Statistic == "PSxG/SoT" |
          Statistic == "Save Percentage" | 
            Statistic == "PSxG-GA" |
            Statistic == "Crosses Stopped %" |
            Statistic == "Corner Kick Goals Against" |
          Statistic == "Avg. Distance of Def. Actions" |  
            Statistic == "Def. Actions Outside Pen. Area" ~ "Defending",
          Statistic == "Touches" |
            Statistic == "Passes Attempted (GK)" |
            Statistic == "Throws Attempted" |
            Statistic == "Average Pass Length" |
            Statistic == "Passes Attempted (Launched)" |
            Statistic == "Pass Completion Percentage (Launched)" |
            Statistic == "Launch% (Goal Kicks)" |
          Statistic == "Avg. Length of Goal Kicks" ~ "Possession",
          TRUE ~ NA_character_
        )) %>%
        drop_na(stat)
      
      data1 <- data_comp %>%
        filter(Player == player_1) %>%
        filter(scouting_period == season_player_1) %>%
        distinct(Statistic, .keep_all = TRUE)
      data2 <- data_comp %>%
        filter(Player == player_2) %>%
        filter(scouting_period == season_player_2) %>%
        distinct(Statistic, .keep_all = TRUE)
      
    }
    
    data2 <- data2 %>%
      rename(player = Player,
             per90 = Per90,
             percentile = Percentile)
    
    player_name1 <- unique(data1$Player)
    player_name2 <- unique(data2$Player)
    #min1 <- unique(data1$BasedOnMinutes)
    #min2 <- unique(data2$BasedOnMinutes)
    sub <- unique(data1$Versus)
    lg1 <- unique(data1$scouting_period)
    #lg2 <- unique(data2$scouting_period)
    title <- paste(player_name1,"|", data1_tm$current_club,"|", "Age:", data1_age,"|","Transfermarkt Valuation: ",data1_value)
    subtitle <- paste(data2_tm$player_name,"|", data2_tm$current_club,"|", "Age:", data2_age,"|","Transfermarkt Valuation: ",data2_value)
    caption <- paste("Compared to", sub,"|", lg1,"|","Stats per 90 minutes",".\nPlot code credits to ggshakeR R package and @RobinWilhelmus.")
    
    x <- data1$Statistic
    
    data1 <- data1 %>%
      # arrange(desc(stat), desc(Percentile)) %>%
      arrange(desc(stat)) %>%
      mutate(Statistic = factor(Statistic, levels = Statistic))
    
    data2 <- data2 %>%
      # arrange(desc(stat), desc(percentile)) %>%
      arrange(desc(stat)) %>%
      mutate(Statistic = factor(Statistic, levels = Statistic))
    
    ggplot(data1, aes(x = Statistic, y = Percentile)) +
      geom_bar(aes(y = 100), fill = fill_b, stat = "identity", width = 1, color = gridline,
               alpha = 0.5, show.legend = FALSE) +
      geom_bar(data = data1, aes(y = Percentile, fill = color_compare), color = color_compare, stat = "identity", width = 1, alpha = 1) +
      scale_fill_manual(values = color_compare) +
      geom_bar(data = data2, aes(y = percentile, fill = NA), stat = "identity", width = 1, alpha = 0, color = colorLine, size = 3) +
      coord_polar(clip = "off") +
      geom_hline(yintercept = 25, color = colorLine, linetype = "dashed", alpha = 0.7) +
      geom_hline(yintercept = 50, color = colorLine, linetype = "dashed", alpha = 0.7) +
      geom_hline(yintercept = 75, color = colorLine, linetype = "dashed", alpha = 0.7) +
      scale_y_continuous(limits = c(-20, 100)) +
      labs(caption = caption,
           title = title,
           subtitle = str_wrap(subtitle)) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = fill_b, color = color_b),
            panel.background = element_rect(fill = fill_b, color = color_b),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 10, color = colorText),
            text = element_text(color = colorText, size = 10),
            plot.title = element_text(hjust = 0.5, size = 10, color = color_compare, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 10, color = colorLine, face = "bold"),
            plot.caption = element_text(hjust = 0.5, size = 8, color = colorText),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
       scale_x_discrete(labels = function(x) text_wrap(x = x))
    #  title = glue('{player_df$Player} | {player_tm$current_club}') ,
    #        subtitle = glue('Attacking Midfielder | Age: {player_tm$Age} | Stats per 90 minutes | Last 365 Days \n Transfermarkt.com valuation: €{player_val_mill} million | Contract expires: {player_tm$contract_expires}') , x = NULL, y = NULL,sep="\n",
    #        caption = "Source: Opta via Fbref.com")
  }
}


# Single Player
fb_league_urls("ENG", gender = "M", season_end_year = "2024", tier = "1st")
fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
fb_player_urls("https://fbref.com/en/squads/361ca564/Tottenham-Hotspur-Stats", time_pause = 3)

# single_player <- fb_player_scouting_report("https://fbref.com/en/players/4d5a9185/Viktor-Gyokeres", pos_versus = "primary")
# single_player <- fb_player_scouting_report("https://fbref.com/en/players/df04eb4b/Crysencio-Summerville", pos_versus = "primary")
# single_player <- fb_player_scouting_report("https://fbref.com/en/players/37337aea/Santiago-Gimenez", pos_versus = "primary")
# single_player <- fb_player_scouting_report("https://fbref.com/en/players/e77dc3b2/Dominic-Solanke", pos_versus = "primary")
single_player <- fb_player_scouting_report("https://fbref.com/en/players/4a1a9578/Luis-Diaz", pos_versus = "primary")

#Get League TM urls


#Get Team TM urls
tm_league_team_urls(country_name = "Portugal", start_year = "2023")
# tm_league_team_urls(country_name = "England", start_year = "2023")



#Accessing the player fbref urls.
# tm_team_player_urls(team_url = "https://www.transfermarkt.com/sporting-lissabon/startseite/verein/336/saison_id/2023")
#tm_team_player_urls(team_url = "https://www.transfermarkt.com/crysencio-summerville/profil/spieler/474701")


#Creating a dataframe from player's transfermarkt url.
single_player_tm <- tm_player_bio("https://www.transfermarkt.com/luis-diaz/profil/spieler/480692")

#Converting market value into millions of EUR.
single_player_value <- paste0('€',format(round(single_player_tm$player_valuation/1000000, digits = 2),nsmall = 2),' million')

#Calculate age
single_player_dob <- as.Date(single_player_tm$date_of_birth)
date_today <- Sys.Date()

# player_tm <- player_tm |>
#   mutate(Age = age_calc(single_player_dob,date_today, units = "years"))

single_player_age <- floor(age_calc(single_player_dob,date_today, units = "years"))

# player_tm$Age <- floor(player_tm$Age)

plot_pizza(single_player, type="single", Versus="Att Mid / Wingers") 
      
#### Player comparison

## Player 1
fb_league_urls("ENG", gender = "M", season_end_year = "2024", tier = "1st")
fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
fb_player_urls("https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats", time_pause = 3)

data1 <- fb_player_scouting_report("https://fbref.com/en/players/6434f10d/Rodri", pos_versus = "primary")

#Get Team TM urls
tm_league_team_urls(country_name = "England", start_year = "2023")

#Accessing the player fbref urls.
tm_team_player_urls(team_url = "https://www.transfermarkt.com/manchester-city/startseite/verein/281/saison_id/2023")

#Creating a dataframe from player's transfermarkt url.
data1_tm <- tm_player_bio("https://www.transfermarkt.com/rodri/profil/spieler/357565")

#Converting market value into millions of EUR.
data1_value <- paste0('€',format(round(data1_tm$player_valuation/1000000, digits = 1),nsmall = 2),' million')

#Calculate age
data1_dob <- as.Date(data1_tm$date_of_birth)
date_today <- Sys.Date()

# player_tm <- player_tm |>
#   mutate(Age = age_calc(single_player_dob,date_today, units = "years"))

data1_age <- floor(age_calc(data1_dob,date_today, units = "years"))

## Player 2
fb_league_urls("FRA", gender = "M", season_end_year = "2024", tier = "1st")
fb_teams_urls("https://fbref.com/en/comps/13/Ligue-1-Stats")
fb_player_urls("https://fbref.com/en/squads/132ebc33/Nice-Stats", time_pause = 3)

data2 <- fb_player_scouting_report("https://fbref.com/en/players/7d9397f8/Khephren-Thuram", pos_versus = "primary")

#Get Team TM urls
tm_league_team_urls(country_name = "France", start_year = "2023")

#Accessing the player fbref urls.
tm_team_player_urls(team_url = "https://www.transfermarkt.com/ogc-nizza/startseite/verein/417/saison_id/2023")

#Creating a dataframe from player's transfermarkt url.
data2_tm <- tm_player_bio("https://www.transfermarkt.com/khephren-thuram/profil/spieler/463618")

#Converting market value into millions of EUR.
data2_value <- paste0('€',format(round(data2_tm$player_valuation/1000000, digits = 1),nsmall = 2),' million')

#Calculate age
data2_dob <- as.Date(data2_tm$date_of_birth)
date_today <- Sys.Date()

# player_tm <- player_tm |>
#   mutate(Age = age_calc(single_player_dob,date_today, units = "years"))

data2_age <- floor(age_calc(data2_dob,date_today, units = "years"))

# Comparison plot

data_comp <- rbind(data1, data2)

comp_pizza1 <- plot_pizza(data=data_comp, type="comparison", Versus="Midfielders",
                          player_1 = "Rodri", player_2 = "Khephren Thuram",
                          season_player_1 = "Last 365 Days Men's Big 5 Leagues, UCL, UEL",
                          season_player_2 = "Last 365 Days Men's Big 5 Leagues, UCL, UEL",
                          color_compare = "#fec44f", theme = "black")

comp_pizza1