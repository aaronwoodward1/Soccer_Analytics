library("tidyverse")
library("dplyr")
library("magrittr")
library("tidyr")
library("ggplot2")
library("plotly")
library("rvest")
library("janitor")
library("xml2")
library("worldfootballR")
library("ggtext")
library("glue")
library("eeptools")
library("ggshakeR")
library("ggrepel")

# single_player <- fb_player_scouting_report("https://fbref.com/en/players/c275947b/Pedro-Goncalves", pos_versus = "primary")
# 
# pizza <- plot_pizza(data = single_player, type = "single", template = "outfielder", 
#                     color_possession = "#41ab5d", color_attack = "#fec44f", color_defense = "#de2d26", 
#                     season = "Last 365 Days Men's Big 5 Leagues, UCL, UEL", theme = "dark")
# 
# pizza

# Timo Werner Plots
## Successful Take-ons %
poss_df <- fb_big5_advanced_season_stats(
  season_end_year = 2024,
  stat_type = "possession",
  team_or_player = "player"
)

#dplyr::glimpse(big5_2324_player_possession)


# df <- big5_2324_player_possession %>%
#   filter(big5_2324_player_possession, Pos=="MF,FW" | Pos=="FW")

#poss_df <- subset(poss_df, (Pos=="MF,FW" | Pos=="FW") & (Mins_Per_90 > 5.0))
poss_df <- subset(poss_df, Pos=="MF,FW" | Pos=="FW"| Pos=="FW,MF")

misc_df <- fb_big5_advanced_season_stats(
  season_end_year = 2024,
  stat_type = "misc",
  team_or_player = "player") 

misc_df <- subset(misc_df, Pos=="MF,FW" | Pos=="FW" )

standard_df <- fb_big5_advanced_season_stats(
  season_end_year = 2024,
  stat_type = "standard",
  team_or_player = "player")

standard_df <- subset(standard_df, Pos=="MF,FW" | Pos=="FW" | Pos=="FW,MF")

# df <- left_join(standard_df, poss_df, by=c("Player", "Squad"))
# df <- left_join(df, misc_df, by=c("Player", "Squad"))

merged_df <- merge(standard_df, poss_df)
# merged_df <- merge(df1, misc_df)

#filter by Minutes played
df <- subset(merged_df, Min_Playing >= 500)
df <- df |>
  dplyr::rename("xG_AG_p90" = "xG+xAG_Per")

# ggplot(df, aes(x= Succ_percent_Take , y= xG_AG_p90)) + geom_point(aes(color=factor("Comp")))
player_mapping <- player_dictionary_mapping()

focus_players <- c("Timo Werner", "Bukayo Saka", "Cole Palmer", "Jack Grealish", "Lautaro Martínez",
                   "Son Heung-min", "Luis Díaz", "Leroy Sané", "Ángel Correa","Junya Ito", "Phil Foden")

selected_players <- subset(df, Player %in% focus_players)


p <- ggplot(df,aes(x=Succ_percent_Take, y=xG_AG_p90)) + 
  geom_point(aes(color = Comp)) + 
  ggrepel::geom_text_repel(data = selected_players, aes(label = Player), color = "black",
                           size = 12/.pt, # font size 9 pt
                           point.padding = 0.1, 
                           box.padding = 0.6,
                           min.segment.length = 0,
                           max.overlaps = 1000,
                           seed = 7654 # For reproducibility reasons
  )

p