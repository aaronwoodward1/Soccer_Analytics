###WorldfootballR - Pizza Plots ###

#PackageNames <- c("tidyverse","tidyr","ggplot2","rvest","janitor")

#for(i in PackageNames){
#  if(!require(i, character.only = T)){
#    install.packages(i, dependencies = T)
#    require(i, character.only = T)
#  }
#}
#install.packages("glue")
###
library("tidyverse")
library("dplyr")
library("tidyr")
library("ggplot2")
library("rvest")
library("janitor")
library("xml2")
library(worldfootballR)
library("ggtext")
library("glue")

###Radial/Pizza Polar Plots 

#Attacking Midfielder / Winger template

#Transfermarkt data

#Accessing the player fbref urls.
tm_team_player_urls(team_url = "https://www.transfermarkt.com/paris-saint-germain/startseite/verein/583")

#Creating a dataframe from player's transfermarkt url.
player_tm <- tm_player_bio("https://www.transfermarkt.com/lionel-messi/profil/spieler/28003" )

#Converting market value into millions of EUR.
player_val_mill <- format(round(player_tm$player_valuation/1000000, digits = 2),nsmall = 2)

#Fbref/Statsbomb data
#Accessing the player fbref urls.
fb_player_urls("https://fbref.com/en/squads/e2d8892c/2022-2023/c13/Paris-Saint-Germain-Stats-Ligue-1") 

#Creating a dataframe from player's fbref url.
df <- fb_player_scouting_report("https://fbref.com/en/players/d70ce98e/scout/365_m1/Lionel-Messi-Scouting-Report", pos_versus = "primary") 
#df <- fb_player_season_stats("https://fbref.com/en/players/d70ce98e/Lionel-Messi",stat_type = "shooting")

#Selecting the relevant rows for stats per 90 in the last 360 days.
player_df <- df[c(9,24,10,14,73,47,29,43,52,53,125,131,119,115,97,107),] 
#In late 2022, FBref ended its partnership with StatsBomb, so the fb_player_scouting_report call may only pull Standard data

#Allocating the rows into categories
player_df$index <- 1:16
player_df <- player_df %>% 
  mutate(type = case_when(
    index %in% 1:5 ~ "Attacking",
    index %in% 6:10 ~ "Passing",
    index %in% 11:14 ~ "Possession",
    index %in% 15:16 ~ "Defending"
  ))
player_df$type <- factor(player_df$type, levels = c("Attacking","Passing", "Possession", "Defending"))

## ---- Rotate axis text in plot
# angle of axis text
temp <- (360/(length(player_df$index))/2)                             
#find the difference in angle between to labels and divide by two.
myAng <- seq(-temp, -360+temp, length.out = length(player_df$index))  
#get the angle for every label
ang<-ifelse(myAng < -90, myAng+180, myAng)                                    
#rotate label by 180 in some places for readability
ang<-ifelse(ang < -90, ang+180, ang)

#setting the color selection for the categories
color1 <- "#48C9B0"
color2 <- "#F7DC6F"
color3 <- "#5DADE2"
color4 <- "#EC7063"

#using ggplot:
ggplot(data = player_df, aes(x = reorder(Statistic, index), y = Percentile, label= Per90, fill = type)) +
  # add the bar/pizza slices that are colored
  geom_bar(data = player_df, width = 1,
           color = "white",
           stat = "identity") +
  # wrap bar chart as around polar center
  coord_polar() +    
  # add the background behind each bar (alpha at .5 for slight transparency so the bars standout)
  geom_bar(aes(y=100, fill=type), stat="identity", width=1, alpha=0.5) +
  # add & customize line that border whole pizza
  geom_hline(yintercept=25, colour="#BDC3C7",linetype ="longdash", alpha=1.0,size=0.5)+
  geom_hline(yintercept=50, colour="#BDC3C7",linetype ="longdash", alpha=1.0,size=0.5)+
  geom_hline(yintercept=75, colour="#BDC3C7",linetype ="longdash", alpha=1.0,size=0.5)+
  # add & customize lines between each pizza slice
  geom_vline(xintercept = seq(.5, 16, by = 1),
             color = "white",
             size = .5) +
  # add percentile labels (labels are fill by bar colors) - option 1
  #geom_label(aes(label=value, fill=type), color = "white", size=2.5, fontface="bold", family = "Comic Sans MS", show.legend = FALSE) +
  # add percentile labels (labels are choice of fill and color) - option 2
  geom_label(color = "gray20", fill = "white", size=2.5, fontface="bold", family = "Helvetica", show.legend = FALSE) +
  # manually set the colors of bars (3 here for each group of stats (scoring, possession, defending))
  scale_fill_manual(values=c(color1, color2, color3, color4)) +
  # theme manipulation to customize plot (play around with these!)
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", color="white"),
        legend.title = element_blank(),
        legend.text = element_text(colour = "gray20", family = "Helvetica", face = "bold"),
        legend.key.size = unit(.5, "cm"),
        legend.box.spacing = unit(0, "mm"),
        plot.title = element_text(hjust = .5, colour = "gray20", face = "bold", size = 16, family = "Helvetica"),
        plot.subtitle = element_text(hjust = .5, colour = "gray20", size = 10, family = "Helvetica"),
        #plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0.50, colour = "gray20", size = 8, family = "Helvetica"),
        plot.background = element_rect(fill = "white", color="white"),
        panel.background = element_rect(fill = "white", color="white"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold", size = 6.8, colour = "gray20"),
        axis.title = element_blank(),
        axis.text.x = element_text(face = "bold", size = 7, family = "Helvetica", angle = ang)) +
  # add title and subtitle
  labs(title = glue('{player_df$Player} | {player_tm$current_club}') ,
       subtitle = glue('Attacking Midfielder | Age: {player_tm$age} | Stats per 90 minutes | Last 365 Days \n Transfermarkt.com valuation: €{player_val_mill} million | Contract expires: {player_tm$contract_expires}') , x = NULL, y = NULL,sep="\n",
       caption = "Source: Statsbomb via Fbref.com") 

#\n = line break

##Radial/Pizza Polar Plots 
#Central & Defensive Midfielder template 
#Transfermarkt data
tm_team_player_urls(team_url = "https://www.transfermarkt.com/tottenham-hotspur/startseite/verein/148")
player_tm <- tm_player_bio("https://www.transfermarkt.com/rodrigo-bentancur/profil/spieler/354362")
player_val_mill <- format(round(player_tm$player_valuation/1000000, digits = 2),nsmall = 2)

fb_player_urls("https://fbref.com/en/squads/361ca564/Tottenham-Hotspur-Stats")

df <- fb_player_scouting_report("https://fbref.com/en/players/3b8674e6/Rodrigo-Bentancur", pos_versus = "primary")

player_df <- df[c(9,10,73,45,29,47,52,53,125,131,119,129,95,107,146,137),]

player_df$index <- 1:16
player_df <- player_df %>% 
  mutate(type = case_when(
    index %in% 1:4 ~ "Attacking",
    index %in% 5:8 ~ "Passing",
    index %in% 9:12 ~ "Possession",
    index %in% 13:16 ~ "Defense"
  ))
player_df$type <- factor(player_df$type, levels = c("Attacking","Passing", "Possession", "Defense"))

## ---- Rotate axis text in plot
# angle of axis text
temp <- (360/(length(player_df$index))/2)                             
#find the difference in angle between to labels and divide by two.
myAng <- seq(-temp, -360+temp, length.out = length(player_df$index))  
#get the angle for every label
ang<-ifelse(myAng < -90, myAng+180, myAng)                                    
#rotate label by 180 in some places for readability
ang<-ifelse(ang < -90, ang+180, ang)

color1 <- "#48C9B0"
color2 <- "#F7DC6F"
color3 <- "#5DADE2"
color4 <- "#EC7063"

ggplot(data = player_df, aes(x = reorder(Statistic, index), y = Percentile, label= Per90, fill = type)) +
  # add the bar/pizza slices that are colored
  geom_bar(data = player_df, width = 1,
           color = "white",
           stat = "identity") +
  # wrap bar chart as around polar center
  coord_polar() +    
  # add the background behind each bar (alpha at .5 for slight transparency so the bars standout)
  geom_bar(aes(y=100, fill=type), stat="identity", width=1, alpha=0.5) +
  # add & customize line that border whole pizza
  geom_hline(yintercept=25, colour="#BDC3C7",linetype ="longdash", alpha=1.0,size=0.5)+
  geom_hline(yintercept=50, colour="#BDC3C7",linetype ="longdash", alpha=1.0,size=0.5)+
  geom_hline(yintercept=75, colour="#BDC3C7",linetype ="longdash", alpha=1.0,size=0.5)+
  # add & customize lines between each pizza slice
  geom_vline(xintercept = seq(.5, 16, by = 1),
             color = "white",
             size = .5) +
  # add percentile labels (labels are fill by bar colors) - option 1
  #geom_label(aes(label=value, fill=type), color = "white", size=2.5, fontface="bold", family = "Comic Sans MS", show.legend = FALSE) +
  # add percentile labels (labels are choice of fill and color) - option 2
  geom_label(color = "gray20", fill = "white", size=2.5, fontface="bold", family = "Helvetica", show.legend = FALSE) +
  # manually set the colors of bars (3 here for each group of stats (scoring, possession, defending))
  scale_fill_manual(values=c(color1, color2, color3, color4)) +
  # theme manipulation to customize plot (play around with these!)
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", color="white"),
        legend.title = element_blank(),
        legend.text = element_text(colour = "gray20", family = "Helvetica", face = "bold"),
        legend.key.size = unit(.5, "cm"),
        legend.box.spacing = unit(0, "mm"),
        plot.title = element_text(hjust = .5, colour = "gray20", face = "bold", size = 16, family = "Helvetica"),
        plot.subtitle = element_text(hjust = .5, colour = "gray20", size = 10, family = "Helvetica"),
        #plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0.50, colour = "gray20", size = 8, family = "Helvetica"),
        plot.background = element_rect(fill = "white", color="white"),
        panel.background = element_rect(fill = "white", color="white"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold", size = 6.8, colour = "gray20"),
        axis.title = element_blank(),
        axis.text.x = element_text(face = "bold", size = 7, family = "Helvetica", angle = ang)) +
  # add title and subtitle
  labs(title = glue('{player_df$Player} | {player_tm$current_club}') ,
       subtitle = glue('Central Midfielder | Age: {player_tm$age} | Stats per 90 minutes | Last 365 Days \n Transfermarkt.com valuation: €{player_val_mill} million | Contract expires: {player_tm$contract_expires}'), x = NULL, y = NULL,sep="\n",
       caption = "Source: Statsbomb via Fbref.com") 

###END OF CODE###