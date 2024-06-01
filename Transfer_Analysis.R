library(devtools)
library(tidyverse)
library(forcats)
library(worldfootballR)
library(glue)
library("eeptools")

#----- Get scouting report for the players primary position (first position listed in fbref): -----#
#tete_primary <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/c81d773d/Tosin-Adarabioyo", pos_versus = "primary")
#dplyr::glimpse(tete_primary)

tete <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/c81d773d/Tosin-Adarabioyo")


# TO GET THE LAST 365 DAYS REPORT:
messi_last_365 <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/d70ce98e/Lionel-Messi",
                                            pos_versus = "primary", 
                                            league_comp_name = "Last 365 Days Men's Big 5 Leagues, UCL, UEL")

# TO GET SCOUTING REPORT FOR MULTIPLE COMPS/LEAGUES:
messi_multiple <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/d70ce98e/Lionel-Messi",
                                            pos_versus = "primary", 
                                            league_comp_name = c("Last 365 Days Men's Big 5 Leagues, UCL, UEL", "2022 World Cup"))


#----- Get scouting report for the players secondary position (second position listed in fbref): -----#
messi_secondary <- fb_player_scouting_report(player_url = "https://fbref.com/en/players/d70ce98e/Lionel-Messi", pos_versus = "secondary")
dplyr::glimpse(messi_secondary)


#### Tottenham Transfer Analysis
tm_league_team_urls(country_name = "England", start_year = "2023")
tm_team_player_urls(team_url = "https://www.transfermarkt.com/tottenham-hotspur/startseite/verein/148/saison_id/2023")

## Potential Outgoings
#Sergio Reguilon - Left-back
tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-brentford/startseite/verein/1148/saison_id/2023")

reguilon_df <- tm_player_bio("https://www.transfermarkt.com/sergio-reguilon/profil/spieler/282429")

#Converting market value into millions of EUR.
reguilon_df <-  reguilon_df |>
  mutate(market_value = paste0('€',format(round(reguilon_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
reguilon_df$date_of_birth <- as.Date(reguilon_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

reguilon_df <- reguilon_df |>
   mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Pierre Emile Hojbjerg - Defensive Midfielder
# tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-brentford/startseite/verein/1148/saison_id/2023")

hojbjerg_df <- tm_player_bio("https://www.transfermarkt.com/pierre-emile-hojbjerg/profil/spieler/167799")

#Converting market value into millions of EUR.
hojbjerg_df <-  hojbjerg_df |>
  mutate(market_value = paste0('€',format(round(hojbjerg_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
hojbjerg_df$date_of_birth <- as.Date(hojbjerg_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

hojbjerg_df <- hojbjerg_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Giovani Lo Celso - Central/Attacking Midfielder
# tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-brentford/startseite/verein/1148/saison_id/2023")

locelso_df <- tm_player_bio("https://www.transfermarkt.com/giovani-lo-celso/profil/spieler/348795")

#Converting market value into millions of EUR.
locelso_df <-  locelso_df |>
  mutate(market_value = paste0('€',format(round(locelso_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
locelso_df$date_of_birth <- as.Date(locelso_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

locelso_df <- locelso_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Emerson Royal - Right-back
# tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-brentford/startseite/verein/1148/saison_id/2023")

royal_df <- tm_player_bio("https://www.transfermarkt.com/emerson-royal/profil/spieler/476344")

#Converting market value into millions of EUR.
royal_df <-  royal_df |>
  mutate(market_value = paste0('€',format(round(royal_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
royal_df$date_of_birth <- as.Date(royal_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

royal_df <- royal_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Tanguy Ndombele - Central Midfielder
# tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-brentford/startseite/verein/1148/saison_id/2023")

ndombele_df <- tm_player_bio("https://www.transfermarkt.com/tanguy-ndombele/profil/spieler/450936")

#Converting market value into millions of EUR.
ndombele_df <-  ndombele_df |>
  mutate(market_value = paste0('€',format(round(ndombele_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
ndombele_df$date_of_birth <- as.Date(ndombele_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

ndombele_df <- ndombele_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Joe Rodon - Center-Back
# tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-brentford/startseite/verein/1148/saison_id/2023")

rodon_df <- tm_player_bio("https://www.transfermarkt.com/joe-rodon/profil/spieler/297212")

#Converting market value into millions of EUR.
rodon_df <-  rodon_df |>
  mutate(market_value = paste0('€',format(round(rodon_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
rodon_df$date_of_birth <- as.Date(rodon_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

rodon_df <- rodon_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Fraser Forster - Goalkeeper
# tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-brentford/startseite/verein/1148/saison_id/2023")

forster_df <- tm_player_bio("https://www.transfermarkt.com/fraser-forster/profil/spieler/52570")

#Converting market value into millions of EUR.
forster_df <-  forster_df |>
  mutate(market_value = paste0('€',format(round(forster_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
forster_df$date_of_birth <- as.Date(forster_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

forster_df <- forster_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Bryan Gil - Right Winger
# tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-brentford/startseite/verein/1148/saison_id/2023")

gil_df <- tm_player_bio("https://www.transfermarkt.com/bryan-gil/profil/spieler/537382")

#Converting market value into millions of EUR.
gil_df <-  gil_df |>
  mutate(market_value = paste0('€',format(round(gil_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
gil_df$date_of_birth <- as.Date(gil_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

gil_df <- gil_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Yves Bissouma - Central/Defensive Midfielder
# tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-brentford/startseite/verein/1148/saison_id/2023")

bissouma_df <- tm_player_bio("https://www.transfermarkt.com/yves-bissouma/profil/spieler/410425")

#Converting market value into millions of EUR.
bissouma_df <-  bissouma_df |>
  mutate(market_value = paste0('€',format(round(bissouma_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
bissouma_df$date_of_birth <- as.Date(bissouma_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

bissouma_df <- bissouma_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Oliver Skipp - Defensive Midfielder
# tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-brentford/startseite/verein/1148/saison_id/2023")

skipp_df <- tm_player_bio("https://www.transfermarkt.com/oliver-skipp/profil/spieler/406638")

#Converting market value into millions of EUR.
skipp_df <-  skipp_df |>
  mutate(market_value = paste0('€',format(round(skipp_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
skipp_df$date_of_birth <- as.Date(skipp_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

skipp_df <- skipp_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Richarlison - Center-forward
# tm_team_player_urls(team_url = "https://www.transfermarkt.com/fc-brentford/startseite/verein/1148/saison_id/2023")

richarlison_df <- tm_player_bio("https://www.transfermarkt.com/richarlison/profil/spieler/378710")

#Converting market value into millions of EUR.
richarlison_df <-  richarlison_df |>
  mutate(market_value = paste0('€',format(round(richarlison_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
richarlison_df$date_of_birth <- as.Date(richarlison_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

richarlison_df <- richarlison_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Manor Solomon
solomon_df <- tm_player_bio("https://www.transfermarkt.com/manor-solomon/profil/spieler/396638")

#Converting market value into millions of EUR.
solomon_df <-  solomon_df |>
  mutate(market_value = paste0('€',format(round(solomon_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
solomon_df$date_of_birth <- as.Date(solomon_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

solomon_df <- solomon_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Ben Davies - Center Back/Left Back
davies_df <- tm_player_bio("https://www.transfermarkt.com/ben-davies/profil/spieler/192765")

#Converting market value into millions of EUR.
davies_df <-  davies_df |>
  mutate(market_value = paste0('€',format(round(davies_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
davies_df$date_of_birth <- as.Date(davies_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

davies_df <- davies_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Ryan Sessegnon
sessegnon_df <- tm_player_bio("https://www.transfermarkt.com/ryan-sessegnon/profil/spieler/392775")

#Converting market value into millions of EUR.
sessegnon_df <-  sessegnon_df |>
  mutate(market_value = paste0('€',format(round(sessegnon_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
sessegnon_df$date_of_birth <- as.Date(sessegnon_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

sessegnon_df <- sessegnon_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))

#Japhet Tanganga
tanganga_df <- tm_player_bio("https://www.transfermarkt.com/japhet-tanganga/profil/spieler/346478")

#Converting market value into millions of EUR.
tanganga_df <-  tanganga_df |>
  mutate(market_value = paste0('€',format(round(tanganga_df$player_valuation/1000000, digits = 2),nsmall = 2),' million'))

#Calculate age
tanganga_df$date_of_birth <- as.Date(tanganga_df$date_of_birth, "%Y-%m-%d")
date_today <- Sys.Date()

tanganga_df <- tanganga_df |>
  mutate(Age = age_calc(date_of_birth,date_today, units = "years"))




###Joining all dfs
tottenham_outgoings <- do.call("rbind", list(reguilon_df,hojbjerg_df,locelso_df,
                                             royal_df,ndombele_df,rodon_df,
                                             forster_df,gil_df,bissouma_df,
                                             skipp_df,richarlison_df,solomon_df,
                                             davies_df))
