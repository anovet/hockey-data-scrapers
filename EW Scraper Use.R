## Dependencies
library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
library(lubridate)
library(tidyverse) ## -- specifically: stringr, readr, tidyr, and dplyr

## Source scraper functions from GitHub
devtools::source_url("https://raw.githubusercontent.com/evolvingwild/evolving-hockey/master/EH_scrape_functions.R")

#####
#Scrape each season
#####
## Scrape games
#Season ends early
games_vec <- c(as.character(seq(2019020001, 2019021083, by = 1)))
pbp19 <- sc.scrape_pbp(games = games_vec)
#save(pbp19, file = "EW Scraper 2019.Rda")

## Scrape games
games_vec <- c(as.character(seq(2018020001, 2018021272, by = 1)))
pbp18 <- sc.scrape_pbp(games = games_vec)
#save(pbp18, file = "EW Scraper 2018.Rda")

## Scrape games
#First year with Vegas = more games
games_vec <- c(as.character(seq(2017020001, 2018021272, by = 1)))
pbp17 <- sc.scrape_pbp(games = games_vec)
#save(pbp17, file = "EW Scraper 2017.Rda")

## Scrape games
games_vec <- c(as.character(seq(2016020001, 2016021231, by = 1)))
pbp16 <- sc.scrape_pbp(games = games_vec)
#save(pbp16, file = "EW Scraper 2016.Rda")

## Scrape games
games_vec <- c(as.character(seq(2015020001, 2015021231, by = 1)))
pbp15 <- sc.scrape_pbp(games = games_vec)
#save(pbp15, file = "EW Scraper 2015.Rda")

## Scrape games
games_vec <- c(as.character(seq(2014020001, 2014021231, by = 1)))
pbp14 <- sc.scrape_pbp(games = games_vec)
#save(pbp14, file = "EW Scraper 2014.Rda")

## Scrape games
games_vec <- c(as.character(seq(2013020001, 2013021231, by = 1)))
pbp13 <- sc.scrape_pbp(games = games_vec)
#save(pbp13, file = "EW Scraper 2013.Rda")

## Scrape games
games_vec <- c(as.character(seq(2012020001, 2012021231, by = 1)))
pbp12 <- sc.scrape_pbp(games = games_vec)
#save(pbp12, file = "EW Scraper 2012.Rda")

## Scrape games
games_vec <- c(as.character(seq(2011020001, 2011021231, by = 1)))
pbp11 <- sc.scrape_pbp(games = games_vec)
#save(pbp11, file = "EW Scraper 2011.Rda")

## Scrape games
games_vec <- c(as.character(seq(2010020001, 2010021231, by = 1)))
pbp10 <- sc.scrape_pbp(games = games_vec)
#save(pbp10, file = "EW Scraper 2010.Rda")

## Scrape games
games_vec <- c(as.character(seq(2009020001, 2009021231, by = 1)))
pbp09 <- sc.scrape_pbp(games = games_vec)
#save(pbp09, file = "EW Scraper 2009.Rda")

## Scrape games
games_vec <- c(as.character(seq(2008020001, 2008021231, by = 1)))
pbp08 <- sc.scrape_pbp(games = games_vec)
#save(pbp08, file = "EW Scraper 2008.Rda")


#####
#Scrape games that had connectivity issues when I first pulled 08
#####

#Pull games
error_games <- c(2008020474, 2008020475, 2008020476, 2008020598, 2008020725, 2008021022, 2008021116)
extra08 <- sc.scrape_pbp(games = error_games)

#Convert game IDs to numeric and combine dataframs
pbp08_fixed <- pbp08
extra08_numeric <- extra08
#new_08 <- list()
for (i in 1:9){
  pbp08_fixed[[i]]$game_id <- as.numeric(pbp08_fixed[[i]]$game_id)
  extra08_numeric[[i]]$game_id <- as.numeric(extra08_numeric[[i]]$game_id)
  pbp08_fixed[[i]] <- bind_rows(pbp08_fixed[[i]], extra08_numeric[[i]])
}

#Check dataframe size
nrow(pbp08_fixed$game_info_df)
nrow(pbp08$game_info_df)
30*41

#Save results
pbp08 <- pbp08_fixed
#save(pbp08, file = "EW Scraper 2008.Rda")



#Checked 2009 and it loooks good, 1,227 games, just missing 3 dead ones
nrow(pbp10$game_info_df) # Lookos goood, just missing the 1 dead one
nrow(pbp11$game_info_df) # Complete
nrow(pbp12$game_info_df) #720 games because of lockout
nrow(pbp13$game_info_df) # Complete
nrow(pbp14$game_info_df) # Complete
nrow(pbp15$game_info_df) # Complete
nrow(pbp16$game_info_df) # Complete
nrow(pbp17$game_info_df) # Complete
nrow(pbp18$game_info_df) # Complete
nrow(pbp19$game_info_df) # Complete, 1 dead game and partial season

#2007 has 2 dead games


games_vec <- c(as.character(seq(2007020001, 2007021231, by = 1)))
pbp07 <- sc.scrape_pbp(games = games_vec)
#save(pbp07, file = "EW Scraper 2007.Rda")

nrow(pbp07$game_info_df) # 1225, want 1230 - missing the 3 above + 2 dead ones
error_games07 <- c(2007020536, 2007020537, 2007020538)
extra07 <- sc.scrape_pbp(games = error_games07)

#Convert game IDs to numeric and combine dataframs
pbp07_fixed <- pbp07
extra07_numeric <- extra07
for (i in 1:9){
  pbp07_fixed[[i]]$game_id <- as.numeric(pbp07_fixed[[i]]$game_id)
  extra07_numeric[[i]]$game_id <- as.numeric(extra07_numeric[[i]]$game_id)
  pbp07_fixed[[i]] <- bind_rows(pbp07_fixed[[i]], extra07_numeric[[i]])
}
nrow(pbp07_fixed$game_info_df)
pbp07 <- pbp07_fixed
#save(pbp07, file = "EW Scraper 2007.Rda")



#####
#Join all seasons to get a single, somewhat clean pbp
#####
#Need Game ID to be numeric
for (i in 1:9){
  pbp09[[i]]$game_id <- as.numeric(pbp09[[i]]$game_id)
  pbp10[[i]]$game_id <- as.numeric(pbp10[[i]]$game_id)
  pbp11[[i]]$game_id <- as.numeric(pbp11[[i]]$game_id)
  pbp12[[i]]$game_id <- as.numeric(pbp12[[i]]$game_id)
  pbp13[[i]]$game_id <- as.numeric(pbp13[[i]]$game_id)
  pbp14[[i]]$game_id <- as.numeric(pbp14[[i]]$game_id)
  pbp15[[i]]$game_id <- as.numeric(pbp15[[i]]$game_id)
  pbp16[[i]]$game_id <- as.numeric(pbp16[[i]]$game_id)
  pbp17[[i]]$game_id <- as.numeric(pbp17[[i]]$game_id)
  pbp18[[i]]$game_id <- as.numeric(pbp18[[i]]$game_id)
  pbp19[[i]]$game_id <- as.numeric(pbp19[[i]]$game_id)
}

#Combine
pbp <- bind_rows(pbp07$pbp_base, pbp08$pbp_base, pbp09$pbp_base, pbp10$pbp_base, pbp11$pbp_base,
                 pbp12$pbp_base, pbp13$pbp_base, pbp14$pbp_base, pbp15$pbp_base, pbp16$pbp_base,
                 pbp17$pbp_base, pbp18$pbp_base, pbp19$pbp_base)

#Exploratory checks
str(pbp)
count(pbp, season)
group_by(pbp, season) %>% summarise(games = n_distinct(game_id))
events_per_game <- count(pbp, season, game_id)
count(pbp, event_index)

#Clean based on exploration
pbp <- pbp %>%
  mutate(season = as.numeric(season),
         game_date = ymd(game_date)) %>%
  arrange(game_id, event_index)
#save(pbp, file = "All PBP.Rda")

games <- bind_rows(pbp07$game_info_df, pbp08$game_info_df, pbp09$game_info_df, pbp10$game_info_df, pbp11$game_info_df,
                 pbp12$game_info_df, pbp13$game_info_df, pbp14$game_info_df, pbp15$game_info_df, pbp16$game_info_df,
                 pbp17$game_info_df, pbp18$game_info_df, pbp19$game_info_df) %>% 
  mutate(season = as.numeric(season),
         game_date = ymd(game_date)) %>%
  arrange(game_id)
#save(games, file = "All Games Info.Rda")

#Get roster data of who played and how they did from 2 separate tables
roster09 <- inner_join(pbp09$roster_df, pbp09$events_summary_df)
roster10 <- inner_join(pbp10$roster_df, pbp10$events_summary_df)
roster11 <- inner_join(pbp11$roster_df, pbp11$events_summary_df)
roster12 <- inner_join(pbp12$roster_df, pbp12$events_summary_df)
roster13 <- inner_join(pbp13$roster_df, pbp13$events_summary_df)
roster14 <- inner_join(pbp14$roster_df, pbp14$events_summary_df)
roster15 <- inner_join(pbp15$roster_df, pbp15$events_summary_df)
roster16 <- inner_join(pbp16$roster_df, pbp16$events_summary_df)
roster17 <- inner_join(pbp17$roster_df, pbp17$events_summary_df)
roster18 <- inner_join(pbp18$roster_df, pbp18$events_summary_df)
roster19 <- inner_join(pbp19$roster_df, pbp19$events_summary_df)

rosters <- bind_rows(roster09, roster10, roster11,roster12, roster13,roster14, roster15,roster16, roster17,roster18, roster19) %>% 
  mutate(season = as.numeric(season),
         game_date = ymd(game_date)) %>%
  arrange(game_id)
#save(rosters, file = "All Roster Info.Rda")


