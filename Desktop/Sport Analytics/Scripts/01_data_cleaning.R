########################################################################################

# Script reads in match data files joins together in to one data set and cleans data
# adding a number of variables

# Clear environment #####
rm(list = ls())

# Read in libraries #####
library(expm)
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(kableExtra)
library(formattable)
library(purrr)
library(markovchain)
library(gplots)
library(RColorBrewer)


# Data Transformations ####
# Specify the pattern of the files
pattern <- "^match_file"

# List all .xlsx files in the current directory that start with the pattern
files <- list.files(path = "data", pattern = paste0(pattern, ".*\\.xls$"), full.names = TRUE)

# list of actions that are not possession ending
not_ending_action = c('Free','Kickout','Hand Pass','Carry','Kick Pass','Sideline','Free 45','Stop')
pos_end_outcome = c('TO','Point','Goal','Wide')
pos_end_action = c('Shot','TO','Shot Point','Shot Goal')


# Read in each file, extract team names and combine them into a single data frame
all_match_data <- map_df(files, ~ {
  file_parts <- strsplit(.x, "_")[[1]]
  team_a <- file_parts[4]
  team_b <- file_parts[6]
  game_id <- as.integer(file_parts[3])
  
  data <- read_excel(.x)
  data <- data %>%
    mutate(game_id = game_id,  # Add game_id column to the data
           minutes = timestamp %/%60,
           seconds = round(timestamp %% 60, 0),
           team_defend_name = case_when(
             team_attack == 'Team A' ~ team_b,
             team_attack == 'Team B' ~ team_a,
             TRUE ~ team_attack
           ),
           team_attack_name = case_when(
             team_attack == 'Team A' ~ team_a,
             team_attack == 'Team B' ~ team_b,
             TRUE ~ team_attack  # Keep original value if it's neither 'Team A' nor 'Team B'
           ))
  
  
  # Create kickout start zone rows
  kickout_starts <- data %>%
    filter(action == "Kickout") %>%
    mutate(zone = 1,  # Starting zone for kickouts
           action = "Kickout Start",
           timestamp = timestamp - 1)  # Adjust timestamp slightly before actual kickout
  
  # Append kickout start rows to the main data
  data <- bind_rows(data, kickout_starts) %>%
    arrange(game_id, timestamp) 
  
  # Resetting ID field for each game
  data <- data %>%
    group_by(game_id) %>%
    mutate(id = row_number()) %>%
    ungroup()
  
  data
})


# replace NA values
all_match_data <- all_match_data %>%
  mutate(across(.cols = c(team_attack, action, outcome, shot_pressure), ~na_if(.x, "NA")))


# add previous & next zones, zone change & team change logic check
all_match_data <- all_match_data %>%
  arrange(game_id, timestamp) %>%  # Ensure data is ordered by game_id and timestamp
  group_by(game_id) %>%
  mutate(score = 
           case_when(
             outcome == 'Point' ~ 1,
             outcome == 'Goal' ~ 3,
             TRUE ~ 0),
         zone_change = coalesce(if_else(lag(zone) != zone,1,0),0),
         team_change = coalesce(if_else(lag(team_attack) != team_attack,1,0),0),
         previous_action = case_when(lag(game_id) == game_id & team_change == 0 ~ lag(action),
                                     TRUE ~ 'NA'),
         next_action = lead(action),
         previous_zone = case_when(lag(game_id) == game_id & team_change == 0 ~ lag(zone),
                                   TRUE ~ 0),
         previous_outcome = lag(outcome),
         next_zone = lead(zone),
         direction = case_when(
           # zone 1 to any other zone is forwards
           previous_zone == 1 ~ "forwards",
           # for zones 2 to 4
           previous_zone %in% 2:4 & zone > previous_zone ~ "forwards",
           previous_zone %in% 2:4 & zone < previous_zone ~ "backwards",
           # for zones 5, 8, 11, 14
           previous_zone %in% c(5, 8, 11, 14) & zone == previous_zone + 1 ~ "right",
           previous_zone %in% c(5, 8, 11, 14) & zone == previous_zone + 2 ~ "right",
           previous_zone %in% c(5, 8, 11, 14) & zone > previous_zone + 2 ~ "forwards",
           previous_zone %in% c(5, 8, 11, 14) & zone < previous_zone ~ "backwards",
           # for zones 6, 9, 12, 15
           previous_zone %in% c(6, 9, 12, 15) & zone == previous_zone - 1 ~ "left",
           previous_zone %in% c(6, 9, 12, 15) & zone == previous_zone + 1 ~ "right",
           previous_zone %in% c(6, 9, 12, 15) & zone > previous_zone + 2 ~ "forwards",
           previous_zone %in% c(6, 9, 12, 15) & zone < previous_zone - 2 ~ "backwards",
           # for zones 7, 10, 13, 14
           previous_zone %in% c(7, 10, 13) & zone == previous_zone - 1 ~ "left",
           previous_zone %in% c(7, 10, 13) & zone == previous_zone - 2 ~ "left",
           previous_zone %in% c(7, 10, 13, 14) & zone > previous_zone ~ "forwards",
           previous_zone %in% c(7, 10, 13, 14) & zone < previous_zone - 2 ~ "backwards",
           # for zone 16
           previous_zone == 16 & zone < previous_zone ~ "backwards",
           # same zone
           previous_zone == zone ~ "same",
           # all other cases
           TRUE ~ "NA"
         ),
         possession_number = 1 + cumsum(team_change),
  ) %>%
  ungroup() %>%
  group_by(game_id, possession_number) %>%
  arrange(game_id, id) %>%
  mutate(action_number = row_number()) %>%
  ungroup()


# Calculate game state and add column to dataset
all_match_data <- all_match_data %>%
  arrange(game_id, timestamp) %>%
  group_by(game_id) %>%
  mutate(
    cum_teamA_score = cumsum(if_else(team_attack == "Team A", score, 0)),
    cum_teamB_score = cumsum(if_else(team_attack == "Team B", score, 0))
  ) %>% 
  rowwise() %>%
  mutate(
    game_state = case_when(
      team_attack == "Team A" & cum_teamA_score > cum_teamB_score ~ "Winning",
      team_attack == "Team A" & cum_teamA_score == cum_teamB_score ~ "Drawing",
      team_attack == "Team A" & cum_teamA_score < cum_teamB_score ~ "Losing",
      team_attack == "Team B" & cum_teamB_score > cum_teamA_score ~ "Winning",
      team_attack == "Team B" & cum_teamB_score == cum_teamA_score ~ "Drawing",
      team_attack == "Team B" & cum_teamB_score < cum_teamA_score ~ "Losing",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()


# Filter out team changes and shots/TOs which are end and start of moves
match_file_zone_prob <- all_match_data %>%
  filter(team_attack != 'No team',
         zone_change == 1,
         team_change == 0,
         possession_number != 1 & action_number != 1)


rm(files, not_ending_action, pattern, pos_end_action, pos_end_outcome)

